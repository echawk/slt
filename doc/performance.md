# Performance: ideas to come back to

This started from one report: on macOS, `mpv --vo=kitty` in one window makes
`ls` in another window of the same process visibly slow.

## What windows share

Each window parses its output on its own reader thread (`reader-loop` in
`src/emulator.lisp`), and engine state and locks belong to one emulator.
Two things are shared by every window in the process:

- **The main thread.** On Cocoa every window renders on the one run loop:
  display-link callbacks, `render-emulator` and `redraw-cocoa-view`. A window
  with a heavy frame delays every other window's frame.
- **The SBCL heap.** Garbage collection stops every thread, including the
  other windows' readers and the main thread. A window that allocates a lot
  makes every window pause.

## Measuring

`slt/bench` plays video the way mpv sends it (`bench/images.lisp`): each
frame moves the cursor home and transmits a new RGB image, with no id, in
4096-octet base64 chunks, over the last one. The frames go through the
emulator as child output does, and each is followed by a render.

    scripts/lisp --eval '(asdf:load-system :slt/bench)' \
                 --eval '(slt/bench:run-image-stream)'

At 1280x720 on an M-series Mac, before and after the changes below:

|                        | before  | after   |
|------------------------|---------|---------|
| frames per second      | 8.3     | 86      |
| feeding, per frame     | 120 ms  | 11.5 ms |
| consed, per frame      | 17.1 MB | 4.2 MB  |
| images left to draw    | 91      | 1       |

Garbage collection took 0.4% of the time. Rendering, which snapshots the
images for the frontend, took 0.1 ms per frame. Output captured from mpv
itself (`mpv --vo=kitty --untimed`, a 1280x720 test pattern) goes through
at 78 frames per second, with the same 4.3 MB per frame.

## Done

- **Find the end of control strings with SIMD.** The ecma48 scanner looked
  for the end of an APC, DCS or OSC payload with a generic `position-if`,
  one octet at a time: about 85 ms of each 720p frame. It now skips to the
  next control with `scan-text`, the vector scan used for text.
- **Decode Kitty images with typed loops.** Base64 is decoded a whole
  quartet at a time, into a vector of the exact size (it used to be copied
  once more to trim it), and RGB becomes RGBA in a typed loop.
- **Decode Kitty pixels as they arrive.** When the first chunk gives plain
  RGB or RGBA pixels and their size, sent directly and uncompressed, each
  chunk's base64 is decoded straight into the image's RGBA. The
  interceptor hands the decoder each APC payload in place, as a vector
  displaced to the scanner's octets, instead of a copy. A 720p frame used
  to be copied four times on the way (the payload, its base64, the RGB and
  the RGBA); now only the RGBA is made. Other images are still joined and
  decoded at the last chunk.
- **Skip images hidden under an opaque one.** mpv never deletes its frames,
  so they pile up until the 320 MB quota evicts the oldest, as in kitty.
  `terminal-image-render-list` leaves out records an opaque record drawn
  later covers entirely; RGB images are opaque, and others are checked once
  per frame of pixels. Every frontend draws one image per frame instead of
  ninety, and Metal and GL keep one texture instead of ninety.
- **Reuse image textures.** Metal and GL keep up to four retired image
  textures and write a new image of the same size into one of them instead
  of creating a texture. Metal takes a spare only once the frames that may
  still read it have finished, and releases spares no image has wanted for
  120 frames.
- **Send image pixels to the browser once.** The web frontend resent every
  image's pixels, base64 in JSON, whenever the grid changed with an image on
  screen, and resent all ninety stacked frames per video frame. Each client
  is now sent only pixels it does not have, and the client decodes each
  image once.

## Still to do

- **Free covered video frames.** The frames hidden under the newest one stay
  stored until the quota evicts them: about 320 MB per window playing video.
  Freeing an anonymous image once an opaque one covers all its placements
  would save that, but deleting the newer placement would then no longer
  reveal the one under it, as it does in kitty.
- **Upload off the main thread.** A new frame is still copied into its
  texture with `replaceRegion:` on the main thread. Staging it in a shared
  `MTLBuffer` and blitting it would leave the copy to the GPU.
- **Measure the real app.** How long `nextDrawable` in `metal-draw` blocks
  on the main thread, and how long other windows wait for a frame, while mpv
  plays.
- **Render off the main thread.** `CAMetalLayer` can be drawn from a
  background thread. Per-window render threads would leave only event
  handling and layer setup on the main thread. This is the largest change
  and should wait until the cheaper ideas above have been measured.
