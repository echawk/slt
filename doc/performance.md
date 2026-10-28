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

At 1280x720 on an M-series Mac, at first and now:

|                        | at first | now     |
|------------------------|----------|---------|
| frames per second      | 8.3      | 345     |
| feeding, per frame     | 120 ms   | 2.9 ms  |
| consed, per frame      | 17.1 MB  | 4.1 MB  |
| images stored          | 91       | 1       |
| images left to draw    | 91       | 1       |

Rendering, which snapshots the images for the frontend, takes 0.03 ms per
frame. Output captured from mpv itself (`mpv --vo=kitty --untimed`, a
1280x720 test pattern) goes through the same way.

**The real app.** With timers around the Cocoa frame work while a window
plays that capture as fast as `cat` sends it, the main thread spends a
mean 0.05 ms per display-link tick, 0.44 ms at the 95th percentile and
3.4 ms at most. Drawing a frame takes 0.44 ms, of which the image upload
(`replaceRegion:`) is 0.30 ms and waiting for `nextDrawable` under
0.1 ms. Other windows have little to wait for.

**Memory.** bin/slt's physical footprint (`vmmap -summary`) with one
lispvt window, before and after playing the capture three times:

|                          | at first | now        |
|--------------------------|----------|------------|
| idle                     | 188 MB   | 178 MB     |
| after the video          | 635 MB   | 250-300 MB |
| after a full collection  | 411 MB   | 194 MB     |
| images stored            | 221 MB   | 3.7 MB     |

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
  so they piled up until the 320 MB quota evicted the oldest, as in kitty.
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
- **Free video frames hidden for good.** A new opaque anonymous image placed with the same geometry as an older
  anonymous one of the same size now frees it: the two are drawn alike at
  any cell size and every delete command or scroll removes both, so
  nothing could show the older one again, as in kitty.
- **Decode base64 16 digits at a time.** `lib/simd-base64` classifies 16
  digits, maps them to sextets and repacks them as four RGBA pixels in
  vector registers (NEON on arm64, SSE2 on x86-64). kitty-graphics also
  parses each chunk's controls from its octets instead of a string copy.
- **Start glyph atlases small.** They were 2048 pixels square from the
  start, plus a 16 MB color atlas after one emoji, per window and again on
  the GPU. They now start 256 high and double, keeping their glyphs.
- **Collect garbage sooner.** SBCL lets 5% of the heap (215 MB of 4 GB)
  be allocated between collections. bin/slt sets 64 MB, which costs no
  throughput in either benchmark.

## Measured and left alone

- **Upload off the main thread.** The upload is 0.3 ms of a 720p frame;
  staging it through a shared `MTLBuffer` would save little.
- **Render off the main thread.** The main thread is idle nearly all the
  time while video plays, so per-window render threads are not needed.
- **Reuse frame buffers.** A tenth of feeding is the kernel zeroing each
  new 3.7 MB frame. Reusing a freed frame's vector would race with the
  frontend still drawing it.
- **Opacity of RGBA frames.** Checking every alpha of a 720p frame takes
  0.4 ms, once per frame.

## Still to do

- **Scrollback.** lispvt keeps each scrollback line at full width, 14
  octets a cell: 22 MB for 10,000 lines of 160 columns, mostly blank.
  Trimming lines to their text as they enter the scrollback would save
  most of it, but the lines it evicts could then no longer be reused, so
  every scroll would allocate. (The ghostty engine keeps its own.)
