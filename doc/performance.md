# Performance: ideas to come back to

None of this is measured yet. It comes from reading the code after one
report: on macOS, `mpv --vo=kitty` in one window makes `ls` in another window
of the same process visibly slow.

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

## Measure first

- Log `sb-ext:*gc-run-time*` and the bytes consed per second while mpv plays,
  to see how much of the stall is GC.
- Time `render-emulator` and `redraw-cocoa-view` per window on the main
  thread.
- Time how long `nextDrawable` in `metal-draw` blocks. When the GPU falls
  behind it waits for a free drawable, and it waits on the main thread.

## Ideas

- **Reuse image textures** (`metal-image-texture`, `src/macos/metal.lisp`).
  The cache hits only when the RGBA vector is `eq`, and every video frame
  comes with a new vector. Each frame therefore allocates a new `MTLTexture`
  and copies it synchronously with `replaceRegion:` on the main thread:
  about 8 MB per frame at 1080p. When the image id and size are unchanged,
  write into the existing texture instead, or stage the copy through a
  shared `MTLBuffer` with a blit so the main thread does not copy.
- **Allocate less per frame.** Kitty and SIXEL decoding allocate a new RGBA
  vector per frame (`kitty-command-rgba`, the canvas in animation
  compositing, scaling in `src/images.lisp`), plus the base64 and zlib
  buffers. Reusing buffers for an image id whose size has not changed would
  reduce GC frequency for every window.
- **Snapshot images more cheaply.** Every render that touches images builds
  a new store with `terminal-image-display-store` under the engine lock.
  Copying only the placements that changed would shorten both the copy and
  the time the lock is held.
- **Bound one window's share of a frame.** A window flooded with output can
  keep the main thread busy for its whole frame. Ways to cap that: drop
  intermediate image frames when a newer one is already pending, and skip
  the redraw when a newer frame is queued.
- **Render off the main thread.** `CAMetalLayer` can be drawn from a
  background thread. Per-window render threads would leave only event
  handling and layer setup on the main thread. This is the largest change
  and should wait until the cheaper ideas above have been measured.
