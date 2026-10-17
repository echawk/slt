# GPU backends

slt has two GPU backends that share everything above the graphics API:

| backend | window | renderer | glyphs |
|---|---|---|---|
| `cocoa` (`src/macos`) | AppKit | Metal | CoreText |
| `gl` (`src/gl`) | GLFW 3.4: X11 or Wayland, also macOS | OpenGL 3.3 core or OpenGL ES 3.0 | FreeType with font-discovery; CoreText on macOS |

## What is shared

- **Grid** (`src/grid.lisp`). The engine copies only changed rows into typed
  arrays: codepoints, tagged colors and attribute bits.
- **Glyph atlas and instance packing** (`src/gpu.lisp`).
  - Glyphs are rasterized once into an alpha-only atlas of cell-sized slots.
  - A wide glyph takes two adjacent slots.
  - Each cell becomes 4 × u32: foreground RGB, background RGB, atlas slot and
    decoration flags.
  - Only dirty rows are repacked, and the atlas grows if one screen overflows
    it.
- **Frontend** (`src/frontend.lisp`). A reader thread parses PTY output
  continuously. The UI thread renders at most once per frame through
  `backend-render-grid`.

A backend provides a window with input events, a rasterizer for the atlas
(a function that draws a string's coverage into an atlas rectangle), uploads
of the atlas and the instance array, and one instanced draw call.

## Rendering: one draw call per frame

Both renderers draw six vertices per instance, one instance per cell, with no
vertex buffer:

- The vertex stage places the cell from the instance index. It looks up the
  glyph's slot in the atlas and passes the colors and flags through.
- The fragment stage mixes background and foreground by the atlas coverage,
  then draws underline, double underline, strikethrough, and bar or underline
  cursors from the flags.

The shaders are `*metal-shader-source*` in `src/macos/metal.lisp` and
`*gl-vertex-shader-source*` / `*gl-fragment-shader-source*` in
`src/gl/gl-renderer.lisp`. The GLSL is written once and given a
`#version 330 core` or `#version 300 es` header.

Per frame, the OpenGL renderer:

- re-specifies the instance buffer (`glBufferData` with no data, then
  `glBufferSubData`), so the driver never waits on the previous frame;
- uploads the atlas rows that `glyph-atlas-take-dirty` reports with
  `glTexSubImage2D`, into an `R8` texture read with `texelFetch`;
- issues one `glDrawArraysInstanced`.

A persistently mapped ring of buffers with fences, like the Metal backend's
three frames in flight, would save a copy. It is not worth it yet: a 200 × 60
screen is 192 KB per frame.

`snapshot-gl-view` and `snapshot-cocoa-view` render offscreen and return or
write the pixels, which the smoke tests check.

## X11 or Wayland?

**The graphics APIs themselves don't care.** OpenGL and Vulkan only draw into
a surface. The window system matters in four places: creating the window and
surface, keyboard input, the clipboard, and HiDPI scale. GLFW covers all four
and picks X11 or Wayland at run time from the same binary, on Linux and on
FreeBSD/OpenBSD:

| | X11 | Wayland |
|---|---|---|
| OpenGL context | GLX or EGL | EGL |
| Vulkan surface | `VK_KHR_xlib_surface` / `xcb_surface` | `VK_KHR_wayland_surface` |
| Keyboard | XKB | `wl_keyboard` + xkbcommon |
| Clipboard | selections | `wl_data_device` |

The `gl` backend asks for OpenGL 3.3 core first. Off macOS it falls back to
OpenGL ES 3.0 through EGL (`*gl-api*` forces either). cl-opengl resolves
entry points through `glfwGetProcAddress`, which works for GLX, EGL and CGL
alike.

Shirakumo's [glfw](https://codeberg.org/shirakumo/glfw) bindings ship GLFW
3.4 binaries for Linux (amd64, arm64) and macOS. The BSDs load the system's
`libglfw`.

## Glyphs on Linux and the BSDs

`src/gl/freetype.lisp`:

- [font-discovery](https://codeberg.org/shinmera/font-discovery) (fontconfig)
  finds the family's regular, bold and italic files. In a collection
  (`.ttc`), the face whose family and style match is chosen.
- FreeType renders coverage into the atlas. It synthesizes bold or italic
  when the family has no such face.
- Characters the family lacks come from the first font in
  `*font-fallback-families*`, then any installed font, that has them. Results
  are cached per character.
- Without shaping, a combining mark is drawn over its base. A zero-advance
  mark follows the base's advance. A mark with its own advance (as in Menlo)
  is drawn at the base's origin.

On macOS the `gl` backend uses the CoreText rasterizer instead. font-discovery
c6e4728 aborts there, because it reads `kCTFontFormatAttribute`, a CFNumber,
as a string.

## Testing

- `make gl-test` (`scripts/gl-test`, `t/gl-snapshot.lisp`) runs a shell that
  prints styled text in a hidden window. It renders offscreen, checks the
  background, glyphs, an SGR background and box drawing, and leaves
  `build/gl-test/snapshot.ppm`.
- On Linux CI, run it under `xvfb-run` for X11, or with `weston
  --backend=headless` and `WAYLAND_DISPLAY` set for Wayland.

## Still to do

1. Run the `gl` backend on real Linux and BSD desktops, X11 and Wayland, and
   in CI.
2. HarfBuzz shaping for combining marks, a color atlas for emoji, subpixel
   positioning, and ligatures if wanted.
3. Check the Linux build (`make` includes `slt/gl` there): Deploy carries
   `libglfw`, while FreeType and fontconfig stay system libraries.
4. Vulkan, if profiling or MoltenVK parity justifies it. It adds a lot of
   setup (instance, device, swapchain, recreation on resize) for little gain
   on a workload this small. The same GLSL compiles to SPIR-V, and the
   instance array would bind as a storage buffer.
