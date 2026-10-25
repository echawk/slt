# GPU frontends

slt has two GPU frontends that share everything above the graphics API:

| frontend | window | renderer | glyphs |
|---|---|---|---|
| `cocoa` (`src/macos`) | AppKit | Metal | CoreText |
| `gl` (`src/gl`) | GLFW 3.3 or later: X11 or Wayland, also macOS | OpenGL 3.3 core or OpenGL ES 3.0 | FreeType with font-discovery; CoreText on macOS |

## What is shared

- **Grid** (`src/grid.lisp`). The engine copies only changed rows into typed
  arrays: codepoints, tagged colors and attribute bits.
- **Glyph atlas and instance packing** (`src/gpu.lisp`).
  - Glyphs are rasterized once into an alpha-only atlas of cell-sized slots.
  - A wide glyph takes two adjacent slots.
  - Each cell becomes 4 × u32: foreground RGB, background RGB, the atlas
    slot (its top bit marks the default background), and decoration flags
    in the low 8 bits under the underline's RGB.
  - Only dirty rows are repacked, and the atlas grows if one screen overflows
    it.
- **Emulator** (`src/emulator.lisp`). A reader thread parses PTY output
  continuously. The UI thread renders at most once per frame through
  `frontend-render-grid`.

A frontend provides a window with input events, a rasterizer for the atlas
(a function that draws a string's coverage into an atlas rectangle), uploads
of the atlas and the instance array, and one instanced draw call.

## Rendering: one draw call per frame

Both renderers draw six vertices per instance, one instance per cell, with no
vertex buffer:

- The vertex stage places the cell from the instance index. It looks up the
  glyph's slot in the atlas and passes the colors and flags through.
- The fragment stage mixes background and foreground by the atlas coverage,
  then draws strikethrough, the underline in its own color (single, double,
  curly, dotted or dashed; patterns follow the window's x so they continue
  across cells), and bar or underline cursors from the flags.

Images with a negative z-index lie under the text. Then the cells are drawn
twice instead of once, with blending: first only the backgrounds (the
default one transparent), then only the text and decorations. Images with
z below -1073741824 are drawn before the backgrounds, other negative ones
between the two passes, and the rest after.

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

The Metal frontend draws from a `CADisplayLink` (macOS 14 and later): renders
requested between refreshes (`frontend-request-frame`) run just before the
next one, and the grid and images a frame changes are drawn together, so
there is one present per refresh at the display's own cadence. The link is
paused while nothing changes. Elsewhere renders are paced by a timer
(`*frame-interval-ms*`); GL presents with vsync, which paces it.

For smooth scrolling (`smooth-scroll.lisp`) the emulator hands the frontend
a grid with one extra row, the scrollback row above the view, and a pixel
offset (`frontend-set-smooth-offset`). The Metal renderer moves the cells
and images down by it and clips them to the grid's rows in the fragment
shaders.

A persistently mapped ring of buffers with fences, like the Metal frontend's
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

The `gl` frontend asks for OpenGL 3.3 core first. Off macOS it falls back to
OpenGL ES 3.0 through EGL (`*gl-api*` forces either). cl-opengl resolves
entry points through `glfwGetProcAddress`, which works for GLX, EGL and CGL
alike.

[cl-glfw3](https://github.com/AlexCharlton/cl-glfw3) loads the system's
`libglfw`, which is built for its C library (musl or glibc), so the frontend
needs GLFW 3.3 or later installed. A few calls and window hints the frontend
uses are missing from cl-glfw3 (`glfwWaitEventsTimeout`, `glfwGetKeyName`,
the drop and content-scale callbacks, EGL context creation, scaling to the
monitor); `src/gl/gl-frontend.lisp` defines them itself.

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

On macOS the `gl` frontend uses the CoreText rasterizer instead. font-discovery
c6e4728 aborts there, because it reads `kCTFontFormatAttribute`, a CFNumber,
as a string.

## Testing

- `make gl-test` (`scripts/gl-test`, `t/gl-snapshot.lisp`) runs a shell that
  prints styled text in a hidden window. It renders offscreen, checks the
  background, glyphs, an SGR background and box drawing, and leaves
  `build/gl-test/snapshot.ppm`.
- On Linux CI, run it under `xvfb-run` for X11, or with `weston
  --frontend=headless` and `WAYLAND_DISPLAY` set for Wayland.

## Still to do

1. Run the `gl` frontend on real Linux and BSD desktops, X11 and Wayland, and
   in CI.
2. HarfBuzz shaping for combining marks, a color atlas for emoji, subpixel
   positioning, and ligatures if wanted.
3. Check the Linux build (`make` includes `slt/gl` there): GLFW, FreeType
   and fontconfig stay system libraries.
4. Vulkan, if profiling or MoltenVK parity justifies it. It adds a lot of
   setup (instance, device, swapchain, recreation on resize) for little gain
   on a workload this small. The same GLSL compiles to SPIR-V, and the
   instance array would bind as a storage buffer.
