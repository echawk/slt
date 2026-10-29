# GPU frontends

slt's GPU frontends share everything above the graphics API:

| frontend | window | renderer | glyphs |
|---|---|---|---|
| `cocoa` (`src/macos`) | AppKit | Metal | CoreText |
| `gl` (`src/gl`) | GLFW 3.3 or later: X11 or Wayland, also macOS | OpenGL 3.3 core or OpenGL ES 3.0 | FreeType with font-discovery; CoreText on macOS |
| `x11` (`src/x11`) | the `clx` frontend's, through CLX | OpenGL 3.3 core or OpenGL ES 3.0 through EGL, or Vulkan | FreeType with font-discovery |

## What is shared

- **Grid** (`src/grid.lisp`). The engine copies only changed rows into typed
  arrays: codepoints, tagged colors and attribute bits.
- **Glyph atlas and instance packing** (`src/gpu.lisp`).
  - Glyphs are rasterized once into an alpha-only atlas of cell-sized slots.
  - Color glyphs (emoji) go into the same slots of a second, RGBA atlas,
    made when the first one appears. A rasterizer returns a color glyph's
    premultiplied RGBA pixels instead of drawing coverage.
  - A wide glyph takes two adjacent slots.
  - Each cell becomes 4 × u32: foreground RGB, background RGB, the atlas
    slot (its top bit marks the default background, the next one a color
    glyph), and decoration flags in the low 8 bits under the underline's
    RGB.
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
  or lays a color glyph over the background, then draws strikethrough, the underline in its own color (single, double,
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

## Vulkan

`slt/vulkan` (`src/vulkan/vulkan-renderer.lisp`) draws the same frames as
the OpenGL renderer, from the same packed cells and atlases:

- **Bindings.** It uses [`lib/vulkan-cffi`](../lib/vulkan-cffi/), which is
  generated when compiled from the Vulkan registry (`vk.xml`). The renderer
  needs only Vulkan 1.0 plus surfaces and swapchains.
- **Drawing.** Each cell is six vertices of one instanced draw, as in GL.
  The instance array is a vertex buffer read per instance, and push
  constants take the place of the uniforms. The cells' shaders have the
  GL fragment shader's logic, with Vulkan's y axis pointing down.
- **Shaders.** They live in `src/vulkan/shaders/` and are compiled to
  SPIR-V with `make shaders` (glslc). The `.spv` files are committed and
  read in when the renderer is compiled.
- **Frames.** One frame is in flight at a time. Each waits for the last on
  a fence, so atlas staging, the instance buffer and image uploads reuse
  their memory without further synchronization. Glyph atlas rows are copied
  in as GL's are, only the dirty ones. Images of the same size reuse a
  retired texture, as in GL.
- **Targets.** The renderer presents to a swapchain on a surface its host
  makes. It renders offscreen (`vulkan-snapshot`) with or without one. The
  target format is `B8G8R8A8_UNORM` for both, so blending matches the other
  renderers.

**Hosts.** `slt/glfw-vulkan` draws the `gl` frontend's GLFW window with
Vulkan when `*gl-api*` is `:vulkan`:

- GLFW makes a window without a context and names the instance extensions
  its surfaces need (Xlib or xcb, Wayland, Metal), and makes the surface.
- GLFW is handed slt's Vulkan loader (`glfwInitVulkanLoader`, GLFW 3.4+),
  because it does not find Homebrew's on its own.
- On macOS this runs on MoltenVK, with portability enumeration.

The renderer knows nothing of GLFW, so other hosts can follow, such as the
Cocoa frontend's `CAMetalLayer`, through `VK_EXT_metal_surface`.

## Windows without GLFW

`slt/gpu-window` (`src/gpu-window`) draws a window a frontend made itself
with the API `*gl-api*` names (`make-window-renderer`):

- **OpenGL** through EGL 1.5 (`lib/egl-cffi`): a display for the window
  system's platform, a context for OpenGL 3.3 core or OpenGL ES 3.0 (`:auto`
  tries them in turn), and a window surface. cl-opengl resolves entry
  points through `eglGetProcAddress`.
- **Vulkan** on a surface of the window, made with the window system's
  surface extension.

The `x11` frontend (`src/x11`) is the `clx` frontend with its drawing
replaced. CLX keeps the windows, keyboard, mouse, selections and drag and
drop. EGL and Vulkan need a C Xlib `Display*`, so the frontend opens one
to the same display and hands it the window's id, which the server gives
and every connection shares. The `clx` frontend's drawing is a handful of
generic functions (`clx-drawing-resources`, `clx-redraw` ...), which the
`x11` frontend specializes.

## Glyphs on Linux and the BSDs

`src/glyphs/freetype.lisp`, in the `slt/glyphs` system the `clx` frontend
shares:

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

- Fonts with color glyphs (Noto Color Emoji's bitmaps, or color layers)
  are loaded in color, and their glyphs scaled into the cells, averaging
  the pixels under each, for the color atlas. The fallback search takes
  them although they are not scalable. Emoji sequences and flags are
  ligatures in these fonts and need shaping (HarfBuzz); until then a
  cluster is drawn with its first character's glyph.
- The fonts slt ships (`fonts/`) are found by family name before fontconfig
  is asked, so Symbols Nerd Font Mono needs no installing.

On macOS the `gl` frontend uses the CoreText rasterizer instead. CoreText
shapes each cluster, and draws those it sets in a color font (Apple Color
Emoji) in color, scaled to fit their cells. Each font's cascade list holds
`*font-fallback-families*` (the fonts in `fonts/` are registered for the
process), ahead of CoreText's own fallbacks; a character CoreText still
sets in LastResort is looked up among the installed fonts with a
character-set `CTFontCollection` query, as Ghostty's CoreText discovery
does.  A fallback font's glyph whose ink would leave its cells is shrunk
to fit and centered.

Ligatures: each row's runs of ASCII punctuation in one style (up to
`+max-ligature-cells+`) go to the atlas's shaper, which says whether the
font draws the run otherwise than character by character (a ligature or a
contextual alternate).  Such a run is rasterized once across as many
adjacent slots as it has cells, as a wide character takes two, and the
answer is cached per run and style.  Only the CoreText rasterizer shapes;
`*ligatures*` turns it off. font-discovery
c6e4728 aborts there, because it reads `kCTFontFormatAttribute`, a CFNumber,
as a string.

## Testing

- `make gl-test` (`scripts/gl-test`, `t/gl-snapshot.lisp`) runs a shell that
  prints styled text in a hidden window. It renders offscreen, checks the
  background, glyphs, an SGR background and box drawing, and leaves
  `build/gl-test/snapshot.ppm`.
- On Linux CI, run it under `xvfb-run` for X11, or with `weston
  --frontend=headless` and `WAYLAND_DISPLAY` set for Wayland.
- `SLT_GL_API=vulkan make gl-test` runs the same checks with the window
  drawn by Vulkan.
- `make vulkan-test` (`t/vulkan-snapshot.lisp`) renders styled text, an
  emoji and an image with Vulkan offscreen. It needs no window, only a
  Vulkan driver: MoltenVK on macOS, Mesa's lavapipe on Linux CI.
- `make window-test` (`scripts/window-test`, `t/window-snapshot.lisp`)
  runs the same kind of checks on the `x11` frontend under Xvfb, through
  `frontend-snapshot`. `SLT_GL_API` picks `gl`, `gles` or `vulkan`, and
  `SLT_FRONTEND` another frontend. `scripts/linux-test make window-test`
  runs it in a Linux container (`ci/linux.Containerfile`) with Mesa's
  llvmpipe and lavapipe.

## Still to do

1. Run the `gl` frontend on real Linux and BSD desktops, X11 and Wayland, and
   in CI.
2. HarfBuzz shaping for combining marks and emoji sequences, color emoji
   from FreeType (see above), subpixel
   positioning, and ligatures if wanted.
3. Check the Linux build (`make` includes `slt/gl` there): GLFW, FreeType
   and fontconfig stay system libraries.
4. Vulkan: a host for the Cocoa frontend's layer; frames in flight beyond
   one, if profiling shows the wait; and running it on hardware Linux
   drivers (Mesa, NVIDIA).
