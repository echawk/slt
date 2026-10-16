# GPU backends: OpenGL and Vulkan for Linux and the BSDs

Status: design sketch. The macOS backend (`src/macos`, Metal) is
implemented. This document describes how the same design carries over to
OpenGL and Vulkan.

## What is already shared

Everything above the graphics API is platform-neutral and already in use by
the Metal backend:

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

A backend has to provide four things: a window with input events,
a glyph rasterizer for the atlas, a way to upload the atlas and the instance
array, and one instanced draw call.

## Rendering: one draw call per frame

The Metal shader (`*metal-shader-source*` in `src/macos/metal.lisp`) ports
directly:

- Draw 6 vertices per instance, one instance per cell, with no vertex buffer.
- The vertex stage places the cell from `instance_id`. It looks up the
  glyph's slot in the atlas and passes the colors and flags through.
- The fragment stage mixes background and foreground by the atlas coverage,
  then draws underline, double underline, strikethrough, and bar or underline
  cursors from the flags.

### OpenGL 3.3 core / OpenGL ES 3.0

```glsl
// vertex
#version 330 core            // or: #version 300 es
layout(location = 0) in uvec4 cell;   // per instance: glVertexAttribIPointer + divisor 1
uniform vec2 cell_size, viewport;
uniform uint columns, atlas_columns;
flat out vec3 fg, bg; flat out uint flags, slot;
out vec2 atlas_px, local_px;
vec3 rgb(uint c) { return vec3((c >> 16) & 255u, (c >> 8) & 255u, c & 255u) / 255.0; }
void main() {
  const vec2 corners[6] = vec2[6](vec2(0,0), vec2(1,0), vec2(0,1),
                                  vec2(0,1), vec2(1,0), vec2(1,1));
  vec2 corner = corners[gl_VertexID];
  uint i = uint(gl_InstanceID);
  vec2 px = (vec2(i % columns, i / columns) + corner) * cell_size;
  gl_Position = vec4(px.x / viewport.x * 2.0 - 1.0, 1.0 - px.y / viewport.y * 2.0, 0.0, 1.0);
  fg = rgb(cell.x); bg = rgb(cell.y); slot = cell.z; flags = cell.w;
  atlas_px = (vec2(cell.z % atlas_columns, cell.z / atlas_columns) + corner) * cell_size;
  local_px = corner * cell_size;
}
```

The fragment shader is the Metal one with `texelFetch(atlas,
ivec2(atlas_px), 0).r` in place of the pixel-coordinate sampler. Use an
`R8` texture (`GL_R8` on desktop GL, `GL_R8` with `GL_RED` on GLES 3).

Uploads per frame:

- `glBufferSubData` for the instance array. Better is a persistently mapped
  ring of three buffers with `glFenceSync`, mirroring the Metal backend's
  three frames in flight.
- `glTexSubImage2D` for the atlas rows that `glyph-atlas-take-dirty`
  reports.

cl-opengl (already in `ocicl/`) covers all of this, and `%gl:` exposes the
instanced entry points.

### Vulkan

The same shader compiles to SPIR-V (`glslc`, done at build time). The
pipeline is one graphics pipeline with no vertex input and the instance
array bound as a storage buffer. Per frame:

1. Copy the dirty atlas rows to a staging buffer, then
   `vkCmdCopyBufferToImage`.
2. Write the instance array into a host-visible ring buffer.
3. One `vkCmdDraw(cmd, 6, rows * columns, 0, 0)`.

Vulkan adds a lot of setup code (instance, device, swapchain, recreation on
resize) for little gain on a workload this small. The benefit is one
renderer that also runs on macOS through MoltenVK. The recommendation is
OpenGL first, with Vulkan as an option once there is a reason for it.

## X11 or Wayland?

**The graphics APIs themselves don't care.** OpenGL and Vulkan only draw into
a surface. The window system matters in exactly four places: creating the
window and surface, keyboard input, the clipboard, and HiDPI scale.

| | X11 | Wayland |
|---|---|---|
| OpenGL context | GLX or EGL | EGL |
| Vulkan surface | `VK_KHR_xlib_surface` / `xcb_surface` | `VK_KHR_wayland_surface` |
| Keyboard | XKB via xcb | `wl_keyboard` + xkbcommon |
| Clipboard | selections | `wl_data_device` |

So a windowing library that supports both gives us X11 *and* Wayland from one
backend:

- **GLFW 3.4** (recommended). It picks X11 or Wayland at run time from the
  same binary, works on FreeBSD and OpenBSD, and creates both GL/EGL contexts
  and Vulkan surfaces (`glfwCreateWindowSurface`). It also supplies text
  input (`glfwSetCharCallback`, which uses xkbcommon on Wayland), the
  clipboard, content scale and window events, so a renderer never touches
  X11 or Wayland directly.
  - cl-glfw3 is in ocicl. It may need small additions for 3.4's
    `GLFW_PLATFORM` hint and newer functions.
- **SDL2/SDL3.** Also X11 + Wayland, and slt already has an SDL2 backend for
  window and input plumbing. The OpenGL renderer could first be tried by
  replacing that backend's `SDL_Renderer` with a GL context. SDL3 is the
  better long-term choice, because SDL2's Wayland support depends on how
  distributions build it.

Plan: a backend `slt/gl` on GLFW, with an OpenGL 3.3 core profile (falling
back to GLES 3.0 through EGL where needed). It should run unchanged on
X11, Wayland, FreeBSD and OpenBSD, and on macOS for testing.

## Glyphs on Linux and the BSDs

CoreText's job, rasterizing a cluster into the atlas slot with font
fallback, falls to:

- **fontconfig** to resolve the family and fallback fonts (`FcFontSort` for
  characters the primary font lacks).
- **FreeType** to rasterize (`FT_Render_Glyph` in `FT_RENDER_MODE_NORMAL`
  for the alpha-only atlas).
- **HarfBuzz** to shape clusters, so combining marks and ligature-free
  monospace runs land correctly.

All three are standard on every desktop Linux and BSD. The rasterizer slots
into `make-glyph-atlas` exactly like `coretext-rasterizer` does: it is a
function that draws a string's coverage into an atlas rectangle. A second
pass can add a color (RGBA) atlas for emoji.

## Order of work

1. `slt/gl` on GLFW with the shaders above, plus a FreeType/fontconfig rasterizer.
2. Continuous integration on Linux (X11 under Xvfb, Wayland under a headless
   compositor such as `weston --backend=headless`). Use offscreen snapshots
   as golden images, like `metal-snapshot`.
3. A color atlas for emoji, subpixel positioning, and ligatures if wanted.
4. Vulkan, if profiling or MoltenVK parity justifies it.
