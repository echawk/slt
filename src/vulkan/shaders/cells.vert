#version 450

// The grid as one instanced draw: six vertices per cell, each instance a
// cell's four packed words (see PACK-GRID), as the OpenGL renderer draws it.

layout(push_constant) uniform Frame {
  vec4 cursor_color;
  vec2 cell_size;
  vec2 viewport;
  uint columns;
  uint atlas_columns;
  uint layer;
} frame;

layout(location = 0) in uvec4 cell;

layout(location = 0) flat out vec3 fg;
layout(location = 1) flat out vec3 bg;
layout(location = 2) flat out uint flags;
layout(location = 3) flat out uint slot;
layout(location = 4) flat out uint color_glyph;
layout(location = 5) flat out float bg_alpha;
layout(location = 6) out vec2 atlas_px;
layout(location = 7) out vec2 local_px;

vec3 unpack_rgb(uint c) {
  return vec3(float((c >> 16) & 255u), float((c >> 8) & 255u), float(c & 255u)) / 255.0;
}

void main() {
  vec2 corners[6] = vec2[6](vec2(0.0, 0.0), vec2(1.0, 0.0), vec2(0.0, 1.0),
                            vec2(0.0, 1.0), vec2(1.0, 0.0), vec2(1.0, 1.0));
  vec2 corner = corners[gl_VertexIndex];
  uint i = uint(gl_InstanceIndex);
  vec2 px = (vec2(float(i % frame.columns), float(i / frame.columns)) + corner) * frame.cell_size;
  // Vulkan's y runs down, as the grid's rows do.
  gl_Position = vec4(px / frame.viewport * 2.0 - 1.0, 0.0, 1.0);
  fg = unpack_rgb(cell.x);
  bg = unpack_rgb(cell.y);
  // The top bit marks the default background, transparent in layer 0;
  // the next one a color glyph, from the color atlas.
  slot = cell.z & 0x3FFFFFFFu;
  color_glyph = (cell.z >> 30) & 1u;
  bg_alpha = (cell.z >> 31) != 0u ? 0.0 : 1.0;
  flags = cell.w;
  atlas_px = (vec2(float(slot % frame.atlas_columns), float(slot / frame.atlas_columns)) + corner)
             * frame.cell_size;
  local_px = corner * frame.cell_size;
}
