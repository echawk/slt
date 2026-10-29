#version 450

// Cells as the OpenGL renderer's fragment shader draws them: glyph
// coverage or a color glyph over the background, then decorations and the
// cursor.

layout(push_constant) uniform Frame {
  vec4 cursor_color;
  vec2 cell_size;
  vec2 viewport;
  uint columns;
  uint atlas_columns;
  // 2 draws whole cells.  Under images with negative z the cells are drawn
  // in two layers instead: 0, the backgrounds (the default one
  // transparent), then 1, the text and decorations alone.
  uint layer;
} frame;

layout(set = 0, binding = 0) uniform sampler2D atlas;
layout(set = 0, binding = 1) uniform sampler2D color_atlas;

layout(location = 0) flat in vec3 fg;
layout(location = 1) flat in vec3 bg;
layout(location = 2) flat in uint flags;
layout(location = 3) flat in uint slot;
layout(location = 4) flat in uint color_glyph;
layout(location = 5) flat in float bg_alpha;
layout(location = 6) in vec2 atlas_px;
layout(location = 7) in vec2 local_px;

layout(location = 0) out vec4 color_out;

void main() {
  if (frame.layer == 0u) { color_out = vec4(bg, bg_alpha); return; }
  // A color glyph is premultiplied RGBA; other glyphs are coverage.
  vec4 glyph = vec4(0.0);
  float ink = 0.0;
  if (slot != 0u) {
    if (color_glyph != 0u) glyph = texelFetch(color_atlas, ivec2(atlas_px), 0);
    else ink = texelFetch(atlas, ivec2(atlas_px), 0).r;
  }
  float h = frame.cell_size.y;
  float t = max(1.0, floor(h / 18.0));
  float y = local_px.y;
  if ((flags & 4u) != 0u && abs(y - h * 0.55) < t * 0.5 + 0.25) ink = 1.0;
  // Underlines, in their own color; patterns follow the window's x so
  // they run on across cells.
  float x = gl_FragCoord.x;
  float band = (y >= h - 2.0 * t && y < h - t) ? 1.0 : 0.0;
  float line = 0.0;
  if ((flags & 1u) != 0u) line = band;
  if ((flags & 2u) != 0u && ((y >= h - t) || (y >= h - 3.0 * t && y < h - 2.0 * t))) line = 1.0;
  if ((flags & 32u) != 0u) {
    float amplitude = max(1.0, t);
    float centre = h - 1.5 * t - amplitude;
    float wave = centre + amplitude * sin(x * 6.2831853 / max(6.0, 8.0 * amplitude));
    line = clamp(t * 0.5 + 0.75 - abs(y - wave), 0.0, 1.0);
  }
  if ((flags & 64u) != 0u && mod(floor(x / (2.0 * t)), 2.0) < 1.0) line = band;
  if ((flags & 128u) != 0u && mod(x, 8.0 * t) < 6.0 * t) line = band;
  float cursor = (((flags & 8u) != 0u && local_px.x < 2.0 * t)
                  || ((flags & 16u) != 0u && y >= h - 2.0 * t)) ? 1.0 : 0.0;
  vec3 underline = vec3(float((flags >> 24) & 255u), float((flags >> 16) & 255u),
                        float((flags >> 8) & 255u)) / 255.0;
  if (frame.layer == 1u) {
    float alpha = max(glyph.a, ink);
    vec3 text = glyph.a > 0.0 ? mix(glyph.rgb / glyph.a, fg, ink) : fg;
    if (cursor > 0.0) color_out = vec4(frame.cursor_color.rgb, 1.0);
    else if (line > alpha) color_out = vec4(underline, line);
    else color_out = vec4(text, alpha);
    return;
  }
  vec3 color = glyph.rgb + bg * (1.0 - glyph.a);
  color = mix(mix(color, fg, ink), underline, line);
  color_out = vec4(cursor > 0.0 ? frame.cursor_color.rgb : color, 1.0);
}
