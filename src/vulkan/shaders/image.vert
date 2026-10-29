#version 450

// A terminal image's quad: positions in normalized device coordinates,
// then texture coordinates.

layout(location = 0) in vec4 vertex;
layout(location = 0) out vec2 uv;

void main() {
  gl_Position = vec4(vertex.xy, 0.0, 1.0);
  uv = vertex.zw;
}
