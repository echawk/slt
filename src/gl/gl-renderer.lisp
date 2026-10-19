(in-package #:slt)

;;;; OpenGL renderer: the Metal renderer's design in OpenGL 3.3 core (or
;;;; OpenGL ES 3.0).  The grid is one instanced draw, six vertices per cell
;;;; and no vertex buffer; each instance is a cell's four packed words from
;;;; PACK-GRID, and glyphs come from an R8 atlas texture.
;;;;
;;;; GL calls go through cl-opengl.  Its entry points are resolved with
;;;; glfwGetProcAddress (see GL-BACKEND), which works for EGL (used on X11
;;;; and Wayland) and NSGL alike.

(defparameter *gl-vertex-shader-source* "
layout(location = 0) in uvec4 cell;
uniform vec2 cell_size;
uniform vec2 viewport;
uniform uint columns;
uniform uint atlas_columns;
flat out vec3 fg;
flat out vec3 bg;
flat out uint flags;
flat out uint slot;
out vec2 atlas_px;
out vec2 local_px;

vec3 unpack_rgb(uint c) {
  return vec3(float((c >> 16) & 255u), float((c >> 8) & 255u), float(c & 255u)) / 255.0;
}

void main() {
  vec2 corners[6] = vec2[6](vec2(0.0, 0.0), vec2(1.0, 0.0), vec2(0.0, 1.0),
                            vec2(0.0, 1.0), vec2(1.0, 0.0), vec2(1.0, 1.0));
  vec2 corner = corners[gl_VertexID];
  uint i = uint(gl_InstanceID);
  vec2 px = (vec2(float(i % columns), float(i / columns)) + corner) * cell_size;
  gl_Position = vec4(px.x / viewport.x * 2.0 - 1.0, 1.0 - px.y / viewport.y * 2.0, 0.0, 1.0);
  fg = unpack_rgb(cell.x);
  bg = unpack_rgb(cell.y);
  slot = cell.z;
  flags = cell.w;
  atlas_px = (vec2(float(cell.z % atlas_columns), float(cell.z / atlas_columns)) + corner) * cell_size;
  local_px = corner * cell_size;
}
")

(defparameter *gl-fragment-shader-source* "
uniform vec2 cell_size;
uniform vec4 cursor_color;
uniform sampler2D atlas;
flat in vec3 fg;
flat in vec3 bg;
flat in uint flags;
flat in uint slot;
in vec2 atlas_px;
in vec2 local_px;
out vec4 color_out;

void main() {
  float coverage = slot == 0u ? 0.0 : texelFetch(atlas, ivec2(atlas_px), 0).r;
  float h = cell_size.y;
  float t = max(1.0, floor(h / 18.0));
  float y = local_px.y;
  if ((flags & 1u) != 0u && y >= h - 2.0 * t && y < h - t) coverage = 1.0;
  if ((flags & 2u) != 0u && ((y >= h - t) || (y >= h - 3.0 * t && y < h - 2.0 * t))) coverage = 1.0;
  if ((flags & 4u) != 0u && abs(y - h * 0.55) < t * 0.5 + 0.25) coverage = 1.0;
  vec3 color = mix(bg, fg, coverage);
  if ((flags & 8u) != 0u && local_px.x < 2.0 * t) color = cursor_color.rgb;
  if ((flags & 16u) != 0u && y >= h - 2.0 * t) color = cursor_color.rgb;
  color_out = vec4(color, 1.0);
}
")

(defun gl-shader-header (api)
  (ecase api
    (:gl "#version 330 core
")
    (:gles "#version 300 es
precision highp float;
precision highp int;
")))

(defun gl-compile-shader (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (unless (gl:get-shader shader :compile-status)
      (error "GLSL compilation failed:~%~a" (gl:get-shader-info-log shader)))
    shader))

(defun gl-link-program (api)
  (let ((vertex (gl-compile-shader :vertex-shader
                                   (concatenate 'string (gl-shader-header api)
                                                *gl-vertex-shader-source*)))
        (fragment (gl-compile-shader :fragment-shader
                                     (concatenate 'string (gl-shader-header api)
                                                  *gl-fragment-shader-source*)))
        (program (gl:create-program)))
    (gl:attach-shader program vertex)
    (gl:attach-shader program fragment)
    (gl:link-program program)
    (gl:delete-shader vertex)
    (gl:delete-shader fragment)
    (unless (gl:get-program program :link-status)
      (error "GLSL linking failed:~%~a" (gl:get-program-info-log program)))
    program))

(defstruct (gl-renderer (:constructor %make-gl-renderer))
  (api :gl :type (member :gl :gles))
  (program 0 :type (unsigned-byte 32))
  (vertex-array 0 :type (unsigned-byte 32))
  (buffer 0 :type (unsigned-byte 32))
  (atlas-texture 0 :type (unsigned-byte 32))
  (atlas-generation -1 :type fixnum)
  (atlas-height 0 :type fixnum)
  (uniforms (make-hash-table :test #'equal) :type hash-table))

(defun make-gl-renderer (&optional (api :gl))
  "Create a renderer in the current GL context."
  (let ((renderer (%make-gl-renderer :api api :program (gl-link-program api))))
    (dolist (name '("cell_size" "viewport" "columns" "atlas_columns" "cursor_color" "atlas"))
      (setf (gethash name (gl-renderer-uniforms renderer))
            (gl:get-uniform-location (gl-renderer-program renderer) name)))
    (setf (gl-renderer-vertex-array renderer) (gl:gen-vertex-array)
          (gl-renderer-buffer renderer) (gl:gen-buffer)
          (gl-renderer-atlas-texture renderer) (gl:gen-texture))
    (gl:bind-vertex-array (gl-renderer-vertex-array renderer))
    (gl:bind-buffer :array-buffer (gl-renderer-buffer renderer))
    (gl:enable-vertex-attrib-array 0)
    (%gl:vertex-attrib-ipointer 0 4 :unsigned-int 16 (cffi:null-pointer))
    (%gl:vertex-attrib-divisor 0 1)
    (gl:bind-texture :texture-2d (gl-renderer-atlas-texture renderer))
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    renderer))

(defun free-gl-renderer (renderer)
  (when (plusp (gl-renderer-program renderer))
    (gl:delete-textures (list (gl-renderer-atlas-texture renderer)))
    (gl:delete-buffers (list (gl-renderer-buffer renderer)))
    (gl:delete-vertex-arrays (list (gl-renderer-vertex-array renderer)))
    (gl:delete-program (gl-renderer-program renderer))
    (setf (gl-renderer-program renderer) 0)))

(defun gl-upload-atlas (renderer atlas)
  (let ((width (glyph-atlas-width atlas))
        (height (glyph-atlas-height atlas))
        (pixels (glyph-atlas-pixels atlas)))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (gl-renderer-atlas-texture renderer))
    (gl:pixel-store :unpack-alignment 1)
    (unless (and (= (gl-renderer-atlas-generation renderer) (glyph-atlas-generation atlas))
                 (= (gl-renderer-atlas-height renderer) height))
      ;; A cleared or grown atlas is uploaded whole.
      (sb-sys:with-pinned-objects (pixels)
        (gl:tex-image-2d :texture-2d 0 :r8 width height 0
                          :red :unsigned-byte (sb-sys:vector-sap pixels)))
      (glyph-atlas-take-dirty atlas)
      (setf (gl-renderer-atlas-generation renderer) (glyph-atlas-generation atlas)
            (gl-renderer-atlas-height renderer) height))
    (multiple-value-bind (start end) (glyph-atlas-take-dirty atlas)
      (when start
        (sb-sys:with-pinned-objects (pixels)
          (%gl:tex-sub-image-2d :texture-2d 0 0 start width (- end start)
                                :red :unsigned-byte
                                (sb-sys:sap+ (sb-sys:vector-sap pixels) (* start width))))))))

(defun gl-upload-instances (renderer instances)
  (declare (type u32-vector instances))
  (let ((bytes (* 4 (length instances))))
    (gl:bind-buffer :array-buffer (gl-renderer-buffer renderer))
    ;; Respecifying the store each frame lets the driver hand us fresh
    ;; memory instead of waiting for the previous frame's draw.
    (%gl:buffer-data :array-buffer (max 16 bytes) (cffi:null-pointer) :stream-draw)
    (sb-sys:with-pinned-objects (instances)
      (%gl:buffer-sub-data :array-buffer 0 bytes (sb-sys:vector-sap instances)))))

(defun gl-draw (renderer instances columns cell-count cell-width cell-height
                width height atlas palette)
  "Draw CELL-COUNT packed cells into the current framebuffer, WIDTH x HEIGHT."
  (let ((background (palette-background palette))
        (cursor (palette-cursor palette))
        (uniforms (gl-renderer-uniforms renderer)))
    (flet ((channel (color shift) (/ (ldb (byte 8 shift) color) 255.0))
           (uniform (name) (gethash name uniforms)))
      (gl:viewport 0 0 width height)
      (gl:clear-color (channel background 16) (channel background 8) (channel background 0) 1.0)
      (gl:clear :color-buffer-bit)
      (when (plusp cell-count)
        (gl-upload-atlas renderer atlas)
        (gl-upload-instances renderer instances)
        (gl:use-program (gl-renderer-program renderer))
        (gl:uniformf (uniform "cell_size") (float cell-width 1.0) (float cell-height 1.0))
        (gl:uniformf (uniform "viewport") (float (max 1 width) 1.0) (float (max 1 height) 1.0))
        (%gl:uniform-1ui (uniform "columns") columns)
        (%gl:uniform-1ui (uniform "atlas_columns") (glyph-atlas-slots-per-row atlas))
        (gl:uniformf (uniform "cursor_color")
                     (channel cursor 16) (channel cursor 8) (channel cursor 0) 1.0)
        (gl:uniformi (uniform "atlas") 0)
        (gl:bind-vertex-array (gl-renderer-vertex-array renderer))
        (%gl:draw-arrays-instanced :triangles 0 6 cell-count)))))

(defun gl-snapshot (renderer instances columns cell-count cell-width cell-height
                    width height atlas palette)
  "Render offscreen and return the frame as WIDTH x HEIGHT RGB octets,
top row first, like METAL-SNAPSHOT."
  (let ((framebuffer (gl:gen-framebuffer))
        (texture (gl:gen-texture))
        (rgb (make-array (* 3 width height) :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           (gl:bind-texture :texture-2d texture)
           (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0
                             :rgba :unsigned-byte nil)
           (gl:bind-framebuffer :framebuffer framebuffer)
           (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d texture 0)
           (unless (member (gl:check-framebuffer-status :framebuffer)
                           ;; cl-opengl may name the value by an extension.
                           '(:framebuffer-complete :framebuffer-complete-oes
                             :framebuffer-complete-ext))
             (error "The offscreen framebuffer is incomplete."))
           (gl-draw renderer instances columns cell-count cell-width cell-height
                    width height atlas palette)
           (cffi:with-foreign-object (pixels :uint8 (* 4 width height))
             (gl:pixel-store :pack-alignment 1)
             (%gl:read-pixels 0 0 width height :rgba :unsigned-byte pixels)
             ;; GL rows run bottom to top.
             (dotimes (row height)
               (let ((source (* 4 width (- height row 1)))
                     (target (* 3 width row)))
                 (dotimes (column width)
                   (dotimes (channel 3)
                     (setf (aref rgb (+ target (* 3 column) channel))
                           (cffi:mem-aref pixels :uint8 (+ source (* 4 column) channel)))))))))
      (gl:bind-framebuffer :framebuffer 0)
      (gl:delete-framebuffers (list framebuffer))
      (gl:delete-textures (list texture)))
    rgb))
