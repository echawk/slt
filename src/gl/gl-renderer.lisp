(in-package #:slt)

;;;; OpenGL renderer: the Metal renderer's design in OpenGL 3.3 core (or
;;;; OpenGL ES 3.0).  The grid is one instanced draw, six vertices per cell
;;;; and no vertex buffer; each instance is a cell's four packed words from
;;;; PACK-GRID, and glyphs come from an R8 atlas texture.
;;;;
;;;; GL calls go through cl-opengl.  Its entry points are resolved with
;;;; glfwGetProcAddress (see GL-FRONTEND), which works for EGL (used on X11
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
flat out float bg_alpha;
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
  // The top bit marks the default background, transparent in layer 0.
  slot = cell.z & 0x7FFFFFFFu;
  bg_alpha = (cell.z >> 31) != 0u ? 0.0 : 1.0;
  flags = cell.w;
  atlas_px = (vec2(float(slot % atlas_columns), float(slot / atlas_columns)) + corner) * cell_size;
  local_px = corner * cell_size;
}
")

(defparameter *gl-fragment-shader-source* "
uniform vec2 cell_size;
uniform vec4 cursor_color;
uniform sampler2D atlas;
// 2 draws whole cells.  Under images with negative z the cells are drawn
// in two layers instead: 0, the backgrounds (the default one transparent),
// then 1, the text and decorations alone.
uniform uint layer;
flat in vec3 fg;
flat in vec3 bg;
flat in uint flags;
flat in uint slot;
flat in float bg_alpha;
in vec2 atlas_px;
in vec2 local_px;
out vec4 color_out;

void main() {
  if (layer == 0u) { color_out = vec4(bg, bg_alpha); return; }
  float ink = slot == 0u ? 0.0 : texelFetch(atlas, ivec2(atlas_px), 0).r;
  float h = cell_size.y;
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
  if (layer == 1u) {
    vec3 color = cursor > 0.0 ? cursor_color.rgb : (line > ink ? underline : fg);
    color_out = vec4(color, max(max(ink, line), cursor));
    return;
  }
  vec3 color = mix(mix(bg, fg, ink), underline, line);
  color_out = vec4(cursor > 0.0 ? cursor_color.rgb : color, 1.0);
}
")

(defparameter *gl-image-vertex-shader-source* "
layout(location = 0) in vec4 vertex;
out vec2 uv;
void main() {
  gl_Position = vec4(vertex.xy, 0.0, 1.0);
  uv = vertex.zw;
}
")

(defparameter *gl-image-fragment-shader-source* "
uniform sampler2D image_texture;
in vec2 uv;
out vec4 color_out;
void main() { color_out = texture(image_texture, uv); }
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

(defun gl-link-program (api &optional (vertex-source *gl-vertex-shader-source*)
                                      (fragment-source *gl-fragment-shader-source*))
  (let ((vertex (gl-compile-shader :vertex-shader
                                   (concatenate 'string (gl-shader-header api)
                                                vertex-source)))
        (fragment (gl-compile-shader :fragment-shader
                                     (concatenate 'string (gl-shader-header api)
                                                  fragment-source)))
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
  (image-program 0 :type (unsigned-byte 32))
  (vertex-array 0 :type (unsigned-byte 32))
  (buffer 0 :type (unsigned-byte 32))
  (image-vertex-array 0 :type (unsigned-byte 32))
  (image-buffer 0 :type (unsigned-byte 32))
  (atlas-texture 0 :type (unsigned-byte 32))
  (atlas-generation -1 :type fixnum)
  (atlas-height 0 :type fixnum)
  (image-textures (make-hash-table) :type hash-table)
  (uniforms (make-hash-table :test #'equal) :type hash-table))

(defun make-gl-renderer (&optional (api :gl))
  "Create a renderer in the current GL context."
  (let ((renderer (%make-gl-renderer
                   :api api :program (gl-link-program api)
                   :image-program (gl-link-program api *gl-image-vertex-shader-source*
                                                   *gl-image-fragment-shader-source*))))
    (dolist (name '("cell_size" "viewport" "columns" "atlas_columns" "cursor_color" "atlas"
                    "layer"))
      (setf (gethash name (gl-renderer-uniforms renderer))
            (gl:get-uniform-location (gl-renderer-program renderer) name)))
    (setf (gl-renderer-vertex-array renderer) (gl:gen-vertex-array)
          (gl-renderer-buffer renderer) (gl:gen-buffer)
          (gl-renderer-image-vertex-array renderer) (gl:gen-vertex-array)
          (gl-renderer-image-buffer renderer) (gl:gen-buffer)
          (gl-renderer-atlas-texture renderer) (gl:gen-texture))
    (gl:bind-vertex-array (gl-renderer-vertex-array renderer))
    (gl:bind-buffer :array-buffer (gl-renderer-buffer renderer))
    (gl:enable-vertex-attrib-array 0)
    (%gl:vertex-attrib-ipointer 0 4 :unsigned-int 16 (cffi:null-pointer))
    (%gl:vertex-attrib-divisor 0 1)
    (gl:bind-vertex-array (gl-renderer-image-vertex-array renderer))
    (gl:bind-buffer :array-buffer (gl-renderer-image-buffer renderer))
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 4 :float nil 16 (cffi:null-pointer))
    (gl:bind-texture :texture-2d (gl-renderer-atlas-texture renderer))
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    renderer))

(defun free-gl-renderer (renderer)
  (when (plusp (gl-renderer-program renderer))
    (gl:delete-textures (list (gl-renderer-atlas-texture renderer)))
    (maphash (lambda (id entry) (declare (ignore id))
               (gl:delete-textures (list (cdr entry))))
             (gl-renderer-image-textures renderer))
    (gl:delete-buffers (list (gl-renderer-buffer renderer)))
    (gl:delete-buffers (list (gl-renderer-image-buffer renderer)))
    (gl:delete-vertex-arrays (list (gl-renderer-vertex-array renderer)
                                   (gl-renderer-image-vertex-array renderer)))
    (gl:delete-program (gl-renderer-program renderer))
    (gl:delete-program (gl-renderer-image-program renderer))
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

(defun gl-image-texture (renderer image)
  (let* ((id (terminal-image-id image))
         (entry (gethash id (gl-renderer-image-textures renderer))))
    (if (and entry (eq (terminal-image-rgba image) (car entry)))
        (cdr entry)
        (let ((texture (gl:gen-texture))
              (rgba (terminal-image-rgba image)))
          (when entry (gl:delete-textures (list (cdr entry))))
          (gl:bind-texture :texture-2d texture)
          (gl:tex-parameter :texture-2d :texture-min-filter :linear)
          (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
          (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
          (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
          (gl:pixel-store :unpack-alignment 1)
          (sb-sys:with-pinned-objects (rgba)
            (gl:tex-image-2d :texture-2d 0 :rgba8
                             (terminal-image-width image) (terminal-image-height image) 0
                             :rgba :unsigned-byte (sb-sys:vector-sap rgba)))
          (setf (gethash id (gl-renderer-image-textures renderer)) (cons rgba texture))
          texture))))

(defun gl-prune-image-textures (renderer records)
  (let ((live (mapcar (lambda (record) (terminal-image-id (second record))) records)))
    (maphash (lambda (id entry)
               (unless (member id live)
                 (gl:delete-textures (list (cdr entry)))
                 (remhash id (gl-renderer-image-textures renderer))))
             (gl-renderer-image-textures renderer))))

(defun gl-draw-images (renderer records viewport-width viewport-height)
  (when records
    (gl:use-program (gl-renderer-image-program renderer))
    (gl:uniformi (gl:get-uniform-location (gl-renderer-image-program renderer)
                                           "image_texture") 0)
    (gl:active-texture :texture0)
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:bind-vertex-array (gl-renderer-image-vertex-array renderer))
    (dolist (record records)
      (destructuring-bind (placement image x y width height) record
        (let* ((iw (terminal-image-width image)) (ih (terminal-image-height image))
               (sx (terminal-image-placement-source-x placement))
               (sy (terminal-image-placement-source-y placement))
               (sw (or (terminal-image-placement-source-width placement) (- iw sx)))
               (sh (or (terminal-image-placement-source-height placement) (- ih sy)))
               (x0 (- (* 2.0 (/ x viewport-width)) 1.0))
               (x1 (- (* 2.0 (/ (+ x width) viewport-width)) 1.0))
               (y0 (- 1.0 (* 2.0 (/ y viewport-height))))
               (y1 (- 1.0 (* 2.0 (/ (+ y height) viewport-height))))
               (u0 (/ sx iw)) (u1 (/ (+ sx sw) iw))
               (v0 (/ sy ih)) (v1 (/ (+ sy sh) ih))
               (vertices (make-array 24 :element-type 'single-float
                                        :initial-contents
                                        (mapcar (lambda (value) (coerce value 'single-float))
                                                (list x0 y0 u0 v0  x1 y0 u1 v0  x0 y1 u0 v1
                                                      x0 y1 u0 v1  x1 y0 u1 v0  x1 y1 u1 v1)))))
          (gl:bind-texture :texture-2d (gl-image-texture renderer image))
          (gl:bind-buffer :array-buffer (gl-renderer-image-buffer renderer))
          (sb-sys:with-pinned-objects (vertices)
            (%gl:buffer-data :array-buffer (* 4 (length vertices))
                             (sb-sys:vector-sap vertices) :stream-draw))
          (gl:draw-arrays :triangles 0 6))))
    (gl:disable :blend)))

(defun gl-draw (renderer instances columns cell-count cell-width cell-height
                width height atlas palette &optional image-records)
  "Draw CELL-COUNT packed cells into the current framebuffer, WIDTH x HEIGHT."
  (let ((background (palette-background palette))
        (cursor (palette-cursor palette))
        (uniforms (gl-renderer-uniforms renderer)))
    (flet ((channel (color shift) (/ (ldb (byte 8 shift) color) 255.0))
           (uniform (name) (gethash name uniforms)))
      (gl:viewport 0 0 width height)
      (gl:clear-color (channel background 16) (channel background 8) (channel background 0) 1.0)
      (gl:clear :color-buffer-bit)
      (gl-prune-image-textures renderer image-records)
      (when (plusp cell-count)
        (gl-upload-atlas renderer atlas)
        (gl-upload-instances renderer instances))
      (multiple-value-bind (below-backgrounds below-text above) (image-layers image-records)
        (flet ((cells (layer)
                 (when (plusp cell-count)
                   (gl:use-program (gl-renderer-program renderer))
                   (gl:uniformf (uniform "cell_size") (float cell-width 1.0) (float cell-height 1.0))
                   (gl:uniformf (uniform "viewport")
                                (float (max 1 width) 1.0) (float (max 1 height) 1.0))
                   (%gl:uniform-1ui (uniform "columns") columns)
                   (%gl:uniform-1ui (uniform "atlas_columns") (glyph-atlas-slots-per-row atlas))
                   (%gl:uniform-1ui (uniform "layer") layer)
                   (gl:uniformf (uniform "cursor_color")
                                (channel cursor 16) (channel cursor 8) (channel cursor 0) 1.0)
                   (gl:uniformi (uniform "atlas") 0)
                   (gl:bind-vertex-array (gl-renderer-vertex-array renderer))
                   (if (= layer 2)
                       (gl:disable :blend)
                       (progn (gl:enable :blend)
                              (gl:blend-func :src-alpha :one-minus-src-alpha)))
                   (%gl:draw-arrays-instanced :triangles 0 6 cell-count)
                   (gl:disable :blend))))
          (gl-draw-images renderer below-backgrounds width height)
          (if (or below-backgrounds below-text)
              (progn (cells 0) (gl-draw-images renderer below-text width height) (cells 1))
              (cells 2))
          (gl-draw-images renderer above width height))))))

(defun gl-snapshot (renderer instances columns cell-count cell-width cell-height
                    width height atlas palette &optional image-records)
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
                    width height atlas palette image-records)
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
