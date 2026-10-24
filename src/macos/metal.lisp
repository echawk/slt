(in-package #:slt)

;;;; Metal renderer.  The whole grid is one instanced draw: six vertices
;;;; per cell, with per-cell data (colors, atlas slot, decorations) read
;;;; from the packed instance buffer built by PACK-GRID.

(defconstant +mtl-pixel-format-r8-unorm+ 10)
(defconstant +mtl-pixel-format-rgba8-unorm+ 70)
(defconstant +mtl-pixel-format-bgra8-unorm+ 80)
(defconstant +mtl-primitive-type-triangle+ 3)
(defconstant +mtl-load-action-clear+ 2)
(defconstant +mtl-store-action-store+ 1)
(defconstant +metal-frames-in-flight+ 3)

(cffi:defcfun ("MTLCreateSystemDefaultDevice" %mtl-create-system-default-device) :pointer)
(cffi:defcfun ("memcpy" %memcpy) :pointer (destination :pointer) (source :pointer) (count :size))

(defparameter *metal-shader-source* "
#include <metal_stdlib>
using namespace metal;

struct Uniforms {
  float2 cell_size;
  float2 viewport;
  uint columns;
  uint atlas_columns;
  float2 padding;
  float4 cursor_color;
};

struct Fragment {
  float4 position [[position]];
  float3 fg [[flat]];
  float3 bg [[flat]];
  uint flags [[flat]];
  uint slot [[flat]];
  float2 atlas_px;
  float2 local_px;
};

static float3 unpack_rgb(uint c) {
  return float3((c >> 16) & 255u, (c >> 8) & 255u, c & 255u) / 255.0;
}

vertex Fragment cell_vertex(uint vid [[vertex_id]],
                            uint iid [[instance_id]],
                            const device uint4 *cells [[buffer(0)]],
                            constant Uniforms &u [[buffer(1)]]) {
  const float2 corners[6] = { {0,0}, {1,0}, {0,1}, {0,1}, {1,0}, {1,1} };
  float2 corner = corners[vid];
  uint4 cell = cells[iid];
  float2 cell_origin = u.padding + float2(iid % u.columns, iid / u.columns) * u.cell_size;
  float2 px = cell_origin + corner * u.cell_size;
  Fragment out;
  out.position = float4(px.x / u.viewport.x * 2.0 - 1.0,
                        1.0 - px.y / u.viewport.y * 2.0, 0.0, 1.0);
  out.fg = unpack_rgb(cell.x);
  out.bg = unpack_rgb(cell.y);
  out.slot = cell.z;
  out.flags = cell.w;
  float2 slot_origin = float2(cell.z % u.atlas_columns, cell.z / u.atlas_columns) * u.cell_size;
  out.atlas_px = slot_origin + corner * u.cell_size;
  out.local_px = corner * u.cell_size;
  return out;
}

fragment float4 cell_fragment(Fragment in [[stage_in]],
                              texture2d<float> atlas [[texture(0)]],
                              constant Uniforms &u [[buffer(0)]]) {
  constexpr sampler nearest(coord::pixel, filter::nearest, address::clamp_to_edge);
  float coverage = in.slot == 0u ? 0.0 : atlas.sample(nearest, in.atlas_px).r;
  float h = u.cell_size.y;
  float t = max(1.0, floor(h / 18.0));
  float y = in.local_px.y;
  if ((in.flags & 1u) && y >= h - 2.0 * t && y < h - t) coverage = 1.0;
  if ((in.flags & 2u) && ((y >= h - t) || (y >= h - 3.0 * t && y < h - 2.0 * t))) coverage = 1.0;
  if ((in.flags & 4u) && abs(y - h * 0.55) < t * 0.5 + 0.25) coverage = 1.0;
  float3 color = mix(in.bg, in.fg, coverage);
  if ((in.flags & 8u) && in.local_px.x < 2.0 * t) color = u.cursor_color.rgb;
  if ((in.flags & 16u) && y >= h - 2.0 * t) color = u.cursor_color.rgb;
  return float4(color, 1.0);
}

struct ImageFragment {
  float4 position [[position]];
  float2 uv;
};

vertex ImageFragment image_vertex(uint vid [[vertex_id]],
                                  const device float4 *vertices [[buffer(0)]]) {
  ImageFragment out;
  out.position = float4(vertices[vid].xy, 0.0, 1.0);
  out.uv = vertices[vid].zw;
  return out;
}

fragment float4 image_fragment(ImageFragment in [[stage_in]],
                               texture2d<float> image [[texture(0)]]) {
  constexpr sampler linear_sampler(coord::normalized, filter::linear,
                                    address::clamp_to_edge);
  return image.sample(linear_sampler, in.uv);
}
")

(defstruct (metal-renderer (:constructor %make-metal-renderer))
  device
  queue
  pipeline
  image-pipeline
  layer
  (atlas-texture nil)
  (image-textures (make-hash-table) :type hash-table)
  (atlas-texture-generation -1)
  (buffers (make-array +metal-frames-in-flight+ :initial-element nil))
  (buffer-sizes (make-array +metal-frames-in-flight+ :initial-element 0))
  (frame 0 :type fixnum)
  ;; Device pixels between the drawable's top left corner and the grid.
  (padding 0 :type fixnum)
  (uniforms (cffi:foreign-alloc :uint8 :count 48)))

(defun metal-error-string (error-pointer)
  (let ((error (cffi:mem-ref error-pointer :pointer)))
    (if (cffi:null-pointer-p error)
        "unknown error"
        (ns-string->string (msg error "localizedDescription")))))

(defun make-metal-pipeline (device &key (vertex-name "cell_vertex")
                                        (fragment-name "cell_fragment") blend)
  (cffi:with-foreign-object (error :pointer)
    (setf (cffi:mem-ref error :pointer) (cffi:null-pointer))
    (let ((library (msg device "newLibraryWithSource:options:error:"
                        :pointer (ns-string *metal-shader-source*)
                        :pointer (cffi:null-pointer)
                        :pointer error)))
      (when (cffi:null-pointer-p library)
        (error "Metal shader compilation failed: ~a" (metal-error-string error)))
      (let* ((descriptor (msg (msg "MTLRenderPipelineDescriptor" "alloc") "init"))
             (vertex (msg library "newFunctionWithName:" :pointer (ns-string vertex-name)))
             (fragment (msg library "newFunctionWithName:" :pointer (ns-string fragment-name))))
        (msg descriptor "setVertexFunction:" :pointer vertex :void)
        (msg descriptor "setFragmentFunction:" :pointer fragment :void)
        (let ((attachment (msg (msg descriptor "colorAttachments")
                               "objectAtIndexedSubscript:" :unsigned-long 0)))
          (msg attachment "setPixelFormat:" :unsigned-long +mtl-pixel-format-bgra8-unorm+ :void)
          (when blend
            (msg attachment "setBlendingEnabled:" :bool t :void)
            (msg attachment "setSourceRGBBlendFactor:" :unsigned-long 4 :void)
            (msg attachment "setDestinationRGBBlendFactor:" :unsigned-long 5 :void)
            (msg attachment "setSourceAlphaBlendFactor:" :unsigned-long 1 :void)
            (msg attachment "setDestinationAlphaBlendFactor:" :unsigned-long 5 :void)))
        (let ((pipeline (msg device "newRenderPipelineStateWithDescriptor:error:"
                             :pointer descriptor :pointer error)))
          (msg vertex "release" :void)
          (msg fragment "release" :void)
          (msg descriptor "release" :void)
          (msg library "release" :void)
          (when (cffi:null-pointer-p pipeline)
            (error "Metal pipeline creation failed: ~a" (metal-error-string error)))
          pipeline)))))

(defun make-metal-renderer ()
  "Create a renderer and the CAMetalLayer it draws into."
  (let ((device (%mtl-create-system-default-device)))
    (when (cffi:null-pointer-p device)
      (error "No Metal device is available."))
    (let ((layer (msg (msg "CAMetalLayer" "alloc") "init")))
      (msg layer "setDevice:" :pointer device :void)
      (msg layer "setPixelFormat:" :unsigned-long +mtl-pixel-format-bgra8-unorm+ :void)
      (msg layer "setFramebufferOnly:" :bool t :void)
      (msg layer "setMaximumDrawableCount:" :unsigned-long +metal-frames-in-flight+ :void)
      (%make-metal-renderer :device device
                            :queue (msg device "newCommandQueue")
                            :pipeline (make-metal-pipeline device)
                            :image-pipeline (make-metal-pipeline
                                             device :vertex-name "image_vertex"
                                             :fragment-name "image_fragment" :blend t)
                            :layer layer))))

(defun free-metal-renderer (renderer)
  (flet ((release (object)
           (when (and object (not (cffi:null-pointer-p object)))
             (msg object "release" :void))))
    (map nil #'release (metal-renderer-buffers renderer))
    (release (metal-renderer-atlas-texture renderer))
    (maphash (lambda (id entry) (declare (ignore id)) (release (cdr entry)))
             (metal-renderer-image-textures renderer))
    (release (metal-renderer-pipeline renderer))
    (release (metal-renderer-image-pipeline renderer))
    (release (metal-renderer-queue renderer))
    (release (metal-renderer-layer renderer))
    (cffi:foreign-free (metal-renderer-uniforms renderer))
    (setf (metal-renderer-pipeline renderer) nil
          (metal-renderer-atlas-texture renderer) nil)))

(defun metal-image-texture (renderer image)
  (let* ((id (terminal-image-id image))
         (entry (gethash id (metal-renderer-image-textures renderer))))
    (if (and entry (eq image (car entry)))
        (cdr entry)
        (progn
          (when entry (msg (cdr entry) "release" :void))
          (let* ((width (terminal-image-width image))
                 (height (terminal-image-height image))
                 (descriptor (msg "MTLTextureDescriptor"
                                  "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
                                  :unsigned-long +mtl-pixel-format-rgba8-unorm+
                                  :unsigned-long width :unsigned-long height :bool nil))
                 (texture (msg (metal-renderer-device renderer)
                               "newTextureWithDescriptor:" :pointer descriptor))
                 (rgba (terminal-image-rgba image)))
            (cffi:with-foreign-object (region :unsigned-long 6)
              (loop for value in (list 0 0 0 width height 1)
                    for i from 0
                    do (setf (cffi:mem-aref region :unsigned-long i) value))
              (sb-sys:with-pinned-objects (rgba)
                (msg texture "replaceRegion:mipmapLevel:withBytes:bytesPerRow:"
                     :pointer region :unsigned-long 0
                     :pointer (sb-sys:vector-sap rgba) :unsigned-long (* width 4) :void)))
            (setf (gethash id (metal-renderer-image-textures renderer))
                  (cons image texture))
            texture)))))

(defun metal-prune-image-textures (renderer records)
  (let ((live (mapcar (lambda (record) (terminal-image-id (second record))) records)))
    (maphash (lambda (id entry)
               (unless (member id live)
                 (msg (cdr entry) "release" :void)
                 (remhash id (metal-renderer-image-textures renderer))))
             (metal-renderer-image-textures renderer))))

(defun metal-encode-images (renderer encoder records viewport-width viewport-height padding)
  (when records
    (msg encoder "setRenderPipelineState:" :pointer
         (metal-renderer-image-pipeline renderer) :void)
    (dolist (record records)
      (destructuring-bind (placement image x y width height) record
        (let* ((iw (terminal-image-width image)) (ih (terminal-image-height image))
               (sx (terminal-image-placement-source-x placement))
               (sy (terminal-image-placement-source-y placement))
               (sw (or (terminal-image-placement-source-width placement) (- iw sx)))
               (sh (or (terminal-image-placement-source-height placement) (- ih sy)))
               (x0 (- (* 2.0 (/ (+ padding x) viewport-width)) 1.0))
               (x1 (- (* 2.0 (/ (+ padding x width) viewport-width)) 1.0))
               (y0 (- 1.0 (* 2.0 (/ (+ padding y) viewport-height))))
               (y1 (- 1.0 (* 2.0 (/ (+ padding y height) viewport-height))))
               (u0 (/ sx iw)) (u1 (/ (+ sx sw) iw))
               (v0 (/ sy ih)) (v1 (/ (+ sy sh) ih)))
          (cffi:with-foreign-object (vertices :float 24)
            (loop for value in (list x0 y0 u0 v0  x1 y0 u1 v0  x0 y1 u0 v1
                                     x0 y1 u0 v1  x1 y0 u1 v0  x1 y1 u1 v1)
                  for i from 0 do (setf (cffi:mem-aref vertices :float i)
                                        (coerce value 'single-float)))
            (msg encoder "setVertexBytes:length:atIndex:"
                 :pointer vertices :unsigned-long (* 24 4) :unsigned-long 0 :void)
            (msg encoder "setFragmentTexture:atIndex:"
                 :pointer (metal-image-texture renderer image) :unsigned-long 0 :void)
            (msg encoder "drawPrimitives:vertexStart:vertexCount:"
                 :unsigned-long +mtl-primitive-type-triangle+
                 :unsigned-long 0 :unsigned-long 6 :void)))))))

(defun metal-set-drawable-size (renderer width height)
  (msg (metal-renderer-layer renderer) "setDrawableSize:"
       :double (coerce (max 1 width) 'double-float)
       :double (coerce (max 1 height) 'double-float) :void))

(defun metal-upload-atlas (renderer atlas)
  (let ((width (glyph-atlas-width atlas))
        (height (glyph-atlas-height atlas)))
    (unless (and (metal-renderer-atlas-texture renderer)
                 (= (metal-renderer-atlas-texture-generation renderer)
                    (glyph-atlas-generation atlas)))
      (when (metal-renderer-atlas-texture renderer)
        (msg (metal-renderer-atlas-texture renderer) "release" :void))
      (let ((descriptor (msg "MTLTextureDescriptor"
                             "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
                             :unsigned-long +mtl-pixel-format-r8-unorm+
                             :unsigned-long width :unsigned-long height :bool nil)))
        (setf (metal-renderer-atlas-texture renderer)
              (msg (metal-renderer-device renderer) "newTextureWithDescriptor:"
                   :pointer descriptor)
              (metal-renderer-atlas-texture-generation renderer)
              (glyph-atlas-generation atlas)
              (glyph-atlas-dirty-start atlas) 0
              (glyph-atlas-dirty-end atlas) height)))
    (multiple-value-bind (start end) (glyph-atlas-take-dirty atlas)
      (when start
        (let ((pixels (glyph-atlas-pixels atlas)))
          ;; MTLRegion is six NSUIntegers: larger than 16 bytes, so by reference.
          (cffi:with-foreign-object (region :unsigned-long 6)
            (loop for value in (list 0 start 0 width (- end start) 1)
                  for i from 0
                  do (setf (cffi:mem-aref region :unsigned-long i) value))
            (sb-sys:with-pinned-objects (pixels)
              (msg (metal-renderer-atlas-texture renderer)
                   "replaceRegion:mipmapLevel:withBytes:bytesPerRow:"
                   :pointer region
                   :unsigned-long 0
                   :pointer (sb-sys:sap+ (sb-sys:vector-sap pixels) (* start width))
                   :unsigned-long width
                   :void))))))))

(defun metal-instance-buffer (renderer bytes)
  (let* ((index (mod (metal-renderer-frame renderer) +metal-frames-in-flight+))
         (buffers (metal-renderer-buffers renderer))
         (sizes (metal-renderer-buffer-sizes renderer)))
    (when (< (aref sizes index) bytes)
      (when (aref buffers index)
        (msg (aref buffers index) "release" :void))
      (setf (aref buffers index)
            (msg (metal-renderer-device renderer) "newBufferWithLength:options:"
                 :unsigned-long (max 16 bytes) :unsigned-long 0)
            (aref sizes index) (max 16 bytes)))
    (aref buffers index)))

(defun metal-write-uniforms (renderer cell-width cell-height width height columns atlas palette)
  (let ((u (metal-renderer-uniforms renderer))
        (cursor (palette-cursor palette)))
    (setf (cffi:mem-aref u :float 0) (coerce cell-width 'single-float)
          (cffi:mem-aref u :float 1) (coerce cell-height 'single-float)
          (cffi:mem-aref u :float 2) (coerce width 'single-float)
          (cffi:mem-aref u :float 3) (coerce height 'single-float)
          (cffi:mem-aref u :uint32 4) columns
          (cffi:mem-aref u :uint32 5) (glyph-atlas-slots-per-row atlas)
          (cffi:mem-aref u :float 6) (coerce (metal-renderer-padding renderer) 'single-float)
          (cffi:mem-aref u :float 7) (coerce (metal-renderer-padding renderer) 'single-float)
          (cffi:mem-aref u :float 8) (/ (ldb (byte 8 16) cursor) 255.0)
          (cffi:mem-aref u :float 9) (/ (ldb (byte 8 8) cursor) 255.0)
          (cffi:mem-aref u :float 10) (/ (ldb (byte 8 0) cursor) 255.0)
          (cffi:mem-aref u :float 11) 1.0)
    u))

(defun metal-encode-frame (renderer target instances columns cell-count cell-width cell-height
                           width height atlas palette &optional image-records)
  "Encode drawing CELL-COUNT packed cells into TARGET; return the command buffer."
  (let* ((bytes (* 4 (length instances)))
         (buffer (metal-instance-buffer renderer bytes))
         (pass (msg "MTLRenderPassDescriptor" "renderPassDescriptor"))
         (attachment (msg (msg pass "colorAttachments")
                          "objectAtIndexedSubscript:" :unsigned-long 0))
         (background (palette-background palette))
         (uniforms (metal-write-uniforms renderer cell-width cell-height
                                         width height columns atlas palette)))
    (metal-upload-atlas renderer atlas)
    (metal-prune-image-textures renderer image-records)
    (sb-sys:with-pinned-objects (instances)
      (%memcpy (msg buffer "contents") (sb-sys:vector-sap instances) bytes))
    (msg attachment "setTexture:" :pointer target :void)
    (msg attachment "setLoadAction:" :unsigned-long +mtl-load-action-clear+ :void)
    (msg attachment "setStoreAction:" :unsigned-long +mtl-store-action-store+ :void)
    (msg attachment "setClearColor:"
         :double (/ (ldb (byte 8 16) background) 255d0)
         :double (/ (ldb (byte 8 8) background) 255d0)
         :double (/ (ldb (byte 8 0) background) 255d0)
         :double 1d0 :void)
    (let* ((commands (msg (metal-renderer-queue renderer) "commandBuffer"))
           (encoder (msg commands "renderCommandEncoderWithDescriptor:" :pointer pass)))
      (metal-encode-images renderer encoder
                           (remove-if-not (lambda (record)
                                            (minusp (terminal-image-placement-z-index
                                                     (first record))))
                                          image-records)
                           width height (metal-renderer-padding renderer))
      (when (plusp cell-count)
        (msg encoder "setRenderPipelineState:" :pointer (metal-renderer-pipeline renderer) :void)
        (msg encoder "setVertexBuffer:offset:atIndex:"
             :pointer buffer :unsigned-long 0 :unsigned-long 0 :void)
        (msg encoder "setVertexBytes:length:atIndex:"
             :pointer uniforms :unsigned-long 48 :unsigned-long 1 :void)
        (msg encoder "setFragmentBytes:length:atIndex:"
             :pointer uniforms :unsigned-long 48 :unsigned-long 0 :void)
        (msg encoder "setFragmentTexture:atIndex:"
             :pointer (metal-renderer-atlas-texture renderer) :unsigned-long 0 :void)
        (msg encoder "drawPrimitives:vertexStart:vertexCount:instanceCount:"
             :unsigned-long +mtl-primitive-type-triangle+
             :unsigned-long 0 :unsigned-long 6 :unsigned-long cell-count :void))
      (metal-encode-images renderer encoder
                           (remove-if (lambda (record)
                                        (minusp (terminal-image-placement-z-index
                                                 (first record))))
                                      image-records)
                           width height (metal-renderer-padding renderer))
      (msg encoder "endEncoding" :void)
      (incf (metal-renderer-frame renderer))
      commands)))

(defun metal-draw (renderer instances columns cell-count cell-width cell-height
                   width height atlas palette &optional image-records)
  "Draw CELL-COUNT packed cells into the next drawable of the layer."
  (with-autorelease-pool ()
    (let ((drawable (msg (metal-renderer-layer renderer) "nextDrawable")))
      (unless (cffi:null-pointer-p drawable)
        (let ((commands (metal-encode-frame renderer (msg drawable "texture")
                                            instances columns cell-count
                                            cell-width cell-height width height
                                            atlas palette image-records)))
          (if (msg (metal-renderer-layer renderer) "presentsWithTransaction" :bool)
              ;; During a live resize, present in step with the window's
              ;; Core Animation transaction so frames are never stretched.
              (progn
                (msg commands "commit" :void)
                (msg commands "waitUntilScheduled" :void)
                (msg drawable "present" :void))
              (progn
                (msg commands "presentDrawable:" :pointer drawable :void)
                (msg commands "commit" :void))))))))

(defun metal-snapshot (renderer instances columns cell-count cell-width cell-height
                       width height atlas palette &optional image-records)
  "Render offscreen and return the frame as a WIDTH x HEIGHT array of RGB
octets (row-major), for tests and debugging."
  (with-autorelease-pool ()
    (let* ((descriptor (msg "MTLTextureDescriptor"
                            "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
                            :unsigned-long +mtl-pixel-format-bgra8-unorm+
                            :unsigned-long width :unsigned-long height :bool nil)))
      ;; MTLTextureUsageShaderRead | MTLTextureUsageRenderTarget, shared storage.
      (msg descriptor "setUsage:" :unsigned-long 5 :void)
      (msg descriptor "setStorageMode:" :unsigned-long 0 :void)
      (let* ((texture (msg (metal-renderer-device renderer) "newTextureWithDescriptor:"
                           :pointer descriptor))
             (commands (metal-encode-frame renderer texture instances columns cell-count
                                           cell-width cell-height width height
                                           atlas palette image-records))
             (rgb (make-array (* 3 width height) :element-type '(unsigned-byte 8))))
        (msg commands "commit" :void)
        (msg commands "waitUntilCompleted" :void)
        (cffi:with-foreign-objects ((region :unsigned-long 6)
                                    (pixels :uint8 (* 4 width height)))
          (loop for value in (list 0 0 0 width height 1)
                for i from 0
                do (setf (cffi:mem-aref region :unsigned-long i) value))
          (msg texture "getBytes:bytesPerRow:fromRegion:mipmapLevel:"
               :pointer pixels :unsigned-long (* 4 width)
               :pointer region :unsigned-long 0 :void)
          (dotimes (i (* width height))
            (setf (aref rgb (* 3 i)) (cffi:mem-aref pixels :uint8 (+ 2 (* 4 i)))
                  (aref rgb (+ 1 (* 3 i))) (cffi:mem-aref pixels :uint8 (+ 1 (* 4 i)))
                  (aref rgb (+ 2 (* 3 i))) (cffi:mem-aref pixels :uint8 (* 4 i)))))
        (msg texture "release" :void)
        rgb))))
