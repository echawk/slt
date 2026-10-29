# egl-cffi

CFFI bindings to EGL, generated from the EGL registry when they are
compiled, as [vulkan-cffi](../vulkan-cffi/) is from Vulkan's. They do not
depend on slt.

The registry is `registry/egl.xml` from
[KhronosGroup/EGL-Registry](https://github.com/KhronosGroup/EGL-Registry)
(commit `e964211ff5f7c2bcada786954a16d5b813379395`), under its own license
(Apache-2.0). `src/bindings.lisp` selects EGL 1.5 with the X11 and Wayland
platform constants. EGL 1.5's `eglGetPlatformDisplay` and
`eglCreatePlatformWindowSurface` take either window system's display and
window.

```lisp
(egl:load-egl)                       ; opens libEGL; loads the commands
(egl:with-attributes (attributes :intptr '())
  (egl:check-egl (egl:get-platform-display egl:+platform-wayland-khr+
                                           wl-display attributes)
                 'egl:get-platform-display))
```

- **Names.** `eglGetPlatformDisplay` is `get-platform-display` and
  `EGL_PLATFORM_WAYLAND_KHR` is `+platform-wayland-khr+`. The package
  `egl-cffi` uses no other package.
- **Loading.** The library is opened by `load-egl`, not on load, so a
  program carrying the bindings runs where there is no EGL. Commands come
  from the library's exports, or else from `eglGetProcAddress`.
- **Errors.** `check-egl` signals `egl-error`, naming `eglGetError`'s
  code, for a false or null result.
- **Attributes.** `with-attributes` makes an attribute list ending in
  `EGL_NONE`.

Tests: `(asdf:test-system :egl-cffi)`.
