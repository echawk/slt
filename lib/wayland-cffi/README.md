# wayland-cffi

CFFI bindings to libwayland-client and libwayland-egl, with each protocol
request generated, when the bindings are compiled, from the protocol's XML,
as wayland-scanner generates C. They do not depend on slt.

Other Common Lisp Wayland libraries do not fit a client that draws with EGL
or Vulkan: [wayflan](https://git.sr.ht/~shunter/wayflan) speaks the protocol
itself, so EGL and Vulkan, which take libwayland's `wl_display` and
`wl_surface`, cannot draw into its windows; [cl-wayland](https://github.com/malcolmstill/cl-wayland)
binds only the server side.

The protocols in `registry/` are
[wayland](https://gitlab.freedesktop.org/wayland/wayland) `protocol/wayland.xml`
(1.24.0, commit `736d12ac67c20c60dc406dc49bb06be878501f86`), and from
[wayland-protocols](https://gitlab.freedesktop.org/wayland/wayland-protocols)
(1.49, commit `ee78491a237eaff9389a0ccf8680521d074407d3`) `xdg-shell.xml`,
`xdg-decoration-unstable-v1.xml` and `cursor-shape-v1.xml`, each under the
license in its header. `src/bindings.lisp` selects them.

```lisp
(wl:load-wayland)                         ; opens libwayland-client
(let* ((display (wl:display-connect))     ; $WAYLAND_DISPLAY
       (registry (wl:wl-display-get-registry display)))
  (wl:set-listener registry
                   (lambda (event &rest arguments)
                     (when (eq event :global)
                       (destructuring-bind (name interface version) arguments
                         (format t "~a ~a ~a~%" name interface version)))))
  (wl:display-roundtrip display))
```

- **Requests.** `wl_surface.set_buffer_scale` is `wl-surface-set-buffer-scale`,
  taking the surface and then the request's arguments. A request making a
  new object returns it; `wl-registry-bind` takes the global's name, the
  interface's name and the version. Strings and objects may be `nil` where
  the protocol allows null, and fixed-point numbers are any real.
- **Events.** `set-listener` gives a proxy one function, called with the
  event's name as a keyword (`:configure`) and its arguments: objects as
  proxies, fixed-point numbers as doubles, arrays as octet vectors. A new
  object an event makes needs its own listener before the next dispatch.
  An error in a listener is reported on `*error-output*`, since unwinding
  through libwayland's dispatch would leave it locked.
- **Enums.** `wl_seat.capability`'s `keyboard` is `+wl-seat-capability-keyboard+`.
- **Loading.** The libraries are opened by `load-wayland` and
  `load-wayland-egl`, not on load, so a program carrying the bindings runs
  where there is no Wayland. libwayland 1.20 or later is needed
  (`wl_proxy_marshal_array_flags`).
- **Dispatching.** `display-prepare-read`, `display-read-events`,
  `display-cancel-read`, `display-dispatch-pending` and `display-flush` are
  libwayland's, for a loop polling `display-fd` with other descriptors.
  `check-display` signals `wayland-error` for a failed call.

Tests: `(asdf:test-system :wayland-cffi)`, which binds `wl_compositor` when
`WAYLAND_DISPLAY` names a compositor.
