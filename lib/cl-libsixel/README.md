# cl-libsixel

A small CFFI binding to libsixel's in-memory decoder. `decode-sixel` accepts a
SIXEL byte vector or ASCII string and returns indexed pixels plus an RGBA
palette; `sixel-image-rgba` expands it for renderers that want RGBA8.

The system uses the platform `libsixel` shared library. On macOS it can be
installed with `brew install libsixel`.
