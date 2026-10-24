# kitty-graphics

A pure Common Lisp codec for the [Kitty terminal graphics
protocol](https://sw.kovidgoyal.net/kitty/graphics-protocol/). It is independent
of slt and provides:

- parsing and encoding of APC graphics commands and responses;
- case-sensitive control data access;
- quota-limited base64 chunk reassembly;
- normalization of 24-bit RGB, 32-bit RGBA, and PNG transfers to RGBA8;
- zlib decompression and direct, regular-file, temporary-file, and POSIX
  shared-memory transmission media.

Image storage, placements, deletion, animation, and terminal cursor policy
belong to the embedding terminal and are not performed implicitly by this
wire-format library.
