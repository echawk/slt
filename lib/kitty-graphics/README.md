# kitty-graphics

A pure Common Lisp codec for the [Kitty terminal graphics
protocol](https://sw.kovidgoyal.net/kitty/graphics-protocol/). It is independent
of slt and provides:

- parsing and encoding of APC graphics commands and responses;
- case-sensitive control data access;
- quota-limited base64 chunk reassembly;
- normalization of direct 24-bit RGB and 32-bit RGBA transfers to RGBA8.

File/shared-memory transmission, zlib, PNG decoding, image storage, placements,
and animation policy belong to the embedding terminal and are not performed
implicitly by this codec.
