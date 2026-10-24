# kitty-graphics

A pure Common Lisp codec for the [Kitty terminal graphics
protocol](https://sw.kovidgoyal.net/kitty/graphics-protocol/). It is independent
of slt and provides:

- parsing and encoding of APC graphics commands and responses;
- case-sensitive control data access;
- quota-limited base64 chunk reassembly (padding optional; continuation
  chunks may carry `q`), with `graphics-decoder-abort` for delete commands;
- normalization of 24-bit RGB, 32-bit RGBA, indexed (`f=8`/`f=16`, with a
  replaceable palette) and PNG transfers to RGBA8;
- zlib decompression for every format and direct, regular-file,
  temporary-file, and POSIX shared-memory transmission media, with the
  spec's file safety rules (regular files only, no `/proc`, `/sys` or
  `/dev`, temporary files only in temporary directories);
- errors carrying the response code (`EINVAL`, `EBADF`, `ENODATA`, `EFBIG`)
  via `graphics-protocol-error-code`.

Image storage, placements, deletion, animation, and terminal cursor policy
belong to the embedding terminal and are not performed implicitly by this
wire-format library.
