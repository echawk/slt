# cl-libvterm

A standalone Common Lisp (CFFI) binding to [libvterm](https://www.leonerd.org.uk/code/libvterm/),
the VT220/xterm emulation library used by Neovim. ASDF system: `libvterm`
(depends on `cffi` and `babel`). It has no dependency on slt.

## libvterm version

Pinned to **libvterm 0.3.3**
(`https://launchpad.net/libvterm/trunk/v0.3/+download/libvterm-0.3.3.tar.gz`,
mirror `https://www.leonerd.org.uk/code/libvterm/libvterm-0.3.3.tar.gz`).
The SHA-256 in `scripts/build-libvterm.sh`
(`09156f43dd2128bd347cbeebe50d9a571d32c64e0cf18d211197946aff7226e0`) was
recorded on the first fetch of the tarball, not taken from an upstream
announcement.

## Building

    lib/cl-libvterm/scripts/build-libvterm.sh          # idempotent
    lib/cl-libvterm/scripts/build-libvterm.sh --force  # clean rebuild

The script downloads and verifies the tarball into `.deps/`, compiles
`src/*.c` directly with `$CC` (no libtool or perl needed; the tarball ships
the generated tables), and writes:

- `build/lib/libvterm.{dylib,so}`
- `build/lib/libvterm-cl-shim.{dylib,so}`: `src/shim.c`, see below
- `build/include/vterm.h`, `vterm_keycodes.h`

`build/` and `.deps/` are gitignored. Tested on macOS arm64; the Linux path
uses `-shared -fPIC`.

### Library lookup

`library.lisp` loads libvterm from the first of these that works:
`$LIBVTERM_LIBRARY`, `build/lib/libvterm.<ext>`, or the system default
(`libvterm.so.0`/`libvterm.so`, `libvterm.0.dylib`/`libvterm.dylib`). Then it
loads the shim from `$LIBVTERM_SHIM_LIBRARY` or `build/lib/`. The shim is
linked with unresolved `vterm_*` symbols (`-undefined dynamic_lookup` on
macOS), so it binds to whichever libvterm was loaded first. Any 0.3.x build
works.

### The shim

libvterm's callbacks take `VTermRect` and `VTermPos` by value. String
properties arrive as a bitfield struct (`VTermStringFragment`), cells have
bitfield attributes, and colours are a tagged union. CFFI cannot express these
cleanly, so `src/shim.c` (about 250 lines) does four things:

- installs the screen and output callbacks and flattens their arguments into
  ints;
- packs cells and rows into `uint32` words;
- creates a terminal configured with UTF-8, the alternate screen,
  `VTERM_DAMAGE_SCROLL` damage merging and a hard reset;
- exports the `sizeof` values it was compiled with. At load time, `check-abi`
  compares them with the Lisp side and signals an error on a mismatch.

The shim compares sizes against the headers in `build/include`. If you point
`$LIBVTERM_LIBRARY` at a libvterm that is not 0.3.x, that mismatch is not
detected.

Callbacks carry an integer terminal id as libvterm's `user` pointer. Lisp
maps the id to a `vterm` object through a registry, so any number of
terminals can be live at the same time, and each one has its own closures.
Errors raised by user closures inside a callback are caught, so they never
unwind through C. They are signalled again when control returns from the
`vterm-feed`, `vterm-key` or other call that triggered them.

## Testing

From the slt repository root:

    scripts/lisp --eval '(asdf:test-system :libvterm)'

## API (package `libvterm`)

### Lifecycle

- `(make-vterm &key (rows 24) (columns 80) on-output on-bell on-title on-scrollback)` returns a `vterm`.
- `(vterm-free vt)` can be called more than once safely. `(vterm-live-p vt)` reports whether `vt` is still live.
- `(with-vterm (var &rest make-vterm-args) &body body)` frees the terminal when `body` exits.
- `(load-libvterm &key force)` runs automatically at load time. `*libvterm-version*` is `"0.3.3"`.

### Callbacks

These are slots set by `make-vterm`, and each one can be changed with `setf`:

| Slot | Called with |
|---|---|
| `vterm-on-output` | `(vt octets)`, the bytes to write to the pty: key encodings and DA/DSR replies |
| `vterm-on-bell` | `(vt)` |
| `vterm-on-title` | `(vt string)`. OSC 0/2 fragments are joined and decoded as UTF-8 first |
| `vterm-on-scrollback` | `(vt codes fg bg attrs)`, the packed row arrays of a line pushed off the top of the primary screen |

### Input

- `(vterm-feed vt data &key (start 0) end)`: `data` is an octet vector or a
  string, which is encoded as UTF-8. Returns the number of octets consumed.
- `(vterm-resize vt rows columns)`; `(vterm-rows vt)`; `(vterm-columns vt)`.
- `(vterm-key vt key &key mods)`: `key` is one of `:up :down :left :right
  :home :end :page-up :page-down :insert :delete :backspace :tab :enter
  :escape :f1`…`:f12` (any `:fN` up to 255) or `:kp-0`…`:kp-equal`. `mods`
  is a list of `:shift :alt/:meta :ctrl/:control`, or an integer. The output
  goes to `on-output`.
- `(vterm-char vt char-or-codepoint &key mods)`
- `(vterm-encode-key vt key &key mods)` and `(vterm-encode-char vt char &key mods)`
  return the encoded octets directly and skip `on-output`. The encoding
  follows the current terminal modes, so `:up` gives `ESC [ A`, or
  `ESC O A` after `CSI ? 1 h`.
- `(vterm-paste-start vt)`, `(vterm-paste-end vt)`: bracketed paste.

### State

- `(vterm-cursor vt)` returns `(values row col visible-p)`. Visibility comes
  from `VTERM_PROP_CURSORVISIBLE`.
- `(vterm-title vt)`, `(vterm-icon-name vt)`, `(vterm-altscreen-p vt)`
- `(vterm-property vt key)`: `key` is one of `:cursor-blink :altscreen
  :reverse :focus-report :cursor-shape :mouse`.

### Screen contents

- `(vterm-cell vt row col)` returns
  `(:codepoints (cp ...) :width w :fg color :bg color :attrs (kw ...))`.
  - `color` is `(:default)`, `(:palette n)` or `(:rgb r g b)`. Default colours
    are never flattened to RGB.
  - `attrs` is a subset of `:bold :italic :underline :double-underline
    :curly-underline :blink :inverse :conceal :strike`.
  - The right half of a wide character has `:width 0` and no codepoints.
- `(vterm-row-text vt row &key start end)` and `(vterm-screen-text vt)` drop
  trailing blanks on each row. Rows are joined with newlines.
- `(vterm-row-into vt row codes fg bg attrs &key (start 0) end)` is the fast
  path; see the encoding below. Returns the number of cells written.
- `(make-row-buffer columns)`, `(decode-color word)`, `(decode-attrs word)`,
  `(attrs-width word)` and `(attrs-combining-p word)` are helpers for it.

#### `vterm-row-into` encoding

`vterm-row-into` writes to caller-supplied
`(simple-array (unsigned-byte 32) (*))` arrays. Pass `nil` for any array to
skip it. Element *i* describes column *i*, so each array needs at least `end`
elements.

- **codes**: the base codepoint. `0` means an empty cell or the right half of
  a wide character. Combining marks are not included here. Bit 8 of `attrs`
  marks a cell that has them; call `vterm-cell` to get them.
- **fg / bg**: a colour packed into one word:

  | Value | Meaning |
  |---|---|
  | `0` | default colour |
  | `#x01000000` + index | palette entry, index 0–255 |
  | `#x02000000` + `#xRRGGBB` | 24-bit RGB |

- **attrs**:

  | Bits | Meaning | Values |
  |---|---|---|
  | 0 | bold | |
  | 1 | italic | |
  | 2–3 | underline | 0 off, 1 single, 2 double, 3 curly |
  | 4 | blink | |
  | 5 | inverse | |
  | 6 | conceal | |
  | 7 | strike | |
  | 8 | has combining characters | |
  | 9–10 | width | 1, 2, or 0 for the right half of a wide character |
  | 12–15 | font | 0–9 |
  | 16 | small | |
  | 17–18 | baseline | |
  | 19 | DECDWL line | |
  | 20–21 | DECDHL half | |

  The constants `+attr-bold+`, `+attr-italic+`, `+attr-underline-mask+`,
  `+attr-blink+`, `+attr-inverse+`, `+attr-conceal+`, `+attr-strike+`,
  `+attr-combining+`, `+attr-width-mask+` and `+attr-width-shift+` are
  exported.

### Damage

`(vterm-take-damage vt)` returns `(values bit-vector any-dirty-p)`, with one
bit per row, and clears the terminal's damage.

These all mark rows dirty:

- damage callbacks;
- `moverect` (scrolling), which marks the destination rows;
- cursor movement, which marks the old and new cursor rows;
- cursor visibility changes;
- switching between the main and alternate screens;
- reverse-video changes;
- resizes, which mark every row.

`(vterm-mark-all-dirty vt)` marks every row dirty.

## License

MIT; see `LICENSE`. libvterm itself is MIT-licensed (© 2008 Paul Evans). Its
source is downloaded at build time and is not vendored here.
