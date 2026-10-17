# slt

A terminal emulator in Common Lisp. The terminal emulation layer (the
*engine*) and the user interface (the *backend*) are separate pieces, and
either can be swapped, configured or extended from Lisp.

## Quick start

```sh
make deps            # fetch Lisp dependencies into ocicl/ (needs ocicl)
make test            # unit tests, including the conformance suite on 3bst
./run-slt            # run from source
```

`scripts/lisp` runs SBCL with ASDF limited to this project. Everything is
resolved from `ocicl.csv`, `ocicl/` and `lib/`.

## Engines

| engine | what it is | setup |
|---|---|---|
| `ghostty` | Ghostty's libghostty-vt, via cl-ghostty-vt; fastest, with scrollback | `make ghostty` |
| `libvterm` | libvterm 0.3.3, the library Neovim uses (`lib/cl-libvterm`) | `make libvterm` |
| `3bst` | pure Lisp; a fork of 3b's port of st (`lib/3bst`) | none |

By default slt uses the first engine in this order that is available.

Choose one with `--engine NAME`, `SLT_ENGINE=NAME`, `(launch-terminal :engine
:NAME)` or `(setf slt:*default-engine* :NAME)` in your init file. Engines with
native libraries live in their own `slt/NAME` systems, which load on first use.

Output is parsed on a reader thread, and the UI draws at most once per frame,
so heavy output never waits on rendering. Ghostty copies only changed rows
into the grid through a small C helper (`src/native/slt-ghostty.c`), which is
compiled on first use. On a 23 MB `cat`, ghostty manages about 140 MB/s,
libvterm about 12 MB/s and 3bst about 2.6 MB/s.

Scrollback (ghostty): use the mouse wheel or trackpad, Shift+PageUp/PageDown,
and Shift+Home/End. Typing returns to the live screen. On the alternate
screen (vim, less), the wheel sends arrow keys instead.

`make conformance` runs the same byte streams through every available engine
and compares the resulting screens. `slt/bench` measures throughput.

## Backends

| backend | |
|---|---|
| `cocoa` | macOS default: AppKit window, Metal renderer, CoreText glyphs |
| `gl` | Linux/BSD default: GLFW window (X11 or Wayland), OpenGL 3.3 or OpenGL ES 3.0, FreeType glyphs found through font-discovery; also runs on macOS with CoreText glyphs |
| `ltk` | Tk; portable fallback (needs `wish`) where `gl` cannot run |
| `sdl2` | SDL2 renderer (`slt/sdl2`; needs SDL2, SDL2_ttf and SDL2_image) |
| `web` | serves a browser terminal; see [web/README.md](web/README.md) |
| `headless` | no UI; for tests, scripting and remote frontends |

Choose one with `--backend NAME` or `SLT_BACKEND=NAME`.

The `gl` backend uses Shirakumo's [glfw](https://codeberg.org/shirakumo/glfw)
bindings, which ship GLFW 3.4 for Linux and macOS (the BSDs use the system's
`libglfw`), and cl-opengl. GLFW chooses X11 or Wayland when it starts.
`make gl-test` renders styled text in a hidden window and checks the pixels.
[doc/gpu-backends.md](doc/gpu-backends.md) describes the design and what
Vulkan would add.

## Configuration

`~/.config/slt/init.lisp` is loaded at startup (skip it with `--no-config`).
It runs in the `slt` package:

```lisp
(setf *default-engine* :ghostty)
;; The default is a soft dark theme; :ansi :xterm restores classic colors.
(setf *palette* (make-palette :foreground #xcdd6f4 :background #x1e1e2e))
(add-hook '*title-change-hook* (lambda (emulator title) (declare (ignore emulator)) (print title)))
(add-hook '*key-hook* (lambda (emulator key modifiers)   ; return true to consume the key
                        (declare (ignore emulator))
                        (and (eql key #\q) (member :super modifiers))))
```

## Distribution

```sh
make bundle          # builds the native engines, then scripts/bundle
```

This produces a self-contained `bin/` directory: the `slt` executable with
every engine built in, plus the native libraries it loads (libvterm,
libghostty-vt and their shims, osicat's helper, and zstd for the compressed
image). Copy or tar the whole directory; nothing else needs to be installed.
It is built with [Deploy](https://shinmera.com/docs/deploy), which copies each
foreign library next to the executable and loads it from there at startup.
`src/deploy.lisp` lists the system libraries to leave out.

On macOS, `scripts/bundle` also:

- Builds with a copy of Homebrew's SBCL runtime whose zstd reference points
  at `@executable_path`. The finished executable cannot be edited afterwards,
  because the Lisp image is appended to it.
- Ad-hoc signs the libraries and the executable, since Apple Silicon kills
  binaries whose signatures are missing or invalid. To distribute outside
  your own machines, sign with a Developer ID and notarize.

The SDL2 backend is left out of the bundle because Homebrew's SDL2 brings a
chain of further libraries. LTK stays in, but needs `wish` on the target
machine.

`make dump` writes a single executable, `build/slt`, with only the
pure-Lisp engine. It is handy locally, but it loads native libraries from
their build-time paths.
