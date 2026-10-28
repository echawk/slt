# iterm2-images

iTerm2's inline images (OSC 1337 `File`): the protocol, the size to show
an image at, and decoding the files into RGBA frames. It knows nothing of
any terminal's screen; it does not depend on slt.

```lisp
(let ((decoder (iterm2-images:make-decoder)))
  ;; OCTETS hold an OSC string's payload, START to END.
  (let ((command (iterm2-images:file-command-start octets start end)))
    (when command
      (let ((image (iterm2-images:decoder-feed decoder octets command end)))
        (when image
          (multiple-value-bind (width height stretch-p)
              (iterm2-images:image-display-size image :cell-width 9 :cell-height 18
                                                      :screen-width 720 :screen-height 432)
            ;; Show (iterm2-images:image-frames image) at the cursor, WIDTH x
            ;; HEIGHT pixels, then move the cursor past it unless
            ;; (iterm2-images:image-moves-cursor-p image) is false.
            ))))))
```

**Commands.** `File=ARGUMENTS:BASE64` sends a file whole;
`MultipartFile=ARGUMENTS`, `FilePart=BASE64` (as often as needed) and
`FileEnd` send it in parts, which the decoder keeps until the end (up to
`max-encoded-bytes`). `file-command-start` tells these from other OSC 1337
strings.

**Arguments.** Only `inline=1` files are images; others are downloads,
which `decoder-feed` returns no image for. `width` and `height` take cells
(`N`), pixels (`Npx`), a percentage of the screen (`N%`) or `auto`; with
both given, `preserveAspectRatio=0` stretches the image to them, and
otherwise it is fitted inside. With neither, an image keeps its size,
shrunk to fit the screen. `doNotMoveCursor=1` (from WezTerm) leaves the
cursor where it was. Sizes follow WezTerm's reading of iTerm2's
(wezterm/wezterm e68a8ec, `term/src/terminalstate/iterm.rs`).

**Files.** `decode-image-file` tells the format by its signature and
returns RGBA frames: PNG through kitty-graphics' libpng binding, JPEG
through cl-jpeg (baseline only: progressive JPEGs are refused), GIF through
skippy, every frame composed as its disposal method says, with its delay
(short delays read as 100 ms, as browsers do) and whether it loops.
Images over `*max-image-octets*` of RGBA are refused. Failures signal
`image-error`.

Tests: `(asdf:test-system :iterm2-images)`.
