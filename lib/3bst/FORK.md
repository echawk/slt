# slt fork of 3bst

Vendored from https://github.com/3b/3bst at commit
6a208957fdec3842ca5b8265914a20d7b676aba3 (MIT, see LICENSE). Changes are
kept small so they can be sent upstream; each is covered by a case in
slt's conformance suite (`conformance/cases.lisp`).

- ICH (`CSI @`) now inserts blanks; the handler only defaulted its argument.
- `tinsertblank` and insert mode (IRM) pass `:end2` to `move-glyphs`, so
  cells actually shift right instead of being copied onto themselves.
- ED 1 (`CSI 1 J`) clears every row above the cursor, including row 0
  when the cursor is on row 1 (an off-by-one inherited from st).
- DSR: `CSI 6 n` replies `CSI row ; col R` (it replied to `CSI 0 n`, with
  a comma), and `CSI 5 n` replies `CSI 0 n`.
- REP (`CSI b`) repeats the last printed character.
- `tputc` refetches the glyph after wrapping, so a wide character that
  wraps gets its wide attribute on the cell it was written to.
- `tresize` keeps the cursor where it was: st restores a by-value copy of
  the cursor after clearing both screens, but the port "copied" the same
  object, so the cursor was reset to the saved position.  It also moves
  the right terminal's cursor (`:term` was missing from `tmoveto`) and
  slides lines up rather than down when shrinking below the cursor.
- Erased cells hold `#\Nul` rather than a space, so a printed space can be
  told apart from a blank (slt erases SIXEL images under printed text).
- `term-line-length` reads a line's length from the line vector; it took
  the screen for a two-dimensional array.
