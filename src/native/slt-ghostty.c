/* Bulk access to libghostty-vt for slt's ghostty engine.
 *
 * Reading a screen cell by cell through the Lisp binding costs several
 * foreign calls per cell.  This helper walks Ghostty's render state once
 * per frame and writes only the dirty rows straight into slt's grid
 * arrays, so the engine lock is held for microseconds rather than
 * milliseconds.  The encodings must match src/grid.lisp:
 *
 *   codes  uint32  base codepoint, 0 for an empty cell
 *   fg/bg  uint32  0 default, 0x01000000|index palette, 0x02000000|RGB
 *   attrs  uint16  +attr-*+ bits; SLT_ATTR_COMBINING marks cells whose
 *                  extra codepoints are listed in the EXTRA buffer
 *
 * Symbols are resolved against the libghostty-vt already loaded into the
 * process (-undefined dynamic_lookup), so load that library first.
 */
#include <ghostty/vt.h>
#include <stdint.h>
#include <stdlib.h>

enum {
  SLT_ATTR_BOLD = 1u << 0,
  SLT_ATTR_FAINT = 1u << 1,
  SLT_ATTR_ITALIC = 1u << 2,
  SLT_ATTR_UNDERLINE = 1u << 3,
  SLT_ATTR_BLINK = 1u << 4,
  SLT_ATTR_INVERSE = 1u << 5,
  SLT_ATTR_INVISIBLE = 1u << 6,
  SLT_ATTR_STRIKE = 1u << 7,
  SLT_ATTR_WIDE = 1u << 8,
  SLT_ATTR_WIDE_SPACER = 1u << 9,
  SLT_ATTR_DOUBLE_UNDERLINE = 1u << 10,
  SLT_ATTR_COMBINING = 1u << 15,
};

typedef struct {
  GhosttyRenderState state;
  GhosttyRenderStateRowIterator rows;
  GhosttyRenderStateRowCells cells;
} slt_ghostty;

slt_ghostty *slt_ghostty_new(void) {
  slt_ghostty *s = calloc(1, sizeof *s);
  if (!s) return NULL;
  if (ghostty_render_state_new(NULL, &s->state) != GHOSTTY_SUCCESS ||
      ghostty_render_state_row_iterator_new(NULL, &s->rows) != GHOSTTY_SUCCESS ||
      ghostty_render_state_row_cells_new(NULL, &s->cells) != GHOSTTY_SUCCESS) {
    free(s);
    return NULL;
  }
  return s;
}

void slt_ghostty_free(slt_ghostty *s) {
  if (!s) return;
  ghostty_render_state_row_cells_free(s->cells);
  ghostty_render_state_row_iterator_free(s->rows);
  ghostty_render_state_free(s->state);
  free(s);
}

static uint32_t style_color(GhosttyStyleColor c) {
  switch (c.tag) {
  case GHOSTTY_STYLE_COLOR_PALETTE:
    return 0x01000000u | c.value.palette;
  case GHOSTTY_STYLE_COLOR_RGB:
    return 0x02000000u | ((uint32_t)c.value.rgb.r << 16) |
           ((uint32_t)c.value.rgb.g << 8) | c.value.rgb.b;
  default:
    return 0;
  }
}

static uint16_t style_attrs(const GhosttyStyle *style) {
  uint16_t a = 0;
  if (style->bold) a |= SLT_ATTR_BOLD;
  if (style->faint) a |= SLT_ATTR_FAINT;
  if (style->italic) a |= SLT_ATTR_ITALIC;
  if (style->blink) a |= SLT_ATTR_BLINK;
  if (style->inverse) a |= SLT_ATTR_INVERSE;
  if (style->invisible) a |= SLT_ATTR_INVISIBLE;
  if (style->strikethrough) a |= SLT_ATTR_STRIKE;
  if (style->underline == 2) a |= SLT_ATTR_DOUBLE_UNDERLINE;
  else if (style->underline != 0) a |= SLT_ATTR_UNDERLINE;
  return a;
}

/* Copy the viewport into the caller's ROWS x COLUMNS arrays (row-major).
 *
 * Rows that changed since the last call (all rows with FORCE) are written
 * and flagged in DIRTY (one byte per row).  Extra codepoints of combining
 * clusters are appended to EXTRA as [cell-index, count, cp...] records;
 * *EXTRA_LEN receives the number of uint32s used (records that would not
 * fit in EXTRA_CAPACITY are dropped).  CURSOR receives row, column,
 * visible, and visual style (0 bar, 1 block, 2 underline, 3 hollow block).
 * Returns the number of rows written, or -1 on error. */
int slt_ghostty_copy(slt_ghostty *s, GhosttyTerminal terminal, int rows, int columns,
                     int force, uint32_t *codes, uint32_t *fg, uint32_t *bg,
                     uint16_t *attrs, uint8_t *dirty, uint32_t *extra,
                     int extra_capacity, int *extra_len, int32_t *cursor) {
  *extra_len = 0;
  if (ghostty_render_state_update(s->state, terminal) != GHOSTTY_SUCCESS) return -1;

  GhosttyRenderStateDirty state_dirty = GHOSTTY_RENDER_STATE_DIRTY_FULL;
  ghostty_render_state_get(s->state, GHOSTTY_RENDER_STATE_DATA_DIRTY, &state_dirty);
  int all = force || state_dirty == GHOSTTY_RENDER_STATE_DIRTY_FULL;
  int written = 0;

  if (all || state_dirty != GHOSTTY_RENDER_STATE_DIRTY_FALSE) {
    ghostty_render_state_get(s->state, GHOSTTY_RENDER_STATE_DATA_ROW_ITERATOR, &s->rows);
    for (int y = 0; y < rows && ghostty_render_state_row_iterator_next(s->rows); y++) {
      bool row_dirty = true;
      if (!all) ghostty_render_state_row_get(s->rows, GHOSTTY_RENDER_STATE_ROW_DATA_DIRTY, &row_dirty);
      if (!row_dirty) continue;
      ghostty_render_state_row_get(s->rows, GHOSTTY_RENDER_STATE_ROW_DATA_CELLS, &s->cells);
      size_t base = (size_t)y * (size_t)columns;
      int x = 0;
      for (; x < columns && ghostty_render_state_row_cells_next(s->cells); x++) {
        size_t i = base + (size_t)x;
        GhosttyCell raw = 0;
        GhosttyStyle style = GHOSTTY_INIT_SIZED(GhosttyStyle);
        uint32_t graphemes = 0;
        GhosttyCellContentTag content = GHOSTTY_CELL_CONTENT_CODEPOINT;
        GhosttyCellWide wide = GHOSTTY_CELL_WIDE_NARROW;
        uint32_t code = 0;
        ghostty_render_state_row_cells_get(s->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_RAW, &raw);
        ghostty_render_state_row_cells_get(s->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_STYLE, &style);
        ghostty_render_state_row_cells_get(s->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_GRAPHEMES_LEN, &graphemes);
        ghostty_cell_get(raw, GHOSTTY_CELL_DATA_CONTENT_TAG, &content);
        ghostty_cell_get(raw, GHOSTTY_CELL_DATA_WIDE, &wide);
        if (graphemes > 0) ghostty_cell_get(raw, GHOSTTY_CELL_DATA_CODEPOINT, &code);

        uint32_t background = style_color(style.bg_color);
        if (content == GHOSTTY_CELL_CONTENT_BG_COLOR_PALETTE) {
          GhosttyColorPaletteIndex index = 0;
          ghostty_cell_get(raw, GHOSTTY_CELL_DATA_COLOR_PALETTE, &index);
          background = 0x01000000u | index;
        } else if (content == GHOSTTY_CELL_CONTENT_BG_COLOR_RGB) {
          GhosttyColorRgb rgb = {0};
          ghostty_cell_get(raw, GHOSTTY_CELL_DATA_COLOR_RGB, &rgb);
          background = 0x02000000u | ((uint32_t)rgb.r << 16) | ((uint32_t)rgb.g << 8) | rgb.b;
        }

        uint16_t a = style_attrs(&style);
        if (wide == GHOSTTY_CELL_WIDE_WIDE) a |= SLT_ATTR_WIDE;
        else if (wide == GHOSTTY_CELL_WIDE_SPACER_TAIL) a |= SLT_ATTR_WIDE_SPACER;

        if (graphemes > 1) {
          int need = 2 + (int)graphemes - 1;
          if (*extra_len + 1 + need <= extra_capacity) {
            /* Buffer layout: cell index, count, then the extra codepoints. */
            uint32_t buffer[64];
            uint32_t n = graphemes < 64 ? graphemes : 64;
            ghostty_render_state_row_cells_get(s->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_GRAPHEMES_BUF, buffer);
            extra[(*extra_len)++] = (uint32_t)i;
            extra[(*extra_len)++] = n - 1;
            for (uint32_t k = 1; k < n; k++) extra[(*extra_len)++] = buffer[k];
            a |= SLT_ATTR_COMBINING;
          }
        }

        codes[i] = code;
        fg[i] = style_color(style.fg_color);
        bg[i] = background;
        attrs[i] = a;
      }
      for (; x < columns; x++) {
        size_t i = base + (size_t)x;
        codes[i] = 0;
        fg[i] = 0;
        bg[i] = 0;
        attrs[i] = 0;
      }
      dirty[y] = 1;
      written++;
    }
  }

  GhosttyRenderStateCursor c = GHOSTTY_INIT_SIZED(GhosttyRenderStateCursor);
  if (ghostty_render_state_get(s->state, GHOSTTY_RENDER_STATE_DATA_CURSOR, &c) == GHOSTTY_SUCCESS) {
    cursor[0] = c.viewport_y;
    cursor[1] = c.viewport_x;
    cursor[2] = c.visible && c.viewport_has_value;
    cursor[3] = (int32_t)c.visual_style;
  }
  ghostty_render_state_clean(s->state);
  return written;
}

/* Scroll the viewport: MODE 0 top, 1 bottom, 2 by DELTA rows (negative is up). */
void slt_ghostty_scroll(GhosttyTerminal terminal, int mode, long delta) {
  GhosttyTerminalScrollViewport scroll = {0};
  switch (mode) {
  case 0: scroll.tag = GHOSTTY_SCROLL_VIEWPORT_TOP; break;
  case 1: scroll.tag = GHOSTTY_SCROLL_VIEWPORT_BOTTOM; break;
  default:
    scroll.tag = GHOSTTY_SCROLL_VIEWPORT_DELTA;
    scroll.value.delta = (intptr_t)delta;
    break;
  }
  ghostty_terminal_scroll_viewport(terminal, scroll);
}
