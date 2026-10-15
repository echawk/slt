/*
 * cl-libvterm shim: the small amount of C that CFFI cannot express cleanly.
 *
 *  - libvterm's callbacks take VTermPos/VTermRect by value and string
 *    properties as a bitfield struct (VTermStringFragment).  The shim
 *    installs C callbacks that flatten those into plain ints and forwards
 *    them to one table of hooks registered from Lisp.  The per-terminal
 *    `user` pointer is an integer id that Lisp maps to its own terminal
 *    object, so any number of terminals can be live at once.
 *  - VTermScreenCell has bitfield attributes and VTermColor is a tagged
 *    union.  The shim packs a cell (or a whole row) into fixed-width
 *    uint32 words; see the encoding below and README.md.
 *
 * Packed colour (uint32):
 *   0x00000000            default colour (VTERM_COLOR_DEFAULT_FG/BG set)
 *   0x01000000 | idx      palette index 0..255
 *   0x02000000 | 0xRRGGBB 24-bit RGB
 *
 * Packed attributes (uint32):
 *   bit 0      bold
 *   bit 1      italic
 *   bits 2-3   underline (0 off, 1 single, 2 double, 3 curly)
 *   bit 4      blink
 *   bit 5      reverse/inverse
 *   bit 6      conceal/invisible
 *   bit 7      strike
 *   bit 8      cell holds more than one codepoint (combining marks)
 *   bits 9-10  width (1 normal, 2 wide, 0 = right half of a wide char)
 *   bits 12-15 font (0-9)
 *   bit 16     small
 *   bits 17-18 baseline (0 normal, 1 raise, 2 lower)
 *   bit 19     DECDWL/DECDHL line
 *   bits 20-21 DECDHL half (1 top, 2 bottom)
 *
 * MIT License, Copyright (c) 2026 Ethan Hawk.
 */
#include <stdint.h>
#include <string.h>
#include "vterm.h"

#define CLVT_ABI 1

typedef struct {
  int  (*damage)(int id, int sr, int er, int sc, int ec);
  int  (*moverect)(int id, int dsr, int der, int dsc, int dec,
                   int ssr, int ser, int ssc, int sec);
  int  (*movecursor)(int id, int row, int col, int visible);
  int  (*prop_int)(int id, int prop, int value);
  int  (*prop_string)(int id, int prop, const char *s, int len, int initial, int final);
  int  (*bell)(int id);
  int  (*resize)(int id, int rows, int cols);
  int  (*sb_pushline)(int id, int cols, const VTermScreenCell *cells);
  void (*output)(int id, const char *s, size_t len);
} clvt_hooks;

typedef struct {
  uint32_t chars[VTERM_MAX_CHARS_PER_CELL];
  uint32_t fg, bg, attrs;
} clvt_cell;

static clvt_hooks hooks;

#define ID(user) ((int)(intptr_t)(user))

static int cb_damage(VTermRect r, void *user)
{
  return hooks.damage ? hooks.damage(ID(user), r.start_row, r.end_row, r.start_col, r.end_col) : 0;
}

static int cb_moverect(VTermRect d, VTermRect s, void *user)
{
  return hooks.moverect
    ? hooks.moverect(ID(user), d.start_row, d.end_row, d.start_col, d.end_col,
                     s.start_row, s.end_row, s.start_col, s.end_col)
    : 0;
}

static int cb_movecursor(VTermPos pos, VTermPos oldpos, int visible, void *user)
{
  (void)oldpos;
  return hooks.movecursor ? hooks.movecursor(ID(user), pos.row, pos.col, visible) : 0;
}

static int cb_settermprop(VTermProp prop, VTermValue *val, void *user)
{
  switch(vterm_get_prop_type(prop)) {
  case VTERM_VALUETYPE_BOOL:
    return hooks.prop_int ? hooks.prop_int(ID(user), prop, val->boolean) : 1;
  case VTERM_VALUETYPE_INT:
    return hooks.prop_int ? hooks.prop_int(ID(user), prop, val->number) : 1;
  case VTERM_VALUETYPE_STRING:
    return hooks.prop_string
      ? hooks.prop_string(ID(user), prop, val->string.str, (int)val->string.len,
                          val->string.initial, val->string.final)
      : 1;
  default:
    return 1;
  }
}

static int cb_bell(void *user)
{
  return hooks.bell ? hooks.bell(ID(user)) : 0;
}

static int cb_resize(int rows, int cols, void *user)
{
  return hooks.resize ? hooks.resize(ID(user), rows, cols) : 0;
}

static int cb_sb_pushline(int cols, const VTermScreenCell *cells, void *user)
{
  return hooks.sb_pushline ? hooks.sb_pushline(ID(user), cols, cells) : 0;
}

static void cb_output(const char *s, size_t len, void *user)
{
  if(hooks.output)
    hooks.output(ID(user), s, len);
}

static const VTermScreenCallbacks screen_callbacks = {
  .damage      = cb_damage,
  .moverect    = cb_moverect,
  .movecursor  = cb_movecursor,
  .settermprop = cb_settermprop,
  .bell        = cb_bell,
  .resize      = cb_resize,
  .sb_pushline = cb_sb_pushline,
  .sb_popline  = NULL,
  .sb_clear    = NULL,
};

/* ---- setup ---------------------------------------------------------- */

int clvt_abi_version(void) { return CLVT_ABI; }
int clvt_header_version(void) { return VTERM_VERSION_MAJOR * 100 + VTERM_VERSION_MINOR; }

/* Layout sanity values for the Lisp side. */
int clvt_sizeof(int which)
{
  switch(which) {
  case 0: return (int)sizeof(VTermScreenCell);
  case 1: return (int)sizeof(clvt_cell);
  case 2: return (int)sizeof(VTermPos);
  case 3: return (int)sizeof(VTermRect);
  case 4: return (int)sizeof(clvt_hooks);
  case 5: return VTERM_MAX_CHARS_PER_CELL;
  default: return -1;
  }
}

void clvt_set_hooks(const clvt_hooks *h)
{
  if(h) hooks = *h;
  else memset(&hooks, 0, sizeof hooks);
}

VTerm *clvt_new(int rows, int cols, int id)
{
  VTerm *vt = vterm_new(rows, cols);
  if(!vt)
    return NULL;
  void *user = (void *)(intptr_t)id;
  vterm_set_utf8(vt, 1);
  vterm_output_set_callback(vt, cb_output, user);
  VTermScreen *screen = vterm_obtain_screen(vt);
  vterm_screen_set_callbacks(screen, &screen_callbacks, user);
  vterm_screen_enable_altscreen(screen, 1);
  vterm_screen_set_damage_merge(screen, VTERM_DAMAGE_SCROLL);
  vterm_screen_reset(screen, 1);
  return vt;
}

void clvt_get_cursor(VTerm *vt, int *row, int *col)
{
  VTermPos pos;
  vterm_state_get_cursorpos(vterm_obtain_state(vt), &pos);
  *row = pos.row;
  *col = pos.col;
}

/* ---- cell packing --------------------------------------------------- */

static uint32_t pack_color(const VTermColor *c)
{
  if(c->type & VTERM_COLOR_DEFAULT_MASK)
    return 0;
  if(VTERM_COLOR_IS_INDEXED(c))
    return 0x01000000u | c->indexed.idx;
  return 0x02000000u | ((uint32_t)c->rgb.red << 16) | ((uint32_t)c->rgb.green << 8) | c->rgb.blue;
}

static uint32_t pack_attrs(const VTermScreenCell *cell)
{
  const VTermScreenCellAttrs *a = &cell->attrs;
  uint32_t width = cell->chars[0] == (uint32_t)-1 ? 0 : (uint32_t)(cell->width & 3);
  uint32_t w = 0;
  w |= a->bold;
  w |= (uint32_t)a->italic << 1;
  w |= (uint32_t)a->underline << 2;
  w |= (uint32_t)a->blink << 4;
  w |= (uint32_t)a->reverse << 5;
  w |= (uint32_t)a->conceal << 6;
  w |= (uint32_t)a->strike << 7;
  w |= (uint32_t)(width && cell->chars[0] && cell->chars[1]) << 8;
  w |= width << 9;
  w |= (uint32_t)a->font << 12;
  w |= (uint32_t)a->small << 16;
  w |= (uint32_t)a->baseline << 17;
  w |= (uint32_t)a->dwl << 19;
  w |= (uint32_t)a->dhl << 20;
  return w;
}

static void pack_cell(const VTermScreenCell *cell, uint32_t *code, uint32_t *fg,
                      uint32_t *bg, uint32_t *attrs)
{
  uint32_t c = cell->chars[0];
  if(code)  *code  = c == (uint32_t)-1 ? 0 : c;
  if(fg)    *fg    = pack_color(&cell->fg);
  if(bg)    *bg    = pack_color(&cell->bg);
  if(attrs) *attrs = pack_attrs(cell);
}

static int fetch_cell(VTermScreen *screen, int row, int col, VTermScreenCell *cell)
{
  memset(cell, 0, sizeof *cell);
  VTermPos pos = { .row = row, .col = col };
  return vterm_screen_get_cell(screen, pos, cell);
}

/* Unpack one cell with every codepoint.  Returns 0 if out of range. */
int clvt_get_cell(VTerm *vt, int row, int col, clvt_cell *out)
{
  VTermScreenCell cell;
  memset(out, 0, sizeof *out);
  if(!fetch_cell(vterm_obtain_screen(vt), row, col, &cell))
    return 0;
  if(cell.chars[0] != (uint32_t)-1)
    for(int i = 0; i < VTERM_MAX_CHARS_PER_CELL && cell.chars[i]; i++)
      out->chars[i] = cell.chars[i];
  pack_cell(&cell, NULL, &out->fg, &out->bg, &out->attrs);
  return 1;
}

/* Unpack columns [start_col, end_col) of one row into parallel uint32
 * arrays (any may be NULL).  Element i corresponds to column start_col+i.
 * Returns the number of cells written. */
int clvt_get_row(VTerm *vt, int row, int start_col, int end_col,
                 uint32_t *codes, uint32_t *fg, uint32_t *bg, uint32_t *attrs)
{
  VTermScreen *screen = vterm_obtain_screen(vt);
  VTermScreenCell cell;
  int n = 0;
  for(int col = start_col; col < end_col; col++, n++) {
    if(!fetch_cell(screen, row, col, &cell))
      break;
    pack_cell(&cell, codes ? codes + n : NULL, fg ? fg + n : NULL,
              bg ? bg + n : NULL, attrs ? attrs + n : NULL);
  }
  return n;
}

/* Same packing for a VTermScreenCell array (e.g. from sb_pushline). */
void clvt_pack_cells(const VTermScreenCell *cells, int n,
                     uint32_t *codes, uint32_t *fg, uint32_t *bg, uint32_t *attrs)
{
  for(int i = 0; i < n; i++)
    pack_cell(&cells[i], codes ? codes + i : NULL, fg ? fg + i : NULL,
              bg ? bg + i : NULL, attrs ? attrs + i : NULL);
}

/* Codepoints of cell i of a VTermScreenCell array; returns the count. */
int clvt_cell_chars(const VTermScreenCell *cells, int i, uint32_t *out)
{
  const VTermScreenCell *c = &cells[i];
  int n = 0;
  if(c->chars[0] == (uint32_t)-1)
    return 0;
  for(; n < VTERM_MAX_CHARS_PER_CELL && c->chars[n]; n++)
    out[n] = c->chars[n];
  return n;
}

size_t clvt_get_text(VTerm *vt, char *buf, size_t len,
                     int start_row, int end_row, int start_col, int end_col)
{
  VTermRect r = { .start_row = start_row, .end_row = end_row,
                  .start_col = start_col, .end_col = end_col };
  return vterm_screen_get_text(vterm_obtain_screen(vt), buf, len, r);
}
