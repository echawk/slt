import sys, os
# Writes d-symbolics.svg, e-interlisp.svg and f-glyph.svg into the directory
# given as the only argument: python3 lisp-machine-icons.py icon-concepts
out = sys.argv[1]
B, W = "#000", "#fff"

LAMBDA = """
XXXX.........
XXXXX........
...XXX.......
....XX.......
....XXX......
.....XX......
.....XXX.....
....XXXX.....
....XXXXX....
...XXX.XX....
...XX..XXX...
..XXX...XX...
..XX....XXX..
.XXX.....XX..
.XX......XXX.
XXX.......XXX
""".strip().splitlines()

def bitmap(rows, x0, y0, px, fill):
    s = []
    for r, line in enumerate(rows):
        c = 0
        while c < len(line):
            if line[c] == "X":
                start = c
                while c < len(line) and line[c] == "X": c += 1
                s.append(f'<rect x="{x0+start*px}" y="{y0+r*px}" width="{(c-start)*px}" height="{px}" fill="{fill}"/>')
            else:
                c += 1
    return "\n".join(s)

def svg(body, defs=""):
    return f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1024 1024" shape-rendering="crispEdges">
<defs>{defs}</defs>
{body}
</svg>
'''

CHECKER = '<pattern id="half" width="8" height="8" patternUnits="userSpaceOnUse" shape-rendering="geometricPrecision"><rect width="8" height="8" fill="#fff"/><rect width="4" height="4" fill="#000"/><rect x="4" y="4" width="4" height="4" fill="#000"/></pattern>'
SQUIRCLE = 'x="100" y="100" width="824" height="824" rx="185"'

# D: Symbolics console -- a tall portrait screen, black on white.
px = 22
lw, lh = len(LAMBDA[0])*px, len(LAMBDA)*px
d = f'''<rect {SQUIRCLE} fill="{B}"/>
<rect x="262" y="166" width="500" height="630" rx="20" fill="{W}"/>
<rect x="286" y="190" width="452" height="582" fill="{W}" stroke="{B}" stroke-width="10"/>
<rect x="286" y="190" width="452" height="44" fill="{B}"/>
<rect x="306" y="206" width="120" height="12" fill="{W}"/>
{bitmap(LAMBDA, 512-lw//2-24, 270, px, B)}
<rect x="{512-lw//2-24+lw+px+8}" y="{270+lh-px}" width="{px*2}" height="{px}" fill="{B}"/>
<rect x="318" y="{270+lh+40}" width="250" height="14" fill="{B}"/>
<rect x="318" y="{270+lh+70}" width="170" height="14" fill="{B}"/>
<rect x="318" y="{270+lh+100}" width="210" height="14" fill="{B}"/>
<rect x="352" y="830" width="320" height="22" fill="{W}"/>'''

# E: Interlisp-D -- a window on the halftone desktop.
px = 21
lw, lh = len(LAMBDA[0])*px, len(LAMBDA)*px
e = f'''<rect {SQUIRCLE} fill="url(#half)"/>
<rect {SQUIRCLE} fill="none" stroke="{B}" stroke-width="16"/>
<rect x="216" y="226" width="616" height="586" fill="{B}"/>
<rect x="196" y="206" width="616" height="586" fill="{W}" stroke="{B}" stroke-width="12"/>
<rect x="196" y="206" width="616" height="64" fill="{B}"/>
<rect x="226" y="228" width="200" height="20" fill="{W}"/>
{bitmap(LAMBDA, 504-lw//2-30, 320, px, B)}
<rect x="{504-lw//2-30+lw+px+8}" y="{320+lh-px}" width="{px*2}" height="{px}" fill="{B}"/>'''

# F: the bare glyph -- white bitmap lambda and block cursor on black.
px = 32
lw, lh = len(LAMBDA[0])*px, len(LAMBDA)*px
x0 = 512 - (lw + px + 2*px)//2
y0 = 512 - lh//2
f = f'''<rect {SQUIRCLE} fill="{B}"/>
<rect x="132" y="132" width="760" height="760" rx="155" fill="none" stroke="{W}" stroke-width="10"/>
{bitmap(LAMBDA, x0, y0, px, W)}
<rect x="{x0+lw+px}" y="{y0+lh-2*px}" width="{px*2}" height="{px*2}" fill="{W}"/>'''

for name, body in [("d-symbolics", d), ("e-interlisp", e), ("f-glyph", f)]:
    with open(os.path.join(out, name + ".svg"), "w") as fh:
        fh.write(svg(body, CHECKER))
