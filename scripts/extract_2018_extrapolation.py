#!/usr/bin/env python3
"""Digitize the magenta 'speculative extrapolation' lines from the September
2018 working figure (manuscript_2026/figures/usanor_extrapolation_2018draft.png)
into data coordinates, by pixel extraction with panel-frame/tick calibration.

Replaces the hand-eyeballed coordinates first used in
scripts/forecast_check_2x2_2026.R (flagged as misplaced in review, F-028).

Outputs:
  data/derived_2026/extrapolation_2018_digitized.csv  (country, birth_year, age)
  figures/figures_2026/extrapolation_2018_overlay_check.png  (QC overlay)
"""

import numpy as np
from PIL import Image, ImageDraw
import csv
import os

SRC = "manuscript_2026/figures/usanor_extrapolation_2018draft.png"

img = Image.open(SRC).convert("RGB")
a = np.asarray(img).astype(int)
H, W, _ = a.shape
r, g, b = a[..., 0], a[..., 1], a[..., 2]

# ---- panel borders: long black horizontal/vertical lines ---------------------
blackish = (r < 90) & (g < 90) & (b < 90)
row_frac = blackish.mean(axis=1)
col_frac = blackish.mean(axis=0)

hlines = [i for i in range(H) if row_frac[i] > 0.55]
vlines = [j for j in range(W) if col_frac[j] > 0.35]

def cluster(idx, gap=6):
    out, cur = [], [idx[0]]
    for v in idx[1:]:
        if v - cur[-1] <= gap:
            cur.append(v)
        else:
            out.append(int(np.mean(cur)))
            cur = [v]
    out.append(int(np.mean(cur)))
    return out

hl = cluster(hlines)
vl = cluster(vlines)
print("horizontal border lines (y px):", hl)
print("vertical border lines (x px):", vl)

# Expect: vl = [left, right] plot frame; hl includes colorkey box edges then
# NOR top, NOR bottom(=strip top for USA region)... identify the two tallest
# panels: NOR panel between the two hl lines bracketing the upper data area,
# USA between the lower pair. Take the last four hl as [nor_top, nor_bot,
# usa_top, usa_bot] (colorkey lines come first, higher up).
left_px, right_px = vl[0], vl[-1]
nor_top, nor_bot, usa_top, usa_bot = hl[-4], hl[-3], hl[-2], hl[-1]
print("frames: x", left_px, right_px, "| NOR y", nor_top, nor_bot,
      "| USA y", usa_top, usa_bot)

# ---- ticks ------------------------------------------------------------------
# x ticks: short black marks just BELOW usa_bot
band = blackish[usa_bot + 3: usa_bot + 14, :]
tick_cols = np.where(band.mean(axis=0) > 0.5)[0]
xticks = cluster(list(tick_cols), gap=6)
print("x tick px:", xticks)
# six evenly spaced ticks; the leftmost (1900) is unlabelled in the figure
xvals = [1900, 1920, 1940, 1960, 1980, 2000]
assert len(xticks) == len(xvals), f"expected 6 x ticks, got {len(xticks)}"

def fit(px, vals):
    px = np.asarray(px, float)
    vals = np.asarray(vals, float)
    A = np.vstack([px, np.ones_like(px)]).T
    coef, *_ = np.linalg.lstsq(A, vals, rcond=None)
    return lambda p: coef[0] * np.asarray(p, float) + coef[1]

px2year = fit(xticks, xvals)

# y ticks per panel: marks just LEFT of left border
def yticks_for(top, bot):
    band = blackish[:, left_px - 14: left_px - 3]
    rows = np.where(band.mean(axis=1) > 0.5)[0]
    rows = [rr for rr in rows if top < rr < bot]
    return cluster(rows, gap=6)

nor_yt = yticks_for(nor_top, nor_bot)
usa_yt = yticks_for(usa_top, usa_bot)
print("NOR y tick px:", nor_yt, "| USA y tick px:", usa_yt)
# four evenly spaced ticks; the topmost (50) is unlabelled in the figure
yvals = [50, 40, 30, 20]
assert len(nor_yt) == 4 and len(usa_yt) == 4, "expected 4 y ticks per panel"
nor_px2age = fit(nor_yt, yvals)
usa_px2age = fit(usa_yt, yvals)

# ---- magenta pixels ---------------------------------------------------------
magenta = (r > 215) & (b > 215) & (g < 90)
ys, xs = np.where(magenta)
print("magenta pixels:", len(xs))

rows_out = []
for name, top, bot, p2a in [("Norway", nor_top, nor_bot, nor_px2age),
                            ("United States", usa_top, usa_bot, usa_px2age)]:
    sel = (ys > top) & (ys < bot)
    pxs, pys = xs[sel], ys[sel]
    # order by x; average y per x-bin to get one path
    order = np.argsort(pxs)
    pxs, pys = pxs[order], pys[order]
    # bin every ~12 px
    for x0 in range(pxs.min(), pxs.max() + 1, 12):
        m = (pxs >= x0) & (pxs < x0 + 12)
        if m.sum() == 0:
            continue
        px_mid = pxs[m].mean()
        py_mid = pys[m].mean()
        rows_out.append((name, round(float(px2year(px_mid)), 2),
                         round(float(p2a(py_mid)), 2), px_mid, py_mid))

os.makedirs("data/derived_2026", exist_ok=True)
with open("data/derived_2026/extrapolation_2018_digitized.csv", "w",
          newline="") as f:
    w = csv.writer(f)
    w.writerow(["country", "birth_year", "age"])
    for name, yr, ag, _, _ in rows_out:
        w.writerow([name, yr, ag])

for name in ("Norway", "United States"):
    pts = [(yr, ag) for n, yr, ag, _, _ in rows_out if n == name]
    print(name, "first/last digitized:", pts[0], pts[-1], f"({len(pts)} pts)")

# ---- QC overlay -------------------------------------------------------------
qc = img.copy()
d = ImageDraw.Draw(qc)
for name, yr, ag, px_mid, py_mid in rows_out:
    d.ellipse([px_mid - 5, py_mid - 5, px_mid + 5, py_mid + 5],
              outline=(0, 200, 0), width=3)
# mark calibration ticks
for t in xticks:
    d.line([t, usa_bot, t, usa_bot + 25], fill=(0, 120, 255), width=4)
for t in nor_yt + usa_yt:
    d.line([left_px - 25, t, left_px, t], fill=(0, 120, 255), width=4)
qc.save("figures/figures_2026/extrapolation_2018_overlay_check.png")
print("QC overlay written")
