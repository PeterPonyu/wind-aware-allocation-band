# From directional sensing to task completion: mapping the operating envelope of wind-aware quadrotor allocation and control

Simulation logs, derived statistical tables, figure code and manuscript source for a 13-figure, 12-table study with a 54-entry cited bibliography measuring the disturbance band in which a wind-aware allocation-and-control stack helps a four-quadrotor swarm, including estimate quality, temporally structured profiles, pathway sensitivity, scene geometry and homogeneous team-size transfer.

Archived at [10.5281/zenodo.22644574](https://doi.org/10.5281/zenodo.22644574).

Repository: https://github.com/PeterPonyu/wind-aware-allocation-band

## What is here

- `paper/tex/` — manuscript source
- `paper/figs/` — the R code that draws the figures and writes the printed numbers
- `paper/evidence/` — a file list with SHA-256 hashes
- `data/` — the 28 data files named in that list

## Not included

This archive leaves out one extra file named in the paper's evidence list. The paper does not take any number from it.

- A development log. The paper does not use any number from it. The defect it mentions is already described in the methods.

## Rebuild

```bash
bash build.sh
```

The build checks every data file against its hash and stops if a file has
changed. Figures and printed numbers are generated from those files, not typed
in by hand.

Requires `python3`, `Rscript` with `digest`, `ggplot2`, `jsonlite`, `patchwork`
and `systemfonts`, and a TeX distribution with `latexmk`.

## Status

Working draft. Not submitted to any venue.

## Licence

Code: MIT (`LICENSE`). Manuscript text, figures and recorded result data:
CC BY 4.0 (`LICENSE-CONTENT`).
