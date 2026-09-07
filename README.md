# From directional sensing to task completion: mapping the operating envelope of wind-aware quadrotor allocation and control

Simulation logs, derived statistical tables, figure code and manuscript source for a 15-figure, 14-table study with a 54-entry cited bibliography measuring the disturbance band in which a wind-aware allocation-and-control stack helps a four-quadrotor swarm, including estimate quality, temporally structured profiles, pathway sensitivity, scene geometry and homogeneous team-size transfer.

This repository has not been deposited in a public archive, so it has no persistent identifier yet. One will be recorded here when an archive exists.

## What is here

- `paper/tex/` — manuscript source. The abstract, the methods and the figure
  captions are separate files and each is self-contained.
- `paper/figs/` — the R code that draws every figure and emits every number the
  manuscript prints.
- `paper/evidence/` — the manifest binding each artifact to its SHA-256 digest.
- `data/` — the 28 artifacts the manifest names, at the bytes that
  were hashed.

## Not redistributed

The manuscript's evidence manifest binds one further artifact that this archive does not carry. No number in the manuscript is derived from that material; it is bound because the manuscript refers to the content, and held back for the reason below.

- A working log kept during development. It is a project record rather than a result: it mixes the build history with scoping notes and forward-looking recommendations that are not claims about the world, and no number in the manuscript comes from it. The defect it records is described in full in the manuscript's methods section.

## Rebuild

```bash
bash build.sh
```

The build re-hashes every artifact before reading it and stops if any byte has
moved. Figures and printed numbers are regenerated from those bytes rather than
transcribed, so the manuscript cannot quietly disagree with its own data.

Requires `python3`, `Rscript` with `digest`, `ggplot2`, `jsonlite`, `patchwork`
and `systemfonts`, and a TeX distribution with `latexmk`.

## Status

Working draft. Not submitted to any venue.

## Licence

Code: MIT (`LICENSE`). Manuscript text, figures and recorded result data:
CC BY 4.0 (`LICENSE-CONTENT`).
