# Reproducing the numbers

Every quantity printed in the manuscript is emitted by `paper/figs/make_figs.R`
from the artifacts listed below. None is typed into the prose. The figure code
re-hashes each artifact before reading it, so a modified or missing file stops
the build instead of producing a stale number.

## Bound artifacts

| path | role | bytes | sha256 |
|---|---|---|---|
| `data/e-raw-oracle/sweep_20260713T122953Z.jsonl` | raw_episode_log | 50485 | `b855b63ebd2b8db3…` |
| `data/e-raw-noise/sweep_noise_20260713T124223Z.jsonl` | raw_episode_log | 58050 | `89f1ae113983e491…` |
| `data/e-knee/paper_measurement_wind_knee.json` | derived_table | 4335 | `5855b54129773604…` |
| `data/e-noise/complement_wind_noise_015.json` | derived_table | 6385 | `ac245394268b680f…` |
| `data/e-spread/complement_wind_knee_seed_spread.json` | derived_table | 808 | `f5e8c149ef492328…` |
| `data/e-pairs/complement_wind_knee_seed_pairs.json` | derived_table | 3635 | `2eb8ce3d4a2e0a26…` |
| `data/e-miss/complement_wind_miss_distance.json` | derived_table | 3618 | `4b3482f0fbaf04d4…` |
| `data/e-raw-designed/sweep_designed_20260828T182851Z.jsonl` | raw_episode_log | 1832016 | `2d4797933aaddd5a…` |
| `data/e-designed/complement_wind_designed_replicates.json` | derived_table | 6215 | `f4586da25a28d376…` |
| `data/e-repro/complement_wind_reproduction.json` | derived_table | 1467 | `ed4a167ec0d5b26f…` |
| `data/e-raw-temporal/temporal_campaign_20260828.jsonl` | raw_episode_log | 1075173 | `9089fe0e5f9879e0…` |
| `data/e-temporal/temporal_analysis_20260828.json` | derived_table | 20993 | `2dd9f31ba6425f65…` |
| `data/e-raw-ablation/temporal_ablation_20260828.jsonl` | raw_episode_log | 973302 | `eb4f0015128a19bd…` |
| `data/e-ablation/temporal_ablation_analysis_20260828.json` | derived_table | 29035 | `ccd1c23fd5511e92…` |
| `data/e-raw-gain/gain_campaign_20260828.jsonl` | raw_episode_log | 2353112 | `ace8e336308150c6…` |
| `data/e-gain/gain_analysis_20260828.json` | derived_table | 49107 | `4119f0095c87a1bf…` |
| `data/e-raw-geometry/geometry_campaign_20260829.jsonl` | raw_episode_log | 1828659 | `884f0a4e1b972ca0…` |
| `data/e-geometry/geometry_analysis_20260829.json` | derived_table | 184114 | `63708d936e82aac1…` |
| `data/e-raw-scale/scale_campaign_20260829.jsonl` | raw_episode_log | 858067 | `51d63382868778c4…` |
| `data/e-scale/scale_analysis_20260829.json` | derived_table | 52820 | `1559ddc9fce6d68a…` |
| `data/e-raw-radius/revision_radius_campaign_20260829.jsonl` | raw_episode_log | 458239 | `f109e4a78f96b39b…` |
| `data/e-radius/revision_radius_analysis_20260829.json` | derived_table | 52429 | `affa3b255a60d21d…` |
| `data/e-raw-factorial/revision_factorial_campaign_20260829.jsonl` | raw_episode_log | 434802 | `7cff4d2252d8724f…` |
| `data/e-factorial/revision_factorial_analysis_20260829.json` | derived_table | 44669 | `24a57cac88e57959…` |
| `data/e-raw-safety/revision_safety_campaign_20260829.jsonl` | raw_episode_log | 96879 | `e21ea79e4ed6502e…` |
| `data/e-safety/revision_safety_analysis_20260829.json` | derived_table | 31135 | `911856b2252a1a9f…` |
| `data/e-rev-stats/revision_statistics_20260829.json` | derived_table | 312535 | `bcf250a294d17647…` |
| `data/e-safety-geom/safety_geometry_confound_20260831.json` | derived_table | 836 | `8e90d670486ed2e1…` |
| `data/e-audit-predecl/PREDECLARATION.json` | derived_table | 8160 | `583b7d8efe219ecc…` |
| `data/e-audit-receipt/RECEIPT.json` | derived_table | 10138 | `df07c961450cd24c…` |
| `data/e-audit-a1/a1_summary.json` | derived_table | 73245 | `ed70975035f73a3b…` |
| `data/e-audit-a1b/a1b_summary.json` | derived_table | 35279 | `6ccf3d0a940b7148…` |
| `data/e-audit-a2/a2_summary.json` | derived_table | 56984 | `59ed775f0a352f55…` |
| `data/e-audit-a3/a3_summary.json` | derived_table | 26331 | `f5da647f675e5e76…` |
| `data/e-audit-a4/a4_crash_accounting.json` | derived_table | 1627081 | `9b4fdc1e936ae37e…` |
| `data/e-audit-a5/a5_summary.json` | derived_table | 67550 | `90c9cbe6619445c7…` |
| `data/e-audit-a7/a7_assignment_divergence.json` | derived_table | 34391 | `68fb0d622bcc4e8f…` |
| `data/e-audit-a1-raw/a1_gain_sweep.jsonl` | raw_episode_log | 2958571 | `e7584cc2373d1469…` |
| `data/e-audit-a2-raw/a2_horizon_sweep.jsonl` | raw_episode_log | 2377309 | `9a91617d1cb92929…` |
| `data/e-audit-a7-raw/a7_assignment_log.jsonl` | raw_episode_log | 1275783 | `302f2cfbee035472…` |

Some of these files recorded the paths of the machine that produced them. Those path strings and source links were refreshed before deposit; `REDACTION.md` states the rules, lists every file touched with both digests, and describes the check that proves no number changed.

## Not included

This archive leaves out one extra file named in the paper's evidence list. The paper does not take any number from it.

- A development log. The paper does not use any number from it. The defect it mentions is already described in the methods.

## Checking the archive without building it

```bash
python3 tools/bind_evidence.py paper --check
```

This re-hashes every path above against `paper/evidence/evidence_manifest.json`
and reports the first artifact that has drifted.

## Rebuilding

```bash
bash build.sh
```

The steps are check the files, redraw the figures, then typeset. Each step
must finish before the next one starts.
