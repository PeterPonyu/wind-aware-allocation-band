# Redaction of recorded paths

The result files in this archive were written by the runs that produced them,
and they recorded where they were running. Those paths describe a private
machine and are not published, so the archived copies were rewritten before
deposit. Describing the rules below without reproducing the paths they remove is
the point of this file, so the left column names each class of string rather
than quoting it.

The rewrite is textual and total: it substitutes path strings and refreshes the
explicit source/hash links in derived receipts, without changing a numeric or
structural value. Rules are applied longest match first.

| replaced | with |
|---|---|
| the absolute filesystem prefix of the machine the archive was assembled on | removed |
| the checkout prefix of a rented machine a run executed on | removed |
| the remaining scratch-mount prefix of that rented machine | `<remote>/` |
| the recorded path of an artifact that is archived here | the path it now has in this archive |
| the home directory of the account the runs executed under | `~/` |
| any remaining directory prefix belonging to the private source tree | `source-tree/` |

Both machine prefixes are removed before an archived artifact is mapped to its
new location, so a path recorded on the rented machine and the same path
recorded locally become the same archived string rather than two. The rules are
the full declared set; a direction whose runs never left one machine will show
substitutions for only some of them.

## What was checked

Every rewritten file was reparsed after substitution and compared against the
original with all string leaves erased. A changed number, a dropped key, a
reordered list or a lost record fails the export rather than being deposited.
For line-oriented records the record count is compared as well.

## Files rewritten

The digest on the left is the file as the run wrote it; the digest on the right
is the file in this archive, and it is the one the manifest binds and the build
verifies.

| path | path substitutions | receipt-link refreshes | original sha256 | archived sha256 |
|---|---:|---:|---|---|
| `data/e-raw-oracle/sweep_20260713T122953Z.jsonl` | 2 | 0 | `558839280f344b20…` | `b855b63ebd2b8db3…` |
| `data/e-raw-noise/sweep_noise_20260713T124223Z.jsonl` | 6 | 0 | `3526fa98100c0c1b…` | `89f1ae113983e491…` |
| `data/e-knee/paper_measurement_wind_knee.json` | 3 | 0 | `344a92ec5c24ed0d…` | `5855b54129773604…` |
| `data/e-noise/complement_wind_noise_015.json` | 9 | 0 | `816068dc0369d0bb…` | `ac245394268b680f…` |
| `data/e-spread/complement_wind_knee_seed_spread.json` | 2 | 0 | `b5950826ecbb5b0d…` | `f5e8c149ef492328…` |
| `data/e-pairs/complement_wind_knee_seed_pairs.json` | 2 | 0 | `da005051d7165f5e…` | `2eb8ce3d4a2e0a26…` |
| `data/e-miss/complement_wind_miss_distance.json` | 3 | 0 | `851123a16add5af4…` | `4b3482f0fbaf04d4…` |
| `data/e-designed/complement_wind_designed_replicates.json` | 2 | 0 | `3e30c97037c7f32b…` | `f4586da25a28d376…` |
| `data/e-repro/complement_wind_reproduction.json` | 1 | 0 | `47de1c913b9b113f…` | `ed4a167ec0d5b26f…` |
| `data/e-raw-temporal/temporal_campaign_20260828.jsonl` | 4 | 0 | `991034f056f4191e…` | `9089fe0e5f9879e0…` |
| `data/e-temporal/temporal_analysis_20260828.json` | 0 | 2 | `5355a2a3d565a484…` | `2dd9f31ba6425f65…` |
| `data/e-raw-ablation/temporal_ablation_20260828.jsonl` | 4 | 0 | `03361600d06db140…` | `eb4f0015128a19bd…` |
| `data/e-ablation/temporal_ablation_analysis_20260828.json` | 0 | 4 | `ecaaa69406d2a4fd…` | `ccd1c23fd5511e92…` |
| `data/e-raw-gain/gain_campaign_20260828.jsonl` | 4 | 0 | `9d8279c86e446782…` | `ace8e336308150c6…` |
| `data/e-gain/gain_analysis_20260828.json` | 0 | 4 | `f7235b1c627d0a46…` | `4119f0095c87a1bf…` |
| `data/e-raw-geometry/geometry_campaign_20260829.jsonl` | 6 | 0 | `bfaa341b35ecf7ff…` | `884f0a4e1b972ca0…` |
| `data/e-geometry/geometry_analysis_20260829.json` | 0 | 2 | `e8481d4aff539cfc…` | `63708d936e82aac1…` |
| `data/e-raw-scale/scale_campaign_20260829.jsonl` | 6 | 0 | `c56c07602496866e…` | `51d63382868778c4…` |
| `data/e-scale/scale_analysis_20260829.json` | 3 | 2 | `3c4639c52eab618c…` | `1559ddc9fce6d68a…` |
| `data/e-raw-radius/revision_radius_campaign_20260829.jsonl` | 6 | 0 | `bb12d012c2052be4…` | `f109e4a78f96b39b…` |
| `data/e-radius/revision_radius_analysis_20260829.json` | 8 | 4 | `9ae2d9fabb908dc7…` | `affa3b255a60d21d…` |
| `data/e-raw-factorial/revision_factorial_campaign_20260829.jsonl` | 6 | 0 | `5f032ffc0f95b57f…` | `7cff4d2252d8724f…` |
| `data/e-factorial/revision_factorial_analysis_20260829.json` | 6 | 4 | `4a1a69b8d339fe06…` | `24a57cac88e57959…` |
| `data/e-raw-safety/revision_safety_campaign_20260829.jsonl` | 6 | 0 | `9c547448dc374bf1…` | `e21ea79e4ed6502e…` |
| `data/e-safety/revision_safety_analysis_20260829.json` | 8 | 4 | `bc9fb46707890b97…` | `911856b2252a1a9f…` |
| `data/e-rev-stats/revision_statistics_20260829.json` | 88 | 86 | `85923d3c7695a561…` | `bcf250a294d17647…` |
