# Figure entry point for 001. Emits into figs/out/.
# Refuses to draw anything that is not bound in evidence/evidence_manifest.json.
#
# Side effect by design: this script also writes tex/generated_numbers.tex and the
# ten generated result tables. Every quantity the manuscript prints comes from
# here, so prose cannot drift away from the bytes that were hashed.
#
# The work is split so that each file has one reason to change: figs/lib holds
# reading, the design checks, the interval and formatting; figs/panels holds one
# figure each; this file holds the order they run in and the checks that must
# pass before any of them run.
#
# Run from the paper directory:  Rscript figs/make_figs.R

suppressPackageStartupMessages({
  library(ggplot2)
  library(jsonlite)
  library(patchwork)
})

for (unit in c("rtx_theme.R", "lib/evidence.R", "lib/design.R", "lib/directions.R",
               "lib/designed.R", "lib/stats.R", "lib/temporal.R", "lib/scale.R",
               "lib/emit.R")) {
  source(file.path("figs", unit))
}
source(file.path("figs", "lib", "geometry.R"))

dir.create(file.path("figs", "out"), showWarnings = FALSE, recursive = TRUE)

manifest <- load_manifest()
read_bound <- evidence_reader(manifest, find_repo_root())

knee <- read_bound$json("E-KNEE")
noise <- read_bound$json("E-NOISE")
spread <- read_bound$json("E-SPREAD")
miss <- read_bound$json("E-MISS")
seed_pairs <- read_bound$json("E-PAIRS")
designed <- read_bound$json("E-DESIGNED")
reproduction <- read_bound$json("E-REPRO")
oracle_sweep <- read_bound$sweep("E-RAW-ORACLE")
noise_sweep <- read_bound$sweep("E-RAW-NOISE")
designed_sweep <- read_bound$sweep("E-RAW-DESIGNED")

# The temporal extension is analyzed independently in Python.  Both the raw
# JSONL and its receipt are bound here; the source/hash and row-count checks
# prevent a stale receipt from silently becoming a new figure.
temporal <- read_bound$json("E-TEMPORAL")
temporal_raw_path <- read_bound$path("E-RAW-TEMPORAL")
assert_receipt_source(temporal, temporal_raw_path, "temporal campaign")
ablation <- read_bound$json("E-ABLATION")
ablation_raw_path <- read_bound$path("E-RAW-ABLATION")
assert_receipt_source(ablation, ablation_raw_path, "feedforward ablation")
gain <- read_bound$json("E-GAIN")
gain_raw_path <- read_bound$path("E-RAW-GAIN")
assert_receipt_source(gain, gain_raw_path, "gain sensitivity")
geometry <- read_bound$json("E-GEOMETRY")
geometry_raw_path <- read_bound$path("E-RAW-GEOMETRY")
assert_receipt_source(geometry, geometry_raw_path, "scene geometry")
scale <- read_bound$json("E-SCALE")
scale_raw_path <- read_bound$path("E-RAW-SCALE")
assert_receipt_source(scale, scale_raw_path, "swarm-size campaign")
# The cluster-aware Python receipt is the sole source for the revised support
# intervals and p-values.  The campaign-specific receipts above still provide
# condition means and plotting matrices, but no endpoint is silently
# re-inferred from episode rows in this R pass.
revision_stats <- read_bound$json("E-REV-STATS")
if (!identical(as.character(revision_stats$status), "analysis_ok") ||
    !isTRUE(revision_stats$inferential_ready) ||
    !all(vapply(revision_stats$gates, isTRUE, logical(1)))) {
  stop("cluster-aware revision statistics receipt has a failed gate")
}
assert_revision_source <- function(label, bound_id) {
  ref <- revision_stats$source_receipts[[label]]
  if (is.null(ref) || length(ref$sha256) != 1L) {
    stop("revision statistics receipt is missing source ", label)
  }
  actual <- digest::digest(read_bound$path(bound_id), algo = "sha256", file = TRUE)
  if (!identical(as.character(ref$sha256), actual)) {
    stop("revision statistics source hash drifted for ", label)
  }
  invisible(TRUE)
}
assert_revision_source("primary", "E-RAW-ORACLE")
assert_revision_source("crossed", "E-RAW-DESIGNED")
assert_revision_source("geometry", "E-RAW-GEOMETRY")
assert_revision_source("scale", "E-RAW-SCALE")

HOVER_WEIGHT_N <- 0.265 # per-airframe hover thrust, from the simulator's own model
DRONE_MASS_G <- 27      # per-airframe mass, from the same model
KNEE_N <- 0.15
SATURATION_N <- 0.10
COLLAPSE_N <- 0.20
FEEDFORWARD_GAIN_DEFAULT <- 0.35
PREREG_SIGMA <- 0.25 # decision point fixed before the run
ALPHA <- 0.05
N_LEVELS <- nrow(knee$rows)

## ---------------------------------------------------------------------------
## What the runs recorded about themselves, checked against what they declared.
## ---------------------------------------------------------------------------

N_EPISODES_ORACLE <- sweep_size(oracle_sweep, c("wind_levels", "seeds", "treatments"))
N_EPISODES_NOISE <- sweep_size(noise_sweep, c("wind_levels", "noise_levels", "seeds"))
EPISODES <- rbind(oracle_sweep$episodes[, c("num_drones", "success_radius_m")],
                  noise_sweep$episodes[, c("num_drones", "success_radius_m")])
N_DRONES <- single_valued(EPISODES$num_drones, "how many airframes flew")
CAPTURE_RADIUS_M <- single_valued(EPISODES$success_radius_m, "the capture radius")
SIGMA_GRID <- sort(unique(noise_sweep$design$noise_levels))

kr <- knee$rows
nr <- noise$rows
mr <- miss$rows
N_SEEDS <- single_valued(nr$n, "the seed count")

assert_summaries_match_sweep(oracle_sweep, noise_sweep, N_LEVELS, N_SEEDS,
                             SIGMA_GRID, nr$noise_frac)

## ---------------------------------------------------------------------------
## Object-level endpoint bridge.  This is a descriptive view of the already
## bound primary record, not a new experiment or a new inferential family.  It
## keeps the same seed as the pairing unit while placing the continuous
## closest-approach endpoint beside the thresholded completion endpoint.
## ---------------------------------------------------------------------------

bridge_episodes <- oracle_sweep$episodes[
  abs(as.numeric(oracle_sweep$episodes$wind_mag_N) - KNEE_N) < 1e-12,
  c("seed", "treatment", "completion_rate", "mean_min_dist_to_target_m")
]
bridge_episodes$seed <- as.integer(bridge_episodes$seed)
bridge_episodes$treatment <- as.character(bridge_episodes$treatment)
if (nrow(bridge_episodes) != 2L * N_SEEDS ||
    !setequal(unique(bridge_episodes$treatment), c("baseline", "wind_aware"))) {
  stop("endpoint bridge does not contain one baseline and one aware row per seed")
}
bridge_baseline <- bridge_episodes[bridge_episodes$treatment == "baseline", ]
bridge_aware <- bridge_episodes[bridge_episodes$treatment == "wind_aware", ]
if (anyDuplicated(bridge_baseline$seed) || anyDuplicated(bridge_aware$seed) ||
    !setequal(bridge_baseline$seed, bridge_aware$seed)) {
  stop("endpoint bridge pairing is not one-to-one by seed")
}
BRIDGE_PAIRS <- merge(
  data.frame(seed = bridge_baseline$seed,
             baseline_completion = as.numeric(bridge_baseline$completion_rate),
             baseline_distance = as.numeric(bridge_baseline$mean_min_dist_to_target_m)),
  data.frame(seed = bridge_aware$seed,
             aware_completion = as.numeric(bridge_aware$completion_rate),
             aware_distance = as.numeric(bridge_aware$mean_min_dist_to_target_m)),
  by = "seed", sort = TRUE
)
BRIDGE_PAIRS$distance_delta <- BRIDGE_PAIRS$aware_distance - BRIDGE_PAIRS$baseline_distance
BRIDGE_PAIRS$completion_delta <- BRIDGE_PAIRS$aware_completion - BRIDGE_PAIRS$baseline_completion
BRIDGE_N_LOWER <- sum(BRIDGE_PAIRS$distance_delta < 0)
BRIDGE_DISTANCE_DELTA_MEAN <- mean(BRIDGE_PAIRS$distance_delta)
BRIDGE_COMPLETION_DELTA_MEAN <- mean(BRIDGE_PAIRS$completion_delta)
BRIDGE_DISTANCE_RANGE <- range(c(BRIDGE_PAIRS$baseline_distance, BRIDGE_PAIRS$aware_distance,
                                 CAPTURE_RADIUS_M))

## ---------------------------------------------------------------------------
## The heading each seed drew, and the repetitions that fell out of it. The
## tolerance below is stated here rather than searched for in the separations;
## what makes it defensible is the window the replicate set survives, which is
## computed from the data and printed in the manuscript beside the set.
## ---------------------------------------------------------------------------

REPLICATE_TOL_DEG <- 2

assert_endpoint_is_a_count(oracle_sweep$episodes)
HEADINGS <- seed_headings(oracle_sweep$episodes)
assert_headings_agree(HEADINGS, seed_headings(noise_sweep$episodes))
assert_record_matches_episodes(seed_pairs, oracle_sweep$episodes,
                               list(baseline = "baseline", wind_aware = "wind_aware"))

REPLICATES <- heading_replicates(HEADINGS, REPLICATE_TOL_DEG * pi / 180)
GAPS <- replicate_gaps(oracle_sweep$episodes, REPLICATES, c("baseline", "wind_aware"))
GAP_BY_PAIR <- replicate_gap_by_pair(oracle_sweep$episodes, REPLICATES,
                                     c("baseline", "wind_aware"))
UNSAMPLED_ARC <- largest_unsampled_arc(HEADINGS)
HEADING_GROUPS <- heading_groups(HEADINGS, REPLICATES)

# Two readers over the frames above, so that a level is named by its force in
# the prose rather than by its position in a vector.
gap_at <- function(level) GAPS$gap[GAPS$wind_N == level]
disagreeing_at <- function(level) sum(GAP_BY_PAIR[[sprintf("L%s", level)]] > 0)
REPLAN <- replan_split(seed_pairs$rows[order(seed_pairs$rows$seed), ])

# Every pairwise separation, sorted, so that the figure can show the cut sitting
# in empty space rather than the manuscript asserting that it does.
SEPARATION_SPECTRUM <- sort(angular_separation(
  HEADINGS[utils::combn(length(HEADINGS), 2)[1, ]],
  HEADINGS[utils::combn(length(HEADINGS), 2)[2, ]]
))

## ---------------------------------------------------------------------------
## The crossed record: the same paired contrast, on a heading assigned rather
## than drawn, with every scene flown at every heading. The derived table is
## re-derived from the episodes before either is printed, on the same rule the
## drawn record is held to.
## ---------------------------------------------------------------------------

T95_DF11 <- 2.200985 # Student t, two-sided 95%, one interval per force level

assert_design_is_crossed(designed_sweep$episodes, designed_sweep$design,
                         c("baseline", "wind_aware"))
assert_endpoint_is_a_count(designed_sweep$episodes)
assert_designed_table_matches_episodes(designed, designed_sweep$episodes,
                                       designed_sweep$design)

DESIGNED_BY_FORCE <- designed$by_force
N_HEADINGS <- designed$design$n_headings
N_SCENES <- designed$design$n_scenes
N_PAIRS_PER_FORCE <- designed$design$n_pairs_per_force
N_EPISODES_DESIGNED <- designed$design$n_episodes

REVISION_CROSSED <- revision_stats$families$crossed$effects
if (nrow(REVISION_CROSSED) != length(designed$design$wind_levels) ||
    !all(as.character(REVISION_CROSSED$endpoint) == "completion_rate") ||
    !all(as.integer(REVISION_CROSSED$n_cells) == N_PAIRS_PER_FORCE) ||
    !all(as.integer(REVISION_CROSSED$n_clusters) == N_SCENES)) {
  stop("cluster-aware crossed receipt does not match the declared heading-layout design")
}
crossed_match <- match(as.numeric(DESIGNED_BY_FORCE$wind_N),
                        as.numeric(REVISION_CROSSED$force_N))
if (anyNA(crossed_match)) stop("cluster-aware crossed receipt is missing a force level")
# The legacy derived table stores means rounded to six decimals.  Compare the
# new receipt against an independent raw-episode reconstruction at full
# precision, then replace the rounded display value with the exact bound mean.
raw_crossed_means <- vapply(as.numeric(designed$design$wind_levels), function(level) {
  mean(advantage_matrix(designed_sweep$episodes, designed_sweep$design, level))
}, numeric(1))
raw_match <- match(as.numeric(REVISION_CROSSED$force_N),
                    as.numeric(designed$design$wind_levels))
if (anyNA(raw_match) ||
    any(abs(raw_crossed_means[raw_match] - as.numeric(REVISION_CROSSED$mean)) > 1e-12)) {
  stop("cluster-aware crossed means disagree with the bound crossed episodes")
}
DESIGNED_BY_FORCE$mean_advantage <- as.numeric(REVISION_CROSSED$mean[crossed_match])
DESIGNED_BY_FORCE$ci95_lo_over_scenes <- ci_component(
  REVISION_CROSSED$bootstrap_ci95[crossed_match], 1L
)
DESIGNED_BY_FORCE$ci95_hi_over_scenes <- ci_component(
  REVISION_CROSSED$bootstrap_ci95[crossed_match], 2L
)
N_CROSSED_CLUSTERS <- single_valued(REVISION_CROSSED$n_clusters,
                                    "the crossed layout cluster count")
N_CROSSED_BOOTSTRAP <- single_valued(REVISION_CROSSED$bootstrap$n_boot,
                                     "the crossed bootstrap draw count")

KNEE_MATRIX <- advantage_matrix(designed_sweep$episodes, designed_sweep$design, KNEE_N)
KNEE_SPLIT <- decompose_advantage(KNEE_MATRIX)
# The old scene interval remains available as a regression diagnostic, but the
# manuscript-facing interval is the deterministic cluster bootstrap over the 12
# layout-level means supplied by the revision receipt.
KNEE_INTERVAL_OLD <- scene_interval(KNEE_MATRIX, T95_DF11)
KNEE_CROSSED_ROW <- REVISION_CROSSED[
  abs(as.numeric(REVISION_CROSSED$force_N) - KNEE_N) < 1e-12, , drop = FALSE
]
if (nrow(KNEE_CROSSED_ROW) != 1L) stop("missing one crossed transition-force row")
KNEE_INTERVAL <- list(
  mean = as.numeric(KNEE_CROSSED_ROW$mean),
  lo = as.numeric(KNEE_CROSSED_ROW$bootstrap_ci95[[1]][1]),
  hi = as.numeric(KNEE_CROSSED_ROW$bootstrap_ci95[[1]][2])
)

# Read as a percentage in three places, so it is rounded once here.
pct <- function(x) formatC(x * 100, format = "f", digits = 0)
DesignedShareHeading <- pct(KNEE_SPLIT$share_heading)
DesignedShareScene <- pct(KNEE_SPLIT$share_scene)
DesignedShareResidual <- pct(KNEE_SPLIT$share_residual)

designed_at <- function(level) DESIGNED_BY_FORCE[DESIGNED_BY_FORCE$wind_N == level, ]

# The crossed record carries its own reading of the drawn record's coverage,
# computed by a different pass in a different language. The manuscript prints one
# of the two; the other is here so that a disagreement stops the build rather
# than becoming a second number for the same quantity.
if (!isTRUE(all.equal(degrees(UNSAMPLED_ARC$width),
                      designed$drawn_sweep_for_contrast$largest_unsampled_arc_deg,
                      tolerance = 1e-5))) {
  stop("the two passes disagree about the arc the drawn headings left unsampled")
}

## ---------------------------------------------------------------------------
## The two frames every noise statement is read from. They are built here rather
## than inside the panel because the manuscript quotes them as numbers as well as
## drawing them, and one frame cannot say two things.
## ---------------------------------------------------------------------------

interval <- paired_interval(nr$paired_delta_mean, nr$paired_delta_std, nr$n)
f2a_data <- data.frame(
  sigma = nr$noise_frac,
  delta = nr$paired_delta_mean,
  lo = interval$lo,
  hi = interval$hi,
  p = nr$wilcoxon$p_two_sided
)

ar <- noise$appendix_non_discriminating_0p10_0p20
f2b_data <- data.frame(
  sigma = ar$noise_frac,
  delta = ar$paired_delta_mean,
  level = factor(sprintf("%.2f N", ar$wind_N))
)

# The noise manipulation is one family of tests, and the manuscript adjusts
# across all of it rather than across the panel it happens to be quoting.
N_NOISE_COMPARISONS <- nrow(f2a_data) + nrow(f2b_data)

## ---------------------------------------------------------------------------
## Temporal operating-envelope extension.  The Python receipts re-derive the
## paired statistics; this block only flattens their JSON shape and emits the
## same values into figures, tables and prose macros.
## ---------------------------------------------------------------------------

TEMPORAL_PROFILES <- c("constant", "slow_gust", "fast_gust")
TEMPORAL_ESTIMATORS <- c("oracle", "causal", "causal_biased")
if (!setequal(as.character(temporal$design$profiles), TEMPORAL_PROFILES) ||
    !setequal(as.character(temporal$design$estimators), TEMPORAL_ESTIMATORS)) {
  stop("temporal receipt declares an unexpected profile or estimator grid")
}
temp_rows <- temporal$summaries
temp_rows$delta <- as.numeric(temp_rows$paired_delta_mean)
temp_rows$delta_lo <- ci_component(temp_rows$paired_delta_ci95, 1L)
temp_rows$delta_hi <- ci_component(temp_rows$paired_delta_ci95, 2L)
temp_rows$p <- as.numeric(temp_rows$wilcoxon_p_two_sided)
temp_rows$profile <- factor(temp_rows$profile, levels = TEMPORAL_PROFILES)
temp_rows$estimator <- factor(temp_rows$estimator, levels = TEMPORAL_ESTIMATORS)
TEMPORAL_N_ROWS <- temporal$n_rows
TEMPORAL_TRANSITION <- temp_rows[temp_rows$force_N == KNEE_N, ]
TEMPORAL_CAUSAL_TRANSITION <- TEMPORAL_TRANSITION[TEMPORAL_TRANSITION$estimator == "causal", ]
TEMPORAL_CAUSAL_SHOULDER <- temp_rows[temp_rows$force_N != KNEE_N & temp_rows$estimator == "causal", ]
if (nrow(TEMPORAL_CAUSAL_TRANSITION) != length(TEMPORAL_PROFILES)) {
  stop("temporal receipt does not contain one causal row per transition profile")
}
tracking_rows <- temporal$tracking_summary
tracking_rows$profile <- factor(tracking_rows$profile, levels = TEMPORAL_PROFILES)
tracking_rows$estimator <- factor(tracking_rows$estimator, levels = TEMPORAL_ESTIMATORS)
TEMPORAL_CAUSAL_TRACKING <- tracking_rows[tracking_rows$estimator == "causal", ]

abl_rows <- ablation$summaries
abl_rows$alloc_mean <- as.numeric(abl_rows$allocation_only_gain$mean)
abl_rows$alloc_lo <- ci_component(abl_rows$allocation_only_gain$ci95, 1L)
abl_rows$alloc_hi <- ci_component(abl_rows$allocation_only_gain$ci95, 2L)
abl_rows$alloc_p <- as.numeric(abl_rows$allocation_only_gain$wilcoxon_p_two_sided)
abl_rows$complete_mean <- as.numeric(abl_rows$complete_arm_gain$mean)
abl_rows$complete_lo <- ci_component(abl_rows$complete_arm_gain$ci95, 1L)
abl_rows$complete_hi <- ci_component(abl_rows$complete_arm_gain$ci95, 2L)
abl_rows$complete_p <- as.numeric(abl_rows$complete_arm_gain$wilcoxon_p_two_sided)
abl_rows$profile <- factor(abl_rows$profile, levels = TEMPORAL_PROFILES)
abl_rows$estimator <- factor(abl_rows$estimator, levels = TEMPORAL_ESTIMATORS)
ABLATION_N_ROWS <- ablation$n_rows
ABLATION_CAUSAL_TRANSITION <- abl_rows[abl_rows$force_N == KNEE_N & abl_rows$estimator == "causal", ]
if (nrow(ABLATION_CAUSAL_TRANSITION) != length(TEMPORAL_PROFILES)) {
  stop("ablation receipt does not contain one causal row per transition profile")
}

gain_rows <- gain$summaries
gain_rows$gain_m_per_N <- as.numeric(gain_rows$gain_m_per_N)
gain_rows$delta <- as.numeric(gain_rows$paired_gain$mean)
gain_rows$delta_lo <- ci_component(gain_rows$paired_gain$ci95, 1L)
gain_rows$delta_hi <- ci_component(gain_rows$paired_gain$ci95, 2L)
gain_rows$p <- as.numeric(gain_rows$paired_gain$wilcoxon_p_two_sided)
gain_rows$profile <- factor(gain_rows$profile, levels = TEMPORAL_PROFILES)
GAIN_N_ROWS <- gain$n_rows
GAIN_TRANSITION <- gain_rows[gain_rows$force_N == KNEE_N, ]
GAIN_DEFAULT <- GAIN_TRANSITION[abs(GAIN_TRANSITION$gain_m_per_N - FEEDFORWARD_GAIN_DEFAULT) < 1e-12, ]
GAIN_PLATEAU <- GAIN_TRANSITION[GAIN_TRANSITION$gain_m_per_N >= 0.525 &
                                 GAIN_TRANSITION$gain_m_per_N <= 1.05, ]
if (nrow(GAIN_DEFAULT) != length(TEMPORAL_PROFILES) || nrow(GAIN_PLATEAU) == 0) {
  stop("gain receipt does not contain the declared transition sensitivity grid")
}
GAIN_PLATEAU_MIN <- min(GAIN_PLATEAU$delta)
GAIN_PLATEAU_MAX <- max(GAIN_PLATEAU$delta)
GAIN_EDGE <- GAIN_TRANSITION[abs(GAIN_TRANSITION$gain_m_per_N - 1.4) < 1e-12, ]
if (nrow(GAIN_EDGE) != length(TEMPORAL_PROFILES)) stop("gain receipt is missing its upper edge")

## ---------------------------------------------------------------------------
## Factorised scene geometry.  The receipt is a paired 2 x 2 x 2 design at
## three force levels.  No inference is reconstructed from raw cells here:
## the Python pass has already averaged the four crossed settings within each
## seed, then reports 30-cluster intervals and exact signed-rank results.  The
## former 120-term readout is retained only as a named sensitivity field.
## ---------------------------------------------------------------------------

GEOMETRY_N_ROWS <- as.integer(geometry$n_rows)
GEOMETRY_CONDITION_ROWS <- geometry$condition_summaries
GEOMETRY_CONDITION_ROWS$delta <- as.numeric(GEOMETRY_CONDITION_ROWS$paired_gain$mean)
GEOMETRY_CONDITION_ROWS$delta_lo <- ci_component(GEOMETRY_CONDITION_ROWS$paired_gain$ci95, 1L)
GEOMETRY_CONDITION_ROWS$delta_hi <- ci_component(GEOMETRY_CONDITION_ROWS$paired_gain$ci95, 2L)
GEOMETRY_CONDITION_ROWS$p <- as.numeric(GEOMETRY_CONDITION_ROWS$paired_gain$wilcoxon_p_two_sided)
GEOMETRY_CONDITION_ROWS$condition_short <- geometry_condition_short(
  GEOMETRY_CONDITION_ROWS$spread,
  GEOMETRY_CONDITION_ROWS$range,
  GEOMETRY_CONDITION_ROWS$crossing
)
GEOMETRY_CONDITION_ROWS$force_label <- geometry_force_label(GEOMETRY_CONDITION_ROWS$force_N)

GEOMETRY_FACTORS <- geometry$factor_effects
GEOMETRY_FACTORS$factor_label <- unname(geometry_factor_label[GEOMETRY_FACTORS$factor])
GEOMETRY_FACTORS$effect <- as.numeric(GEOMETRY_FACTORS$high_minus_low)
GEOMETRY_FACTORS$effect_lo <- ci_component(GEOMETRY_FACTORS$paired_factor_contrast$ci95, 1L)
GEOMETRY_FACTORS$effect_hi <- ci_component(GEOMETRY_FACTORS$paired_factor_contrast$ci95, 2L)
GEOMETRY_FACTORS$effect_p <- as.numeric(GEOMETRY_FACTORS$paired_factor_contrast$wilcoxon_p_two_sided)
REVISION_GEOMETRY <- revision_stats$families$geometry$cluster_effects
if (nrow(REVISION_GEOMETRY) != 9L ||
    !all(as.character(REVISION_GEOMETRY$endpoint) == "completion_rate") ||
    !all(as.integer(REVISION_GEOMETRY$n_clusters) == 30L) ||
    !all(as.integer(REVISION_GEOMETRY$n_contrasts) == 120L)) {
  stop("cluster-aware geometry receipt does not match the declared factorial")
}
geometry_match <- match(
  paste(GEOMETRY_FACTORS$factor, as.numeric(GEOMETRY_FACTORS$force_N)),
  paste(as.character(REVISION_GEOMETRY$factor), as.numeric(REVISION_GEOMETRY$force_N))
)
if (anyNA(geometry_match) ||
    any(abs(as.numeric(GEOMETRY_FACTORS$high_minus_low) -
            as.numeric(REVISION_GEOMETRY$mean[geometry_match])) > 1e-12)) {
  stop("cluster-aware geometry means disagree with the bound geometry receipt")
}
GEOMETRY_FACTORS$effect <- as.numeric(REVISION_GEOMETRY$mean[geometry_match])
GEOMETRY_FACTORS$effect_lo <- ci_component(REVISION_GEOMETRY$ci95[geometry_match], 1L)
GEOMETRY_FACTORS$effect_hi <- ci_component(REVISION_GEOMETRY$ci95[geometry_match], 2L)
GEOMETRY_FACTORS$effect_p <- as.numeric(REVISION_GEOMETRY$p_value[geometry_match])
GEOMETRY_FACTORS$holm_p <- as.numeric(REVISION_GEOMETRY$holm_p_value[geometry_match])
GEOMETRY_FACTORS$n_clusters <- as.integer(REVISION_GEOMETRY$n_clusters[geometry_match])
GEOMETRY_FACTORS$n_term_sensitivity <- as.integer(REVISION_GEOMETRY$n_contrasts[geometry_match])
GEOMETRY_FACTORS$force_label <- geometry_force_label(GEOMETRY_FACTORS$force_N)
GEOMETRY_FACTORS$factor_label <- factor(
  GEOMETRY_FACTORS$factor_label,
  levels = unname(geometry_factor_label)
)
GEOMETRY_FACTORS$force_label <- factor(
  GEOMETRY_FACTORS$force_label,
  levels = geometry_force_label(sort(unique(GEOMETRY_FACTORS$force_N)))
)
if (GEOMETRY_N_ROWS != 8L * 3L * 30L * 2L ||
    nrow(GEOMETRY_CONDITION_ROWS) != 8L * 3L ||
    nrow(GEOMETRY_FACTORS) != 3L * 3L) {
  stop("scene-geometry receipt does not match the declared factorial")
}
if (!all(vapply(geometry$gates, isTRUE, logical(1)))) {
  stop("scene-geometry receipt has a failed gate")
}
N_GEOMETRY_CLUSTERS <- single_valued(REVISION_GEOMETRY$n_clusters,
                                     "the geometry seed-cluster count")
N_GEOMETRY_TERM_SENSITIVITY <- single_valued(REVISION_GEOMETRY$n_contrasts,
                                             "the geometry term-sensitivity count")

# A precision diagnostic for the near-zero range contrast.  The primary
# inferential unit is the seed cluster; the 120 seed-by-setting terms remain a
# sensitivity readout and are not treated as independent observations here.
GEOMETRY_RANGE_INDEX <- which(
  as.character(REVISION_GEOMETRY$factor) == "range" &
    abs(as.numeric(REVISION_GEOMETRY$force_N) - KNEE_N) < 1e-12
)
if (length(GEOMETRY_RANGE_INDEX) != 1L) {
  stop("geometry receipt must contain one transition range contrast")
}
GEOMETRY_RANGE_ROW <- REVISION_GEOMETRY[GEOMETRY_RANGE_INDEX, , drop = FALSE]
GEOMETRY_RANGE_CLUSTER_N <- as.integer(GEOMETRY_RANGE_ROW$n_clusters[[1]])
GEOMETRY_RANGE_CLUSTER_SD <- as.numeric(GEOMETRY_RANGE_ROW$sd[[1]])
GEOMETRY_MDE_FAMILY <- nrow(REVISION_GEOMETRY)
GEOMETRY_MDE_POWER <- 0.80
GEOMETRY_MDE_ALPHA <- bonferroni(ALPHA, GEOMETRY_MDE_FAMILY)
GEOMETRY_RANGE_MDE <- (
  stats::qt(1 - GEOMETRY_MDE_ALPHA / 2,
            df = GEOMETRY_RANGE_CLUSTER_N - 1L) +
    stats::qnorm(GEOMETRY_MDE_POWER)
) * GEOMETRY_RANGE_CLUSTER_SD / sqrt(GEOMETRY_RANGE_CLUSTER_N)
GEOMETRY_RANGE_SENSITIVITY <- REVISION_GEOMETRY$sensitivity$summary[
  GEOMETRY_RANGE_INDEX, , drop = FALSE
]
GEOMETRY_RANGE_SENSITIVITY_CI <- GEOMETRY_RANGE_SENSITIVITY$ci95[[1]]
if (!is.finite(GEOMETRY_RANGE_MDE) || GEOMETRY_RANGE_MDE <= 0 ||
    length(GEOMETRY_RANGE_SENSITIVITY_CI) != 2L) {
  stop("geometry MDE diagnostic is not finite")
}

## ---------------------------------------------------------------------------
## Swarm-size operating-envelope extension.  The Python receipt re-derives the
## paired contrasts; R checks its declared grid and flattens only those values.
## ---------------------------------------------------------------------------

if (!identical(as.character(scale$status), "analysis_ok") ||
    !all(vapply(scale$gates, isTRUE, logical(1)))) {
  stop("swarm-size receipt has a failed gate")
}
SCALE_SIZES <- as.numeric(scale$design$swarm_sizes)
SCALE_FORCES <- as.numeric(scale$design$forces_N)
SCALE_SEEDS <- as.numeric(scale$design$seeds)
if (!identical(SCALE_SIZES, c(2, 4, 6, 8)) ||
    !identical(SCALE_FORCES, c(0.1, 0.15, 0.2)) ||
    length(SCALE_SEEDS) != 30L ||
    scale$compatibility$checked != length(SCALE_SEEDS) * 3L * 2L) {
  stop("swarm-size receipt does not match the declared scale design")
}
SCALE_ROWS <- scale$summaries
SCALE_ROWS$num_drones <- as.numeric(SCALE_ROWS$num_drones)
SCALE_ROWS$force_N <- as.numeric(SCALE_ROWS$force_N)
SCALE_ROWS$delta <- nested_numeric(SCALE_ROWS$paired_gain, "mean")
SCALE_ROWS$delta_lo <- ci_component(SCALE_ROWS$paired_gain$ci95, 1L)
SCALE_ROWS$delta_hi <- ci_component(SCALE_ROWS$paired_gain$ci95, 2L)
SCALE_ROWS$p <- nested_numeric(SCALE_ROWS$paired_gain, "wilcoxon_p_two_sided")
REVISION_SCALE <- revision_stats$families$scale$effects
if (nrow(REVISION_SCALE) != 12L ||
    !all(as.character(REVISION_SCALE$endpoint) == "completion_rate") ||
    !all(as.integer(REVISION_SCALE$n_clusters) == 30L)) {
  stop("cluster-aware scale receipt does not match the declared size-force grid")
}
scale_match <- match(
  paste(as.numeric(SCALE_ROWS$num_drones), as.numeric(SCALE_ROWS$force_N)),
  paste(as.numeric(REVISION_SCALE$num_drones), as.numeric(REVISION_SCALE$force_N))
)
if (anyNA(scale_match) ||
    any(abs(as.numeric(SCALE_ROWS$delta) -
            as.numeric(REVISION_SCALE$mean[scale_match])) > 1e-12)) {
  stop("cluster-aware scale means disagree with the bound scale receipt")
}
SCALE_ROWS$delta <- as.numeric(REVISION_SCALE$mean[scale_match])
SCALE_ROWS$delta_lo <- ci_component(REVISION_SCALE$ci95[scale_match], 1L)
SCALE_ROWS$delta_hi <- ci_component(REVISION_SCALE$ci95[scale_match], 2L)
SCALE_ROWS$p <- as.numeric(REVISION_SCALE$p_value[scale_match])
SCALE_ROWS$holm_p <- as.numeric(REVISION_SCALE$holm_p_value[scale_match])
SCALE_ROWS$n_clusters <- as.integer(REVISION_SCALE$n_clusters[scale_match])
SCALE_ROWS$baseline_mean <- nested_numeric(SCALE_ROWS$baseline_completion, "mean")
SCALE_ROWS$baseline_sd <- nested_numeric(SCALE_ROWS$baseline_completion, "sd")
SCALE_ROWS$aware_mean <- nested_numeric(SCALE_ROWS$aware_completion, "mean")
SCALE_ROWS$aware_sd <- nested_numeric(SCALE_ROWS$aware_completion, "sd")
SCALE_ROWS$size_label <- factor(
  scale_size_label(SCALE_ROWS$num_drones),
  levels = scale_size_label(SCALE_SIZES)
)
SCALE_N_ROWS <- as.integer(scale$n_rows)
SCALE_TRANSITION <- SCALE_ROWS[abs(SCALE_ROWS$force_N - KNEE_N) < 1e-12, ]
if (SCALE_N_ROWS != length(SCALE_SIZES) * length(SCALE_FORCES) * length(SCALE_SEEDS) * 2L ||
    nrow(SCALE_ROWS) != length(SCALE_SIZES) * length(SCALE_FORCES) ||
    nrow(SCALE_TRANSITION) != length(SCALE_SIZES)) {
  stop("swarm-size receipt row count is inconsistent")
}

profile_label <- c(constant = "Constant", slow_gust = "Slow gust", fast_gust = "Fast gust")
estimator_label <- c(oracle = "Oracle", causal = "Causal", causal_biased = "Causal + bias")

## ---------------------------------------------------------------------------
## Names shared by every panel, so that an arm is spelled one way in this paper.
## ---------------------------------------------------------------------------

arm_labels <- c(agnostic = "Wind-agnostic allocation", aware = "Wind-aware allocation")
arm_colours <- c("Wind-agnostic allocation" = "#B2182B", "Wind-aware allocation" = "#2166AC")

## ---------------------------------------------------------------------------
## Reviewer-validation campaigns.
##
## Three receipts answer questions the primary sweep cannot ask of itself: does
## the transition survive a different capture radius, which of the two pathways
## carries the gain, and what was actually measured about safety. None of them
## is a primary result and none is retroactively prespecified; each is corrected
## inside its own declared family and reported whether or not it is convenient.
## ---------------------------------------------------------------------------

radius <- read_bound$json("E-RADIUS")
assert_receipt_source(radius, read_bound$path("E-RAW-RADIUS"), "endpoint radius")
factorial <- read_bound$json("E-FACTORIAL")
assert_receipt_source(factorial, read_bound$path("E-RAW-FACTORIAL"), "pathway factorial")
safety <- read_bound$json("E-SAFETY")
assert_receipt_source(safety, read_bound$path("E-RAW-SAFETY"), "safety diagnostics")

for (receipt in list(list(radius, "endpoint radius"),
                     list(factorial, "pathway factorial"),
                     list(safety, "safety diagnostics"))) {
  if (!all(vapply(receipt[[1]]$gates, isTRUE, logical(1)))) {
    stop(receipt[[2]], " receipt has a failed gate")
  }
}
# The safety campaign is the one receipt in the set that declares itself not
# inferentially ready. That is not a defect to be worked around: it is a
# descriptive diagnostic, and the flag is checked here so no later edit can
# quietly promote it into a significance claim.
if (isTRUE(safety$inferential_ready) ||
    !identical(as.character(safety$campaign_stage), "full")) {
  stop("safety receipt must be a full descriptive campaign, not an inferential one")
}
if (!isTRUE(radius$inferential_ready) || !isTRUE(factorial$inferential_ready)) {
  stop("a reviewer-validation receipt is not marked inferentially ready")
}

RADIUS_GRID_M <- sort(as.numeric(radius$design$success_radii_m))
RADIUS_ANCHOR_M <- CAPTURE_RADIUS_M
RADIUS_SEEDS <- length(radius$design$seeds)
RADIUS_N_ROWS <- as.integer(radius$n_rows)
RADIUS_HOLM_M <- as.integer(radius$holm$m_completion)

radius_rows <- radius$summaries
RADIUS_COMPLETION <- data.frame(
  radius_m = as.numeric(radius_rows$success_radius_m),
  force_N = as.numeric(radius_rows$force_N),
  baseline = as.numeric(radius_rows$completion$baseline_mean),
  aware = as.numeric(radius_rows$completion$aware_mean),
  delta = as.numeric(radius_rows$completion$paired_delta_mean),
  lo = ci_component(radius_rows$completion$paired_delta_ci95, 1L),
  hi = ci_component(radius_rows$completion$paired_delta_ci95, 2L),
  p_two_sided = as.numeric(radius_rows$completion$wilcoxon_p_two_sided),
  p_holm = as.numeric(radius_rows$completion$wilcoxon_p_holm),
  stringsAsFactors = FALSE
)
RADIUS_COMPLETION <- RADIUS_COMPLETION[order(RADIUS_COMPLETION$radius_m,
                                             RADIUS_COMPLETION$force_N), ]
if (nrow(RADIUS_COMPLETION) != length(RADIUS_GRID_M) * N_LEVELS ||
    RADIUS_N_ROWS != length(RADIUS_GRID_M) * N_LEVELS * RADIUS_SEEDS * 2L ||
    !any(abs(RADIUS_GRID_M - RADIUS_ANCHOR_M) < 1e-12)) {
  stop("endpoint-radius receipt does not match the declared radius sweep")
}

# The anchor column has to reproduce the primary sweep exactly, or the radius
# panel is describing a different experiment from the one the paper is about.
radius_anchor <- RADIUS_COMPLETION[
  abs(RADIUS_COMPLETION$radius_m - RADIUS_ANCHOR_M) < 1e-12, , drop = FALSE
]
anchor_match <- match(round(radius_anchor$force_N, 10), round(kr$wind_N, 10))
if (anyNA(anchor_match) ||
    any(abs(radius_anchor$delta - kr$paired_delta_mean[anchor_match]) > 1e-9)) {
  stop("the anchor radius does not reproduce the primary force ladder")
}

# Where the transition sits, per radius, read off the receipt's own crossing
# summary rather than recomputed here.  The peak is the largest paired gain on
# the ladder; the receipt's crossing fields describe the same column.
RADIUS_PEAK <- do.call(rbind, lapply(RADIUS_GRID_M, function(r) {
  rows <- RADIUS_COMPLETION[abs(RADIUS_COMPLETION$radius_m - r) < 1e-12, ]
  rows[which.max(rows$delta), , drop = FALSE]
}))
RADIUS_CROSSINGS <- radius$radius_crossings[
  order(as.numeric(radius$radius_crossings$success_radius_m)), , drop = FALSE
]
if (!all(as.logical(RADIUS_CROSSINGS$anchor_present))) {
  stop("endpoint-radius receipt is missing the anchor force on some radius")
}
RADIUS_ANY_HOLM <- sum(RADIUS_COMPLETION$p_holm < ALPHA, na.rm = TRUE)

# Completion is a thresholded endpoint, so it can only move when an episode
# crosses the radius. The receipt carries the underlying distance as its own
# corrected family, and reporting it is what stops the radius sweep from being
# read as a sequence of unrelated threshold accidents. It is summarised rather
# than tabulated because the direction, not the cell, is the point.
RADIUS_CONTINUOUS <- data.frame(
  radius_m = as.numeric(radius_rows$success_radius_m),
  force_N = as.numeric(radius_rows$force_N),
  delta = as.numeric(radius_rows$continuous_mean_min_distance$paired_delta_mean),
  p_holm = as.numeric(radius_rows$continuous_mean_min_distance$wilcoxon_p_holm),
  stringsAsFactors = FALSE
)
RADIUS_CONTINUOUS_CLOSER <- sum(RADIUS_CONTINUOUS$delta < 0)

## The completed 2 x 2. The three effects are the two main effects and their
## interaction, each a paired seed-level contrast inside the profile-force cell.
factorial_rows <- factorial$summaries
FACTORIAL_EFFECT_LABELS <- c(assignment = "Assignment cost",
                             feedforward = "Target feedforward",
                             interaction = "Interaction")
FACTORIAL_EFFECTS <- do.call(rbind, lapply(names(FACTORIAL_EFFECT_LABELS), function(effect) {
  cell <- factorial_rows$effects$completion[[effect]]
  data.frame(
    profile = as.character(factorial_rows$profile),
    force_N = as.numeric(factorial_rows$force_N),
    effect = effect,
    effect_label = unname(FACTORIAL_EFFECT_LABELS[[effect]]),
    delta = as.numeric(cell$paired_delta_mean),
    lo = ci_component(cell$paired_delta_ci95, 1L),
    hi = ci_component(cell$paired_delta_ci95, 2L),
    p_two_sided = as.numeric(cell$wilcoxon_p_two_sided),
    p_holm = as.numeric(cell$wilcoxon_p_holm),
    n = as.integer(cell$n),
    stringsAsFactors = FALSE
  )
}))
FACTORIAL_EFFECTS$profile_label <- factor(
  unname(profile_label[FACTORIAL_EFFECTS$profile]),
  levels = unname(profile_label[TEMPORAL_PROFILES])
)
FACTORIAL_EFFECTS$effect_label <- factor(FACTORIAL_EFFECTS$effect_label,
                                         levels = unname(FACTORIAL_EFFECT_LABELS))
FACTORIAL_SEEDS <- as.integer(factorial$n_pairs_per_cell)
FACTORIAL_N_ROWS <- as.integer(factorial$n_rows)
FACTORIAL_FORCES <- sort(unique(FACTORIAL_EFFECTS$force_N))
if (nrow(FACTORIAL_EFFECTS) != 3L * length(TEMPORAL_PROFILES) * length(FACTORIAL_FORCES) ||
    !isTRUE(factorial$gates$four_cells_present) ||
    !identical(as.character(factorial$metric), "completion_rate")) {
  stop("pathway factorial receipt does not match the declared 2 x 2 design")
}

# The claim the factorial supports is about which pathway carries the gain, so
# the two quantities the prose needs are the largest effect of each kind at the
# transition force. Both are read from the same corrected column.
factorial_knee <- FACTORIAL_EFFECTS[
  abs(FACTORIAL_EFFECTS$force_N - KNEE_N) < 1e-12, , drop = FALSE
]
FACTORIAL_FEEDFORWARD_KNEE <- factorial_knee[factorial_knee$effect == "feedforward", ]
FACTORIAL_ASSIGNMENT_KNEE <- factorial_knee[factorial_knee$effect == "assignment", ]
FACTORIAL_INTERACTION_KNEE <- factorial_knee[factorial_knee$effect == "interaction", ]
FACTORIAL_MAX_NON_FEEDFORWARD <- max(abs(c(FACTORIAL_ASSIGNMENT_KNEE$delta,
                                           FACTORIAL_INTERACTION_KNEE$delta)))
FACTORIAL_HOLM_SIGNIFICANT <- FACTORIAL_EFFECTS[
  FACTORIAL_EFFECTS$p_holm < ALPHA, , drop = FALSE
]
if (!all(FACTORIAL_HOLM_SIGNIFICANT$effect == "feedforward")) {
  stop("a non-feedforward pathway effect cleared Holm; the prose must be rewritten")
}

# Same argument as the radius sweep: the factorial receipt corrects a second
# family on the continuous endpoint, and the attribution is only worth stating
# if the distance moves the same way the threshold does.
FACTORIAL_CONTINUOUS <- do.call(rbind, lapply(names(FACTORIAL_EFFECT_LABELS), function(effect) {
  cell <- factorial_rows$effects$continuous_distance[[effect]]
  data.frame(
    profile = as.character(factorial_rows$profile),
    force_N = as.numeric(factorial_rows$force_N),
    effect = effect,
    delta = as.numeric(cell$paired_delta_mean),
    p_holm = as.numeric(cell$wilcoxon_p_holm),
    stringsAsFactors = FALSE
  )
}))
factorial_cont_knee <- FACTORIAL_CONTINUOUS[
  abs(FACTORIAL_CONTINUOUS$force_N - KNEE_N) < 1e-12, , drop = FALSE
]
FACTORIAL_CONT_FEEDFORWARD_KNEE <- factorial_cont_knee[
  factorial_cont_knee$effect == "feedforward", , drop = FALSE
]
FACTORIAL_CONT_OTHER_ABS_MAX <- max(abs(
  FACTORIAL_CONTINUOUS$delta[FACTORIAL_CONTINUOUS$effect != "feedforward"]
))
if (any(FACTORIAL_CONT_FEEDFORWARD_KNEE$delta >= 0)) {
  stop("the feedforward term does not reduce distance at the transition force")
}

## Descriptive safety and feasibility. No test, no interval used as a test: the
## point of the block is to say what was measured and what was not.
safety_rows <- safety$summaries
SAFETY_SEEDS <- as.integer(safety$n_pairs_per_cell)
SAFETY_N_ROWS <- as.integer(safety$n_rows)
SAFETY_METRICS <- as.character(safety$design$metrics)
safety_arm <- c(baseline = unname(arm_labels[["agnostic"]]),
                wind_aware = unname(arm_labels[["aware"]]))
safety_metric_frame <- function(metric) {
  do.call(rbind, lapply(names(safety_arm), function(arm) {
    cell <- safety_rows$by_treatment[[arm]][[metric]]
    data.frame(
      force_N = as.numeric(safety_rows$force_N),
      arm = factor(unname(safety_arm[[arm]]), levels = unname(arm_labels)),
      metric = metric,
      mean = as.numeric(cell$mean),
      sd = as.numeric(cell$sd),
      min = as.numeric(cell$min),
      max = as.numeric(cell$max),
      n_nonzero = as.integer(cell$n_nonzero),
      stringsAsFactors = FALSE
    )
  }))
}
SAFETY_SEPARATION <- safety_metric_frame("min_pairwise_separation_m")
SAFETY_MOTOR <- safety_metric_frame("motor_limit_fraction")
SAFETY_NONFINITE <- safety_metric_frame("nonfinite_state_count")
SAFETY_STEP_MAX <- safety_metric_frame("max_control_step_wall_sec")
SAFETY_STEP_MEAN <- safety_metric_frame("mean_control_step_wall_sec")
SAFETY_REPLAN <- safety_metric_frame("replan_events")
if (SAFETY_N_ROWS != nrow(safety_rows) * SAFETY_SEEDS * 2L ||
    !all(c("min_pairwise_separation_m", "motor_limit_fraction",
           "nonfinite_state_count") %in% SAFETY_METRICS)) {
  stop("safety receipt does not match the declared descriptive design")
}
# The one hard safety statement the paper is allowed to make is that no episode
# produced a non-finite state. It is asserted here so the sentence cannot
# survive a receipt that stops supporting it.
SAFETY_NONFINITE_TOTAL <- sum(SAFETY_NONFINITE$max)
if (SAFETY_NONFINITE_TOTAL != 0) {
  stop("a safety episode recorded a non-finite state; the scope prose is now wrong")
}
SAFETY_WORST_SEPARATION <- SAFETY_SEPARATION[which.min(SAFETY_SEPARATION$min), ]
SAFETY_SEPARATION_KNEE <- SAFETY_SEPARATION[
  abs(SAFETY_SEPARATION$force_N - KNEE_N) < 1e-12, , drop = FALSE
]
SAFETY_MOTOR_MAX <- max(SAFETY_MOTOR$max)

## The separation metric's geometry confound.  Bound as its own receipt because
## it is what licenses the descriptive reading in the Results and Discussion:
## most of the variance in the metric is set by where the benchmark put the
## targets, so the paper is not allowed to read it as an allocator property.
GEOM_CONFOUND <- read_bound$json("E-SAFETY-GEOM")
if (as.integer(GEOM_CONFOUND$episodes) != SAFETY_N_ROWS) {
  stop("the geometry-confound receipt and the safety receipt disagree on episode count")
}
# Keyed by capture radius as a string; the primary radius is the one the
# completion criterion actually used.
GEOM_CONFOUND_OVERLAP <- unlist(GEOM_CONFOUND$structural_overlap_rate)
GEOM_CONFOUND_OVERLAP_PRIMARY <- unname(GEOM_CONFOUND_OVERLAP[[
  sprintf("%.2f", as.numeric(GEOM_CONFOUND$capture_radius_m))]])
GEOM_CONFOUND_OVERLAP_MIN <- min(GEOM_CONFOUND_OVERLAP)
GEOM_CONFOUND_OVERLAP_MAX <- max(GEOM_CONFOUND_OVERLAP)

## ---------------------------------------------------------------------------
## Cross-campaign synthesis.  This is a presentation layer over already bound
## receipts, not a new statistical family: each row keeps the inferential unit
## and interval supplied by its own campaign.  The summary figure makes the
## evidence chain legible without replacing any load-bearing result above.
## ---------------------------------------------------------------------------

PRIMARY_KNEE_ROW <- kr[abs(kr$wind_N - KNEE_N) < 1e-12, , drop = FALSE]
PRIMARY_KNEE_INTERVAL <- paired_interval(
  PRIMARY_KNEE_ROW$paired_delta_mean,
  PRIMARY_KNEE_ROW$paired_delta_std,
  PRIMARY_KNEE_ROW$n
)

crossed_knee <- designed_at(KNEE_N)
crossed_knee_ci <- list(
  lo = crossed_knee$ci95_lo_over_scenes,
  hi = crossed_knee$ci95_hi_over_scenes
)

noise_summary_levels <- c(0.0, 0.3, 0.5, 1.0)
noise_summary <- f2a_data[f2a_data$sigma %in% noise_summary_levels, , drop = FALSE]

EVIDENCE_FOREST <- rbind(
  data.frame(
    label = "Primary oracle",
    group = "Primary",
    estimate = PRIMARY_KNEE_ROW$paired_delta_mean,
    lo = PRIMARY_KNEE_INTERVAL$lo,
    hi = PRIMARY_KNEE_INTERVAL$hi,
    p = PRIMARY_KNEE_ROW$wilcoxon$p_two_sided,
    unit = sprintf("%d seeds", PRIMARY_KNEE_ROW$n),
    stringsAsFactors = FALSE
  ),
  data.frame(
    label = paste0("Noise σ = ", fmt(noise_summary$sigma, 1)),
    group = "Estimate noise",
    estimate = noise_summary$delta,
    lo = noise_summary$lo,
    hi = noise_summary$hi,
    p = noise_summary$p,
    unit = sprintf("%d seeds", nr$n[1]),
    stringsAsFactors = FALSE
  ),
  data.frame(
    label = "Crossed layouts",
    group = "Crossed design",
    estimate = crossed_knee$mean_advantage,
    lo = crossed_knee_ci$lo,
    hi = crossed_knee_ci$hi,
    p = NA_real_,
    unit = sprintf("%d layout clusters", N_CROSSED_CLUSTERS),
    stringsAsFactors = FALSE
  ),
  data.frame(
    label = paste0("Temporal ", unname(profile_label[as.character(TEMPORAL_CAUSAL_TRANSITION$profile)])),
    group = "Causal temporal",
    estimate = TEMPORAL_CAUSAL_TRANSITION$delta,
    lo = TEMPORAL_CAUSAL_TRANSITION$delta_lo,
    hi = TEMPORAL_CAUSAL_TRANSITION$delta_hi,
    p = TEMPORAL_CAUSAL_TRANSITION$p,
    unit = sprintf("%d seeds", length(temporal$design$seeds)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    label = paste0("Scale ", SCALE_TRANSITION$num_drones, " airframes"),
    group = "Team-size transfer",
    estimate = SCALE_TRANSITION$delta,
    lo = SCALE_TRANSITION$delta_lo,
    hi = SCALE_TRANSITION$delta_hi,
    p = SCALE_TRANSITION$p,
    unit = sprintf("%d seeds", length(SCALE_SEEDS)),
    stringsAsFactors = FALSE
  )
)

# Plot from the most specific campaign to the primary anchor, so the anchor is
# visually closest to the zero line and the group labels read top-to-bottom.
EVIDENCE_FOREST$label <- factor(
  EVIDENCE_FOREST$label,
  levels = rev(EVIDENCE_FOREST$label)
)
EVIDENCE_FOREST$group <- factor(
  EVIDENCE_FOREST$group,
  levels = c("Team-size transfer", "Causal temporal", "Crossed design",
             "Estimate noise", "Primary")
)
## The crossed receipt reports a layout-cluster bootstrap and no exact test. An
## empty cell in a column where every other row carries a p-value reads as a
## number that went missing rather than one that was never claimed, so the
## absence is named instead of left blank.
EVIDENCE_FOREST$p_label <- vapply(EVIDENCE_FOREST$p, function(value) {
  if (is.na(value)) "no exact test" else paste0("p = ", pval(value))
}, character(1))

EVIDENCE_VOLUME <- data.frame(
  campaign = c("Primary force ladder", "Estimate noise", "Crossed design",
               "Temporal profiles", "Pathway ablation", "Gain sensitivity",
               "Scene geometry", "Swarm-size scale", "Endpoint radius",
               "Pathway factorial", "Safety diagnostics"),
  episodes = c(N_EPISODES_ORACLE, N_EPISODES_NOISE, N_EPISODES_DESIGNED,
               TEMPORAL_N_ROWS, ABLATION_N_ROWS, GAIN_N_ROWS,
               GEOMETRY_N_ROWS, SCALE_N_ROWS, RADIUS_N_ROWS,
               FACTORIAL_N_ROWS, SAFETY_N_ROWS),
  unit = c(sprintf("%d seeds", N_SEEDS), sprintf("%d seeds", N_SEEDS),
           sprintf("%d layout\nclusters", N_CROSSED_CLUSTERS), sprintf("%d seeds", length(temporal$design$seeds)),
           sprintf("%d seeds", length(temporal$design$seeds)), sprintf("%d seeds", length(temporal$design$seeds)),
           sprintf("%d seed\nclusters", N_GEOMETRY_CLUSTERS),
           sprintf("%d seeds\nper cell", length(SCALE_SEEDS)),
           sprintf("%d seeds\nper radius", RADIUS_SEEDS),
           sprintf("%d seeds\nper cell", FACTORIAL_SEEDS),
           sprintf("%d seeds,\ndescriptive", SAFETY_SEEDS)),
  stringsAsFactors = FALSE
)
EVIDENCE_VOLUME$campaign <- factor(EVIDENCE_VOLUME$campaign, levels = rev(EVIDENCE_VOLUME$campaign))

## A compact campaign table is generated alongside the figure, so the volume
## panel and the manuscript text have one source of truth for row counts.
evidence_table <- data.frame(
  block = c("Primary force ladder", "Estimate noise", "Crossed design",
            "Causal temporal", "Pathway and gain", "Scene geometry", "Swarm-size scale",
            "Endpoint radius", "Pathway factorial", "Safety diagnostics"),
  axis = c("force", "estimate quality", "heading × layout", "profile × estimator",
           "pathway / gain", "spread × range × crossing", "team size × force",
           "capture radius × force", "assignment × feedforward", "descriptive only"),
  rows = c(N_EPISODES_ORACLE, N_EPISODES_NOISE, N_EPISODES_DESIGNED,
           TEMPORAL_N_ROWS, ABLATION_N_ROWS + GAIN_N_ROWS, GEOMETRY_N_ROWS, SCALE_N_ROWS,
           RADIUS_N_ROWS, FACTORIAL_N_ROWS, SAFETY_N_ROWS),
  unit = c(sprintf("%d seeds", N_SEEDS), sprintf("%d seeds", N_SEEDS),
           sprintf("%d layout clusters", N_CROSSED_CLUSTERS), sprintf("%d seeds", length(temporal$design$seeds)),
           sprintf("%d seeds", length(temporal$design$seeds)),
           sprintf("%d seed clusters", N_GEOMETRY_CLUSTERS),
           sprintf("%d seed clusters per cell", length(SCALE_SEEDS)),
           sprintf("%d seeds per radius", RADIUS_SEEDS),
           sprintf("%d seeds per cell", FACTORIAL_SEEDS),
           sprintf("%d seeds, no test", SAFETY_SEEDS)),
  result = c(
    paste0("+", fmt(PRIMARY_KNEE_ROW$paired_delta_mean, 2), " [",
           fmt(PRIMARY_KNEE_INTERVAL$lo, 2), ", ", fmt(PRIMARY_KNEE_INTERVAL$hi, 2), "]"),
    paste0("+", fmt(min(noise_summary$delta), 2), " to +", fmt(max(noise_summary$delta), 2)),
    paste0("+", fmt(crossed_knee$mean_advantage, 2), " [",
           fmt(crossed_knee_ci$lo, 2), ", ", fmt(crossed_knee_ci$hi, 2), "]"),
    paste0("+", fmt(min(TEMPORAL_CAUSAL_TRANSITION$delta), 2), " to +",
           fmt(max(TEMPORAL_CAUSAL_TRANSITION$delta), 2)),
    paste0("+", fmt(min(ABLATION_CAUSAL_TRANSITION$complete_mean), 2), " to +",
           fmt(max(ABLATION_CAUSAL_TRANSITION$complete_mean), 2), " (complete-arm)"),
    paste0("crossing +", fmt(GEOMETRY_FACTORS$effect[GEOMETRY_FACTORS$force_N == KNEE_N &
                                                       GEOMETRY_FACTORS$factor == "crossing"], 2), " [",
           fmt(GEOMETRY_FACTORS$effect_lo[GEOMETRY_FACTORS$force_N == KNEE_N &
                                          GEOMETRY_FACTORS$factor == "crossing"], 2), ", ",
           fmt(GEOMETRY_FACTORS$effect_hi[GEOMETRY_FACTORS$force_N == KNEE_N &
                                          GEOMETRY_FACTORS$factor == "crossing"], 2), "]"),
    paste0("+", fmt(min(SCALE_TRANSITION$delta), 2), " to +",
           fmt(max(SCALE_TRANSITION$delta), 2)),
    paste0("peak +", fmt(RADIUS_PEAK$delta[1], 2), " at ", fmt(RADIUS_PEAK$force_N[1], 2),
           " N to +", fmt(RADIUS_PEAK$delta[3], 2), " at ", fmt(RADIUS_PEAK$force_N[3], 2), " N"),
    paste0("feedforward +", fmt(min(FACTORIAL_FEEDFORWARD_KNEE$delta), 2), " to +",
           fmt(max(FACTORIAL_FEEDFORWARD_KNEE$delta), 2), "; others $\\le$ ",
           fmt(FACTORIAL_MAX_NON_FEEDFORWARD, 3)),
    paste0("descriptive; closest pair ", fmt(min(SAFETY_SEPARATION$min), 3), " m")
  ),
  stringsAsFactors = FALSE
)

## ---------------------------------------------------------------------------
## Figures. Each panel reads the objects above and writes one file.
## ---------------------------------------------------------------------------

for (unit in c("fig0_system_overview.R", "fig1_wind_band.R", "fig2_noise_sensitivity.R", "fig3_seed_spread.R",
               "fig4_miss_distance.R", "fig5_heading_coverage.R",
               "fig6_replicate_gap.R", "fig7_designed_replicates.R",
               "fig8_temporal_envelope.R", "fig9_mechanism_gain.R",
               "fig10_geometry_factorial.R", "fig11_swarm_scale.R",
               "fig12_endpoint_radius.R", "fig13_pathway_safety.R",
               "fig14_evidence_summary.R")) {
  source(file.path("figs", "panels", unit))
}

## ---------------------------------------------------------------------------
## Numbers and tables. Emitted, never retyped.
## ---------------------------------------------------------------------------

knee_row <- kr[kr$wind_N == KNEE_N, ]
sigma_zero <- f2a_data[f2a_data$sigma == 0, ]
sigma_best <- f2a_data[which.max(f2a_data$delta), ]
sigma_max <- f2a_data[which.max(f2a_data$sigma), ]
sigma_nearest_prereg <- f2a_data[which.min(abs(f2a_data$sigma - PREREG_SIGMA)), ]
weakest_lower <- f2a_data[which.min(f2a_data$lo), ]

write_generated(c(
  macro("NSeeds", N_SEEDS),
  macro("NWindLevels", N_LEVELS),
  macro("NEpisodesOracle", N_EPISODES_ORACLE),
  macro("NEpisodesNoise", N_EPISODES_NOISE),
  macro("NDrones", N_DRONES),
  macro("SigmaGrid", paste(formatC(SIGMA_GRID, format = "g"), collapse = ", ")),
  macro("CaptureRadius", fmt(CAPTURE_RADIUS_M, 2)),
  macro("HoverThrust", fmt(HOVER_WEIGHT_N, 3)),
  macro("DroneMass", DRONE_MASS_G),
  macro("KneeLevel", fmt(KNEE_N, 2)),
  macro("KneePctHover", formatC(KNEE_N / HOVER_WEIGHT_N * 100, format = "f", digits = 0)),
  macro("SaturationLevel", fmt(SATURATION_N, 2)),
  macro("CollapseLevel", fmt(COLLAPSE_N, 2)),
  macro("KneeAgnostic", fmt(knee_row$baseline_mean, 2)),
  macro("KneeAgnosticSD", fmt(knee_row$baseline_std, 2)),
  macro("KneeAware", fmt(knee_row$wind_aware_mean, 2)),
  macro("KneeAwareSD", fmt(knee_row$wind_aware_std, 2)),
  macro("KneeDelta", fmt(knee_row$paired_delta_mean, 2)),
  macro("KneeP", format(knee_row$wilcoxon$p_two_sided, scientific = FALSE)),
  macro("BonferroniAlpha", formatC(bonferroni(ALPHA, N_LEVELS), format = "f", digits = 4)),
  macro("PreregSigma", fmt(PREREG_SIGMA, 2)),
  macro("NearestSigma", fmt(sigma_nearest_prereg$sigma, 2)),
  macro("NearestSigmaDelta", fmt(sigma_nearest_prereg$delta, 2)),
  macro("NearestSigmaLower", fmt(sigma_nearest_prereg$lo, 2)),
  macro("NearestSigmaP", pval(sigma_nearest_prereg$p)),
  macro("SigmaZeroDelta", fmt(sigma_zero$delta, 2)),
  macro("SigmaZeroAware", fmt(nr$wind_aware_mean[nr$noise_frac == 0], 2)),
  macro("BestSigma", fmt(sigma_best$sigma, 2)),
  macro("BestSigmaDelta", fmt(sigma_best$delta, 2)),
  macro("MaxSigma", fmt(sigma_max$sigma, 2)),
  macro("MaxSigmaDelta", fmt(sigma_max$delta, 2)),
  macro("MaxSigmaLower", fmt(sigma_max$lo, 2)),
  macro("MaxSigmaP", pval(sigma_max$p)),
  macro("WeakestLower", fmt(weakest_lower$lo, 2)),
  macro("NoiseComparisons", N_NOISE_COMPARISONS),
  macro("NoiseBonferroni",
        formatC(bonferroni(ALPHA, N_NOISE_COMPARISONS), format = "f", digits = 4)),
  ## Object-level endpoint bridge (descriptive reuse of the primary record).
  macro("BridgeLowerPairs", BRIDGE_N_LOWER),
  macro("BridgeDistanceShift", sprintf("%+.3f", BRIDGE_DISTANCE_DELTA_MEAN)),
  macro("BridgeCompletionGain", fmt(BRIDGE_COMPLETION_DELTA_MEAN, 2)),
  macro("AwareTopSeeds", spread$wind_aware_value_counts[["0.75"]]),
  macro("AwareZeroSeeds", spread$wind_aware_value_counts[["0.00"]]),
  macro("AgnosticZeroSeeds", spread$baseline_value_counts[["0.00"]]),
  macro("MissAgnosticKnee", fmt(mr$baseline_mean_min_dist_m[mr$wind_N == KNEE_N], 3)),
  macro("MissAwareKnee", fmt(mr$wind_aware_mean_min_dist_m[mr$wind_N == KNEE_N], 3)),
  macro("MissAgnosticCollapse", fmt(mr$baseline_mean_min_dist_m[mr$wind_N == 0.30], 3)),
  macro("MissAwareCollapse", fmt(mr$wind_aware_mean_min_dist_m[mr$wind_N == 0.30], 3)),
  ## The heading sample, and what two nearly equal headings are able to say.
  macro("NHeadings", HEADING_GROUPS$n_groups),
  macro("LargestHeadingGroup", length(HEADING_GROUPS$largest)),
  macro("HeadingGroupSpan", fmt(degrees(HEADING_GROUPS$widest_group_span), 2)),
  macro("UnsampledArc", fmt(degrees(UNSAMPLED_ARC$width), 0)),
  macro("NRepeats", nrow(REPLICATES$pairs)),
  macro("RepeatTol", REPLICATE_TOL_DEG),
  macro("ClosestRepeat", fmt(degrees(min(REPLICATES$pairs$separation)), 2)),
  macro("WidestRepeat", fmt(degrees(REPLICATES$widest_kept), 2)),
  macro("NearestDistinct", fmt(degrees(REPLICATES$nearest_dropped), 1)),
  macro("NInformativeLevels", sum(GAPS$seed_spread > 0)),
  macro("NDegenerateLevels", sum(GAPS$seed_spread == 0)),
  macro("RepeatGapKnee", fmt(gap_at(KNEE_N), 2)),
  macro("RepeatGapKneeDrones", round(gap_at(KNEE_N) * N_DRONES)),
  macro("RepeatGapShoulder", fmt(gap_at(SATURATION_N), 2)),
  macro("RepeatGapShoulderDrones", round(gap_at(SATURATION_N) * N_DRONES)),
  macro("NDisagreeingLevels", sum(GAPS$gap > 0)),
  macro("RepeatDisagreeKnee", disagreeing_at(KNEE_N)),
  macro("RepeatDisagreeShoulder", disagreeing_at(SATURATION_N)),
  macro("RepeatDisagreeCollapse", disagreeing_at(COLLAPSE_N)),
  macro("CollapseSeedSpread", fmt(GAPS$seed_spread[GAPS$wind_N == COLLAPSE_N], 2)),
  ## The crossed record, where the heading is assigned and every scene is flown
  ## at every heading.
  macro("NEpisodesDesigned", N_EPISODES_DESIGNED),
  macro("NAssignedHeadings", N_HEADINGS),
  macro("HeadingStep", fmt(designed$design$heading_step_deg, 0)),
  macro("NScenes", N_SCENES),
  macro("NPairsPerForce", N_PAIRS_PER_FORCE),
  macro("DesignedArc", fmt(designed$design$largest_unsampled_arc_deg, 0)),
  macro("DesignedKneeDelta", fmt(KNEE_INTERVAL$mean, 2)),
  macro("DesignedKneeLower", fmt(KNEE_INTERVAL$lo, 2)),
  macro("DesignedKneeUpper", fmt(KNEE_INTERVAL$hi, 2)),
  macro("DesignedKneeAgnostic", fmt(designed_at(KNEE_N)$baseline_mean_completion, 2)),
  macro("DesignedKneeAware", fmt(designed_at(KNEE_N)$wind_aware_mean_completion, 2)),
  macro("DesignedKneeAdverse",
        formatC(designed_at(KNEE_N)$frac_cells_advantage_negative * N_PAIRS_PER_FORCE,
                format = "f", digits = 0)),
  macro("DesignedShareHeading", DesignedShareHeading),
  macro("DesignedShareScene", DesignedShareScene),
  macro("DesignedShareResidual", DesignedShareResidual),
  macro("ZeroSeparationGap", fmt(KNEE_SPLIT$gap_at_zero_separation, 2)),
  macro("ZeroSeparationLow", sprintf("%+.2f", KNEE_SPLIT$widest_row_lo)),
  macro("ZeroSeparationHigh", sprintf("%+.2f", KNEE_SPLIT$widest_row_hi)),
  macro("HeadingOnlyGap", fmt(KNEE_SPLIT$gap_from_heading_alone, 2)),
  macro("DesignedShoulderDelta", fmt(designed_at(SATURATION_N)$mean_advantage, 3)),
  macro("DesignedCollapseDelta", fmt(designed_at(COLLAPSE_N)$mean_advantage, 3)),
  macro("NReproEpisodes", reproduction$n_episodes),
  macro("NReproFields", nrow(reproduction$fields)),
  ## Replanning, the one recorded quantity that reports an episode was hard.
  macro("NReplanQuiet", REPLAN$n_quiet),
  macro("NReplanLoud", REPLAN$n_loud),
  macro("ReplanQuietDelta", fmt(REPLAN$delta_quiet, 2)),
  macro("ReplanLoudDelta", fmt(REPLAN$delta_loud, 2)),
  macro("ReplanKneeAgnostic", REPLAN$total_baseline),
  macro("ReplanKneeAware", REPLAN$total_aware),
  ## Temporal operating-envelope extension.
  macro("NTemporalEpisodes", TEMPORAL_N_ROWS),
  macro("NTemporalProfiles", length(TEMPORAL_PROFILES)),
  macro("NTemporalEstimators", length(TEMPORAL_ESTIMATORS)),
  macro("NTemporalSeeds", length(temporal$design$seeds)),
  macro("NTemporalLegacyRows", temporal$gates$legacy_rows_checked),
  macro("TemporalConstantDelta", fmt(TEMPORAL_CAUSAL_TRANSITION$delta[TEMPORAL_CAUSAL_TRANSITION$profile == "constant"], 2)),
  macro("TemporalConstantLower", fmt(TEMPORAL_CAUSAL_TRANSITION$delta_lo[TEMPORAL_CAUSAL_TRANSITION$profile == "constant"], 2)),
  macro("TemporalConstantUpper", fmt(TEMPORAL_CAUSAL_TRANSITION$delta_hi[TEMPORAL_CAUSAL_TRANSITION$profile == "constant"], 2)),
  macro("TemporalConstantP", pval(TEMPORAL_CAUSAL_TRANSITION$p[TEMPORAL_CAUSAL_TRANSITION$profile == "constant"])),
  macro("TemporalSlowDelta", fmt(TEMPORAL_CAUSAL_TRANSITION$delta[TEMPORAL_CAUSAL_TRANSITION$profile == "slow_gust"], 2)),
  macro("TemporalSlowLower", fmt(TEMPORAL_CAUSAL_TRANSITION$delta_lo[TEMPORAL_CAUSAL_TRANSITION$profile == "slow_gust"], 2)),
  macro("TemporalSlowUpper", fmt(TEMPORAL_CAUSAL_TRANSITION$delta_hi[TEMPORAL_CAUSAL_TRANSITION$profile == "slow_gust"], 2)),
  macro("TemporalSlowP", pval(TEMPORAL_CAUSAL_TRANSITION$p[TEMPORAL_CAUSAL_TRANSITION$profile == "slow_gust"])),
  macro("TemporalFastDelta", fmt(TEMPORAL_CAUSAL_TRANSITION$delta[TEMPORAL_CAUSAL_TRANSITION$profile == "fast_gust"], 2)),
  macro("TemporalFastLower", fmt(TEMPORAL_CAUSAL_TRANSITION$delta_lo[TEMPORAL_CAUSAL_TRANSITION$profile == "fast_gust"], 2)),
  macro("TemporalFastUpper", fmt(TEMPORAL_CAUSAL_TRANSITION$delta_hi[TEMPORAL_CAUSAL_TRANSITION$profile == "fast_gust"], 2)),
  macro("TemporalFastP", pval(TEMPORAL_CAUSAL_TRANSITION$p[TEMPORAL_CAUSAL_TRANSITION$profile == "fast_gust"])),
  macro("TemporalTrackingConstant", fmt(100 * TEMPORAL_CAUSAL_TRACKING$mean_relative_error_mean[TEMPORAL_CAUSAL_TRACKING$profile == "constant"], 1)),
  macro("TemporalTrackingSlow", fmt(100 * TEMPORAL_CAUSAL_TRACKING$mean_relative_error_mean[TEMPORAL_CAUSAL_TRACKING$profile == "slow_gust"], 1)),
  macro("TemporalTrackingFast", fmt(100 * TEMPORAL_CAUSAL_TRACKING$mean_relative_error_mean[TEMPORAL_CAUSAL_TRACKING$profile == "fast_gust"], 1)),
  macro("TemporalShoulderMax", fmt(max(abs(TEMPORAL_CAUSAL_SHOULDER$delta), na.rm = TRUE), 2)),
  ## Mechanism ablation at the transition.
  macro("NAblationEpisodes", ABLATION_N_ROWS),
  macro("AllocOnlyConstant", fmt(ABLATION_CAUSAL_TRANSITION$alloc_mean[ABLATION_CAUSAL_TRANSITION$profile == "constant"], 2)),
  macro("AllocOnlySlow", fmt(ABLATION_CAUSAL_TRANSITION$alloc_mean[ABLATION_CAUSAL_TRANSITION$profile == "slow_gust"], 2)),
  macro("AllocOnlyFast", fmt(ABLATION_CAUSAL_TRANSITION$alloc_mean[ABLATION_CAUSAL_TRANSITION$profile == "fast_gust"], 2)),
  macro("CompleteConstant", fmt(ABLATION_CAUSAL_TRANSITION$complete_mean[ABLATION_CAUSAL_TRANSITION$profile == "constant"], 2)),
  macro("CompleteSlow", fmt(ABLATION_CAUSAL_TRANSITION$complete_mean[ABLATION_CAUSAL_TRANSITION$profile == "slow_gust"], 2)),
  macro("CompleteFast", fmt(ABLATION_CAUSAL_TRANSITION$complete_mean[ABLATION_CAUSAL_TRANSITION$profile == "fast_gust"], 2)),
  ## Gain sensitivity.
  macro("NGainEpisodes", GAIN_N_ROWS),
  macro("FeedforwardGainDefault", fmt(FEEDFORWARD_GAIN_DEFAULT, 2)),
  macro("GainPlateauMin", fmt(GAIN_PLATEAU_MIN, 2)),
  macro("GainPlateauMax", fmt(GAIN_PLATEAU_MAX, 2)),
  macro("GainDefaultConstant", fmt(GAIN_DEFAULT$delta[GAIN_DEFAULT$profile == "constant"], 2)),
  macro("GainDefaultSlow", fmt(GAIN_DEFAULT$delta[GAIN_DEFAULT$profile == "slow_gust"], 2)),
  macro("GainDefaultFast", fmt(GAIN_DEFAULT$delta[GAIN_DEFAULT$profile == "fast_gust"], 2)),
  macro("GainEdgeConstant", fmt(GAIN_EDGE$delta[GAIN_EDGE$profile == "constant"], 2)),
  macro("GainEdgeSlow", fmt(GAIN_EDGE$delta[GAIN_EDGE$profile == "slow_gust"], 2)),
  macro("GainEdgeFast", fmt(GAIN_EDGE$delta[GAIN_EDGE$profile == "fast_gust"], 2)),
  ## Factorised scene geometry extension.
  macro("NGeometryEpisodes", GEOMETRY_N_ROWS),
  macro("NGeometryConditions", length(unique(GEOMETRY_CONDITION_ROWS$condition))),
  macro("NGeometrySeeds", length(geometry$design$seeds)),
  macro("NGeometryClusters", N_GEOMETRY_CLUSTERS),
  macro("NGeometryTermSensitivity", N_GEOMETRY_TERM_SENSITIVITY),
  macro("NCrossedClusters", N_CROSSED_CLUSTERS),
  macro("NCrossedCells", N_PAIRS_PER_FORCE),
  macro("NCrossedBootstrap", N_CROSSED_BOOTSTRAP),
  macro("GeometryKneeCrossingP", pval(GEOMETRY_FACTORS$effect_p[
    GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "crossing"])),
  macro("GeometryKneeCrossingHolm", pval(GEOMETRY_FACTORS$holm_p[
    GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "crossing"])),
  macro("GeometryKneeSpread", fmt(GEOMETRY_FACTORS$effect[GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "spread"], 2)),
  macro("GeometryKneeSpreadLower", fmt(GEOMETRY_FACTORS$effect_lo[GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "spread"], 2)),
  macro("GeometryKneeSpreadUpper", fmt(GEOMETRY_FACTORS$effect_hi[GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "spread"], 2)),
  macro("GeometryKneeRange", fmt(GEOMETRY_FACTORS$effect[GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "range"], 2)),
  macro("GeometryKneeRangeLower", fmt(GEOMETRY_FACTORS$effect_lo[GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "range"], 2)),
  macro("GeometryKneeRangeUpper", fmt(GEOMETRY_FACTORS$effect_hi[GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "range"], 2)),
  macro("GeometryKneeCrossing", fmt(GEOMETRY_FACTORS$effect[GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "crossing"], 2)),
  macro("GeometryKneeCrossingLower", fmt(GEOMETRY_FACTORS$effect_lo[GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "crossing"], 2)),
  macro("GeometryKneeCrossingUpper", fmt(GEOMETRY_FACTORS$effect_hi[GEOMETRY_FACTORS$force_N == KNEE_N & GEOMETRY_FACTORS$factor == "crossing"], 2)),
  macro("GeometryMDEFamily", GEOMETRY_MDE_FAMILY),
  macro("GeometryMDEAlpha", formatC(GEOMETRY_MDE_ALPHA, format = "f", digits = 4)),
  macro("GeometryMDEPower", formatC(GEOMETRY_MDE_POWER, format = "f", digits = 2)),
  macro("GeometryRangeClusterN", GEOMETRY_RANGE_CLUSTER_N),
  macro("GeometryRangeClusterSD", fmt(GEOMETRY_RANGE_CLUSTER_SD, 3)),
  macro("GeometryRangeSensitivityLower", fmt(GEOMETRY_RANGE_SENSITIVITY_CI[[1]], 2)),
  macro("GeometryRangeSensitivityUpper", fmt(GEOMETRY_RANGE_SENSITIVITY_CI[[2]], 2)),
  macro("GeometryRangeMDE", fmt(GEOMETRY_RANGE_MDE, 3)),
  macro("GeometryKneeNominal", fmt(GEOMETRY_CONDITION_ROWS$delta[GEOMETRY_CONDITION_ROWS$condition == "nominal_near_aligned" & GEOMETRY_CONDITION_ROWS$force_N == KNEE_N], 2)),
  macro("GeometryKneeBest", fmt(max(GEOMETRY_CONDITION_ROWS$delta[GEOMETRY_CONDITION_ROWS$force_N == KNEE_N]), 2)),
  ## Swarm-size operating-envelope extension.
  macro("NScaleEpisodes", SCALE_N_ROWS),
  macro("NScaleSizes", length(SCALE_SIZES)),
  macro("NScaleForces", length(SCALE_FORCES)),
  macro("NScaleSeeds", length(SCALE_SEEDS)),
  macro("ScaleSizeGrid", paste(as.integer(SCALE_SIZES), collapse = ", ")),
  macro("ScaleForceGrid", paste(formatC(SCALE_FORCES, format = "f", digits = 2), collapse = ", ")),
  macro("ScaleMinSize", min(SCALE_SIZES)),
  macro("ScaleMaxSize", max(SCALE_SIZES)),
  macro("ScaleKneeMin", fmt(min(SCALE_TRANSITION$delta), 2)),
  macro("ScaleKneeMax", fmt(max(SCALE_TRANSITION$delta), 2)),
  macro("ScaleKneeTwo", fmt(SCALE_TRANSITION$delta[SCALE_TRANSITION$num_drones == 2], 2)),
  macro("ScaleKneeFour", fmt(SCALE_TRANSITION$delta[SCALE_TRANSITION$num_drones == 4], 2)),
  macro("ScaleKneeSix", fmt(SCALE_TRANSITION$delta[SCALE_TRANSITION$num_drones == 6], 2)),
  macro("ScaleKneeEight", fmt(SCALE_TRANSITION$delta[SCALE_TRANSITION$num_drones == 8], 2)),
  macro("ScaleKneeTwoLower", fmt(SCALE_TRANSITION$delta_lo[SCALE_TRANSITION$num_drones == 2], 2)),
  macro("ScaleKneeTwoUpper", fmt(SCALE_TRANSITION$delta_hi[SCALE_TRANSITION$num_drones == 2], 2)),
  macro("ScaleKneeFourLower", fmt(SCALE_TRANSITION$delta_lo[SCALE_TRANSITION$num_drones == 4], 2)),
  macro("ScaleKneeFourUpper", fmt(SCALE_TRANSITION$delta_hi[SCALE_TRANSITION$num_drones == 4], 2)),
  macro("ScaleKneeSixLower", fmt(SCALE_TRANSITION$delta_lo[SCALE_TRANSITION$num_drones == 6], 2)),
  macro("ScaleKneeSixUpper", fmt(SCALE_TRANSITION$delta_hi[SCALE_TRANSITION$num_drones == 6], 2)),
  macro("ScaleKneeEightLower", fmt(SCALE_TRANSITION$delta_lo[SCALE_TRANSITION$num_drones == 8], 2)),
  macro("ScaleKneeEightUpper", fmt(SCALE_TRANSITION$delta_hi[SCALE_TRANSITION$num_drones == 8], 2)),
  ## Keep one extra significant digit here because the prose compares the
  ## maximum transition-cell p-value against the generated bound.
  macro("ScaleKneePMax", pval(max(SCALE_TRANSITION$p))),
  macro("ScaleKneePMin", pval(min(SCALE_TRANSITION$p))),
  macro("ScaleKneeHolmMax", pval(max(SCALE_TRANSITION$holm_p))),
  ## Endpoint-radius validation family. The location of the transition is the
  ## quantity that moves, so the peak force is emitted per radius and the prose
  ## is not allowed to name one of them without the others.
  macro("NRadiusEpisodes", RADIUS_N_ROWS),
  macro("NRadiusSeeds", RADIUS_SEEDS),
  macro("NRadiusLevels", length(RADIUS_GRID_M)),
  macro("RadiusGrid", paste(fmt(RADIUS_GRID_M, 2), collapse = ", ")),
  macro("RadiusTightLevel", fmt(min(RADIUS_GRID_M), 2)),
  macro("RadiusWideLevel", fmt(max(RADIUS_GRID_M), 2)),
  macro("RadiusSpanM", fmt(max(RADIUS_GRID_M) - min(RADIUS_GRID_M), 2)),
  macro("RadiusHolmFamily", RADIUS_HOLM_M),
  macro("RadiusTightPeakForce", fmt(RADIUS_PEAK$force_N[1], 2)),
  macro("RadiusTightPeakDelta", fmt(RADIUS_PEAK$delta[1], 2)),
  macro("RadiusAnchorPeakForce", fmt(RADIUS_PEAK$force_N[2], 2)),
  macro("RadiusAnchorPeakDelta", fmt(RADIUS_PEAK$delta[2], 2)),
  macro("RadiusWidePeakForce", fmt(RADIUS_PEAK$force_N[3], 2)),
  macro("RadiusWidePeakDelta", fmt(RADIUS_PEAK$delta[3], 2)),
  macro("RadiusPeakDeltaMin", fmt(min(RADIUS_PEAK$delta), 2)),
  macro("RadiusPeakDeltaMax", fmt(max(RADIUS_PEAK$delta), 2)),
  macro("RadiusPeakForceSpan", fmt(max(RADIUS_PEAK$force_N) - min(RADIUS_PEAK$force_N), 2)),
  macro("RadiusHolmSurvivors", RADIUS_ANY_HOLM),
  macro("RadiusAnchorHolmP", pval(RADIUS_PEAK$p_holm[2])),
  macro("RadiusContinuousCloser", RADIUS_CONTINUOUS_CLOSER),
  macro("RadiusContinuousCells", nrow(RADIUS_CONTINUOUS)),
  macro("RadiusContinuousHolmSurvivors", sum(RADIUS_CONTINUOUS$p_holm < ALPHA, na.rm = TRUE)),
  ## Completed 2 x 2 pathway factorial.
  macro("NFactorialEpisodes", FACTORIAL_N_ROWS),
  macro("NFactorialSeeds", FACTORIAL_SEEDS),
  macro("FactorialFeedforwardMin", fmt(min(FACTORIAL_FEEDFORWARD_KNEE$delta), 2)),
  macro("FactorialFeedforwardMax", fmt(max(FACTORIAL_FEEDFORWARD_KNEE$delta), 2)),
  macro("FactorialFeedforwardHolmMax", pval(max(FACTORIAL_FEEDFORWARD_KNEE$p_holm))),
  macro("FactorialAssignmentAbsMax", fmt(max(abs(FACTORIAL_ASSIGNMENT_KNEE$delta)), 3)),
  macro("FactorialInteractionAbsMax", fmt(max(abs(FACTORIAL_INTERACTION_KNEE$delta)), 3)),
  macro("FactorialOtherAbsMax", fmt(FACTORIAL_MAX_NON_FEEDFORWARD, 3)),
  macro("NumFactorialCells", nrow(FACTORIAL_EFFECTS)),
  macro("NFactorialHolmSignificant", nrow(FACTORIAL_HOLM_SIGNIFICANT)),
  macro("FactorialContFeedforwardMin",
        fmt(min(FACTORIAL_CONT_FEEDFORWARD_KNEE$delta), 3)),
  macro("FactorialContFeedforwardMax",
        fmt(max(FACTORIAL_CONT_FEEDFORWARD_KNEE$delta), 3)),
  macro("FactorialContOtherAbsMax", fmt(FACTORIAL_CONT_OTHER_ABS_MAX, 3)),
  ## Descriptive safety and feasibility diagnostics. No test is emitted here on
  ## purpose; the receipt declares itself descriptive and the macros are limited
  ## to what a descriptive block can support.
  macro("NSafetyEpisodes", SAFETY_N_ROWS),
  macro("NSafetySeeds", SAFETY_SEEDS),
  macro("NSafetyMetrics", length(SAFETY_METRICS)),
  macro("SafetyForceGrid", paste(fmt(sort(unique(SAFETY_SEPARATION$force_N)), 2), collapse = ", ")),
  macro("SafetyWorstSeparation", fmt(min(SAFETY_SEPARATION$min), 3)),
  macro("SafetyWorstSeparationArm",
        sub(" allocation$", "", tolower(as.character(SAFETY_WORST_SEPARATION$arm[1])))),
  macro("SafetyWorstSeparationForce", fmt(SAFETY_WORST_SEPARATION$force_N[1], 2)),
  macro("SafetyAgnosticSeparationKnee",
        fmt(SAFETY_SEPARATION_KNEE$min[SAFETY_SEPARATION_KNEE$arm == unname(arm_labels[["agnostic"]])], 3)),
  macro("SafetyAwareSeparationKnee",
        fmt(SAFETY_SEPARATION_KNEE$min[SAFETY_SEPARATION_KNEE$arm == unname(arm_labels[["aware"]])], 3)),
  macro("SafetyMotorMax", fmt(SAFETY_MOTOR_MAX, 2)),
  macro("SafetyNonfiniteTotal", SAFETY_NONFINITE_TOTAL),
  macro("SafetyStepMaxMs", fmt(max(SAFETY_STEP_MAX$max) * 1000, 1)),
  ## What the separation metric is actually measuring.  These are read straight
  ## from the confound receipt rather than recomputed here, so the manuscript
  ## and the note quote one number.
  macro("SafetyGeomTargetCorr", fmt(GEOM_CONFOUND$corr_target_separation_vs_observed, 2)),
  macro("SafetyGeomCompletionCorr", fmt(GEOM_CONFOUND$corr_completion_vs_observed, 2)),
  macro("SafetyGeomHullContacts", GEOM_CONFOUND$episodes_below_hull_contact),
  macro("SafetyGeomHullContactM", fmt(GEOM_CONFOUND$hull_contact_m, 2)),
  macro("SafetyGeomCloseTargetContacts", GEOM_CONFOUND$close_target_scenes$hull_contacts),
  macro("SafetyGeomFarTargetContacts", GEOM_CONFOUND$far_target_scenes$hull_contacts),
  macro("SafetyGeomPairedCloser", GEOM_CONFOUND$within_scene_pairing$higher_completion_flew_closer),
  macro("SafetyGeomPairedFarther", GEOM_CONFOUND$within_scene_pairing$higher_completion_flew_farther),
  macro("SafetyGeomPairedTied", GEOM_CONFOUND$within_scene_pairing$completion_tied),
  macro("SafetyGeomOverlapPrimary", fmt(100 * GEOM_CONFOUND_OVERLAP_PRIMARY, 1)),
  macro("SafetyGeomOverlapMin", fmt(100 * GEOM_CONFOUND_OVERLAP_MIN, 1)),
  macro("SafetyGeomOverlapMax", fmt(100 * GEOM_CONFOUND_OVERLAP_MAX, 1)),
  ## Descriptive cross-campaign synthesis (no pooled inferential test).
  macro("NEvidenceBlocks", nrow(evidence_table)),
  macro("NEvidenceCampaigns", nrow(EVIDENCE_VOLUME)),
  macro("NEvidenceRows", format(sum(EVIDENCE_VOLUME$episodes), big.mark = ",",
                                  scientific = FALSE))
), "generated_numbers.tex")

## Endpoint-radius sensitivity: the whole grid, so the reader can see the
## transition move rather than take the three peak cells on trust.

write_generated(c(
  "\\begin{tabular}{rrrrcc}",
  "\\toprule",
  "Radius (m) & Wind (N) & Wind-agnostic & Wind-aware & Paired diff. [95\\% CI] & Holm $p$ \\\\",
  "\\midrule",
  paste0(
    ifelse(c(TRUE, diff(RADIUS_COMPLETION$radius_m) != 0),
           fmt(RADIUS_COMPLETION$radius_m, 2), ""), " & ",
    fmt(RADIUS_COMPLETION$force_N, 2), " & ",
    fmt(RADIUS_COMPLETION$baseline, 3), " & ",
    fmt(RADIUS_COMPLETION$aware, 3), " & ",
    signed_cell(RADIUS_COMPLETION$delta, 3), " [",
    fmt(RADIUS_COMPLETION$lo, 3), ",\\, ", fmt(RADIUS_COMPLETION$hi, 3), "] & ",
    vapply(RADIUS_COMPLETION$p_holm, p_cell, character(1)),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_radius.tex")

## The complete factorial, every cell, including the ones that do nothing.

write_generated(c(
  "\\begin{tabular}{llrrcc}",
  "\\toprule",
  "Profile & Term & Force (N) & Effect & 95\\% interval & Holm $p$ \\\\",
  "\\midrule",
  paste0(
    ifelse(c(TRUE, diff(as.integer(FACTORIAL_EFFECTS$profile_label)) != 0 |
               diff(as.integer(FACTORIAL_EFFECTS$effect_label)) != 0),
           as.character(FACTORIAL_EFFECTS$profile_label), ""), " & ",
    as.character(FACTORIAL_EFFECTS$effect_label), " & ",
    fmt(FACTORIAL_EFFECTS$force_N, 2), " & ",
    signed_cell(FACTORIAL_EFFECTS$delta, 3), " & [",
    fmt(FACTORIAL_EFFECTS$lo, 3), ",\\, ", fmt(FACTORIAL_EFFECTS$hi, 3), "] & ",
    vapply(FACTORIAL_EFFECTS$p_holm, p_cell, character(1)),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_factorial.tex")

## Descriptive safety diagnostics. Worst case beside the mean for every metric
## the campaign recorded, and no p-value anywhere in the table.

safety_forces <- sort(unique(SAFETY_SEPARATION$force_N))
safety_table_metrics <- list(
  list(frame = SAFETY_SEPARATION, name = "Min.\\ pairwise separation (m)",
       digits = 3, worst = "min"),
  list(frame = SAFETY_MOTOR, name = "Motor-limit fraction", digits = 3, worst = "max"),
  list(frame = SAFETY_REPLAN, name = "Replan events", digits = 2, worst = "max"),
  list(frame = SAFETY_STEP_MEAN, name = "Mean control step (ms)", digits = 2,
       worst = "max", scale = 1000),
  list(frame = SAFETY_STEP_MAX, name = "Max.\\ control step (ms)", digits = 2,
       worst = "max", scale = 1000),
  list(frame = SAFETY_NONFINITE, name = "Non-finite states", digits = 0, worst = "max")
)
# The prose says this table gives every metric the campaign recorded, so the
# table has to actually cover the receipt's declared metric list. Dropping one
# and leaving the sentence standing is the failure this guards against.
safety_table_covered <- vapply(safety_table_metrics,
                               function(spec) spec$frame$metric[1], character(1))
if (!setequal(safety_table_covered, SAFETY_METRICS)) {
  stop("the safety table does not cover every metric the receipt declares")
}
safety_rows_tex <- unlist(lapply(safety_table_metrics, function(spec) {
  factor_scale <- if (is.null(spec$scale)) 1 else spec$scale
  vapply(seq_along(unname(arm_labels)), function(i) {
    arm <- unname(arm_labels)[i]
    cells <- vapply(safety_forces, function(f) {
      row <- spec$frame[spec$frame$arm == arm & abs(spec$frame$force_N - f) < 1e-12, ]
      sprintf("%s (%s)", fmt(row$mean * factor_scale, spec$digits),
              fmt(row[[spec$worst]] * factor_scale, spec$digits))
    }, character(1))
    paste0(if (i == 1L) spec$name else "", " & ",
           sub(" allocation$", "", arm), " & ",
           paste(cells, collapse = " & "), " \\\\")
  }, character(1))
}), use.names = FALSE)

write_generated(c(
  "\\begingroup\\footnotesize",
  paste0("\\begin{tabular}{ll", strrep("r", length(safety_forces)), "}"),
  "\\toprule",
  paste0("Diagnostic & Allocation & ",
         paste(sprintf("%s~N", fmt(safety_forces, 2)), collapse = " & "), " \\\\"),
  "\\midrule",
  safety_rows_tex,
  "\\bottomrule",
  "\\end{tabular}",
  "\\endgroup"
), "generated_table_safety.tex")

## Completion at every force level, with the paired test at each.

write_generated(c(
  "\\begin{tabular}{rrccrc}",
  "\\toprule",
  "Wind (N) & \\% hover & Wind-agnostic & Wind-aware & Paired diff. & Exact $p$ \\\\",
  "\\midrule",
  paste0(
    fmt(kr$wind_N, 2), " & ",
    formatC(kr$wind_N / HOVER_WEIGHT_N * 100, format = "f", digits = 0), " & ",
    fmt(kr$baseline_mean, 3), " $\\pm$ ", fmt(kr$baseline_std, 3), " & ",
    fmt(kr$wind_aware_mean, 3), " $\\pm$ ", fmt(kr$wind_aware_std, 3), " & ",
    sprintf("%+.3f", kr$paired_delta_mean), " & ",
    vapply(kr$wilcoxon$p_two_sided, p_cell, character(1)),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_band.tex")

## The discriminating level again, this time against wind-estimate noise.

write_generated(c(
  "\\begin{tabular}{rccrc}",
  "\\toprule",
  "$\\sigma/|w|$ & Wind-aware & Paired diff. & 95\\% interval & Exact $p$ \\\\",
  "\\midrule",
  paste0(
    fmt(nr$noise_frac, 2), " & ",
    fmt(nr$wind_aware_mean, 3), " $\\pm$ ", fmt(nr$wind_aware_std, 3), " & ",
    sprintf("%+.3f", nr$paired_delta_mean), " & ",
    "[", fmt(f2a_data$lo, 3), ",\\, ", fmt(f2a_data$hi, 3), "] & ",
    vapply(f2a_data$p, p_cell, character(1)),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_noise.tex")

## The record behind the discriminating cell, printed rather than summarised.

sp <- seed_pairs$rows[order(seed_pairs$rows$seed), ]

write_generated(c(
  "\\begin{tabular}{rrccrcc}",
  "\\toprule",
  "Seed & Heading & Wind-agnostic & Wind-aware & Paired & \\multicolumn{2}{c}{Replanning events} \\\\",
  "\\cmidrule(lr){6-7}",
  " & (deg) & (finished/\\NDrones) & (finished/\\NDrones) & diff. & agnostic & aware \\\\",
  "\\midrule",
  paste0(
    sp$seed, " & ",
    fmt(degrees(sp$wind_angle_rad), 1), " & ",
    sp$baseline_num_finished, " & ",
    sp$wind_aware_num_finished, " & ",
    sprintf("%+.2f", sp$paired_delta), " & ",
    sp$baseline_replan_events, " & ",
    sp$wind_aware_replan_events,
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_seedpairs.tex")

## Every repeated condition, and what it disagreed about at every force level.

lvl_cols <- sprintf("L%s", GAPS$wind_N)

write_generated(c(
  paste0("\\begin{tabular}{cr", strrep("c", length(lvl_cols)), "}"),
  "\\toprule",
  paste0("Seeds & Separation & \\multicolumn{", length(lvl_cols),
         "}{c}{Airframes disagreeing, by wind force (N)} \\\\"),
  paste0("\\cmidrule(lr){3-", 2 + length(lvl_cols), "}"),
  paste0(" & (deg) & ", paste(fmt(GAPS$wind_N, 2), collapse = " & "), " \\\\"),
  "\\midrule",
  paste0(
    GAP_BY_PAIR$a, ",\\,", GAP_BY_PAIR$b, " & ",
    fmt(degrees(GAP_BY_PAIR$separation), 2), " & ",
    apply(vapply(lvl_cols, function(col) {
      as.character(round(GAP_BY_PAIR[[col]] * N_DRONES))
    }, character(nrow(GAP_BY_PAIR))), 1, paste, collapse = " & "),
    " \\\\"
  ),
  "\\midrule",
  paste0("\\multicolumn{2}{r}{Seeds differing at all} & ",
         paste(ifelse(GAPS$seed_spread > 0, "yes", "no"), collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_repeats.tex")

## The crossed record at every force level: the same paired contrast, the gap
## between two episodes that shared a wind exactly, and where the variation sat.

db <- DESIGNED_BY_FORCE

write_generated(c(
  "\\begin{tabular}{rcrcrrr}",
  "\\toprule",
  " & & & Repeat gap & \\multicolumn{3}{c}{Share of the variation (\\%)} \\\\",
  "\\cmidrule(lr){5-7}",
  "Wind (N) & Paired diff. & 95\\% interval & at $0^{\\circ}$ & heading & scene & interaction \\\\",
  "\\midrule",
  paste0(
    fmt(db$wind_N, 2), " & ",
    signed_cell(db$mean_advantage), " & ",
    "[", signed_cell(db$ci95_lo_over_scenes), ",\\, ",
    signed_cell(db$ci95_hi_over_scenes), "] & ",
    fmt(db$repeat_gap_zero_separation_max, 2), " & ",
    ifelse(is.na(db$share_heading), "---", pct(db$share_heading)), " & ",
    ifelse(is.na(db$share_scene), "---", pct(db$share_scene)), " & ",
    ifelse(is.na(db$share_residual), "---", pct(db$share_residual)),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_designed.tex")

## The temporal extension at the transition force.  Tracking error is printed
## beside the outcome so that a large task-level gain cannot be mistaken for an
## unmeasured estimator-quality claim.

temp_transition_table <- TEMPORAL_TRANSITION[order(TEMPORAL_TRANSITION$profile,
                                                   TEMPORAL_TRANSITION$estimator), ]
temp_transition_table$tracking_pct <- vapply(seq_len(nrow(temp_transition_table)), function(i) {
  hit <- tracking_rows$profile == temp_transition_table$profile[i] &
    tracking_rows$estimator == temp_transition_table$estimator[i]
  100 * tracking_rows$mean_relative_error_mean[hit][1]
}, numeric(1))

write_generated(c(
  "\\begin{tabular}{llrrrc}",
  "\\toprule",
  "Profile & Estimate & Paired gain & 95\\% interval & Mean relative error (\\%) & Exact $p$ \\\\",
  "\\midrule",
  paste0(
    unname(profile_label[as.character(temp_transition_table$profile)]), " & ",
    unname(estimator_label[as.character(temp_transition_table$estimator)]), " & ",
    signed_cell(temp_transition_table$delta), " & [", signed_cell(temp_transition_table$delta_lo), ",\\, ",
    signed_cell(temp_transition_table$delta_hi), "] & ", fmt(temp_transition_table$tracking_pct, 1), " & ",
    vapply(temp_transition_table$p, p_cell, character(1)), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_temporal.tex")

## Mechanism table: one row per temporal profile, with the allocation-only
## support check, the complete arm and the measured gain-sensitivity range.

gain_at <- function(profile, value) {
  gain_rows$delta[gain_rows$profile == profile &
                    abs(gain_rows$force_N - KNEE_N) < 1e-12 &
                    abs(gain_rows$gain_m_per_N - value) < 1e-12][1]
}
plateau_at <- function(profile) {
  values <- GAIN_PLATEAU$delta[GAIN_PLATEAU$profile == profile]
  paste0(fmt(min(values), 2), "--", fmt(max(values), 2))
}
mechanism_table <- data.frame(profile = TEMPORAL_PROFILES, stringsAsFactors = FALSE)
mechanism_table$alloc <- vapply(mechanism_table$profile, function(x)
  ABLATION_CAUSAL_TRANSITION$alloc_mean[ABLATION_CAUSAL_TRANSITION$profile == x][1], numeric(1))
mechanism_table$complete <- vapply(mechanism_table$profile, function(x)
  ABLATION_CAUSAL_TRANSITION$complete_mean[ABLATION_CAUSAL_TRANSITION$profile == x][1], numeric(1))
mechanism_table$default <- vapply(mechanism_table$profile, function(x) gain_at(x, FEEDFORWARD_GAIN_DEFAULT), numeric(1))
mechanism_table$plateau <- vapply(mechanism_table$profile, plateau_at, character(1))
mechanism_table$edge <- vapply(mechanism_table$profile, function(x) gain_at(x, 1.4), numeric(1))

write_generated(c(
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Profile & Allocation only & Complete arm & Default gain & Plateau (0.525--1.05) & Upper edge (1.40) \\\\",
  "\\midrule",
  paste0(
    unname(profile_label[mechanism_table$profile]), " & ", fmt(mechanism_table$alloc, 3), " & ",
    fmt(mechanism_table$complete, 3), " & ", fmt(mechanism_table$default, 3), " & ",
    mechanism_table$plateau, " & ", fmt(mechanism_table$edge, 3), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
), "generated_table_mechanism.tex")

## Scene-geometry factor contrasts.  The interval and exact p-value are over
## 30 seed-level contrasts after averaging the four settings of the other two
## factors.  The 120-term result remains available as a sensitivity field in
## the bound revision receipt, but is not used for the manuscript-facing test.
geometry_table <- GEOMETRY_FACTORS[order(GEOMETRY_FACTORS$force_N, GEOMETRY_FACTORS$factor), ]
geometry_header <- paste0(
  "Force (N) & Factor (high $-$ low) & Low level & High level & ",
  "Contrast [95\\% seed-cluster interval] & Exact $p$ / Holm $p$ ", intToUtf8(92), intToUtf8(92)
)
write_generated(c(
  "\\begingroup",
  "\\setlength{\\tabcolsep}{2pt}",
  "\\begin{tabular}{@{}llrrcc@{}}",
  "\\toprule",
  "Force & Factor (high $-$ low) & Low & High & Contrast [95\\% CI] & Raw $p$ / Holm $p$ \\\\",
  "\\midrule",
  paste0(
    geometry_table$force_label, " & ", geometry_table$factor_label, " & ",
    geometry_table$low_level, " & ", geometry_table$high_level, " & ",
    signed_cell(geometry_table$effect), " [", signed_cell(geometry_table$effect_lo), ",\\, ",
    signed_cell(geometry_table$effect_hi), "] & ",
    paste0(vapply(geometry_table$effect_p, p_cell, character(1)), " / ",
           vapply(geometry_table$holm_p, p_cell, character(1))), " ",
    intToUtf8(92), intToUtf8(92)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\endgroup"
), "generated_table_geometry.tex")

## Swarm-size sensitivity table.  Each row is one size/force summary over the
## same paired seed set; the figure carries the interval visually as well.
scale_table <- SCALE_ROWS[order(SCALE_ROWS$num_drones, SCALE_ROWS$force_N), ]
write_generated(c(
  "\\begingroup",
  "\\setlength{\\tabcolsep}{2pt}",
  "\\begin{tabular}{@{}rrccrc@{}}",
  "\\toprule",
  "Airframes & Force (N) & Wind-agnostic & Wind-aware & Gain [95\\% CI] & Raw $p$ / Holm $p$ \\\\",
  "\\midrule",
  paste0(
    scale_table$num_drones, " & ", fmt(scale_table$force_N, 2), " & ",
    fmt(scale_table$baseline_mean, 3), " $\\pm$ ", fmt(scale_table$baseline_sd, 3), " & ",
    fmt(scale_table$aware_mean, 3), " $\\pm$ ", fmt(scale_table$aware_sd, 3), " & ",
    signed_cell(scale_table$delta), " [", signed_cell(scale_table$delta_lo), ",\\, ",
    signed_cell(scale_table$delta_hi), "] & ",
    paste0(vapply(scale_table$p, p_cell, character(1)), " / ",
           vapply(scale_table$holm_p, p_cell, character(1))), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\endgroup"
), "generated_table_scale.tex")

## Cross-campaign design matrix.  This is intentionally a compact architecture
## table rather than a second inferential analysis: each result is a range or
## interval already emitted by the campaign-specific table above.
evidence_axis_tex <- c(
  "force", "estimate quality", "heading $\\times$ layout", "profile $\\times$ estimator",
  "pathway / gain", "scene factors", "team size $\\times$ force"
)
evidence_unit_tex <- c(
  sprintf("%d seeds", N_SEEDS), sprintf("%d seeds", N_SEEDS),
  sprintf("%d layout clusters; bootstrap", N_CROSSED_CLUSTERS), sprintf("%d seeds", length(temporal$design$seeds)),
  sprintf("%d seeds", length(temporal$design$seeds)),
  sprintf("%d seed clusters", N_GEOMETRY_CLUSTERS),
  sprintf("%d seed clusters per cell", length(SCALE_SEEDS))
)
evidence_rows <- paste0(
  evidence_table$block, " & ", evidence_axis_tex, " & ",
  format(evidence_table$rows, big.mark = ",", scientific = FALSE), " & ",
  evidence_unit_tex, " & ", evidence_table$result, " \\\\", collapse = "\n"
)
write_generated(c(
  # Fixed paragraph columns keep the architecture table inside the generic
  # article text width while allowing long axis/unit descriptions to wrap.
  "\\begingroup",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabular}{@{}p{1.30in}p{1.15in}r p{1.15in}p{2.05in}@{}}",
  "\\toprule",
  "Evidence block & Main axis & Rows & Inferential unit & Transition result \\\\",
  "\\midrule",
  evidence_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\endgroup"
), "generated_table_evidence.tex")

message(sprintf("wrote %d figures to figs/out and %d generated result tex files to tex/ (band at %.2f N)",
                length(list.files(file.path("figs", "out"), pattern = "\\.pdf$")),
                length(list.files("tex", pattern = "^generated_.*\\.tex$")),
                KNEE_N))
