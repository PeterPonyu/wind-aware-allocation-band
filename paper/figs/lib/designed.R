# The replicate that was designed rather than found.
#
# In the drawn-heading record the heading is a property of the seed, so no two
# episodes share a heading except by accident and the accidents are what the
# earlier result had to read. In the crossed record the heading is assigned on a
# regular grid and every scene is flown at every heading, so at one force level
# the paired advantage forms a heading-by-scene rectangle with one observation
# per cell.
#
# That rectangle is what the earlier result lacked. Holding a column fixed varies
# only the scene; holding a row fixed varies only the wind. The sum of squares of
# the rectangle therefore splits, with no model fitted and nothing pooled, into a
# heading effect, a scene effect and the remainder, which is the part of the
# advantage that neither margin predicts.
#
# Nothing here re-runs anything. The bound derived table is re-derived from the
# bound episodes before either is printed, on the same rule as the drawn record.

# The rectangle is only a rectangle if the crossing is complete and every cell
# holds exactly one paired episode. A record with a hole in it would still
# produce numbers, and they would silently be about a different design.
assert_design_is_crossed <- function(episodes, design, arms) {
  if (!all(episodes$heading_source == "assigned")) {
    stop("the crossed record contains episodes whose heading was drawn, not assigned")
  }
  cells <- length(design$wind_levels) * length(design$headings_deg) *
    length(design$scene_seeds) * length(arms)
  if (nrow(episodes) != cells) {
    stop("the crossed record holds ", nrow(episodes), " episodes for ", cells, " cells")
  }
  key <- paste(episodes$treatment, episodes$wind_mag_N,
               episodes$heading_index, episodes$seed, sep = "|")
  if (anyDuplicated(key)) stop("the crossed record repeats a cell")
  seen <- sort(unique(episodes$heading_deg))
  if (!isTRUE(all.equal(seen, sort(as.numeric(design$headings_deg)), tolerance = 1e-9))) {
    stop("the episodes do not carry the headings the design declares")
  }
  invisible(TRUE)
}

# The heading-by-scene rectangle of paired advantage at one force level: rows are
# headings in the order the design lists them, columns are scenes.
advantage_matrix <- function(episodes, design, level, arms = c("baseline", "wind_aware")) {
  at <- episodes[episodes$wind_mag_N == level, ]
  headings <- seq_along(design$headings_deg) - 1L
  scenes <- as.integer(design$scene_seeds)
  pick <- function(arm, h, s) {
    single_valued(at$completion_rate[at$treatment == arm & at$heading_index == h &
                                       at$seed == s],
                  "one arm's outcome in one cell")
  }
  m <- outer(headings, scenes, Vectorize(function(h, s) pick(arms[2], h, s) - pick(arms[1], h, s)))
  dimnames(m) <- list(sprintf("%.6g", as.numeric(design$headings_deg)), as.character(scenes))
  m
}

# The arithmetic split of one rectangle. `residual` is the heading-by-scene
# interaction and, with one observation per cell, is not separable from episode
# noise -- but the simulator is deterministic given a cell, so there is no
# episode noise to separate it from, and the remainder is interaction outright.
decompose_advantage <- function(m) {
  grand <- mean(m)
  total <- sum((m - grand)^2)
  by_heading <- ncol(m) * sum((rowMeans(m) - grand)^2)
  by_scene <- nrow(m) * sum((colMeans(m) - grand)^2)
  list(
    mean = grand,
    total = total,
    heading = by_heading,
    scene = by_scene,
    residual = total - by_heading - by_scene,
    share_heading = if (total > 0) by_heading / total else NA_real_,
    share_scene = if (total > 0) by_scene / total else NA_real_,
    share_residual = if (total > 0) (total - by_heading - by_scene) / total else NA_real_,
    # Two episodes sharing a heading exactly, differing only in scene: the repeat
    # gap the drawn record could only approach from above. The two ends are kept
    # as well, because a gap is easier to misread than the pair that produced it.
    gap_at_zero_separation = max(apply(m, 1, function(r) diff(range(r)))),
    widest_row_lo = min(m[which.max(apply(m, 1, function(r) diff(range(r)))), ]),
    widest_row_hi = max(m[which.max(apply(m, 1, function(r) diff(range(r)))), ]),
    # The same quantity read down the other margin: what changing only the wind
    # can do to one scene.
    gap_from_heading_alone = max(apply(m, 2, function(cl) diff(range(cl)))),
    degenerate = total <= 0
  )
}

# The scene is the independently drawn unit and the heading grid is fixed and
# exhaustive, so the replicate for an interval is the scene mean, and there are
# as many of them as there are scenes.
scene_interval <- function(m, t_crit) {
  means <- colMeans(m)
  se <- stats::sd(means) / sqrt(length(means))
  list(mean = mean(means), lo = mean(means) - t_crit * se, hi = mean(means) + t_crit * se,
       n = length(means))
}

# The derived table the manuscript prints was written by a separate pass over the
# same episodes. Re-derive it here and refuse to draw anything if the two passes
# disagree, on the same rule the drawn record is held to.
assert_designed_table_matches_episodes <- function(table, episodes, design) {
  # The record is written rounded to six places, so agreement means agreement to
  # the precision it was written at: half of its last digit, absolute. A relative
  # test would call the smallest advantages a mismatch purely for being small.
  ROUNDING <- 5e-7
  agrees <- function(recorded, derived) abs(as.numeric(recorded) - derived) <= ROUNDING
  for (i in seq_len(nrow(table$by_force))) {
    row <- table$by_force[i, ]
    d <- decompose_advantage(advantage_matrix(episodes, design, row$wind_N))
    for (what in list(
      list(row$mean_advantage, d$mean, "mean advantage"),
      list(row$repeat_gap_zero_separation_max, d$gap_at_zero_separation, "zero-separation gap"),
      list(row$heading_only_gap_max, d$gap_from_heading_alone, "heading-only gap")
    )) {
      if (!agrees(what[[1]], what[[2]])) {
        stop(sprintf("at %.2f N the record's %s (%s) is not the episodes' (%s)",
                     row$wind_N, what[[3]], what[[1]], what[[2]]))
      }
    }
    if (!d$degenerate && !agrees(row$share_residual, d$share_residual)) {
      stop(sprintf("at %.2f N the record's residual share (%s) is not the episodes' (%s)",
                   row$wind_N, row$share_residual, d$share_residual))
    }
  }
  invisible(TRUE)
}
