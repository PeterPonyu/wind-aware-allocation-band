# The disturbance heading, and the repetitions nobody planned.
#
# A seed fixes one heading for the constant force, and the same seed is reused at
# every force level, in both arms, and in the second sweep. The heading is
# therefore a property of the seed rather than of the episode, which is what lets
# two seeds that happened to draw nearly the same heading be read as a repetition
# of one condition instead of as two conditions.
#
# Nothing here re-runs anything and nothing here is a new measurement. Every
# quantity is derived from episodes already bound by digest, and the seed-level
# record the manuscript also reads is re-derived from those episodes before
# either of them is used in a sentence.

TWO_PI <- 2 * pi

# Headings live on a circle, so the distance between two of them is the shorter
# way round. It is worth writing out rather than subtracting, because two of the
# drawn headings sit within a few degrees of zero and a plain difference would
# report them as nearly opposite.
angular_separation <- function(a, b) {
  d <- abs(a - b) %% TWO_PI
  pmin(d, TWO_PI - d)
}

degrees <- function(rad) rad * 180 / pi

# One heading per seed, refused if any seed carries two. A seed that has drifted
# to a second heading is a seed whose episodes are not paired, and every paired
# statement in this paper would then be quietly about something else.
seed_headings <- function(episodes) {
  by_seed <- split(episodes$wind_angle_rad, episodes$seed)
  headings <- vapply(by_seed, single_valued, numeric(1),
                     what = "the heading a seed draws")
  headings[order(as.integer(names(headings)))]
}

# The two sweeps were written by two runs, and the manuscript reads the paired
# comparison from one and the noise manipulation from the other while treating a
# seed as naming the same condition in both. That holds only while both sweeps
# still draw the same heading for it.
assert_headings_agree <- function(a, b) {
  if (!setequal(names(a), names(b))) {
    stop("the two sweeps no longer run the same seeds")
  }
  if (!isTRUE(all.equal(unname(a), unname(b[names(a)]), tolerance = 1e-12))) {
    stop("the two sweeps disagree about the heading a seed draws")
  }
  invisible(TRUE)
}

# The endpoint is a count of airframes wearing the costume of a rate. Checking it
# is what licenses the manuscript to say that a per-episode outcome cannot take a
# value between two adjacent multiples of one over the swarm size.
assert_endpoint_is_a_count <- function(episodes) {
  implied <- episodes$num_finished / episodes$num_drones
  if (!isTRUE(all.equal(episodes$completion_rate, implied, tolerance = 1e-12))) {
    stop("completion is no longer the finished count over the swarm size")
  }
  invisible(TRUE)
}

# Pairs of seeds close enough in heading to count as a repetition of one
# condition. The cut is stated, not searched, and then shown not to matter: the
# widest window of cuts that selects exactly this set is returned beside it, and
# the manuscript prints the window. An unintended repetition that appeared or
# vanished when the cut moved by a degree would not be worth reporting, so the
# function refuses to return a set whose window does not contain the stated cut.
heading_replicates <- function(headings, tol_rad) {
  seeds <- as.integer(names(headings))
  idx <- utils::combn(length(seeds), 2)
  sep <- angular_separation(headings[idx[1, ]], headings[idx[2, ]])
  inside <- sep <= tol_rad
  if (!any(inside) || all(inside)) {
    stop("the stated heading tolerance separates nothing from anything")
  }
  list(
    pairs = data.frame(a = seeds[idx[1, inside]], b = seeds[idx[2, inside]],
                       separation = sep[inside])[order(sep[inside]), ],
    # The two numbers that make the cut arbitrary or not: every pair called a
    # repetition is closer than the first number, and every pair called a
    # distinct condition is further than the second.
    widest_kept = max(sep[inside]),
    nearest_dropped = min(sep[!inside])
  )
}

# The largest run of the circle in which no heading was drawn at all. Ten draws
# cannot cover a circle, and saying by how much they fail to is more informative
# than saying the headings were random. The bounds come back with the width
# because the figure has to shade the arc it names.
largest_unsampled_arc <- function(headings) {
  # Unnamed: the headings arrive keyed by seed, and an arc is a property of the
  # circle rather than of whichever seed happens to bound it. Carrying the name
  # through makes the width compare unequal to the same number computed
  # elsewhere, for a reason that has nothing to do with the number.
  sorted <- unname(sort(headings %% TWO_PI))
  ends <- c(sorted[-1], sorted[1] + TWO_PI)
  widest <- which.max(ends - sorted)
  list(from = sorted[widest], to = ends[widest], width = ends[widest] - sorted[widest])
}

# Seeds joined into groups by the repetition relation, so that the manuscript can
# say how many distinct conditions ten draws actually produced. Transitivity is
# not assumed: the groups are the connected components of the pairs found above,
# and a chain of near-repeats is one group whatever its total width, which is
# reported beside it so that a wide chain cannot pass as a tight cluster.
heading_groups <- function(headings, replicates) {
  seeds <- as.integer(names(headings))
  group <- seq_along(seeds)
  names(group) <- as.character(seeds)
  for (i in seq_len(nrow(replicates$pairs))) {
    a <- as.character(replicates$pairs$a[i])
    b <- as.character(replicates$pairs$b[i])
    group[group == group[[b]]] <- group[[a]]
  }
  members <- split(seeds, group)
  list(
    n_groups = length(members),
    members = members,
    largest = members[[which.max(lengths(members))]],
    widest_group_span = max(vapply(members, function(m) {
      if (length(m) < 2L) return(0)
      h <- headings[as.character(m)]
      max(angular_separation(rep(h, each = length(h)), rep(h, times = length(h))))
    }, numeric(1)))
  )
}

# What a repetition is for. At each force level, the largest outcome difference
# between two episodes of the same arm whose disturbance differed by less than
# the tolerance: a lower bound on the outcome variation the recorded conditions
# do not account for.
#
# A level at which every seed of both arms returned the same value cannot
# disagree with itself, so a bound of zero there is a property of the ceiling or
# the floor and not a demonstration of repeatability. The two facts are returned
# separately because the manuscript is required to print both.
replicate_gaps <- function(episodes, replicates, arms) {
  levels_seen <- sort(unique(episodes$wind_mag_N))
  outcome <- function(arm, level, seed) {
    hit <- episodes$treatment == arm & episodes$wind_mag_N == level &
      episodes$seed == seed
    single_valued(episodes$completion_rate[hit], "an arm's outcome on one seed")
  }
  gap_at <- function(level) {
    max(vapply(arms, function(arm) {
      max(abs(vapply(seq_len(nrow(replicates$pairs)), function(i) {
        outcome(arm, level, replicates$pairs$a[i]) -
          outcome(arm, level, replicates$pairs$b[i])
      }, numeric(1))))
    }, numeric(1)))
  }
  spread_at <- function(level) {
    max(vapply(arms, function(arm) {
      vals <- episodes$completion_rate[episodes$treatment == arm &
                                         episodes$wind_mag_N == level]
      diff(range(vals))
    }, numeric(1)))
  }
  data.frame(
    wind_N = levels_seen,
    gap = vapply(levels_seen, gap_at, numeric(1)),
    # Zero here means no seed of either arm differed from any other, so no
    # pairing of seeds could have disagreed and the level carries no evidence
    # about repeatability either way.
    seed_spread = vapply(levels_seen, spread_at, numeric(1))
  )
}

# Per-pair disagreement, kept separate from the maximum above so that the table
# can show which repetition carries the bound rather than only its height.
replicate_gap_by_pair <- function(episodes, replicates, arms) {
  levels_seen <- sort(unique(episodes$wind_mag_N))
  out <- lapply(seq_len(nrow(replicates$pairs)), function(i) {
    a <- replicates$pairs$a[i]
    b <- replicates$pairs$b[i]
    per_level <- vapply(levels_seen, function(level) {
      max(vapply(arms, function(arm) {
        rows <- episodes$treatment == arm & episodes$wind_mag_N == level
        abs(single_valued(episodes$completion_rate[rows & episodes$seed == a], "an outcome") -
              single_valued(episodes$completion_rate[rows & episodes$seed == b], "an outcome"))
      }, numeric(1)))
    }, numeric(1))
    c(list(a = a, b = b, separation = replicates$pairs$separation[i]),
      setNames(as.list(per_level), sprintf("L%s", levels_seen)))
  })
  do.call(rbind.data.frame, out)
}

# The seed-level record and the episodes it summarises were written by different
# passes over the same run. The manuscript prints the record, so the record is
# re-derived from the episodes before anything is printed: a mismatch means the
# two describe different runs and no sentence resting on either can be trusted.
assert_record_matches_episodes <- function(record, episodes, arm_of) {
  rows <- record$rows[order(record$rows$seed), ]
  at_level <- episodes[episodes$wind_mag_N == record$wind_N, ]
  if (nrow(rows) != record$n) {
    stop("the seed record holds ", nrow(rows), " seeds but declares ", record$n)
  }
  for (i in seq_len(nrow(rows))) {
    seed <- rows$seed[i]
    for (side in names(arm_of)) {
      ep <- at_level[at_level$seed == seed & at_level$treatment == arm_of[[side]], ]
      if (nrow(ep) != 1L) stop("no unique episode for seed ", seed, " in arm ", side)
      for (what in list(
        list(rows[[paste0(side, "_completion")]][i], ep$completion_rate, "completion"),
        list(rows[[paste0(side, "_num_finished")]][i], ep$num_finished, "finished count"),
        list(rows[[paste0(side, "_replan_events")]][i], ep$replan_events, "replan count")
      )) {
        if (!isTRUE(all.equal(as.numeric(what[[1]]), as.numeric(what[[2]])))) {
          stop(sprintf("seed %d, %s arm: the record's %s (%s) is not the episode's (%s)",
                       seed, side, what[[3]], what[[1]], what[[2]]))
        }
      }
    }
    if (!isTRUE(all.equal(rows$wind_angle_rad[i],
                          single_valued(at_level$wind_angle_rad[at_level$seed == seed],
                                        "the heading at one seed")))) {
      stop("seed ", seed, ": the record's heading is not the episode's heading")
    }
  }
  if (!isTRUE(all.equal(record$paired_delta_mean, mean(rows$paired_delta)))) {
    stop("the record's mean paired difference is not the mean of the pairs it lists")
  }
  invisible(TRUE)
}

# Replanning is logged when the allocator changes its mind, so it is the one
# recorded quantity that reports the episode was hard rather than that it went
# well. Splitting the seeds by whether either arm replanned says whether the
# spread in the advantage lives with the episodes that were hard.
replan_split <- function(rows) {
  quiet <- rows$baseline_replan_events == 0 & rows$wind_aware_replan_events == 0
  list(
    n_quiet = sum(quiet), n_loud = sum(!quiet),
    delta_quiet = mean(rows$paired_delta[quiet]),
    delta_loud = mean(rows$paired_delta[!quiet]),
    total_baseline = sum(rows$baseline_replan_events),
    total_aware = sum(rows$wind_aware_replan_events)
  )
}
