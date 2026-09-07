# What the runs recorded about themselves, and the checks that keep the
# manuscript's description of them true.
#
# The manuscript states the size of each sweep, the swarm it flew and the noise
# grid it swept. All three are read off the episodes rather than typed, and each
# is checked against the design the run declared in its own header, so a sweep
# that was rerun at a different shape stops the build instead of leaving a stale
# sentence behind.

# The declared design is a full factorial, so its size is the product of its
# factors. An episode count that disagrees with that product means the run on
# disk is not the run its own header describes.
sweep_size <- function(sweep, factors) {
  declared <- prod(vapply(factors, function(f) length(sweep$design[[f]]), integer(1)))
  if (nrow(sweep$episodes) != declared) {
    stop("a sweep holds ", nrow(sweep$episodes), " episodes for a design of ", declared)
  }
  nrow(sweep$episodes)
}

# A constant of the apparatus rather than a variable of the experiment. Taking
# it from the episodes keeps it out of the prose, and refusing a second value
# keeps two differently-configured runs from being averaged into one sentence.
single_valued <- function(x, what) {
  v <- unique(x)
  if (length(v) != 1L) stop("the episodes disagree about ", what)
  v
}

# The summarised tables and the raw sweeps are separate artifacts that were
# written by separate runs. The manuscript reads its counts from the sweeps and
# its statistics from the summaries, so the two are only interchangeable while
# they still describe the same design.
assert_summaries_match_sweep <- function(oracle_sweep, noise_sweep, n_levels,
                                         seed_count, sigma_grid, summarised_sigma) {
  if (length(oracle_sweep$design$seeds) != seed_count) {
    stop("the sweep no longer runs the number of seeds the measurement pairs over")
  }
  if (length(oracle_sweep$design$wind_levels) != n_levels) {
    stop("the sweep no longer covers the force levels the measurement summarises")
  }
  if (!setequal(sigma_grid, summarised_sigma)) {
    stop("the summarised noise levels are not the ones the sweep swept")
  }
  invisible(TRUE)
}
