# The interval this paper reports around a paired difference.
#
# The exact signed-rank p-values come from the recorded summaries; what is
# recomputed here is the interval drawn around each paired mean, because the
# figures need it at every noise level and the summaries carry only the spread.

# A t interval over the paired differences at each noise level. Ten seeds is few
# enough that the normal quantile would be visibly too narrow, so the interval
# uses the t quantile at the pair count the summary itself reports.
paired_interval <- function(mean, sd, n, level = 0.95) {
  half_width <- stats::qt(1 - (1 - level) / 2, df = n - 1L) * sd / sqrt(n)
  list(lo = mean - half_width, hi = mean + half_width)
}

# Bonferroni across a family whose size is counted rather than asserted. The
# manuscript quotes both this threshold and the number of comparisons it divides
# by, and they are emitted from the same call so they cannot disagree.
bonferroni <- function(alpha, comparisons) alpha / comparisons

# Deterministic Holm step-down correction shared by the revision receipt and
# the figure pipeline.  Ties are ordered by the supplied identifier so that a
# re-render cannot change adjusted values merely because row order changed.
holm_adjust <- function(p_values, identifiers = names(p_values)) {
  # Capture names before numeric coercion, because as.numeric() drops them.
  if (missing(identifiers) || is.null(identifiers)) identifiers <- names(p_values)
  p_values <- as.numeric(p_values)
  if (length(p_values) == 0L || any(!is.finite(p_values)) ||
      any(p_values < 0 | p_values > 1)) {
    stop("p_values must be a non-empty finite vector in [0, 1]")
  }
  if (is.null(identifiers)) identifiers <- as.character(seq_along(p_values))
  identifiers <- as.character(identifiers)
  if (length(identifiers) != length(p_values) || any(!nzchar(identifiers)) ||
      anyDuplicated(identifiers)) {
    stop("identifiers must be unique and have one value per p-value")
  }
  ordering <- order(p_values, identifiers, method = "radix")
  adjusted <- numeric(length(p_values))
  running_max <- 0
  m <- length(ordering)
  for (rank in seq_along(ordering)) {
    candidate <- min(1, (m - rank + 1) * p_values[ordering[[rank]]])
    running_max <- max(running_max, candidate)
    adjusted[ordering[[rank]]] <- running_max
  }
  names(adjusted) <- identifiers
  adjusted[order(names(adjusted), method = "radix")]
}

# Resample declared cluster means, not episode rows.  The returned n_clusters
# is intentionally explicit so downstream captions cannot mistake the number
# of bootstrap draws for the number of independent experimental units.
cluster_bootstrap_mean <- function(values, n_boot = 10000L, seed = 0L,
                                   level = 0.95) {
  values <- as.numeric(values)
  if (length(values) == 0L || any(!is.finite(values))) {
    stop("values must be a non-empty finite vector")
  }
  if (length(n_boot) != 1L || !is.finite(n_boot) || n_boot < 100 ||
      n_boot != as.integer(n_boot)) {
    stop("n_boot must be an integer >= 100")
  }
  if (length(seed) != 1L || !is.finite(seed) || seed < 0 ||
      seed != as.integer(seed)) {
    stop("seed must be a non-negative integer")
  }
  if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) {
    stop("level must lie strictly between zero and one")
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  draws <- matrix(sample(values, size = length(values) * as.integer(n_boot),
                         replace = TRUE), nrow = as.integer(n_boot),
                  ncol = length(values))
  means <- rowMeans(draws)
  alpha <- (1 - level) / 2
  list(
    n_clusters = length(values),
    n_boot = as.integer(n_boot),
    seed = as.integer(seed),
    mean = mean(values),
    ci95 = as.numeric(stats::quantile(means, c(alpha, 1 - alpha),
                                      names = FALSE, type = 7)),
    method = "ordinary bootstrap over layout-level cluster means"
  )
}
