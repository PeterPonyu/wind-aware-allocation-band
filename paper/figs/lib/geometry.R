# Helpers for the scene-geometry factorial extension.  The Python receipt owns
# the paired statistics; this layer checks its hash-bound source and reshapes
# only the already-derived values for figures and generated TeX.

geometry_condition_short <- function(spread, range_level, crossing) {
  paste(
    ifelse(spread == "nominal", "Nominal", "Expanded"),
    ifelse(range_level == "near", "near", "far"),
    ifelse(crossing == "aligned", "aligned", "crossed"),
    sep = " / "
  )
}

geometry_factor_label <- c(
  spread = "Target spread",
  range = "Target range",
  crossing = "Crossing orientation"
)

geometry_force_label <- function(force) sprintf("%.2f N", as.numeric(force))
