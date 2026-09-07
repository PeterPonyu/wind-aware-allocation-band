# Helpers for the local temporal-estimate extension.  The Python analyzers do the
# statistical re-derivation; this layer checks that the receipts still point at
# the hash-bound raw bytes before a number reaches a figure or a TeX macro.

assert_receipt_source <- function(receipt, raw_path, label) {
  if (!identical(receipt$status, "analysis_ok")) {
    stop(label, " receipt is not analysis_ok")
  }
  actual <- digest::digest(raw_path, algo = "sha256", file = TRUE)
  if (!identical(as.character(receipt$source_sha256), actual)) {
    stop(label, " receipt does not hash the bound raw source")
  }
  lines <- readLines(raw_path, warn = FALSE)
  n_rows <- sum(nzchar(trimws(lines))) - 1L
  if (!identical(as.integer(receipt$n_rows), as.integer(n_rows))) {
    stop(label, " receipt row count does not match its bound raw source")
  }
  invisible(TRUE)
}

ci_component <- function(column, component) {
  vapply(column, function(pair) as.numeric(pair[[component]]), numeric(1))
}

flatten_ci <- function(frame, prefix, source_column) {
  frame[[paste0(prefix, "_lo")]] <- ci_component(source_column$ci95, 1L)
  frame[[paste0(prefix, "_hi")]] <- ci_component(source_column$ci95, 2L)
  frame[[paste0(prefix, "_mean")]] <- as.numeric(source_column$mean)
  frame[[paste0(prefix, "_p")]] <- as.numeric(source_column$wilcoxon_p_two_sided)
  frame
}
