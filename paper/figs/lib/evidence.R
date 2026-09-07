# Reading hash-bound evidence, and refusing to read anything else.
#
# A figure in this tree may only see bytes whose digest still matches the one
# recorded in the manifest, so a stale artifact stops the build instead of
# quietly becoming a plausible number.

find_repo_root <- function(start = normalizePath(".")) {
  cur <- start
  repeat {
    if (file.exists(file.path(cur, ".git"))) return(cur)
    nxt <- dirname(cur)
    if (identical(nxt, cur)) stop("no repository root above ", start)
    cur <- nxt
  }
}

load_manifest <- function() {
  manifest <- jsonlite::fromJSON(file.path("evidence", "evidence_manifest.json"),
                                 simplifyVector = TRUE)
  if (!identical(manifest$state, "BOUND") || length(manifest$entries) == 0) {
    stop("evidence manifest is not BOUND; bind the evidence before drawing anything.")
  }
  manifest
}

# Built once by the driver and closed over by the readers below, so a panel
# cannot reach past the manifest by constructing a path of its own.
evidence_reader <- function(manifest, repo_root) {
  bound_path <- function(id) {
    row <- manifest$entries[manifest$entries$id == id, ]
    if (nrow(row) != 1L) stop("no unique evidence entry bound under id ", id)
    path <- file.path(repo_root, row$path[[1]])
    if (!file.exists(path)) stop("bound evidence has disappeared: ", id)
    actual <- digest::digest(path, algo = "sha256", file = TRUE)
    if (!identical(actual, row$sha256[[1]])) stop("bound evidence drifted on disk: ", id)
    path
  }
  list(
    # Expose the verified path for cross-artifact receipts.  A derived analysis
    # may name its raw source, but the figure driver still checks that the name
    # resolves to the same hash-bound bytes before using either artifact.
    path = function(id) bound_path(id),
    json = function(id) jsonlite::fromJSON(bound_path(id), simplifyVector = TRUE),
    # The sweeps were written one JSON object per line, the first of which is the
    # provenance header the run wrote about itself rather than an episode. The
    # header is returned beside the episodes rather than dropped, because the
    # design it declares is what the episode count is checked against.
    sweep = function(id) {
      lines <- readLines(bound_path(id), warn = FALSE)
      lines <- lines[nzchar(trimws(lines))]
      objs <- lapply(lines, jsonlite::fromJSON, simplifyVector = TRUE)
      is_header <- vapply(objs, function(o) !is.null(o$`_provenance`), logical(1))
      if (sum(is_header) != 1L || !is_header[[1]]) {
        stop("sweep ", id, " does not begin with exactly one provenance header")
      }
      list(design = objs[[1]]$`_provenance`,
           episodes = do.call(rbind, lapply(objs[!is_header], as.data.frame)))
    }
  )
}
