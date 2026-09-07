# Helpers for the swarm-size support campaign.  The Python receipt owns all
# paired statistics; R only checks the bound source and flattens nested fields
# for the figure and generated table.

nested_numeric <- function(column, field) {
  # jsonlite simplifies a homogeneous nested object to a data.frame, while a
  # heterogeneous object remains a list of named lists.  Accept both shapes so
  # the receipt reader stays stable when the JSON schema gains a scalar field.
  if (is.data.frame(column)) return(as.numeric(column[[field]]))
  vapply(column, function(item) as.numeric(item[[field]]), numeric(1))
}

scale_size_label <- function(size) sprintf("%d airframes", as.integer(size))
