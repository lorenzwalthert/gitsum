#' Nest/unnest a gitsum log
#'
#' Forwards to [tidyr::nest()] while taking care of which columns have to be
#' nested nested.
#' @param log A log to nest/unnest.
#' @export
#' @importFrom tidyr nest_
nest_log <- function(log) {
  nest_(log, "nested",
    c("changed_file", "edits", "insertions", "deletions", "is_exact")
  )
}

#' Nest/unnest a gitsum log
#'
#' Forwards to [tidyr::unnest()] while taking care of which columns have to be
#' nested unnested.
#' @inheritParams nest_log
#' @rdname nest_log
#' @export
unnest_log <- function(log) {
  unnest_(log, "nested")
}
