#' Nest/unnest a gitsum log
#'
#' Forwards to [tidyr::nest()] while taking care of which columns have to be
#' nested.
#' @param log A log to nest/unnest.
#' @export
#' @importFrom tidyr nest_
nest_log <- function(log) {
  assert_detailed_log(log)
  if (is_detailed_log(log, nested = FALSE)) {
    log <- nest_(log, "nested",
          c("changed_file", "edits", "insertions", "deletions", "is_exact")
    )
  } else {
    warning("log was already nested, returning input log.", call. = FALSE)
  }
  log
}

#' Nest/unnest a gitsum log
#'
#' Forwards to [tidyr::unnest()] while taking care of which columns have to be
#' nested unnested.
#' @inheritParams nest_log
#' @rdname nest_log
#' @export
unnest_log <- function(log) {
  assert_detailed_log(log)
  if (is_detailed_log(log, nested = TRUE)) {
    log <- unnest_(log, "nested")
  } else {
    warning("log was already unnested, returning input log.", call. = FALSE)
  }
  log
}
