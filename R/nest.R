#' Nest/unnest a gitsum log
#'
#' Forwards to [tidyr::nest()] while taking care of which columns have to be
#' nested.
#' @param log A log to nest/unnest.
#' @export
#' @importFrom tidyr nest
nest_log <- function(log) {
  assert_detailed_log(log)
  if (is_detailed_log(log, nested = FALSE)) {
    log <- nest(
      log,
      "nested" = c(
        .data$changed_file, .data$edits, .data$insertions,
      .data$deletions, .data$is_exact)
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
    log <- unnest(log, .data$nested)
  } else {
    warning("log was already unnested, returning input log.", call. = FALSE)
  }
  log
}
