#' Nest/unnest a gitsum log
#'
#' Forwards to [tidyr::nest()] while taking care of which columns have to be
#' nested.
#' @param log A log to nest/unnest.
#' @export
#' @importFrom tidyr nest_
nest_log <- function(log) {
  assert_detailed_log(log)
  if (is_detailed_log(log, nested = TRUE)) {
    warning("log was already nested, returning input log.", call. = FALSE)
  }
  as_nested_log(log)
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
  if (is_detailed_log(log, nested = FALSE)) {
    warning("log was already unnested, returning input log.", call. = FALSE)
  }
  as_unnested_log(log)
}

#' @importFrom tidyr nest
as_unnested_log <- function(log) {
  assert_detailed_log(log)
  if (is_detailed_log(log, nested = FALSE)) {
    unnested <- log
  } else {
    unnested <- unnest(log, .data$nested)
  }
  unnested
}


#' @importFrom tidyr unnest
as_nested_log <- function(log) {
  assert_detailed_log(log)
  if (is_detailed_log(log, nested = TRUE)) {
    nested <- log
  } else {
    nested <- nest(
      log, .data$changed_file, .data$edits,
      .data$insertions, .data$deletions, .data$is_exact,
      .key = "nested"
    )
  }
  nested
}

