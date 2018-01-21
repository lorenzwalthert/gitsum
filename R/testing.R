#' Create the path to a test that file
#' @param ... Arguments passed to [file.path()] to construct the path after
#'   ".../tests/testthat/"
testthat_file <- function(...) {
  file.path(rprojroot::find_testthat_root_file(), ...)
}

#' Parse a test log
#'
#' @param path_to_raw_log The path to the raw log relative to tests/testthat/.
#' @param ... Params passed to [parse_log_detailed_full_run()].
parse_test_log <- function(path_to_raw_log, parser = parse_log_detailed_full_run, ...) {
  path <- testthat_file(path_to_raw_log)
  parser(
    path = dirname(path),
    file_name = basename(path),
    ...
  )
}

#' @importFrom purrr partial
parse_test_log_detailed <- partial(parse_test_log, parser = parse_log_detailed_full_run)
parse_test_log_simple <- partial(parse_test_log, parser = parse_log_simple)
