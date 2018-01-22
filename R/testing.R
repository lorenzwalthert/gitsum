#' Create the path to a test that file
#' @param ... Arguments passed to [file.path()] to construct the path after
#'   ".../tests/testthat/"
testthat_file <- function(...) {
  file.path(rprojroot::find_testthat_root_file(), ...)
}

#' Parse a test log
#'
#' Function to facilitate the reading and parsing of a raw log for unit test
#' purposes.
#' @param path_to_raw_log The path to the raw log relative to tests/testthat/.
#' @param ... Params passed to [parse_log_detailed_full_run()].
#' @param parser The parser to use, i.e. either [parse_log_detailed_full_run()]
#'   or [parse_log_simple()].
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

#' @importFrom purrr map2_lgl
#' @importFrom tibble as_tibble
#' @importFrom dplyr pull
expect_class <- function(data, class_mapping) {
  tbl <- as_tibble(data)
  is_correct_class <- map2_lgl(
    pull(class_mapping, .data$name), pull(class_mapping, .data$class),
    expect_class_one, data
  )
}

#' @importFrom dplyr first pull
#' @importFrom rlang sym
expect_class_one <- function(name, class, data) {
  cls <- pull(data, !!sym(name)) %>%
    class() %>%
    first()

  if (cls != class) {
    warning(
      paste0("class for ", name, " was ", cls, ", but ", class, " was expected"),
      call. = FALSE
    )
  }
  cls == class
}
