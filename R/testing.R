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

#' Class validation
#'
#' Check whether columns of a data frame match are of a certain class and return
#' an informative warning otherwise.
#' @param data A data frame to check.
#' @param class_mapping A data frame that contains two columns: name and class,
#'   whereas the values in name indicate the name of a column to check, class
#'   indicates the target class of the column.
#' @examples
#' library(tibble)
#' library(magrittr)
#' class_mapping <- tribble(
#'   ~ name,  ~ class,
#'   "cyl", "integer",
#'   "model", "character"
#' )
#' rownames_to_column(mtcars, var= "model") %>%
#'   expect_class(class_mapping)
#' @importFrom purrr map2_lgl
#' @importFrom tibble as_tibble
#' @importFrom dplyr pull
expect_class <- function(data, class_mapping) {
  class_mapping <- as_tibble(class_mapping)
  is_correct_class <- map2_lgl(
    pull(class_mapping, .data$name), pull(class_mapping, .data$class),
    expect_class_one, as_tibble(data)
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
