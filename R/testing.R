#' Create the path to a test that file
#' @param ... Arguments passed to [file.path()] to construct the path after
#'   ".../tests/testthat/"
testthat_file <- function(...) {
  file.path(rprojroot::find_testthat_root_file(), ...)
}
