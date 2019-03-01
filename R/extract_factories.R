#' closure to extract multiple regex pattern from vector
#'
#' @param pattern The pattern the function should be able to extract.
#' @importFrom stringr str_match
#' @keywords internal
extract_factory_multiple <- function(pattern) {
  function(raw) {
    out <- str_match(raw, pattern)[, -1]
    out <- out[!is.na(out[, 1]), , drop = FALSE]
    if (length(out) == 0) {
      matrix(NA, ncol = 4)
    } else {
      out
    }
  }
}
