#' regex patterns for extraction
#'
#' @return returns a named vector with regex patterns to extract from a
#'   git log.
#' @keywords internal
get_pattern_multiple <- function() {
  c(
    all_changes = paste0(
      "^\\s(\\d+)\\sfiles?\\schanged,?\\s?(\\d+)?",
      "\\s?i?n?s?e?r?t?i?o?n?s?\\(?\\+?\\)?,",
      "?\\s?(\\d+)?\\s?d?e?l?e?t?i?o?n?s?",
      "\\(?\\-?\\)?$"
    ),
    all_changes_file = "^\\s(.*)\\s+\\|\\s*(\\d+)*\\s*(\\+*)(\\-*)"
  )
}
