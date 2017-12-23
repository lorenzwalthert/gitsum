#' Add line history
#'
#' Add amount of lines added by commits (insertions - deletions) plus number of
#' total lines a file has according to past insertions and deletions
#' (via [base::cumsum()].
#' @param log An unested gitsum log, for example obtained through
#'   [parse_log_detailed()].
#' @importFrom dplyr mutate group_by arrange
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @examples
#' add_line_history(tidyr::unnest(gitsumlog))
#' @export
add_line_history <- function(log) {
  log %>%
    mutate(
      lines_added = .data$insertions - .data$deletions,
      lines_added = ifelse(is.na(.data$lines_added), 0, .data$lines_added)
    ) %>%
    group_by(.data$changed_file) %>%
    arrange(.data$date) %>%
    mutate(current_lines = cumsum(.data$lines_added))
}
