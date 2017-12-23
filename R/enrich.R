#' Add line history
#'
#' Add amount of lines added by commits (insertions - deletions) plus number of
#' total lines a file has according to past insertions and deletions
#' (via [base::cumsum()].
#' @param log An unested gitsum log, for example obtained through
#'   [parse_log_detailed()].
#' @importFrom dplyr mutate group_by arrange
#' @importFrom tidyr unnest
#' @examples
#' add_line_history(tidyr::unnest(gitsumlog))
#' @export
add_line_history <- function(log) {
  log %>%
    mutate(
      lines_added = insertions - deletions,
      lines_added = ifelse(is.na(lines_added), 0, lines_added)
    ) %>%
    group_by(changed_file) %>%
    arrange(date) %>%
    mutate(current_lines = cumsum(lines_added))
}
