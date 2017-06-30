#' Add line history
#'
#' Add amount of lines added by commits (insertions - deletions) plus
#'   number of total lines a file has according to past insertions and
#'   deletions (via [base::cumsum()].
#' @param log A log.
#' @export
add_line_history <- function(log) {
  log %>%
    unnest() %>%
    mutate(lines_added = insertions - deletions) %>%
    group_by(changed_file) %>%
    arrange(date) %>%
    mutate(current_lines = cumsum(lines_added))
}
