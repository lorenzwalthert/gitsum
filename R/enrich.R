#' Add line history
#'
#' Add amount of lines added by each commits by file (insertions - deletions)
#' plus number of total lines a file has according to past insertions and
#' deletions (via [base::cumsum()].
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

#' How many times did file get changed?
#'
#' Add a variable `n_times_changed_file` to the unnested log, which contains
#' the number of times a file was changed according to `log`. This may be
#' helpful for subsetting.
#' @param log An unnested log.
#' @importFrom dplyr group_by summarize left_join
#' @export
add_n_times_changed_file <- function(log) {
  stats <- log %>%
    group_by(changed_file) %>%
    summarize(n_times_changed_file = n())
  left_join(log, stats, by = "changed_file")
}
