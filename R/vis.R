#' commits_by_author <- function(log) {
#'   ggplot(log, aes(x = author)) + geom_bar() + coord_flip()
#' }
#' commits_by_author(log)
#'
#'
#' #' show commits over time
#' #'
#' #' @param log a log table.
#' #' @param binsize The size of the bins in days.
#' #' @param by_author Whether or not to show commits by author.
#' commits_over_time <- function(log, binwidth = 1, by_author = TRUE) {
#'   binwidth <- 60*60*24 * binwidth
#'   fill <- NULL
#'   if (by_author) fill <- as.name("author")
#'   ggplot(log, aes_(x = as.name("final_date"), fill = fill)) +
#'     geom_histogram(binwidth = binwidth, position = "stack")
#' }
#' commits_over_time(log, 7)
#'
#' log %>%
#'   group_by(author) %>%
#'   count()
