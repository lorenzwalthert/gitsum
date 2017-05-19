#' get the log from a git repo
#'
#' @param path the path to a git repo
#' @importFrom readr read_delim
#' @importFrom tidyr separate_
#' @importFrom dplyr mutate
#' @importFrom dplyr select_
#' @importFrom dplyr everything
#' @importFrom lubridate ymd_hms
#' @import magrittr
get_log <- function(path = ".") {
  path <- file.path(path, "commits.local.tsv.txt")
  if (file.exists(path)) {
    stop("file ", path, "exists already")
  }
  system('git log --date=local --pretty=format:"%h%x09%an%x09%ad%x09%s" > commits.local.tsv.txt')
  time <- c("weekday", "month", "monthday", "time", "year")
  log <- read_delim("commits.local.tsv.txt", delim = "\t",
                    col_names = c("commit",
                                  "author",
                                  "date",
                                  "message")) %>%
    separate_("date", into = time, sep = " ") %>%
    mutate(final_date = ymd_hms(paste(year, weekday, month, monthday, time)),
           message_short = substr(message, 1, 20)) %>%
    select_(~author, ~message_short, ~final_date, ~everything())
  unlink(path)
}
