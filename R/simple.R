#   ____________________________________________________________________________
#   simple log                                                              ####

#' Obtain a simple log for a git repo
#'
#' This function returns a git log in a tabular format.
#' @inheritParams get_raw_log
#' @seealso See [git_log_detailed] for a slower alternative with more information.
#' @importFrom readr read_delim
#' @importFrom tidyr separate_
#' @importFrom dplyr mutate_ select_ if_else rowwise rename_
#' @importFrom dplyr everything arrange_
#' @importFrom lubridate ymd_hms
#' @import magrittr
#' @export
git_log_simple <- function(path = ".") {
  path <- file.path(path, "commits.local.tsv.txt")
  if (file.exists(path)) {
    message("file ", path, " exists already")
  }
  system('git log --date=local --pretty=format:"%h%x09%an%x09%ad%x09%s%x09%P" > commits.local.tsv.txt')
  time <- c("weekday", "month", "monthday", "time", "year")
  log <- read_delim("commits.local.tsv.txt", delim = "\t",
                    col_names = c("commit",
                                  "author",
                                  "date",
                                  "message", "parents"),
                    col_types = "ccccc") %>%
    separate_("date", into = time, sep = " ") %>%
    separate_("parents", into = c("left_parent", "right_parent"), sep = " ",
              fill = "right") %>%
    mutate_(final_date = ~ymd_hms(paste(year, weekday, month, monthday, time)),
            message_short = ~substr(message, 1, 20)) %>%
    rowwise() %>%
    mutate_(n_parents = ~sum(!is.na(left_parent),!is.na(right_parent))) %>%
    rename_(date = ~final_date) %>%
    select_(~author, ~message_short, ~date, ~everything()) %>%
    arrange_(~date)
  unlink(path)
  log
}

