#   ____________________________________________________________________________
#   simple log                                                              ####

#' Obtain a simple log for a git repo
#'
#' This function returns a git log in a tabular format.
#' @inheritParams get_raw_log
#' @param file_name The path to a raw log. If `NULL`, a raw log is created and
#'   read in and deleted after read-in, otherwise, an existing log is read.
#' @seealso See [parse_log_detailed] for a slower alternative with more
#'   information.
#' @importFrom readr read_delim
#' @importFrom tidyr separate_
#' @importFrom dplyr mutate_ select_ if_else rowwise rename_
#' @importFrom dplyr everything arrange_
#' @importFrom lubridate ymd_hms
#' @import magrittr
#' @export
parse_log_simple <- function(path = ".", file_name = NULL) {
  file_name_prog <- ifelse(is.null(file_name),
    "commits.local.tsv.txt", file_name
  )

  if (is.null(file_name)) {
    if (file.exists(file_name_prog)) {
      message("file ", file_name_prog, " overwritten")
    }
    sys_call <- paste(
      "cd", path, "&&", "git log",
      "--date=local",
      "--pretty=format:'%h%x09%an%x09%ad%x09%s%x09%P' >",
      file_name_prog
    )
    if (Sys.info()[1] == "Windows") {
      shell(sys_call)
    } else {
      system(sys_call)
    }
  }

  time <- c("weekday", "month", "monthday", "time", "year")
  log <- read_delim(
    file.path(path, file_name_prog),
    delim = "\t",
    col_names = c(
      "commit",
      "author",
      "date",
      "message", "parents"
    ),
    col_types = "ccccc"
  ) %>%
    separate_("date", into = time, sep = " ") %>%
    separate_(
      "parents", into = c("left_parent", "right_parent"), sep = " ",
      fill = "right"
    ) %>%
    mutate_(
      final_date = ~ymd_hms(paste(year, weekday, month, monthday, time)),
      message_short = ~substr(message, 1, 20)
    ) %>%
    rowwise() %>%
    mutate_(n_parents = ~sum(!is.na(left_parent), !is.na(right_parent))) %>%
    rename_(date = ~final_date) %>%
    select_(~author, ~message_short, ~date, ~everything()) %>%
    arrange_(~date)
  if (is.null(file_name)) unlink(file.path(path, file_name_prog))
  log
}
