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
#' @importFrom tidyr separate
#' @importFrom dplyr mutate select_ if_else rowwise rename
#' @importFrom dplyr everything arrange
#' @importFrom lubridate ymd_hms
#' @importFrom magrittr %>%
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
    separate(.data$date, into = time, sep = " ") %>%
    separate(
      .data$parents, into = c("left_parent", "right_parent"), sep = " ",
      fill = "right"
    ) %>%
    mutate(
      final_date = ymd_hms(paste(
        .data$year, .data$weekday, .data$month, .data$monthday, .data$time
      )),
      message_short = substr(.data$message, 1, 20)
    ) %>%
    rowwise() %>%
    mutate(n_parents = sum(
      !is.na(.data$left_parent), !is.na(.data$right_parent))
    ) %>%
    ungroup() %>%
    rename(date = .data$final_date) %>%
    select(.data$author, .data$message_short, .data$date, everything()) %>%
    arrange(.data$date)
  if (is.null(file_name)) unlink(file.path(path, file_name_prog))
  log
}
