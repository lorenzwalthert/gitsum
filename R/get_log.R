#' get the log from a git repo
#'
#' @param path the path to a git repo.
#' @importFrom readr read_delim
#' @importFrom tidyr separate_
#' @importFrom dplyr mutate_ select_ if_else rowwise
#' @importFrom dplyr everything
#' @importFrom lubridate ymd_hms
#' @import magrittr
#' @export
get_log <- function(path = ".") {
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
    select_(~author, ~message_short, ~final_date, ~everything())
  unlink(path)
  log
}


get_change_log <- function(path) {
  system(' git log --pretty=tformat: --numstat > commits.local.tsv.txt')
}


file_in <- readr::read_lines("log_stat.txt")
level <- cumsum(grepl("^commit [[:alnum:]]+$", file_in))

all_raw <- split(file_in, level)

extract_commit <- function(raw) {
  pattern <- "^commit ([[:alnum:]]+)$"
  ind <- grepl(pattern, raw)
  gsub(pattern, "\\1", raw[ind])
}
extract_commit(all_raw[[1]])

extract_factory <- function(pattern) {
  function(raw) {
    ind <- grepl(pattern, raw, perl = TRUE)
    out <- gsub(pattern, "\\1", raw[ind], perl = TRUE)
    trimws(out)
  }
}

extract_commit <- extract_factory("^commit (\\w+)$")
extract_commit(all_raw[[1]])

all_pattern <- c(hash          = "^commit (\\w+)$",
                 author        = "^Author\\:\\s(.*)\\s<.*$",
                 weekday       = "^Date\\:\\s*(\\w+)\\s\\w+\\s+\\d+\\s\\d+:\\d+:\\d+\\s\\d+\\s.*",
                 month         = "^Date\\:\\s*\\w+\\s(\\w+)\\s+\\d+\\s\\d+:\\d+:\\d+\\s\\d+\\s.*",
                 monthday      = "^Date\\:\\s*\\w+\\s\\w+\\s+(\\d+)\\s\\d+:\\d+:\\d+\\s\\d+\\s.*",
                 time          = "^Date\\:\\s*\\w+\\s\\w+\\s+\\d+\\s(\\d+:\\d+:\\d+)\\s\\d+\\s.*",
                 year          = "^Date\\:\\s*\\w+\\s\\w+\\s+\\d+\\s\\d+:\\d+:\\d+\\s(\\d+)\\s.*",
                 changed_files = "^\\s(.*\\s)+\\|\\s+\\d+\\s\\+*\\-*",
                 edits = "^\\s.*\\s+\\|\\s+(\\d+)\\s\\+*\\-*",
                 insertions = "^\\s.*\\s+\\|\\s+\\d+\\s(\\+*)\\-*",
                 deletions = "^\\s.*\\s+\\|\\s+\\d+\\s\\+*(\\-*)")

find_message_and_desc <- function(raw, target) {
  ind <- which(grepl("^\\s{4}$", raw))
  if (length(ind) == 1) {
    # if one line has just four spaces, we have both message and description
    message <- gsub("^\\s{4}(.*)$", "\\1", raw[ind - 1])
    description <- gsub("^\\s{4}(.*)$", "\\1", raw[ind + 1])
  } else {
    ind <- which(grepl("^$", raw))[1]
    message <- gsub("^\\s{4}(.*)$", "\\1", raw[ind + 1])
    description <- NA
  }
  if (target == "message") description <- NULL
  if (target == "description") message <- NULL
  c(message = message, description = description)
}


find_message <- function(raw) {
  find_message_and_desc(raw, target = "message")
}

find_description <- function(raw) {
  find_message_and_desc(raw, target = "message")
}

find_message(all_raw[[1]])

out <- setNames(c(lapply(all_pattern, extract_factory), find_message),
         nm = c(names(all_pattern), "find_message"))

parse_log <- function(raw, fnc_list) {
  lapply(raw, function(raw_one) {
    as.list(lapply(out, function(fnc) fnc(raw = raw_one)))
  })
}

a <- lapply(out, function(fnc) fnc(raw = all_raw[[1]]))

a <- parse_log(all_raw, out)

b <- map(a[2], function(x) do.call("data_frame", args = x))

bind <- function(a) {

}

b %>%
  nest(changed_files)



a <- parse_log(all_raw, out)
b <- transpose(a)
out$author(all_raw[[1]])
out$weekday(all_raw[[1]])
out$year(all_raw[[1]])



find_message(all_raw[[3]])
