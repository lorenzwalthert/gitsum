
#   ____________________________________________________________________________
#   simple log                                                              ####

#' get the log from a git repo
#'
#' @inheritParams get_raw_log
#' @importFrom readr read_delim
#' @importFrom tidyr separate_
#' @importFrom dplyr mutate_ select_ if_else rowwise rename_
#' @importFrom dplyr everything arrange_
#' @importFrom lubridate ymd_hms
#' @import magrittr
#' @export
get_log_simple <- function(path = ".") {
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

#   ____________________________________________________________________________
#   advanced log                                                            ####
# in the advanced method, we use regex to extract patterns from a log file.
# This is probably less safe, but allows us to catch much more information.
#' get log of a github repo via regex
#' @inheritParams get_raw_log
#' @importFrom readr read_lines
#' @importFrom stats setNames
#' @importFrom purrr map
#' @importFrom dplyr mutate_ select_ everything group_by do
#' @importFrom lubridate ymd_hms
#' @importFrom tidyr unnest_ nest_
#' @importFrom tibble data_frame as_tibble
#' @importFrom dplyr arrange_
#' @importFrom readr type_convert cols col_character col_integer col_time
#' @importFrom stringi stri_sub
#' @inheritParams get_log_simple
#' @export
get_log_regex <- function(path = ".", file_name = NULL) {
  # get regex-finder-functions
  fnc_list <- setNames(c(lapply(get_pattern(), extract_factory),
                         find_message, find_description),
                       nm = c(names(get_pattern()),
                              "message", "description"))

  # create log
  tbl <- get_raw_log(path = path, file_name = file_name) %>%
    mutate_(level = ~cumsum(grepl("^commit\\s\\w+?\\s?\\w+?\\s?\\w+?$", lines))) %>%
    group_by(level)
  message("parsing log with regex")
  tbl %>%
    do(nested = parse_log_one(.$lines, fnc_list)) %>%
    unnest_(~nested) %>%
    mutate_(date = ~ymd_hms(paste(year, month, monthday, time)),
            short_hash = ~stri_sub(hash, 1, 4),
            short_message = ~substr(message, 1, 20),
            short_description = ~substr(description, 1, 20),
            deletions_symbol = ~deletions,
            insertions_symbol = ~insertions,
            deletions = ~nchar(deletions),
            insertions = ~nchar(insertions)) %>%
    type_convert(col_types = cols(author_name = col_character(),
                                  author_email = col_character(),
                                  weekday = col_character(),
                                  monthday = col_integer(),
                                  time = col_time(),
                                  timezone = col_integer(),
                                  year = col_integer(),
                                  total_files_changed = col_integer(),
                                  total_insertions = col_integer(),
                                  total_deletions = col_integer(),
                                  changed_file = col_character(),
                                  edits = col_integer(),
                                  message = col_character(),
                                  description =  col_character(),
                                  short_hash = col_character(),
                                  short_message = col_character(),
                                  short_description = col_character(),
                                  deletions_symbol = col_character(),
                                  insertions_symbol = col_character()
                                  )) %>%
    nest_("nested", c("changed_file", "edits", "insertions", "deletions",
                      "deletions_symbol", "insertions_symbol")) %>%
    select_(~short_hash, ~author_name, ~date,
            ~short_message, ~short_description, ~everything()) %>%
    arrange_(~date)

}

#' closure to extract regex pattern from vector
#'
#' @param pattern The pattern the function should be able to extract.
extract_factory <- function(pattern) {
  function(raw) {
    # stri_match_all_regex(raw, "^commit\\s(\\w+)?(\\s\\w+)?(\\s\\w+)?$", omit_no_match = FALSE)[[1]]
    ind <- grepl(pattern, raw, perl = TRUE)
    out <- gsub(pattern, "\\1", raw[ind], perl = TRUE)
    out <- trimws(out)
    if (length(out) == 0) {
      NA
    } else {
      out
    }
  }
}


#' regex patterns for extraction
#'
#' @return returns a named vector with regex patterns to extract from a
#'   git log.
get_pattern <- function() {
  c(hash                = "^commit\\s(\\w+).*$",
    left_parent         = "^commit\\s\\w+\\s(\\w+).*$",
    right_parent        = "^commit\\s\\w+\\s\\w+\\s(\\w+)$",
    author_name         = "^Author\\:\\s(.*)\\s<.*>$",
    author_email        = "^Author\\:\\s.*\\s<(.*)>$",
    weekday             = "^Date\\:\\s*(\\w+)\\s\\w+\\s+\\d+\\s\\d+:\\d+:\\d+\\s\\d+\\s.*",
    month               = "^Date\\:\\s*\\w+\\s(\\w+)\\s+\\d+\\s\\d+:\\d+:\\d+\\s\\d+\\s.*",
    monthday            = "^Date\\:\\s*\\w+\\s\\w+\\s+(\\d+)\\s\\d+:\\d+:\\d+\\s\\d+\\s.*",
    time                = "^Date\\:\\s*\\w+\\s\\w+\\s+\\d+\\s(\\d+:\\d+:\\d+)\\s\\d+\\s[\\+\\-]?\\d+$",
    timezone            = "^Date\\:\\s*\\w+\\s\\w+\\s+\\d+\\s\\d+:\\d+:\\d+\\s\\d+\\s([\\+\\-]?\\d+)$",
    year                = "^Date\\:\\s*\\w+\\s\\w+\\s+\\d+\\s\\d+:\\d+:\\d+\\s(\\d+)\\s.*",
    total_files_changed = "^\\s(\\d+)\\sfiles?\\schanged,.*$",
    total_insertions    = "^\\s\\d+\\sfiles?\\schanged,\\s(\\d+)\\sinsertions\\(\\+\\),.*$",
    total_deletions     = "^\\s\\d+\\sfiles?\\schanged,.*\\s(\\d+)\\sdeletions\\(\\-\\)$",
    changed_file       = "^\\s(.*\\s)+\\|\\s+\\d+\\s\\+*\\-*",
    edits               = "^\\s.*\\s+\\|\\s+(\\d+)\\s\\+*\\-*",
    insertions          = "^\\s.*\\s+\\|\\s+\\d+\\s(\\+*)\\-*",
    deletions           = "^\\s.*\\s+\\|\\s+\\d+\\s\\+*(\\-*)")
}

#' extract message or description from a log
#'
#' For message and description extraction, a different approach is used and
#'   one cannot rely on the approach with the simple extraction factory.
#' @param raw A character vector corresponding to one commit.
#' @param target either "message" or "description
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
  c(message, description)
}

find_message <- function(raw) {
  find_message_and_desc(raw, target = "message")
}

find_description <- function(raw) {
  find_message_and_desc(raw, target = "description")
}

#' parse a raw log
#'
#' @param raw a character vector corresponding to one commit.
#' @param fnc_list the list list of named functions that return the elements
#'   we want to extract from the log, i.e. author, hash etc.
parse_log_one <- function(raw, fnc_list) {
  map(fnc_list, function(fnc) fnc(raw = raw)) %>%
    as_tibble()
}


#' Obtain the log raw data
#' @param path the path to the git directory one wants to create summaries for.
#' @param file_name the name of the temporary file. If `NULL`, a file is created,
#'   otherwise, a file is read.
#' @param remove whether a log should be deleted after read in.
get_raw_log <- function(path, file_name = NULL, remove = is.null(file_name)) {
  message("reading in log")
  file_name_progr <- ifelse(is.null(file_name), ".log.txt", file_name)
  path_to_file <- file.path(path, file_name_progr) %>%
    path.expand()

  if (is.null(file_name)) {
    if (file.exists(path_to_file)) {
      message("file ", path_to_file, " exists already")
    }
    sys_call <- paste('cd', path, '&&', 'git log --stat --parents >', file_name_progr)
    if (Sys.info()[1] == "Windows") {
      error <- shell(sys_call)
    } else {
      error <- system(sys_call)
    }
    if (error == 128) stop(path, " is not a git repository")
  }

  # get list of commits
  temp <- read_lines(path_to_file, progress = TRUE)
  message("done reading in log")
  if (remove) unlink(path_to_file)

  data_frame(lines = temp)
}
