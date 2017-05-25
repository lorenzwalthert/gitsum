#   ____________________________________________________________________________
#   advanced log                                                            ####
# in the advanced method, we use regex to extract patterns from a log file.
# This is probably less safe, but allows us to catch much more information.
#' get log of a github repo via regex
#' @inheritParams get_raw_log
#' @importFrom stats setNames
#' @importFrom dplyr mutate_ select_ everything group_by_ do_
#' @importFrom lubridate ymd_hms
#' @importFrom tidyr unnest_ nest_
#' @importFrom dplyr arrange_
#' @importFrom readr type_convert cols col_integer col_time
#' @inheritParams get_log_simple
#' @export
git_log_detailed <- function(path = ".", file_name = NULL) {
  # get regex-finder-functions
  fnc_list <- setNames(c(find_message_and_desc,
                         lapply(get_pattern_multiple(), extract_factory_multiple)),
                       nm = c("message_and_description",
                              names(get_pattern_multiple())))

  # create log
  get_raw_log(path = path, file_name = file_name) %>%
    mutate_(level = ~cumsum(grepl("^commit", lines)),
            has_merge = ~grepl("^Merge:", lines)) %>%
    group_by_(~level) %>%
    do_(nested = ~parse_log_one(.$lines, fnc_list, any(.$has_merge))) %>%
    unnest_(~nested) %>%
    mutate_(date = ~ymd_hms(paste(year, month, monthday, time)),
            short_hash = ~substr(hash, 1, 4),
            short_message = ~substr(message, 1, 20),
            short_description = ~ifelse(!is.na(message), substr(description, 1, 20), NA),
            deletions = ~ifelse(!deletions == "", nchar(deletions), 0),
            insertions = ~ifelse(!insertions == "", nchar(insertions), 0),
            is_merge = ~ifelse(!is.na(left_parent) & !is.na(right_parent), TRUE, FALSE)) %>%
    type_convert(col_types = cols(monthday = col_integer(),
                                  time = col_time(),
                                  timezone = col_integer(),
                                  year = col_integer(),
                                  total_files_changed = col_integer(),
                                  total_insertions = col_integer(),
                                  total_deletions = col_integer(),
                                  edits = col_integer())) %>%
    nest_("nested", c("changed_file", "edits", "insertions", "deletions")) %>%
    select_(~short_hash, ~author_name, ~date,
            ~short_message, ~everything(), ~-level)
}
