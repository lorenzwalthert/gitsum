#' parse a raw log
#'
#' @param raw a character vector corresponding to one commit.
#' @param fnc_list the list of named functions that return the elements
#'   we want to extract from the log, i.e. author, hash etc.
#' @param has_merge Whether or not `raw` is a merge commit. This is relevant
#'   because merge commits have additional lines.
#' @importFrom stringr str_split str_sub
#' @importFrom tibble data_frame
parse_log_one <- function(raw, fnc_list, has_merge) {
  if (has_merge) {
    count <- 3
  } else {
    count <- 2
  }
  commit <- str_split(raw[1], " ", simplify = TRUE)
  author <- str_split(raw[count], " ", simplify = TRUE)
  date <- str_split(raw[count + 1], " ", simplify = TRUE)
  other <- raw[-c(1:(count + 1))]
  all_changes <- fnc_list$all_changes(other)
  all_changes_file <- fnc_list$all_changes_file(other)
  ms <- fnc_list$message_and_description(other)
  data_frame(
    hash = commit[2],
    left_parent = commit[3],
    right_parent = commit[4],
    author_name = paste(author[2:(length(author) - 1)], collapse = " "),
    author_email = str_sub(author[length(author)], 2, -1),
    weekday = date[4],
    month = date[5],
    monthday = date[6],
    time = date[7],
    year = date[8],
    timezone = date[9],
    message = ms[1],
    description = ms[2],
    total_files_changed = all_changes[, 1],
    total_insertions = all_changes[, 2],
    total_deletions = all_changes[, 3],
    changed_file = all_changes_file[, 1],
    edits = all_changes_file[, 2],
    insertions = all_changes_file[, 3],
    deletions = all_changes_file[, 4]
  )
}

#' Turn a raw log of lines into a tabular format
#'
#' @param lines The output of [get_raw_log()].
#' @importFrom rlang set_names
parse_lines <- function(lines) {
  extractors <- set_names(
    c(
      find_message_and_desc,
      lapply(get_pattern_multiple(), extract_factory_multiple)
    ), nm = c("message_and_description",names(get_pattern_multiple()))
  )

  lines %>%
    mutate_(
    level = ~cumsum(grepl("^commit", lines)),
    has_merge = ~grepl("^Merge:", lines)
  ) %>%
    group_by_(~level) %>%
    do_(nested = ~parse_log_one(.$lines, extractors, any(.$has_merge))) %>%
    ungroup() %>%
    mutate_(commit_nr = ~seq(nrow(.), 1L)) %>%
    select_(~-level) %>%
    unnest_("nested")
}
