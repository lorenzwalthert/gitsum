#   ____________________________________________________________________________
#   advanced log                                                            ####

#' Obtain a detailed git log
#'
#' This function returns a git log in a tabular format.
#' @inheritParams parse_log_simple
#' @inheritParams get_raw_log
#' @param na_to_zero Whether some `NA` values should be converted to zero.
#'   See 'Details'.
#' @param update_dump Whether or not to update the dump in .gitsum after
#'   parsing.
#' @details
#' * Note that for merge commits, the following columns are `NA` if
#'   the option `na_to_zero` is set to `FALSE`.:
#'   total_files_changed, total_insertions, total_deletions, changed_file,
#'   edits, deletions, insertions.
#' * Note that for binary files, the following columns are 0: edits, deletions,
#'   insertions.
#' @section Warning:
#'   The number of edits, insertions, and deletions (on a file level) are based
#'   on `git log --stat` and the number of `+` and `-` in this log. The number
#'   of `+` and `-` may not sum up to the edits indicated as a scalar after "|"
#'   in `git log --stat`
#'   for commits with very many changed lines since for those, the `+` and `-`
#'   only indicate the relative share of insertions and edits. Therefore,
#'   `parse_log_detailed_full_run()` normalizes the insertions and deletions and rounds
#'   these after the normalization to achieve more consistent results. However,
#'   there is no guarantee that these numbers are always exact. The column
#'   is_exact indicates for each changed file within a commit whether the result
#'   is exact (which is the case if the normalizing constant was one).
#' @return A parsed git log as a nested tibble. Each line corresponds to a
#'   commit. The unnested column names are: \cr
#'   short_hash, author_name, date, short_message, hash, left_parent,
#'   right_parent, author_email, weekday, month, monthday, time, year, timezone,
#'   message, description, total_files_changed, total_insertions,
#'   total_deletions, short_description, is_merge \cr
#'   The nested columns contain more information on each commit. The column
#'   names are: \cr
#'   changed_file, edits, insertions, deletions.
#' @seealso See [parse_log_simple] for a fast alternative with less information.
#' @importFrom dplyr mutate_ select_ everything group_by_ do_ last
#' @importFrom lubridate ymd_hms
#' @importFrom tidyr unnest_ nest_
#' @importFrom dplyr arrange_ ungroup bind_rows
#' @export
parse_log_detailed <- function(path = ".", update_dump = TRUE) {
  last_hash <- read_last_hash(path)
  new_log <- read_log(path) %>%
    bind_rows(
      parse_log_detailed_full_run(path, commit_range = paste0(last_hash, "..HEAD"))
    )
  if (update_dump) {
    update_dump_from_log(new_log, path)
  }
  new_log
}

#' @describeIn parse_log_detailed In contrast to parse_log_detailed, this function
#'   does not read any history from the .gitum directory.
#' @export
parse_log_detailed_full_run <- function(path = ".",
                                        na_to_zero = TRUE,
                                        file_name = NULL,
                                        commit_range = NULL) {
  # create log
  lines <- get_raw_log(path, file_name, commit_range = commit_range)
  if (nrow(lines) < 1) return(data_frame())
  if (last(lines$lines) != "") {
    lines[nrow(lines) + 1, 1] <- ""
  }
  parsed_lines <- lines %>%
    parse_lines()
  out <- parsed_lines  %>%
    add_attributes_detailed() %>%
    set_na_to_zero(na_to_zero) %>%
    nest_log() %>%
    select_(
      ~short_hash, ~author_name, ~date,
      ~short_message, ~everything()
    ) %>%
    arrange_(~date)

  class(out) <- append("commit_level_log", class(out))

  out
}


#' @importFrom purrr map_at
#' @importFrom dplyr as_data_frame
set_na_to_zero <- function(log,
                           na_to_zero = TRUE,
                           columns = c(
                             "edits", "insertions", "deletions",
                             "total_files_changed", "total_insertions",
                             "total_deletions"
                           )) {
  if (!na_to_zero) return(log)
  out <- log %>%
    map_at(columns, if_na_to_zero) %>%
    as_data_frame()
  out
}

if_na_to_zero <- function(vec) {
  ifelse(is.na(vec), 0, vec)
}
