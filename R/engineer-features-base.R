#' Add base attributes to a log
#'
#' @param log An unnested log.
#' @details
#' Derives some attributes from the parsed raw log and converts them to the
#' desired format.
#' The following attributes merit special mentioning:
#'
#' * commit_nr: The number of the commit in the repo. This can be used to sort
#'   commits (in contrast to the commit date) in chronological order, that is,
#'   in the oder in which they were committed.
add_attributes_detailed <- function(log) {
  mutate_(log,
    date = ~ymd_hms(paste(year, month, monthday, time)),
    short_hash = ~substr(hash, 1, 4),
    short_message = ~substr(message, 1, 20),
    short_description = ~ifelse(!is.na(message),
                                substr(description, 1, 20), NA
    ),
    is_merge = ~ifelse(!is.na(left_parent) & !is.na(right_parent),
                       TRUE, FALSE
    )
  ) %>%
    add_is_exact()
}

#' @importFrom hms parse_hm
type_convert_base_attributes <- function(log) {
  log %>%
    mutate(
      monthday = as.integer(monthday),
      month = as.character(month),
      hash = as.character(hash),
      time = parse_hm(time),
      year = as.integer(year),
      total_files_changed = as.integer(total_files_changed),
      total_insertions = as.integer(total_insertions),
      total_deletions = as.integer(total_deletions),
      edits = ifelse(edits != "", nchar(edits), 0),
      insertions = ifelse(insertions != "", nchar(insertions), 0),
      deletions = ifelse(deletions != "", nchar(deletions), 0),
      changed_file = trimws(changed_file)
    )
}

#' Is the information exact?
#'
#' Create a column indicating whether insertions and deletions are exact counts.
#' @inheritParams add_attributes_detailed
add_is_exact <- function(log) {
  mutate_(log,
    total_approx = ~ insertions + deletions,
    multiplier = ~ edits / total_approx,
    insertions = ~ as.integer(round(multiplier * insertions)),
    deletions = ~ as.integer(round(multiplier * deletions)),
    is_exact = ~ if_else((is.na(edits) | multiplier == 1), TRUE, FALSE)
  )  %>%
    select_(~-multiplier, ~-total_approx)

}
