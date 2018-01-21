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
    deletions = ~ifelse(deletions != "", nchar(deletions), 0),
    insertions = ~ifelse(insertions != "", nchar(insertions), 0),
    is_merge = ~ifelse(!is.na(left_parent) & !is.na(right_parent),
                       TRUE, FALSE
    )
  ) %>%
    type_convert_attributes_detailed() %>%
    add_is_exact()
}


#' @importFrom readr type_convert cols col_integer col_time col_character
type_convert_attributes_detailed <- function(log) {
  type_convert(log, col_types = cols(
      monthday = col_integer(),
      time = col_time(),
      timezone = col_integer(),
      year = col_integer(),
      total_files_changed = col_integer(),
      total_insertions = col_integer(),
      total_deletions = col_integer(),
      edits = col_integer(),
      short_hash = col_character()
    ))
}


#' Is the information exact?
#'
#' Create a column indicating whether insertions and deletions are exact counts.
#' @inheritParams add_attributes_detailed
add_is_exact <- function(log) {
  mutate_(log,
    total_approx = ~ insertions + deletions,
    multiplier = ~ edits / total_approx,
    insertions = ~ round(multiplier * insertions),
    deletions = ~ round(multiplier * deletions),
    is_exact = ~ if_else((is.na(edits) | multiplier == 1), TRUE, FALSE)
  )  %>%
    select_(~-multiplier, ~-total_approx)

}
