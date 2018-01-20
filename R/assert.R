is_detailed_log <- function(log, nested = NA) {
  expected_names_nested <- c(
    "short_hash", "author_name", "date", "short_message", "hash",
    "left_parent", "right_parent", "author_email", "weekday", "month",
    "monthday", "time", "year", "timezone", "message", "description",
    "total_files_changed", "total_insertions", "total_deletions",
    "commit_id", "short_description", "is_merge", "nested"
  )
  expected_names_unnested <- c(
    setdiff(expected_names_nested, "nested"),
    c("changed_file", "edits", "insertions", "deletions", "is_exact")
  )

  is_nested_log <- expect_names(log, expected_names_nested)
  is_unnested_log <- expect_names(log, expected_names_unnested)
  if (is.na(nested)) {
    passes <- is_nested_log || is_unnested_log
  } else if (nested == TRUE) {
    passes <- is_nested_log
  } else {
    passes <- is_unnested_log
  }
  passes
}

#' @importFrom purrr when
assert_detailed_log <- function(log, nested = NA) {
  msg <- when(nested,
    is.na(.) ~ "a",
    . ~ "a nested",
    !. ~ "an unnested"
  )
  if (!is_detailed_log(log, nested)) {
    stop(paste("log is not", msg, "detailed log"), call. = FALSE)
  }
}

expect_names <- function(log, expected_names) {
  setequal(names(log), expected_names)
}
