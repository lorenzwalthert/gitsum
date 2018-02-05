context("test-type-stability.R")

detailed_log <- parse_test_log_detailed("test_logs/testthat_log_detailed.txt")

test_that("detailed log returns correct classes", {
  mapping <- tibble::tribble(
    ~name, ~class,
    "short_hash",   "character",
    "author_name",   "character",
    "date",   "POSIXct",
    "short_message", "character",
    "commit_nr", "integer",
    "left_parent", "character",
    "right_parent", "character",
    "author_email", "character",
    "weekday", "character",
    "month", "character",
    "hash", "character",
    "monthday", "integer",
    "time", "hms",
    "year", "integer",
    "timezone", "character",
    "message", "character",
    "description", "character",
    "total_files_changed", "integer",
    "total_insertions", "integer",
    "total_deletions", "integer",
    "short_description", "character",
    "is_merge", "logical",
    "changed_file", "character",
    "edits", "integer",
    "insertions", "integer",
    "deletions", "integer",
    "is_exact", "logical"
  )
  expect_silent(expect_class(
    unnest_log(detailed_log), mapping, must_check_all = TRUE
  ))
})
