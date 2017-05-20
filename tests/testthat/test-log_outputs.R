context("check high-level function outputs")

test_that("simple log output is correct", {
  expect_is(get_log_simple(), "tbl_df")
  expect_named(get_log_simple(), c("author", "message_short", "date", "commit",
                                   "weekday", "month", "monthday", "time", "year",
                                   "message", "left_parent", "right_parent", "n_parents"))
})

test_that("advanced log output is correct", {
  expect_is(get_log_regex(), "tbl_df")
  expect_named(get_log_regex(), c("short_hash", "author_name", "date", "short_message", "short_description",
                                  "hash", "left_parent", "right_parent", "author_email", "weekday",
                                  "month", "monthday", "time", "timezone", "year", "total_files_changed",
                                  "total_insertions", "total_deletions", "message", "description", "nested"))
})
