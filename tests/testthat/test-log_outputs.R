context("check high-level function outputs")

test_that("simple log output is correct", {
  expect_is(git_log_simple(path = ".", file_name = "tests/testthat/test_logs/testthat_log_simple.txt"), "tbl_df")
  expect_named(git_log_simple(), c("author", "message_short", "date", "commit",
                                   "weekday", "month", "monthday", "time", "year",
                                   "message", "left_parent", "right_parent", "n_parents"))
})

test_that("advanced log output is correct", {
  expect_is(git_log_detailed(path = ".", file_name = "tests/testthat/test_logs/testthat_log_detailed.txt"), "tbl_df")
  expect_named(git_log_detailed(), c("short_hash", "author_name", "date",
                                     "short_message", "hash", "left_parent",
                                     "right_parent", "author_email", "weekday",
                                     "month", "monthday", "time", "year",
                                     "timezone", "message", "description",
                                     "total_files_changed", "total_insertions",
                                     "total_deletions", "short_description",
                                     "is_merge", "nested"))
})
