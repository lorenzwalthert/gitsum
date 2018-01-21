context("sample logs and names")

log_simp <- testthat_file("test_logs/log_out_simp.rds")
log_det <- testthat_file("test_logs/log_out_det.rds")

git_det <- parse_test_log_detailed("test_logs/testthat_log_detailed.txt")
git_simp <- parse_test_log_simple("test_logs/testthat_log_simple.txt")

test_that("simple log output is correct", {
  expect_is(git_simp, "tbl_df")
  expect_named(git_simp, c(
    "author", "message_short", "date", "commit",
    "weekday", "month", "monthday", "time", "year",
    "message", "left_parent", "right_parent", "n_parents"
  ))

  expect_equal_to_reference(git_simp, log_simp)
})

test_that("advanced log output is correct", {
  expect_is(git_det, "tbl_df")
  expect_named(git_det, c(
    "short_hash", "author_name", "date",
    "short_message", "commit_nr", "hash", "left_parent",
    "right_parent", "author_email", "weekday",
    "month", "monthday", "time", "year",
    "timezone", "message", "description",
    "total_files_changed", "total_insertions",
    "total_deletions", "short_description",
    "is_merge", "nested"
  ))
  expect_equal_to_reference(unnest_(git_det, ~nested), log_det)
})
