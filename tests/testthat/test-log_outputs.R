context("check high-level function outputs")

#   ____________________________________________________________________________
#   parameter                                                               ####

start <- ifelse(interactive(), "tests/testthat/", "")
path_det <- paste0(start, "test_logs/testthat_log_detailed.txt")
path_simp <- paste0(start, "test_logs/testthat_log_simple.txt")
log_simp <- paste0(start, "test_logs/log_out_simp.rds")
log_det <- paste0(start, "test_logs/log_out_det.rds")

git_det <- git_log_detailed(path = ".", file_name = path_det)
git_simp <- git_log_simple(path = ".", file_name = path_simp)

#   ____________________________________________________________________________
#   create reference files                                                  ####

# readr::write_rds(unnest_(git_det, ~nested), "tests/testthat/test_logs/log_out_det.rds")
# readr::write_rds(git_simp, "tests/testthat/test_logs/log_out_simp.rds")

#   ____________________________________________________________________________
#   actual tests                                                            ####

test_that("simple log output is correct", {
  expect_is(git_simp, "tbl_df")
  expect_named(git_simp, c("author", "message_short", "date", "commit",
                          "weekday", "month", "monthday", "time", "year",
                          "message", "left_parent", "right_parent", "n_parents"))

  expect_equal_to_reference(git_simp, log_simp)
})

test_that("advanced log output is correct", {
  expect_is(git_det, "tbl_df")
  expect_named(git_det, c("short_hash", "author_name", "date",
                          "short_message", "hash", "left_parent",
                          "right_parent", "author_email", "weekday",
                          "month", "monthday", "time", "year",
                          "timezone", "message", "description",
                          "total_files_changed", "total_insertions",
                          "total_deletions", "short_description",
                          "is_merge", "nested"))
  expect_equal_to_reference(unnest_(git_det, ~nested), log_det)
})

library("ggplot2")
test_that("repo logs can be created", {
  expect_error(git_report(output_format = "all", input_file = path_det), NA)
  unlink(paste0(start, "gitsum"), recursive = TRUE)
})
