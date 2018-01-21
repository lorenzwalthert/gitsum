context("test-assert-log.R")

parsed_log <- parse_test_log_detailed("test_logs/testthat_log_detailed.txt")

test_that("Can assert log", {
  expect_silent(assert_detailed_log(parsed_log))
  expect_silent(assert_detailed_log(parsed_log, nested = TRUE))
  expect_error(assert_detailed_log(parsed_log, nested = FALSE))

  expect_error(assert_detailed_log(mtcars))
  expect_error(assert_detailed_log(unnest_log(parsed_log), nested = TRUE))
  expect_silent(assert_detailed_log(unnest_log(parsed_log), nested = FALSE))
})
