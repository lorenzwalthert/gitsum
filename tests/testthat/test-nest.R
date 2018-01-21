context("nesting")

parsed_log <- parse_test_log_detailed("test_logs/testthat_log_detailed.txt")

test_that("can unnest", {
  expect_known_output(unnest_log(parsed_log), testthat_file("reference-objects/unnest"))
  expect_warning(unnest_log(unnest_log(parsed_log)))
})

test_that("can nest", {
  expect_known_output(
    nest_log(unnest_log(parsed_log)), testthat_file("reference-objects/nest")
  )
  expect_warning(nest_log(nest_log(unnest_log(parsed_log))))
})
