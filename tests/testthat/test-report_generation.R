library("ggplot2")
path_det <- testthat_file("test_logs/testthat_log_detailed.txt")
test_that("repo reports can be created", {
  skip_on_os("windows")
  if (!is_gitsum_repo()) init_gitsum()
  expect_error(git_report(
    path = "/",
    cached = FALSE,
    input_file = path_det,
    output_format = "all"), NA)
})
