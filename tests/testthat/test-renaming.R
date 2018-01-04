context("renaming")

test_that("name change", {
  expect_true(is_name_change("man/{get_log_regex.Rd => git_log_detailed.Rd}"))
  expect_true(!is_name_change("inst/report_templates/repo_summary_simple.Rmd"))
})


test_that("parsing", {
  expect_known_output(
    parse_reassignment(rep("R/{a => b}", 2), c(1, 2)),
    testthat_file("reference-objects/parse-reassignment")
  )
})

test_that("dir and base assignment", {
  expect_known_output(
    separate_dir_and_reassignment("R/{gitsum.R => gitsum-package.R}"),
    testthat_file("reference-objects/separate-dir-and-assignment")
  )
  expect_known_output(
    combine_dir_and_base("R", list(c("a", "b")), 1),
    testthat_file("reference-objects/combine-dir-and-base")
  )
})



