context("renaming - helper")

raw <- c(
  trailing_extension = "{src => inst/include}/dplyr_types.h",
  bothsided_extension = "vignettes/{ => notes}/mysql-setup.Rmd",
  leading_extension = "man/{get_log_regex.Rd => git_log_detailed.Rd}",
  no_extension = "API => api"
) %>%
  unname()

raw_after_curly <- c(
  "{src => inst/include}/dplyr_types.h",
  "vignettes/{ => notes}/mysql-setup.Rmd",
  "man/{get_log_regex.Rd => git_log_detailed.Rd}",
  "{API => api}"
)

test_that("curly enclosing", {
  expect_equal(ensure_curly_enclosing(raw), raw_after_curly)
})

test_that("dash enclosing", {
  expect_equal(ensure_dash_enclosing(raw_after_curly), c(
    "/{src => inst/include}/dplyr_types.h",
    "vignettes/{ => notes}/mysql-setup.Rmd",
    "man/{get_log_regex.Rd => git_log_detailed.Rd}/",
    "/{API => api}/"
  ))
})

test_that("parse the full reassignment", {
  parsed_reassignment <- parse_reassignment(
    c("R/{a => b}", "API => api2", "{src => inst/include}/dplyr_types.h"),
    c(1, 2, 33)
  )

  expect_known_output(
    parsed_reassignment,
    testthat_file("reference-objects/parse-reassignment-complex")
  )
})

test_that("parse the full reassignment", {
  expect_known_output(
    parse_reassignment("vignettes/{ => internals}/hybrid-evaluation.Rmd", 23),
    testthat_file("reference-objects/parse-reassignment-complex2")
  )
})

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
    combine_dir_and_base("R", list(c("a", "b")), "", 1),
    testthat_file("reference-objects/combine-dir-and-base")
  )
})



