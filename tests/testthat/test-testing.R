context("test-testing.R")

test_that("expect_class() works", {
  class_mapping <- tibble::tribble(
    ~ name,  ~ class,
    "cyl", "integer",
    "model", "character"
  )
  tbl <- tibble::rownames_to_column(mtcars, var= "model")

  expect_warning(
    expect_class(tbl, class_mapping, must_check_all = FALSE),
    "class for cyl was numeric, but integer was expected"
  )
})
