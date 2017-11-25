context("test backend")
test_that("repo can be created", {
  remove_gitsum()
  expect_error(init_gitsum(path = testthat_file(".")), NA)
  expect_true(dir.exists(".gitsum"))
})

test_that("dump can be read", {
  expect_error(read_log(), NA)
  expect_error(read_last_commit(), NA)
  expect_error(read_last_hash(), NA)
})

test_that("repo can be removed", {
  expect_error(remove_gitsum(), NA)
})

