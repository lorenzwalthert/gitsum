context("test backend")
test_that("repo can be created", {
  skip_on_os("windows")
  remove_gitsum()
  expect_error(init_gitsum("."), NA)
  expect_true(dir.exists(".gitsum"))
})

test_that("dump can be read", {
  skip_on_os("windows")
  expect_error(read_log(), NA)
  expect_error(read_last_commit(), NA)
  expect_error(read_last_hash(), NA)
})

test_that("repo can be removed", {
  skip_on_os("windows")
  expect_error(remove_gitsum(), NA)
})
