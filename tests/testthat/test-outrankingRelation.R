library(tibble)

test_that("outranks equal", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))

  expect_true(outranks(r1, r2, "q"))
})

test_that("outranks larger", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (0))

  expect_true(outranks(r1, r2, "q"))
})

test_that("does not outrank smaller", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))

  expect_false(outranks(r1, r2, "q"))
})

test_that("outranks equal - second attribute", {
  r1 = tibble(a = (0), b = (1))
  r2 = tibble(a = (0), b = (1))

  expect_true(outranks(r1, r2, "b"))
})

test_that("does not outrank - second attribute", {
  r1 = tibble(a = (0), b = (1))
  r2 = tibble(a = (0), b = (2))

  expect_false(outranks(r1, r2, "b"))
})



test_that("should fail for empty operands", {
  r1 = data.frame(q = NULL)
  r2 = tibble(q = (1))

  expect_error(outranks(r1, r2, "q"))
  expect_error(outranks(r2, r1, "q"))
  expect_error(outranks(r1, r1, "q"))
})

test_that("should fail for missing criterion", {
  r1 = tibble(q = (1))
  r2 = tibble(r = (1))

  expect_error(outranks(r1, r2, "q"))
  expect_error(outranks(r1, r2, "r"))
  expect_error(outranks(r1, r2, "s"))
})

test_that("should fail for unequal rows", {
  r1 = tibble(q = c(1,2))
  r2 = tibble(q = (1))

  expect_error(outranks(r1, r2, "q"))
  expect_error(outranks(r2, r1, "q"))
})

test_that("vectorized execution - outranks equal", {
  r1 = tibble(q = c(1, 2))
  r2 = tibble(q = c(1, 2))

  expect_equal(outranks(r1, r2, "q"), c(TRUE, TRUE))
})

test_that("vectorized execution - does not outrank", {
  r1 = tibble(q = c(1, 2))
  r2 = tibble(q = c(2, 3))

  expect_equal(outranks(r1, r2, "q"), c(FALSE, FALSE))
})
