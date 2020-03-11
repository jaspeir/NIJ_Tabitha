library(tibble)

test_that("dominates single", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))

  expect_true(dominates(r1, r2, c("q")))
})

test_that("does not dominate single", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))

  expect_false(dominates(r1, r2, c("q")))
})

test_that("dominates two attributes", {
  r1 = tibble(q = (1), r = (0))
  r2 = tibble(q = (1), r = (0))

  expect_true(dominates(r1, r2, c("q", "r")))
})

test_that("does not dominate two attributes", {
  r1 = tibble(q = (1), r = (0))
  r2 = tibble(q = (0), r = (1))

  expect_false(dominates(r1, r2, c("q", "r")))
  expect_false(dominates(r2, r1, c("q", "r")))
})

test_that("should fail - attribute not in examples", {
  r1 = tibble(q = (1), r = (0))
  r2 = tibble(q = (0), r = (1))

  expect_error(dominates(r1, r2, c("a")))
  expect_error(dominates(r2, r1, c("q", "r", "s")))
})

test_that("vectorized execution - dominates", {
  r1 = tibble(q = c(11, 22, 33), r = c(11, 22, 33))
  r2 = tibble(q = c(1, 2, 3), r = c(1, 2, 3))

  expect_equal(dominates(r1, r2, c("q", "r")), c(TRUE, TRUE, TRUE))
})

test_that("vectorized execution - permuting elements of criteria set does not change results", {
  r1 = tibble(q = c(1, 2), r = c(1, 2))
  r2 = tibble(q = c(2, 1), r = c(2, 1))

  expect_equal(dominates(r1, r2, c("q", "r")), c(FALSE, TRUE))
  expect_equal(dominates(r1, r2, c("r", "q")), c(FALSE, TRUE))
})

