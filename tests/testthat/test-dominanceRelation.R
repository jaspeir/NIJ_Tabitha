library(tibble)

test_that("dominates single", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))

  expect_true(dominates(r1, r2, c("q")))
})

test_that("does not dominate single", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))

  expect_false(outranks(r1, r2, c("q")))
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
