library(tibble)
library(purrr)

################################
### Outranking-attibtes only ###
################################

setAllAttributesToDominance = function(t) {
  walk(names(t), function(field) {
    attributes(t[field])$type = 'dominance'
    })
}

test_that("dominates single", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  attributes(r1$q)$type = 'dominance'
  attributes(r2$q)$type = 'dominance'

  expect_true(dominates(r1, r2, c("q")))
})

test_that("does not dominate single", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))
  attributes(r1$q)$type = 'dominance'
  attributes(r2$q)$type = 'dominance'

  expect_false(dominates(r1, r2, c("q")))
})

test_that("dominates two attributes", {
  r1 = tibble(q = (1), r = (0))
  r2 = tibble(q = (1), r = (0))
  attributes(r1$q)$type = 'dominance'
  attributes(r2$q)$type = 'dominance'
  attributes(r1$r)$type = 'dominance'
  attributes(r2$r)$type = 'dominance'

  expect_true(dominates(r1, r2, c("q", "r")))
})

test_that("does not dominate two attributes", {
  r1 = tibble(q = (1), r = (0))
  r2 = tibble(q = (0), r = (1))
  attributes(r1$q)$type = 'dominance'
  attributes(r2$q)$type = 'dominance'
  attributes(r1$r)$type = 'dominance'
  attributes(r2$r)$type = 'dominance'

  expect_false(dominates(r1, r2, c("q", "r")))
  expect_false(dominates(r2, r1, c("q", "r")))
})

test_that("should fail - attribute not in examples", {
  r1 = tibble(q = (1), r = (0))
  r2 = tibble(q = (0), r = (1))
  attributes(r1$q)$type = 'dominance'
  attributes(r2$q)$type = 'dominance'
  attributes(r1$r)$type = 'dominance'
  attributes(r2$r)$type = 'dominance'

  expect_error(dominates(r1, r2, c("a")))
  expect_error(dominates(r2, r1, c("q", "r", "s")))
})

test_that("vectorized execution - dominates", {
  r1 = tibble(q = c(11, 22, 33), r = c(11, 22, 33))
  r2 = tibble(q = c(1, 2, 3), r = c(1, 2, 3))
  attributes(r1$q)$type = 'dominance'
  attributes(r2$q)$type = 'dominance'
  attributes(r1$r)$type = 'dominance'
  attributes(r2$r)$type = 'dominance'

  expect_equal(dominates(r1, r2, c("q", "r")), c(TRUE, TRUE, TRUE))
})

test_that("vectorized execution - permuting elements of criteria set does not change results", {
  r1 = tibble(q = c(1, 2), r = c(1, 2))
  r2 = tibble(q = c(2, 1), r = c(2, 1))
  attributes(r1$q)$type = 'dominance'
  attributes(r2$q)$type = 'dominance'
  attributes(r1$r)$type = 'dominance'
  attributes(r2$r)$type = 'dominance'

  expect_equal(dominates(r1, r2, c("q", "r")), c(FALSE, TRUE))
  expect_equal(dominates(r1, r2, c("r", "q")), c(FALSE, TRUE))
})

