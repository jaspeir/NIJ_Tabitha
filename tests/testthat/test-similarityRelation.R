library(tibble)


test_that("should fail - missing both", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))

  expect_error(similar(r1, r2, c("q")))
})

test_that("should fail - missing alpha", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  attributes(r2$q)$beta = 0.0

  expect_error(similar(r1, r2, c("q")))
})

test_that("should fail - missing beta", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  attributes(r2$q)$alpha = 0.0

  expect_error(similar(r1, r2, c("q")))
})

test_that("should fail - attribute not in examples", {
  r1 = tibble(q = (1), r = (0))
  r2 = tibble(q = (0), r = (1))

  expect_error(dominates(r1, r2, c("a")))
  expect_error(dominates(r2, r1, c("q", "r", "s")))
})


# New:
test_that("similar single diff 0 - v1", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  attributes(r2$q)$alpha = 0.0
  attributes(r2$q)$beta = 0.0

  expect_true(similar(r1, r2, c("q")))
})

test_that("similar single diff 0 - v2", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  attributes(r2$q)$alpha = 1.0
  attributes(r2$q)$beta = 0.0

  expect_true(similar(r1, r2, c("q")))
})

test_that("similar single diff 0 - v3", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  attributes(r2$q)$alpha = 0.0
  attributes(r2$q)$beta = 1.0

  expect_true(similar(r1, r2, c("q")))
})

test_that("similar single diff 0 - v4", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  attributes(r2$q)$alpha = 1.0
  attributes(r2$q)$beta = 1.0

  expect_true(similar(r1, r2, c("q")))
})

test_that("similar single diff nonzero - v1", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))
  attributes(r2$q)$alpha = 0.0
  attributes(r2$q)$beta = 0.0

  expect_false(similar(r1, r2, c("q")))
})

test_that("similar single diff nonzero - v2", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))
  attributes(r2$q)$alpha = 1.0
  attributes(r2$q)$beta = 0.0

  expect_true(similar(r1, r2, c("q")))
})

test_that("similar single diff nonzero - v3", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))
  attributes(r2$q)$alpha = 0.0
  attributes(r2$q)$beta = 1.0

  expect_true(similar(r1, r2, c("q")))
})

test_that("similar single diff nonzero - v4", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))
  attributes(r2$q)$alpha = 1.0
  attributes(r2$q)$beta = 1.0

  expect_true(similar(r1, r2, c("q")))
})
