library(tibble)

test_that("empty dataset", {
  examples = tribble(
    ~object, ~decision
  )

  expect_equal(downwardClassUnion(examples, 1), vector())
})


test_that("a single class only", {
  examples = tribble(
    ~object, ~decision,
    1, 1
  )

  expect_equal(downwardClassUnion(examples, 1), c(1))
})

test_that("a single class only - v2", {
  examples = tribble(
    ~object, ~decision,
    1, 1,
    2, 1
  )

  expect_equal(downwardClassUnion(examples, 1), c(1, 2))
})

test_that("multiple classes - v1", {
  examples = tribble(
    ~object, ~decision,
    1, 1,
    2, 2,
    3, 3
  )

  expect_equal(downwardClassUnion(examples, 3), c(1, 2, 3))
})

test_that("multiple classes - v1", {
  examples = tribble(
    ~object, ~decision,
    1, 1,
    2, 2,
    3, 3
  )

  expect_equal(downwardClassUnion(examples, 2), c(1, 2))
})


test_that("multiple classes - v3", {
  examples = tribble(
    ~object, ~decision,
    1, 1,
    2, 2,
    3, 3
  )

  expect_equal(downwardClassUnion(examples, 1), c(1))
})
