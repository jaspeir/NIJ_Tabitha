library(tibble)

test_that("empty dataset", {
  examples = tribble(
    ~object, ~attribute, ~decision
  )

  it = InformationTable$new(examples)
  expect_equal(it$downwardClassUnion(1), vector())
})


test_that("a single class only", {
  examples = tribble(
    ~object, ~attribute, ~decision,
    1, NA, 1
  )

  it = InformationTable$new(examples)
  expect_equal(it$downwardClassUnion(1), c(1))
})

test_that("a single class only - v2", {
  examples = tribble(
    ~object, ~attribute, ~decision,
    1, NA, 1,
    2, NA, 1
  )

  it = InformationTable$new(examples)
  expect_equal(it$downwardClassUnion(1), c(1, 2))
})

test_that("multiple classes - v1", {
  examples = tribble(
    ~object, ~attribute, ~decision,
    1, NA, 1,
    2, NA, 2,
    3, NA, 3
  )

  it = InformationTable$new(examples)
  expect_equal(it$downwardClassUnion(3), c(1, 2, 3))
})

test_that("multiple classes - v1", {
  examples = tribble(
    ~object, ~attribute, ~decision,
    1, NA, 1,
    2, NA, 2,
    3, NA, 3
  )

  it = InformationTable$new(examples)
  expect_equal(it$downwardClassUnion(2), c(1, 2))
})


test_that("multiple classes - v3", {
  examples = tribble(
    ~object, ~attribute, ~decision,
    1, NA, 1,
    2, NA, 2,
    3, NA, 3
  )

  it = InformationTable$new(examples)
  expect_equal(it$downwardClassUnion(1), c(1))
})
