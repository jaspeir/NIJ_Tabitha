test_that("smallest element dominates no others", {
  examples = tribble(
    ~object, ~a, ~b,
    "X1", 0, 0,
    "X2", 0, 1,
    "X3", 1, 0,
    "X4", 1, 1,
    "X5", 2, 2
  )

  expect_equal(dominatedSet("X1", c("a", "b"), examples), c("X1"))
})

test_that("largest element dominates all others", {
  examples = tribble(
    ~object, ~a, ~b,
    "X1", 0, 0,
    "X2", 0, 1,
    "X3", 1, 0,
    "X4", 1, 1,
    "X5", 2, 2
  )

  expect_equal(dominatedSet("X5", c("a", "b"), examples), c("X1", "X2", "X3", "X4", "X5"))
})

test_that("elements of diamond relation dominate same elements", {
  examples = tribble(
    ~object, ~a, ~b,
    "X1", 0, 0,
    "X2", 0, 1,
    "X3", 1, 0,
    "X4", 1, 1,
    "X5", 2, 2
  )

  expect_equal(dominatedSet("X2", c("a", "b"), examples), c("X1", "X2"))
  expect_equal(dominatedSet("X3", c("a", "b"), examples), c("X1", "X3"))
})

test_that("should fail for missing object", {
  examples = tribble(
    ~object, ~a,
    "X1", 1,
    "X2", 2
  )

  expect_error(dominatedSet("X0", c("a"), examples))
  expect_error(dominatedSet("X3", c("a"), examples))

})

