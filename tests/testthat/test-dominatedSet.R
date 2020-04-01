test_that("smallest element dominates no others", {
  examples = tribble(
    ~object, ~a, ~b,
    "X1", 0, 0,
    "X2", 0, 1,
    "X3", 1, 0,
    "X4", 1, 1,
    "X5", 2, 2
  )
  examples$decision = 1:nrow(examples)

  it = InformationTable$new(examples)
  expect_equal(it$dominatedSet("X1", c("a", "b")), c("X1"))
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
  examples$decision = 1:nrow(examples)

  it = InformationTable$new(examples)
  expect_equal(it$dominatedSet("X5", c("a", "b")), c("X1", "X2", "X3", "X4", "X5"))
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
  examples$decision = 1:nrow(examples)

  it = InformationTable$new(examples)
  expect_equal(it$dominatedSet("X2", c("a", "b")), c("X1", "X2"))
  expect_equal(it$dominatedSet("X3", c("a", "b")), c("X1", "X3"))
})

test_that("should fail for missing object", {
  examples = tribble(
    ~object, ~a,
    "X1", 1,
    "X2", 2
  )
  examples$decision = 1:nrow(examples)

  it = InformationTable$new(examples)
  expect_error(it$dominatedSet("X0", c("a")))
  expect_error(it$dominatedSet("X3", c("a")))

})

