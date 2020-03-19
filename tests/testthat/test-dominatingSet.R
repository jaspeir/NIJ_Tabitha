test_that("smallest element dominated by all others", {
  examples = tribble(
    ~object, ~a, ~b,
    "X1", 0, 0,
    "X2", 0, 1,
    "X3", 1, 0,
    "X4", 1, 1,
    "X5", 2, 2
  )
  examples$decision = NA

  it = InformationTable$new(examples)
  expect_equal(it$dominatingSet("X1", c("a", "b")), c("X1", "X2", "X3", "X4", "X5"))
})

test_that("largest element dominated by no others", {
  examples = tribble(
    ~object, ~a, ~b,
    "X1", 0, 0,
    "X2", 0, 1,
    "X3", 1, 0,
    "X4", 1, 1,
    "X5", 2, 2
  )
  examples$decision = NA

  it = InformationTable$new(examples)
  expect_equal(it$dominatingSet("X5", c("a", "b")), c("X5"))
})

test_that("elements of diamond relation dominated by same elements", {
  examples = tribble(
    ~object, ~a, ~b,
    "X1", 0, 0,
    "X2", 0, 1,
    "X3", 1, 0,
    "X4", 1, 1,
    "X5", 2, 2
  )
  examples$decision = NA

  it = InformationTable$new(examples)
  expect_equal(it$dominatingSet("X2", c("a", "b")), c("X2", "X4", "X5"))
  expect_equal(it$dominatingSet("X3", c("a", "b")), c("X3", "X4", "X5"))
})

test_that("should fail for missing object", {
  examples = tribble(
    ~object, ~a,
    "X1", 1,
    "X2", 2
  )
  examples$decision = NA

  it = InformationTable$new(examples)
  expect_error(it$dominatingSet("X0", c("a")))
  expect_error(it$dominatingSet("X3", c("a")))

})

