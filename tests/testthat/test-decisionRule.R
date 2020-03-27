test_that("create should succeed", {
  c = ComplexCondition$new()
  d = DecisionRule$new(condition = c, t = 0, type = "upward")
  expect_true(TRUE)
})

test_that("isWeaker - different types", {
  c = ComplexCondition$new()
  d1 = DecisionRule$new(condition = c, t = 0, type = "upward")
  d2 = DecisionRule$new(condition = c, t = 0, type = "downward")
  expect_false(d1$isWeaker(it = informationTable, rule = d2))
  expect_false(d2$isWeaker(it = informationTable, rule = d1))
})

test_that("isWeaker - only t differs", {
  c = ComplexCondition$new()
  d1 = DecisionRule$new(condition = c, t = 0, type = "upward")
  d2 = DecisionRule$new(condition = c, t = 1, type = "upward")
  expect_true(d1$isWeaker(it = informationTable, rule = d2))
  expect_false(d2$isWeaker(it = informationTable, rule = d1))
})

test_that("isMinimal - empty rules", {
  c = ComplexCondition$new()
  d = DecisionRule$new(condition = c, t = 0, type = "upward")
  expect_true(d$isMinimal(it = informationTable, rule = c()))
})
