test_that("create should succeed", {
  c = ComplexCondition$new()
  d = DecisionRule$new(condition = c, t = 0, type = "STAT1")
  expect_true(TRUE)
})

test_that("isWeaker - different types", {
  c = ComplexCondition$new()
  d1 = DecisionRule$new(condition = c, t = 0, type = "STAT1")
  d2 = DecisionRule$new(condition = c, t = 0, type = "STAT2")
  expect_false(d1$isWeaker(it = informationTable, rule = d2))
  expect_false(d2$isWeaker(it = informationTable, rule = d1))
})

test_that("isWeaker - only t differs", {
  c = ComplexCondition$new()
  d1 = DecisionRule$new(condition = c, t = 0, type = "STAT1")
  d2 = DecisionRule$new(condition = c, t = 1, type = "STAT1")
  expect_true(d1$isWeaker(it = informationTable, rule = d2))
  expect_false(d2$isWeaker(it = informationTable, rule = d1))
})

test_that("isWeaker - different attributes", {
  e1 = ElementaryCondition$new(attribute = "Q1", value = 42, it = informationTable, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "Q2", value = 421, it = informationTable, isLowerBound = NA)
  c1 = ComplexCondition$new(e1)
  c2 = ComplexCondition$new(e2)
  d1 = DecisionRule$new(condition = c1, t = 0, type = "STAT1")
  d2 = DecisionRule$new(condition = c2, t = 1, type = "STAT1")
  expect_false(d1$isWeaker(it = informationTable, rule = d2))
  expect_false(d2$isWeaker(it = informationTable, rule = d1))
})

test_that("isWeaker - same attribute, different values", {
  e1 = ElementaryCondition$new(attribute = "Q1", value = 42, it = informationTable, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "Q1", value = 0, it = informationTable, isLowerBound = NA)
  c1 = ComplexCondition$new(e1)
  c2 = ComplexCondition$new(e2)
  d1 = DecisionRule$new(condition = c1, t = 1, type = "STAT1")
  d2 = DecisionRule$new(condition = c2, t = 1, type = "STAT1")
  expect_true(d1$isWeaker(it = informationTable, rule = d2))
  expect_false(d2$isWeaker(it = informationTable, rule = d1))
})

test_that("isMinimal - empty rules", {
  c = ComplexCondition$new()
  d = DecisionRule$new(condition = c, t = 0, type = "STAT1")
  expect_true(d$isMinimal(it = informationTable, rule = c()))
})
