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
  e = ElementaryCondition$new(attribute = "Q3OD", value = 42, it = informationTable, isLowerBound = TRUE)
  c = ComplexCondition$new(e)
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

test_that("isWeaker - same non-dominance attribute, different values", {
  e1 = ElementaryCondition$new(attribute = "Q1", value = 42, it = informationTable, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "Q1", value = 0, it = informationTable, isLowerBound = NA)
  c1 = ComplexCondition$new(e1)
  c2 = ComplexCondition$new(e2)
  d1 = DecisionRule$new(condition = c1, t = 1, type = "STAT1")
  d2 = DecisionRule$new(condition = c2, t = 1, type = "STAT1")
  expect_false(d1$isWeaker(it = informationTable, rule = d2))
  expect_false(d2$isWeaker(it = informationTable, rule = d1))
})

test_that("isWeaker - same dominance attribute, different values", {
  e1 = ElementaryCondition$new(attribute = "Q3OD", value = 42, it = informationTable, isLowerBound = TRUE)
  e2 = ElementaryCondition$new(attribute = "Q3OD", value = 0, it = informationTable, isLowerBound = TRUE)
  c1 = ComplexCondition$new(e1)
  c2 = ComplexCondition$new(e2)
  d1 = DecisionRule$new(condition = c1, t = 1, type = "STAT1")
  d2 = DecisionRule$new(condition = c2, t = 1, type = "STAT1")
  expect_true(d1$isWeaker(it = informationTable, rule = d2))
  expect_false(d2$isWeaker(it = informationTable, rule = d1))
})

test_that("isWeaker - D>=<=-rules same non-dom LHS, weaker RHS", {
  e = ElementaryCondition$new(attribute = "Q1", value = 50, it = informationTable, isLowerBound = NA)
  c = ComplexCondition$new(e)
  DecisionRule$debug('isWeaker')
  d1 = DecisionRule$new(condition = c, t = 4:5, type = "STAT3")
  d2 = DecisionRule$new(condition = c, t = 3:6, type = "STAT3")
  expect_false(d1$isWeaker(it = informationTable, rule = d2))
  expect_false(d2$isWeaker(it = informationTable, rule = d1))
})

test_that("isWeaker - D>=<=-rules same dom-only LHS, weaker RHS", {
  e1 = ElementaryCondition$new(attribute = "Q3OD", value = 50, it = informationTable, isLowerBound = TRUE)
  e2 = ElementaryCondition$new(attribute = "Q3OD", value = 70, it = informationTable, isLowerBound = FALSE)
  c = ComplexCondition$new(c(e1, e2))
  DecisionRule$debug('isWeaker')
  d1 = DecisionRule$new(condition = c, t = 4:5, type = "STAT3")
  d2 = DecisionRule$new(condition = c, t = 3:6, type = "STAT3")
  expect_false(d1$isWeaker(it = informationTable, rule = d2))
  expect_true(d2$isWeaker(it = informationTable, rule = d1))
})


test_that("isMinimal - empty rules", {
  c = ComplexCondition$new()
  d = DecisionRule$new(condition = c, t = 0, type = "STAT1")
  expect_true(d$isMinimal(it = informationTable, rule = c()))
})
