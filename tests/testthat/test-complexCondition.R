context("ComplexCondition")

test_that("create empty should succeed", {
  c = ComplexCondition$new()
  expect_true(TRUE)
})

test_that("create single should succeed", {
  e = ElementaryCondition$new(attribute = "Q1", value = 3, it = informationTable, isLowerBound = F)
  c = ComplexCondition$new(e)
  expect_true(TRUE)
})

test_that("create single-element vector should succeed", {
  e = ElementaryCondition$new(attribute = "Q1", value = 3, it = informationTable, isLowerBound = F)
  c = ComplexCondition$new(c(e))
  expect_true(TRUE)
})

test_that("full cover - empty cond", {
  c = ComplexCondition$new()
  expect_equal(c$complexCover(informationTable), informationTable$objects)
})

test_that("full cover - element with full cover", {
  e = ElementaryCondition$new(attribute = "Q2", value = 22, it = informationTable, isLowerBound = F)
  c = ComplexCondition$new(e)
  expect_equal(c$complexCover(informationTable), informationTable$objects)
})

test_that("empty cover - element with empty cover", {
  e = ElementaryCondition$new(attribute = "QF2", value = 0, it = informationTable, isLowerBound = F)
  c = ComplexCondition$new(e)
  expect_equal(c$complexCover(informationTable), character())
})

test_that("empty cover - two elements: one with empty cover and another with full cover", {
  e1 = ElementaryCondition$new(attribute = "QF2", value = 0, it = informationTable, isLowerBound = F)
  e2 = ElementaryCondition$new(attribute = "Q2", value = 22, it = informationTable, isLowerBound = F)
  c = ComplexCondition$new(c(e1, e2))
  expect_equal(c$complexCover(informationTable), character())
})

test_that("conjuction of two elements", {
  e1 = ElementaryCondition$new(attribute = "QF3ST", value = 9, it = informationTable, isLowerBound = F)
  e2 = ElementaryCondition$new(attribute = "Q3PSO", value = 2, it = informationTable, isLowerBound = F)
  c = ComplexCondition$new(c(e1, e2))
  expect_equal(c$complexCover(informationTable), c("X23"))
})

test_that("first metric - empty G", {
  c = ComplexCondition$new()
  expect_equal(c$firstMetric(G = c(), it = informationTable), 0)
})

test_that("first metric - single element in G", {
  c = ComplexCondition$new()
  expect_equal(c$firstMetric(G = c("X1"), it = informationTable), 1/length(informationTable$objects))
})

test_that("first metric - G outside cover", {
  c = ComplexCondition$new()
  expect_equal(c$firstMetric(G = c("XOR"), it = informationTable), 0)
})

test_that("second metric - empty G", {
  c = ComplexCondition$new()
  expect_equal(c$secondMetric(G = c(), it = informationTable), 0)
})

test_that("second metric - single element in G", {
  c = ComplexCondition$new()
  expect_equal(c$secondMetric(G = c("X1"), it = informationTable), 1)
})

test_that("second metric - G outside cover", {
  c = ComplexCondition$new()
  expect_equal(c$secondMetric(G = c("XOR"), it = informationTable), 0)
})

test_that("getConstants - empty rule", {
  c = ComplexCondition$new()
  constants = rep(NA, nrow(informationTable$metaData))
  expect_equal(c$getConstants(informationTable), constants)
})

test_that("getConstants - single rule", {
  e = ElementaryCondition$new(attribute = "Q1", value = 42, it = informationTable, isLowerBound = F)
  c = ComplexCondition$new(e)
  constants = rep(NA_real_, nrow(informationTable$metaData))
  constants[5] = "42"
  expect_equal(c$getConstants(informationTable), constants)
})

test_that("getConstants - two rules", {
  e1 = ElementaryCondition$new(attribute = "Q1", value = 42, it = informationTable, isLowerBound = F)
  e2 = ElementaryCondition$new(attribute = "Q2", value = 421, it = informationTable, isLowerBound = F)

  c = ComplexCondition$new(c(e1, e2))
  constants = rep(NA_real_, nrow(informationTable$metaData))
  constants[5] = "42"
  constants[6] = "421"
  expect_equal(c$getConstants(informationTable), constants)
})

test_that("findBestElementary - empty best", {
  e1 = ElementaryCondition$new(attribute = "Q1", value = 42, it = informationTable, isLowerBound = F)
  e2 = ElementaryCondition$new(attribute = "Q2", value = 421, it = informationTable, isLowerBound = F)

  c = ComplexCondition$new(e1)
  newBest = c$findBestElementary(G = character(0), it = informationTable, check = e2, best = NULL)

  expect_true(e2$equals(newBest))
})

test_that("findBestElementary - same attribute, different values", {
  e1 = ElementaryCondition$new(attribute = "QF3ST", value = 8, it = informationTable, isLowerBound = F)
  e2 = ElementaryCondition$new(attribute = "QF3ST", value = 9, it = informationTable, isLowerBound = F)

  c = ComplexCondition$new()

  newBest = NULL
  newBest = c$findBestElementary(G = informationTable$objects, it = informationTable, check = e1, best = newBest)
  newBest = c$findBestElementary(G = informationTable$objects, it = informationTable, check = e2, best = newBest)

  expect_true(e1$equals(newBest))

  newBest = NULL
  newBest = c$findBestElementary(G = informationTable$objects, it = informationTable, check = e2, best = newBest)
  newBest = c$findBestElementary(G = informationTable$objects, it = informationTable, check = e1, best = newBest)

  expect_true(e1$equals(newBest))
})

test_that("findBestElementary - same attribute, different values, reduced objects set", {
  e1 = ElementaryCondition$new(attribute = "QF3ST", value = 8, it = informationTable, isLowerBound = F)
  e2 = ElementaryCondition$new(attribute = "QF3ST", value = 9, it = informationTable, isLowerBound = F)

  c = ComplexCondition$new()
  G = c("X12", "X15", "X23", "X24")

  newBest = NULL
  newBest = c$findBestElementary(G = G, it = informationTable, check = e1, best = newBest)
  newBest = c$findBestElementary(G = G, it = informationTable, check = e2, best = newBest)

  expect_true(e2$equals(newBest))

  newBest = NULL
  newBest = c$findBestElementary(G = G, it = informationTable, check = e2, best = newBest)
  newBest = c$findBestElementary(G = G, it = informationTable, check = e1, best = newBest)

  expect_true(e2$equals(newBest))
})

test_that("findBestElementary - same attribute, different values, one part already covered", {
  e = ElementaryCondition$new(attribute = "QF3ST", value = 9, it = informationTable, isLowerBound = F)

  e1 = ElementaryCondition$new(attribute = "Q3PSO", value = 5, it = informationTable, isLowerBound = F)
  e2 = ElementaryCondition$new(attribute = "Q3PSO", value = 2, it = informationTable, isLowerBound = F)

  c = ComplexCondition$new(e)
  G = informationTable$objects

  newBest = NULL
  newBest = c$findBestElementary(G = G, it = informationTable, check = e1, best = newBest)
  newBest = c$findBestElementary(G = G, it = informationTable, check = e2, best = newBest)

  expect_true(e1$equals(newBest))

  newBest = NULL
  newBest = c$findBestElementary(G = G, it = informationTable, check = e2, best = newBest)
  newBest = c$findBestElementary(G = G, it = informationTable, check = e1, best = newBest)

  expect_true(e1$equals(newBest))
})

test_that("reduceConditions - empty condition", {
  c = ComplexCondition$new()
  B = informationTable$objects

  expect_true(c$equals(c$reduceConditions(B = B, it = informationTable)))
})

test_that("reduceConditions - a single condition", {
  e = ElementaryCondition$new(attribute = "QF3ST", value = 9, it = informationTable, isLowerBound = F)

  c = ComplexCondition$new(e)
  B = informationTable$objects

  reduced = c$reduceConditions(B = B, it = informationTable)
  expect_true(c$equals(reduced))
})

test_that("reduceConditions - two conditions, first covered", {
  e1 = ElementaryCondition$new(attribute = "QF3ST", value = 9, it = informationTable, isLowerBound = F)
  e2 = ElementaryCondition$new(attribute = "QF2", value = 9, it = informationTable, isLowerBound = F)

  c = ComplexCondition$new(c(e1, e2))
  B = c("X12", "X15", "X23", "X24")

  reduced = c$reduceConditions(B = B, it = informationTable)
  expectedReduced = ComplexCondition$new(c(e1))

  expect_true(expectedReduced$equals(reduced))

  c = ComplexCondition$new(c(e2, e1))
  reduced = c$reduceConditions(B = B, it = informationTable)
  expect_true(expectedReduced$equals(reduced))
})

test_that("getConstantsGrouped - empty", {
  c = ComplexCondition$new()
  constants = c$getConstantsGrouped(it = informationTable)
  expect_true(all(map_lgl(constants, ~ all(is.na(.)))))
})

test_that("getConstantsGrouped - only misc filters", {
  e1 = ElementaryCondition$new(attribute = "QF2", value = 8, it = informationTable, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "QF4", value = 31, it = informationTable, isLowerBound = NA)
  c = ComplexCondition$new(c(e1, e2))
  constants = c$getConstantsGrouped(it = informationTable)
  expect_true(all(is.na(constants$lowerBounds)))
  expect_true(all(is.na(constants$upperBounds)))
  expected = rep(NA_character_, nrow(informationTable$metaData))
  expected[informationTable$metaData$name == "QF2"] = "8"
  expected[informationTable$metaData$name == "QF4"] = "31"
  expect_equal(constants$others, expected)
})

test_that("getConstantsGrouped - all types of filters", {
  e1 = ElementaryCondition$new(attribute = "QF2", value = 8, it = informationTable, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "QF4", value = 31, it = informationTable, isLowerBound = NA)

  e3 = ElementaryCondition$new(attribute = "Q3OD", value = 5, it = informationTable, isLowerBound = T)

  e4 = ElementaryCondition$new(attribute = "Q3PSO", value = 6, it = informationTable, isLowerBound = F)

  e5 = ElementaryCondition$new(attribute = "Q3PSD", value = 7, it = informationTable, isLowerBound = T)
  e6 = ElementaryCondition$new(attribute = "Q3PSD", value = 8, it = informationTable, isLowerBound = F)

  c = ComplexCondition$new(c(e1, e2, e3, e4, e5, e6))
  constants = c$getConstantsGrouped(it = informationTable)

  expected = rep(NA_character_, nrow(informationTable$metaData))
  expected[informationTable$metaData$name == "QF2"] = "8"
  expected[informationTable$metaData$name == "QF4"] = "31"
  expect_equal(constants$others, expected)

  expected = rep(NA_character_, nrow(informationTable$metaData))
  expected[informationTable$metaData$name == "Q3OD"] = "5"
  expected[informationTable$metaData$name == "Q3PSD"] = "7"
  expect_equal(constants$lowerBounds, expected)

  expected = rep(NA_character_, nrow(informationTable$metaData))
  expected[informationTable$metaData$name == "Q3PSO"] = "6"
  expected[informationTable$metaData$name == "Q3PSD"] = "8"
  expect_equal(constants$upperBounds, expected)
})
