context("ComplexCondition")

test_that("create empty should succeed", {
  c = ComplexCondition$new()
  expect_true(TRUE)
})

test_that("create single should succeed", {
  e = ElementaryCondition$new(attribute = "Q1", value = 3, it = trialIT, isLowerBound = NA)
  c = ComplexCondition$new(e)
  expect_true(TRUE)
})

test_that("create single-element vector should succeed", {
  e = ElementaryCondition$new(attribute = "Q1", value = 3, it = trialIT, isLowerBound = NA)
  c = ComplexCondition$new(c(e))
  expect_true(TRUE)
})

test_that("full cover - empty cond", {
  c = ComplexCondition$new()
  expect_equal(c$complexCover(trialIT), trialIT$objects)
})

test_that("full cover - element with full cover", {
  e = ElementaryCondition$new(attribute = "Q2", value = 22, it = trialIT, isLowerBound = NA) # TODO: 22 = "Moderate"
  c = ComplexCondition$new(e)
  expect_equal(c$complexCover(trialIT), trialIT$objects)
})

test_that("empty cover - element with empty cover", {
  e = ElementaryCondition$new(attribute = "QF3PD", value = "Yes", it = trialIT, isLowerBound = NA)
  c = ComplexCondition$new(e)
  expect_equal(c$complexCover(trialIT), character())
})

test_that("empty cover - two elements: one with empty cover and another with full cover", {
  e1 = ElementaryCondition$new(attribute = "QF3PD", value = "Yes", it = trialIT, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "Q2", value = 22, it = trialIT, isLowerBound = NA) # TODO: 22 = "Moderate"
  c = ComplexCondition$new(c(e1, e2))
  expect_equal(c$complexCover(trialIT), character())
})

test_that("conjuction of two elements", {
  e1 = ElementaryCondition$new(attribute = "QF3ST", value = "Yes", it = trialIT, isLowerBound = F)
  e2 = ElementaryCondition$new(attribute = "Q3PSO", value = "Value for exclusion", it = trialIT, isLowerBound = F)
  c = ComplexCondition$new(c(e1, e2))
  expect_equal(c$complexCover(trialIT), c("X23"))
})

test_that("first metric - empty G", {
  c = ComplexCondition$new()
  expect_equal(c$firstMetric(G = c(), it = trialIT), 0)
})

test_that("first metric - single element in G", {
  c = ComplexCondition$new()
  expect_equal(c$firstMetric(G = c("X1"), it = trialIT), 1/length(trialIT$objects))
})

test_that("first metric - G outside cover", {
  c = ComplexCondition$new()
  expect_equal(c$firstMetric(G = c("XOR"), it = trialIT), 0)
})

test_that("second metric - empty G", {
  c = ComplexCondition$new()
  expect_equal(c$secondMetric(G = c(), it = trialIT), 0)
})

test_that("second metric - single element in G", {
  c = ComplexCondition$new()
  expect_equal(c$secondMetric(G = c("X1"), it = trialIT), 1)
})

test_that("second metric - G outside cover", {
  c = ComplexCondition$new()
  expect_equal(c$secondMetric(G = c("XOR"), it = trialIT), 0)
})

test_that("getConstants - empty rule", {
  c = ComplexCondition$new()
  constants = rep(NA, nrow(trialIT$metaData))
  expect_equal(c$getConstants(trialIT), constants)
})

test_that("getConstants - single rule", {
  e = ElementaryCondition$new(attribute = "Q1", value = 42, it = trialIT, isLowerBound = NA)
  c = ComplexCondition$new(e)
  constants = rep(NA_real_, nrow(trialIT$metaData))
  constants[5] = "42"
  expect_equal(c$getConstants(trialIT), constants)
})

test_that("getConstants - two rules", {
  e1 = ElementaryCondition$new(attribute = "Q1", value = 42, it = trialIT, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "Q2", value = 421, it = trialIT, isLowerBound = NA)

  c = ComplexCondition$new(c(e1, e2))
  constants = rep(NA_real_, nrow(trialIT$metaData))
  constants[5] = "42"
  constants[6] = "421"
  expect_equal(c$getConstants(trialIT), constants)
})

test_that("findBestElementary - empty best", {
  e1 = ElementaryCondition$new(attribute = "Q1", value = 42, it = trialIT, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "Q2", value = 421, it = trialIT, isLowerBound = NA)

  c = ComplexCondition$new(e1)
  newBest = c$findBestElementary(G = character(0), it = trialIT, check = e2, best = NULL)

  expect_true(e2$equals(newBest))
})

test_that("findBestElementary - same attribute, different values", {
  e1 = ElementaryCondition$new(attribute = "QF3ST", value = "No", it = trialIT, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "QF3ST", value = "Yes", it = trialIT, isLowerBound = NA)

  c = ComplexCondition$new()

  newBest = NULL
  newBest = c$findBestElementary(G = trialIT$objects, it = trialIT, check = e1, best = newBest)
  newBest = c$findBestElementary(G = trialIT$objects, it = trialIT, check = e2, best = newBest)

  expect_true(e1$equals(newBest))

  newBest = NULL
  newBest = c$findBestElementary(G = trialIT$objects, it = trialIT, check = e2, best = newBest)
  newBest = c$findBestElementary(G = trialIT$objects, it = trialIT, check = e1, best = newBest)

  expect_true(e1$equals(newBest))
})

test_that("findBestElementary - same attribute, different values, reduced objects set", {
  e1 = ElementaryCondition$new(attribute = "QF3ST", value = "No", it = trialIT, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "QF3ST", value = "Yes", it = trialIT, isLowerBound = NA)

  c = ComplexCondition$new()
  G = c("X12", "X15", "X23", "X24")

  newBest = NULL
  newBest = c$findBestElementary(G = G, it = trialIT, check = e1, best = newBest)
  newBest = c$findBestElementary(G = G, it = trialIT, check = e2, best = newBest)

  expect_true(e2$equals(newBest))

  newBest = NULL
  newBest = c$findBestElementary(G = G, it = trialIT, check = e2, best = newBest)
  newBest = c$findBestElementary(G = G, it = trialIT, check = e1, best = newBest)

  expect_true(e2$equals(newBest))
})

test_that("findBestElementary - same attribute, different values, one part already covered", {
  e = ElementaryCondition$new(attribute = "QF3ST", value = "Yes", it = trialIT, isLowerBound = NA)

  e1 = ElementaryCondition$new(attribute = "Q3PSO", value = "Value for association", it = trialIT, isLowerBound = F)
  e2 = ElementaryCondition$new(attribute = "Q3PSO", value = "Value for exclusion", it = trialIT, isLowerBound = F)

  c = ComplexCondition$new(e)
  G = trialIT$objects

  newBest = NULL
  newBest = c$findBestElementary(G = G, it = trialIT, check = e1, best = newBest)
  newBest = c$findBestElementary(G = G, it = trialIT, check = e2, best = newBest)

  expect_true(e1$equals(newBest))

  newBest = NULL
  newBest = c$findBestElementary(G = G, it = trialIT, check = e2, best = newBest)
  newBest = c$findBestElementary(G = G, it = trialIT, check = e1, best = newBest)

  expect_true(e1$equals(newBest))
})

test_that("reduceConditions - empty condition", {
  c = ComplexCondition$new()
  B = trialIT$objects

  expect_true(c$equals(c$reduceConditions(B = B, it = trialIT)))
})

test_that("reduceConditions - a single condition", {
  e = ElementaryCondition$new(attribute = "QF3ST", value = "Yes", it = trialIT, isLowerBound = NA)

  c = ComplexCondition$new(e)
  B = trialIT$objects

  reduced = c$reduceConditions(B = B, it = trialIT)
  expect_true(c$equals(reduced))
})

test_that("reduceConditions - two conditions, first covered", {
  e1 = ElementaryCondition$new(attribute = "QF3ST", value = "Yes", it = trialIT, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "QF2", value = "Yes", it = trialIT, isLowerBound = NA)

  c = ComplexCondition$new(c(e1, e2))
  B = c("X12", "X15", "X23", "X24")

  reduced = c$reduceConditions(B = B, it = trialIT)
  expectedReduced = ComplexCondition$new(c(e1))

  expect_true(expectedReduced$equals(reduced))

  c = ComplexCondition$new(c(e2, e1))
  reduced = c$reduceConditions(B = B, it = trialIT)
  expect_true(expectedReduced$equals(reduced))
})

test_that("getConstantsGrouped - empty", {
  c = ComplexCondition$new()
  constants = c$getConstantsGrouped(it = trialIT)
  expect_true(all(map_lgl(constants, ~ all(is.na(.)))))
})

test_that("getConstantsGrouped - only misc filters", {
  e1 = ElementaryCondition$new(attribute = "QF2", value = "No", it = trialIT, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "QF4", value = 31, it = trialIT, isLowerBound = NA) # TODO: 31 = "Easy"
  c = ComplexCondition$new(c(e1, e2))
  constants = c$getConstantsGrouped(it = trialIT)
  expect_true(all(is.na(constants$lowerBounds)))
  expect_true(all(is.na(constants$upperBounds)))
  expected = rep(NA_character_, nrow(trialIT$metaData))
  expected[trialIT$metaData$name == "QF2"] = "No"
  expected[trialIT$metaData$name == "QF4"] = "31"
  expect_equal(constants$others, expected)
})

test_that("getConstantsGrouped - all types of filters", {
  e1 = ElementaryCondition$new(attribute = "QF2", value = "No", it = trialIT, isLowerBound = NA)
  e2 = ElementaryCondition$new(attribute = "QF4", value = 31, it = trialIT, isLowerBound = NA)

  e3 = ElementaryCondition$new(attribute = "Q3OD", value = "Value for association", it = trialIT, isLowerBound = T)

  e4 = ElementaryCondition$new(attribute = "Q3PSO", value = "Value for association", it = trialIT, isLowerBound = F)

  e5 = ElementaryCondition$new(attribute = "Q3PSD", value = "Insufficient Detail", it = trialIT, isLowerBound = T)
  e6 = ElementaryCondition$new(attribute = "Q3PSD", value = "Value for association", it = trialIT, isLowerBound = F)

  c = ComplexCondition$new(c(e1, e2, e3, e4, e5, e6))
  constants = c$getConstantsGrouped(it = trialIT)

  expected = rep(NA_character_, nrow(trialIT$metaData))
  expected[trialIT$metaData$name == "QF2"] = "No"
  expected[trialIT$metaData$name == "QF4"] = "31"
  expect_equal(constants$others, expected)

  expected = rep(NA_character_, nrow(trialIT$metaData))
  expected[trialIT$metaData$name == "Q3OD"] = "Value for association"
  expected[trialIT$metaData$name == "Q3PSD"] = "Insufficient Detail"
  expect_equal(constants$lowerBounds, expected)

  expected = rep(NA_character_, nrow(trialIT$metaData))
  expected[trialIT$metaData$name == "Q3PSO"] = "Value for association"
  expected[trialIT$metaData$name == "Q3PSD"] = "Value for association"
  expect_equal(constants$upperBounds, expected)
})
