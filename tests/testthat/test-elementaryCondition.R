#load("data/trial-informationTable.RData")
trialIT = InformationTable$new(informationTable$decisionTable, informationTable$metaData)

test_that("should succeed", {
  ElementaryCondition$new(attribute = "Q1", value = 80, it = trialIT, isLowerBound = NA)
  expect_true(TRUE)
})

test_that("should fail - NA lower bound for dominance", {
  expect_error(ElementaryCondition$new(attribute = "Q3OD", value = 'Value for association', it = trialIT, isLowerBound = NA))
})

test_that("empty cover - ind", {
  e = ElementaryCondition$new(attribute = "QF3PD", value = "Yes", it = trialIT, isLowerBound = NA)
  expect_equal(e$elementCover(trialIT), character())
})

test_that("empty cover - sim", {
  e = ElementaryCondition$new(attribute = "Q1", value = 0, it = trialIT, isLowerBound = NA)
  expect_equal(e$elementCover(trialIT), character())
})

test_that("empty cover - dom", {
  e = ElementaryCondition$new(attribute = "Q3OD", value = 'Insufficient Detail', it = trialIT, isLowerBound = F)
  expect_equal(e$elementCover(trialIT), character())
})

test_that("non-empty cover - ind", {
  e = ElementaryCondition$new(attribute = "QF3ST", value = "Yes", it = trialIT, isLowerBound = NA)
  expect_equal(e$elementCover(trialIT), c("X12", "X15", "X23", "X24"))
})

test_that("non-empty cover - sim", {
  e = ElementaryCondition$new(attribute = "Q2", value = 22, it = trialIT, isLowerBound = NA) # TODO: 22 = "Moderate"
  expect_equal(e$elementCover(trialIT), trialIT$objects)
})

test_that("non-empty cover - dom", {
  e = ElementaryCondition$new(attribute = "Q3OTHVAL", value = "Value for exclusion", it = trialIT, isLowerBound = T)
  expect_equal(e$elementCover(trialIT), c("X13", "X16", "X30", "X35", "X36"))
})
