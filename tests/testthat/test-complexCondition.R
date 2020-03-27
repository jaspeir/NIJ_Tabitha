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