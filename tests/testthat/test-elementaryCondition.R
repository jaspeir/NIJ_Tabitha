test_that("should succeed", {
  ElementaryCondition$new(attribute = "Q1", value = 3, it = informationTable, isLowerBound = F)
  expect_true(TRUE)
})

test_that("empty cover - ind", {
  e = ElementaryCondition$new(attribute = "QF2", value = 0, it = informationTable, isLowerBound = F)
  expect_equal(e$elementCover(informationTable), character())
})

test_that("empty cover - sim", {
  e = ElementaryCondition$new(attribute = "Q1", value = 0, it = informationTable, isLowerBound = F)
  expect_equal(e$elementCover(informationTable), character())
})

test_that("empty cover - dom", {
  e = ElementaryCondition$new(attribute = "Q3OD", value = 10, it = informationTable, isLowerBound = T)
  expect_equal(e$elementCover(informationTable), character())
})

test_that("non-empty cover - ind", {
  e = ElementaryCondition$new(attribute = "QF3ST", value = 9, it = informationTable, isLowerBound = F)
  expect_equal(e$elementCover(informationTable), c("X12", "X15", "X23", "X24"))
})

test_that("non-empty cover - sim", {
  e = ElementaryCondition$new(attribute = "Q2", value = 22, it = informationTable, isLowerBound = F)
  expect_equal(e$elementCover(informationTable), informationTable$objects)
})

test_that("non-empty cover - dom", {
  e = ElementaryCondition$new(attribute = "Q3OTHVAL", value = 2, it = informationTable, isLowerBound = T)
  expect_equal(e$elementCover(informationTable), c("X13", "X16", "X30", "X35", "X36"))
})
