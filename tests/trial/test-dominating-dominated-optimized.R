library(DRSA)
library(purrr)
library(testthat)

allEqual = function(list1, list2) {
  all(map_lgl(1:length(list1), function(i) {
    all(list1[[i]] == list2[[i]])
  }))
}

test_that('compare optimized and point operators for dominating- and dominated-sets', {

  P = names(informationTable$decisionTable)

  # Point-operators:
  pointOperators = list(
    dominatedSets_L = map(informationTable$objects,
                         ~ informationTable$dominatedSet(., P, compareSimilaritySwitched = TRUE)),
    dominatedSets_U = map(informationTable$objects,
                        ~ informationTable$dominatedSet(., P, compareSimilaritySwitched = FALSE)),
    dominatingSets_L = map(informationTable$objects,
                         ~ informationTable$dominatingSet(., P, compareSimilaritySwitched = FALSE)),
    dominatingSets_U = map(informationTable$objects,
                         ~ informationTable$dominatingSet(., P, compareSimilaritySwitched = TRUE))
  )

  # Optimized operators:
  optimizedSets = informationTable$dominatingAndDominatedSets(P)
  optimizedOperators = list(
    dominatedSets_L = convertMatrixToList(optimizedSets$dominated_L, informationTable$objects),
    dominatedSets_U = convertMatrixToList(optimizedSets$dominated_U, informationTable$objects),
    dominatingSets_L = convertMatrixToList(optimizedSets$dominating_L, informationTable$objects),
    dominatingSets_U = convertMatrixToList(optimizedSets$dominating_U, informationTable$objects)
  )

  expect_true(allEqual(pointOperators$dominatedSets, optimizedOperators$dominatedSets))
  expect_true(allEqual(pointOperators$dominatedSetsStar, optimizedOperators$dominatedSetsStar))

  expect_true(allEqual(pointOperators$dominatingSets, optimizedOperators$dominatingSets))
  expect_true(allEqual(pointOperators$dominatingSetsStar, optimizedOperators$dominatingSetsStar))
})
