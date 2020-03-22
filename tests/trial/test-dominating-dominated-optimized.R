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
    dominatedSets = map(informationTable$objects,
                         ~ informationTable$dominatedSet(., P, compareSimilaritySwitched = FALSE)),
    dominatedSetsStar = map(informationTable$objects,
                        ~ informationTable$dominatedSet(., P, compareSimilaritySwitched = TRUE)),
    dominatingSets = map(informationTable$objects,
                         ~ informationTable$dominatingSet(., P, compareSimilaritySwitched = FALSE)),
    dominatingSetsStar = map(informationTable$objects,
                         ~ informationTable$dominatingSet(., P, compareSimilaritySwitched = TRUE))
  )

  # Optimized operators:
  optimizedSets = informationTable$dominatingAndDominatedSets(P)
  optimizedOperators = list(
    dominatedSets = convertMatrixToList(optimizedSets$dominated, informationTable$objects),
    dominatedSetsStar = convertMatrixToList(optimizedSets$dominated_star, informationTable$objects),
    dominatingSets = convertMatrixToList(optimizedSets$dominating, informationTable$objects),
    dominatingSetsStar = convertMatrixToList(optimizedSets$dominating_star, informationTable$objects)
  )

  expect_true(allEqual(pointOperators$dominatedSets, optimizedOperators$dominatedSets))
  expect_true(allEqual(pointOperators$dominatedSetsStar, optimizedOperators$dominatedSetsStar))

  # TODO: why is there a "switch" between star and non-star versions:
  expect_true(allEqual(pointOperators$dominatingSetsStar, optimizedOperators$dominatingSets))
  expect_true(allEqual(pointOperators$dominatingSets, optimizedOperators$dominatingSetsStar))
})
