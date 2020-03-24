context("Examples for calculating a rough-set.")

informationTable = InformationTable$new(informationTable$decisionTable, informationTable$metaData)
P = names(informationTable$decisionTable)
optimizedSets = informationTable$dominatingAndDominatedSets(P)

test_that("calculate P-upper approximations of upward class unions", {
  informationTable$upwardClassUnionUpperApproximation(optimizedSets$dominating_U)
})

test_that("calculate P-upper approximations of downward class unions", {
  informationTable$downwardClassUnionUpperApproximation(optimizedSets$dominated_U)
})

test_that("calculate P-lower approximations of upward class unions", {

  downwardClassUnionUpperApproximations = informationTable$downwardClassUnionUpperApproximation(optimizedSets$dominated_U)

  informationTable$upwardClassUnionLowerApproximation(downwardClassUnionUpperApproximations)
})

test_that("calculate P-lower approximations of downward class unions", {

  upwardClassUnionUpperApproximations = informationTable$upwardClassUnionUpperApproximation(optimizedSets$dominating_U)

  informationTable$downwardClassUnionLowerApproximation(upwardClassUnionUpperApproximations)
})

test_that("get all approximations", {
  approx = informationTable$roughSets(P)
})

test_that("get accuracy of approximations", {
  approx = informationTable$roughSets(P)
  informationTable$accuracyOfApproximation(approx)
})

test_that("get boundary regions", {
  approx = informationTable$roughSets(P)
  boundaryRegions = informationTable$boundaryRegions(approx)
  boundaryRegions

  informationTable$qualityOfApproximation(boundaryRegions)
})
