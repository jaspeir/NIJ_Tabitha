library(DRSA)
library(purrr)
library(testthat)

test_that("Extract decision rules", {
  # Set-up code:
  load("data/trial-informationTable.RData")
  trialIT = InformationTable$new(informationTable$decisionTable, informationTable$metaData)

  P = names(trialIT$decisionTable)
  P = trialIT$partitionAttributes(P)
  P = c(P$ind, P$sim, P$dom)

  DOMLEM = DOMLEM$new(it = trialIT, P = P)

  cat(DOMLEM$rules)
})
