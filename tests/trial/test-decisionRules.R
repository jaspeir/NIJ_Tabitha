library(DRSA)
library(purrr)
library(testthat)

test_that("Extract decision rules", {
  # Set-up code:
  informationTable = InformationTable$new(informationTable$decisionTable, informationTable$metaData)

  P = names(informationTable$decisionTable)

  DOMLEM = DOMLEM$new(it = informationTable, P = P)

  cat(DOMLEM$rules)
})
