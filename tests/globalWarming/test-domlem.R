library(testthat)
library(DRSA)

load("data/globalWarming-informationTable.RData")
P = c("SolarEnergy", "VolcanicActivity", "ResidualCO2")

test_that("decision rules obtained for all approximations", {
  domlem = DOMLEM$new(it = globalWarmingIT, P = P)
  domlem$main()
  print(domlem)
})
