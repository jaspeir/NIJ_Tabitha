library(testthat)
library(DRSA)

load("data/warehouse-informationTable.RData")

P = c("A1", "A2", "A3")
roughSets = warehouseIT$roughSets(P)
boundary = warehouseIT$boundaryRegions(roughSets)

test_that("decision rules obtained for some approximations", {
  #DOMLEM$debug('findRules')
  domlem = DOMLEM$new(it = warehouseIT, P = P)

  approx = domlem$roughSets$upward_L[2, ]
  rules = domlem$findRules(approximation = approx, P = P, t = 2, ruleType = "STAT1")
  rules
})

test_that("decision rules obtained for some other approximations", {
  DOMLEM$debug('findRules')
  domlem = DOMLEM$new(it = warehouseIT, P = P)

  approx = domlem$roughSets$downward_L[1, ]
  rules = domlem$findRules(approximation = approx, P = P, t = 1, ruleType = "STAT2")
  rules
})

test_that("decision rules obtained for all approximations", {
  domlem = DOMLEM$new(it = warehouseIT, P = P)
  domlem$main()
  domlem$rules
})
