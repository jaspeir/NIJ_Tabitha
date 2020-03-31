library(testthat)
library(DRSA)
library(purrr)

P = names(trialIT$decisionTable)
P = trialIT$partitionAttributes(P)
P = c(P$ind, P$sim, P$dom)

test_that("STAT1 decision rules", {
  #DOMLEM$debug('findRules')
  domlem = DOMLEM$new(it = trialIT, P = P)
  t = 2
  approx = domlem$roughSets$upward_L[t, ]
  rules = domlem$findRules(approximation = approx, P = P, t = t, ruleType = "STAT1")
  rules
})

test_that("STAT2 decision rules", {
  #DOMLEM$debug('findRules')
  domlem = DOMLEM$new(it = trialIT, P = P)

  t = 1
  approx = domlem$roughSets$downward_L[t, ]
  rules = domlem$findRules(approximation = approx, P = P, t = t, ruleType = "STAT2")
  rules
})

test_that("decision rules obtained for all approximations", {
  domlem = DOMLEM$new(it = trialIT, P = P)
  domlem$main()
  print(domlem)
})
