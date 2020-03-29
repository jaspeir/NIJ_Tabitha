library(DRSA)
library(purrr)
library(testthat)

# Set-up code:
load("data/trial-informationTable.RData")
trialIT = InformationTable$new(informationTable$decisionTable, informationTable$metaData)

P = names(trialIT$decisionTable)
P = trialIT$partitionAttributes(P)
P = c(P$ind, P$sim, P$dom)

test_that("STAT1 decision rules", {
  #DOMLEM$debug('findRules')
  domlem = DOMLEM$new(it = trialIT, P = P)

  # TODO: for t = 6 everything is FALSE, what (should) happen(s) then? ==> currently it returns an empty list
  # TODO: for t = 2 it does not terminate
  t = 6
  approx = domlem$roughSets$upward_L[t, ]
  rules = domlem$findRules(approximation = approx, P = P, t = t, ruleType = "STAT1")
  rules
})

test_that("STAT2 decision rules", {
  #DOMLEM$debug('findRules')
  domlem = DOMLEM$new(it = trialIT, P = P)

  # TODO: BUG - for t = 1 it returns a rule, that has a repeated elementary term in it:
  # [[3]]
  # Q3PSD <= 2 AND QF4 ~ 31 AND Q2 ~ 23 AND Q3OTHVAL <= 2 AND Q3OTHVAL <= 2
  # TODO: for t = {3, 4, 5} it does not terminate

  t = 1
  approx = domlem$roughSets$downward_L[t, ]
  rules = domlem$findRules(approximation = approx, P = P, t = t, ruleType = "STAT2")
  rules
})

test_that("decision rules obtained for all approximations", {
  domlem = DOMLEM$new(it = trialIT, P = P)
  domlem$main()
  domlem$rules
})
