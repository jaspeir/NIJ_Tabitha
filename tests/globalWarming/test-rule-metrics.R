library(DRSA)
library(testthat)

test_that("rule metrics for the global warming dataset as in the Pawlak 2002 paper", {

  #Set-up code:
  load("data/globalWarming-informationTable.RData")

  # NOTE: for testing the rule metrics, we have to modify the dominance variables' types to indiscernibility.
  metaData = tribble(
    ~name, ~type, ~alpha, ~beta,
    'Fact', 'object', NA, NA,

    'SolarEnergy', 'indiscernibility', NA, NA,
    'VolcanicActivity', 'indiscernibility', NA, NA,
    'ResidualCO2', 'indiscernibility', NA, NA,
    'Temperature', 'decision', NA, NA,

    'DaysCount',  'miscellaneous', NA, NA
  )

  # NOTE: we also need to multiply rows according to the DaysCount column
  library(dplyr)
  library(purrr)

  expandedRows = unlist(map(1:6, function(i) rep(i, globalWarmingIT$decisionTable$DaysCount[i])))
  decisionTable = globalWarmingIT$decisionTable[expandedRows, ]
  decisionTable$Fact = as.character(seq_along(decisionTable$Fact))

  globalWarmingIT = InformationTable$new(decisionTable, metaData)

  P = c("SolarEnergy", "VolcanicActivity", "ResidualCO2")

  # Rule 1:
  c = ComplexCondition$new(conditions = c(
    ElementaryCondition$new(attribute = 'SolarEnergy', value = "Medium", isLowerBound = T, it = globalWarmingIT),
    ElementaryCondition$new(attribute = 'VolcanicActivity', value = "High", isLowerBound = T, it = globalWarmingIT)
  ))
  rule = DecisionRule$new(condition = c, t = 2, type = 'STAT1')
  rule$ruleMetrics(globalWarmingIT)

  # Rule 3:
  c = ComplexCondition$new(conditions = c(
    ElementaryCondition$new(attribute = 'SolarEnergy', value = "Medium", isLowerBound = T, it = globalWarmingIT),
    ElementaryCondition$new(attribute = 'VolcanicActivity', value = "Low", isLowerBound = T, it = globalWarmingIT)
  ))
  rule = DecisionRule$new(condition = c, t = 2, type = 'STAT1')
  rule$ruleMetrics(globalWarmingIT)

  # Rule 5:
  c = ComplexCondition$new(conditions = c(
    ElementaryCondition$new(attribute = 'SolarEnergy', value = "Medium", isLowerBound = T, it = globalWarmingIT),
    ElementaryCondition$new(attribute = 'VolcanicActivity', value = "Low", isLowerBound = T, it = globalWarmingIT)
  ))
  rule = DecisionRule$new(condition = c, t = 1, type = 'STAT2')
  rule$ruleMetrics(globalWarmingIT)

})
