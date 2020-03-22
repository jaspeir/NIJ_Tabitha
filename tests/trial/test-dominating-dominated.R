library(DRSA)
library(purrr)
library(testthat)

readSolution = function(fileName) {
  inputFile = file(fileName)
  lines = readLines(inputFile)
  close(inputFile)

  solutions = map(lines, ~ unlist(strsplit(., split = '[[:blank:]]+')))
  solutions = map(solutions, function(solution) solution[solution != "" & solution != "0"][-1])
  solutions
}

test_that("dominating sets", {

  solutions = readSolution("Dominating_Dominated/P_dominating_18_Dec_2019.txt")
  P = names(informationTable$decisionTable)

  dominatingSets = map(informationTable$objects,
                       ~ informationTable$dominatingSet(., P, compareSimilaritySwitched = TRUE))

  for (object in 1:length(informationTable$objects)) {
    expect_equal(dominatingSets[[object]], solutions[[object]])
  }
})

test_that("dominated sets", {

  solutions = readSolution("Dominating_Dominated/P_dominated_18_Dec_2019.txt")
  P = names(informationTable$decisionTable)

  dominatedSets = map(informationTable$objects,
                       ~ informationTable$dominatedSet(., P, compareSimilaritySwitched = FALSE))

  for (object in 1:length(informationTable$objects)) {
    expect_equal(dominatedSets[[object]], solutions[[object]])
  }
})
