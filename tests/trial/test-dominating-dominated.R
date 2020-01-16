readSolution = function(fileName) {
  inputFile = file(fileName)
  lines = readLines(inputFile)
  close(inputFile)

  solutions = map(lines, ~ unlist(strsplit(., split = '[[:blank:]]+')))
  solutions = map(solutions, function(solution) solution[solution != "" & solution != "0"][-1])
}

test_that("dominating sets", {

  dominating = readSolution("tests/trial/Dominating_Dominated/P_dominating_18_Dec_2019.txt")
  P = names(decisionTable)
  P = P[c(7, 8, 9, 10)]

  dominatingSets = map(decisionTable$OBJECT, ~ dominatingSet(., P, decisionTable))

  expect_equal(upwardClassUnion(decisionTable, 5), solution)
})
