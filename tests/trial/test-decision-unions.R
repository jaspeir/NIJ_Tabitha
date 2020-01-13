readSolution = function(fileName) {
  inputFile = file(fileName)
  line = readLines(inputFile)
  close(inputFile)

  solution = unlist(strsplit(line, split = '[[:blank:]]+'))
  solution[solution != "" & solution != "0"]
}

test_that("five or greater", {

  solution = readSolution("Decision_Unions/five_or_greater.txt")

  expect_equal(upwardClassUnion(decisionTable, 5), solution)
})

test_that("five or less", {

  solution = readSolution("Decision_Unions/five_or_less.txt")

  expect_equal(downwardClassUnion(decisionTable, 5), solution)
})

test_that("four or greater", {

  solution = readSolution("Decision_Unions/four_or_greater.txt")

  expect_equal(upwardClassUnion(decisionTable, 4), solution)
})

test_that("four or less", {

  solution = readSolution("Decision_Unions/four_or_less.txt")

  expect_equal(downwardClassUnion(decisionTable, 4), solution)
})

test_that("seven or greater", {

  solution = readSolution("Decision_Unions/seven_or_greater.txt")

  expect_equal(upwardClassUnion(decisionTable, 7), solution)
})

test_that("seven or less", {

  solution = readSolution("Decision_Unions/seven_or_less.txt")

  expect_equal(downwardClassUnion(decisionTable, 7), solution)
})


test_that("six or greater", {

  solution = readSolution("Decision_Unions/six_or_greater.txt")

  expect_equal(upwardClassUnion(decisionTable, 6), solution)
})

test_that("six or less", {

  solution = readSolution("Decision_Unions/six_or_less.txt")

  expect_equal(downwardClassUnion(decisionTable, 6), solution)
})

test_that("three or greater", {

  solution = readSolution("Decision_Unions/three_or_greater.txt")

  expect_equal(upwardClassUnion(decisionTable, 3), solution)
})

test_that("three or less", {

  solution = readSolution("Decision_Unions/three_or_less.txt")

  expect_equal(downwardClassUnion(decisionTable, 3), solution)
})

test_that("two or greater", {

  solution = readSolution("Decision_Unions/two_or_greater.txt")

  expect_equal(upwardClassUnion(decisionTable, 2), solution)
})

test_that("two or less", {

  solution = readSolution("Decision_Unions/two_or_less.txt")

  expect_equal(downwardClassUnion(decisionTable, 2), solution)
})
