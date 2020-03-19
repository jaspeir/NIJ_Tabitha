library(testthat)

readSolution = function(fileName) {
  inputFile = file(fileName)
  line = readLines(inputFile)
  close(inputFile)

  solution = unlist(strsplit(line, split = '[[:blank:]]+'))
  solution[solution != "" & solution != "0"]
}

load('data/trial-informationTable.RData')

test_that("five or greater", {

  solution = readSolution("tests/trial/Decision_Unions/five_or_greater.txt")

  expect_equal(informationTable$upwardClassUnion(5), solution)
})

test_that("five or less", {

  solution = readSolution("tests/trial/Decision_Unions/five_or_less.txt")

  expect_equal(informationTable$downwardClassUnion(5), solution)
})

test_that("four or greater", {

  solution = readSolution("tests/trial/Decision_Unions/four_or_greater.txt")

  expect_equal(informationTable$upwardClassUnion(4), solution)
})

test_that("four or less", {

  solution = readSolution("tests/trial/Decision_Unions/four_or_less.txt")

  expect_equal(informationTable$downwardClassUnion(4), solution)
})

test_that("seven or greater", {

  solution = readSolution("tests/trial/Decision_Unions/seven_or_greater.txt")

  expect_equal(informationTable$upwardClassUnion(7), solution)
})

test_that("seven or less", {

  solution = readSolution("tests/trial/Decision_Unions/seven_or_less.txt")

  expect_equal(informationTable$downwardClassUnion(7), solution)
})


test_that("six or greater", {

  solution = readSolution("tests/trial/Decision_Unions/six_or_greater.txt")

  expect_equal(informationTable$upwardClassUnion(6), solution)
})

test_that("six or less", {

  solution = readSolution("tests/trial/Decision_Unions/six_or_less.txt")

  expect_equal(informationTable$downwardClassUnion(6), solution)
})

test_that("three or greater", {

  solution = readSolution("tests/trial/Decision_Unions/three_or_greater.txt")

  expect_equal(informationTable$upwardClassUnion(3), solution)
})

test_that("three or less", {

  solution = readSolution("tests/trial/Decision_Unions/three_or_less.txt")

  expect_equal(informationTable$downwardClassUnion(3), solution)
})

test_that("two or greater", {

  solution = readSolution("tests/trial/Decision_Unions/two_or_greater.txt")

  expect_equal(informationTable$upwardClassUnion(2), solution)
})

test_that("two or less", {

  solution = readSolution("tests/trial/Decision_Unions/two_or_less.txt")

  expect_equal(informationTable$downwardClassUnion(2), solution)
})

