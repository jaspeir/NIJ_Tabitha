library(stringr)

P = names(informationTable$decisionTable)
roughSets = informationTable$roughSets(P)

classMappings = data.frame(
  index = 1:6,
  t = 2:7,
  word = c('two', 'three', 'four', 'five', 'six', 'seven')
)

readSolution = function(fileName) {
  inputFile = file(fileName)
  lines = readLines(inputFile)
  close(inputFile)

  solutions = strsplit(lines[[1]], split = '[[:blank:]]+')
  solutions = !str_detect(solutions[[1]], "_negated")
  solutions
}

getSolution = function(isUpward, isUpper, t) {

  fileName = paste0("p_",
                    ifelse(isUpper, "upper_", "lower_"),
                    ifelse(isUpward, "upward_", "downward_"),
                    "union_",
                    classMappings$word[classMappings$t == t],
                    "_or_",
                    ifelse(isUpward, "greater", "less")
                    )
  directory = ifelse(dir.exists("tests/trial/P_Lower_and_Upper"), "tests/trial/P_Lower_and_Upper", "P_Lower_and_Upper")
  fileName = paste0(directory, "/", fileName)
  dateEndings = c('_19_Dec_2019.txt', '_18_Dec_2019.txt')
  possibleFileNames = paste0(fileName, dateEndings)
  whichFileName = file.exists(possibleFileNames)

  if (whichFileName[1]) {
    fileName = possibleFileNames[1]
  } else if (whichFileName[2]) {
    fileName = possibleFileNames[2]
  } else {
    stop()
  }

  readSolution(fileName)
}

runAllForSet = function(approx, info, isUpward, isUpper) {
  print(info)

  for (index in classMappings$index) {
    solutionManual = getSolution(isUpward = isUpward, isUpper = isUpper, t = classMappings$t[index])
    equals = all(approx[index, ] == solutionManual)
    print(paste0(index, ": ", equals))
    expect_true(equals, info = paste0(info, " t = ", classMappings$t[index]))
  }
}

test_that("P-lower upward", {

  #solutions = readSolution("tests/trial/P_Lower_and_Upper/p_lower_downward_union_five_or_less*.txt")

  runAllForSet(roughSets$downward_L, "downward_L", isUpward = F, isUpper = F)
  runAllForSet(roughSets$downward_U, "downward_U", isUpward = F, isUpper = T)
  runAllForSet(roughSets$upward_L, "upward_L", isUpward = T, isUpper = F)
  runAllForSet(roughSets$upward_U, "upward_U", isUpward = T, isUpper = T)
})
