library(DRSA)
library(testthat)

test_that('optimized class unions should be equal to class-wise calculations', {

  informationTable = InformationTable$new(informationTable$decisionTable, informationTable$metaData)
  classUnions = informationTable$classUnions()

  upwardClassUnions = convertMatrixToList(classUnions$upward, informationTable$objects)
  downwardClassUnions = convertMatrixToList(classUnions$downward, informationTable$objects)

  decisionColumn = which(informationTable$metaData$type == 'decision', arr.ind = TRUE)
  decisions = informationTable$decisionTable[[decisionColumn]]
  decisionValues = sort(unique(decisions))

  for (classID in 1:length(decisionValues)) {
    expect_true(all(upwardClassUnions[[classID]] == informationTable$upwardClassUnion(decisionValues[classID])))
    expect_true(all(downwardClassUnions[[classID]] == informationTable$downwardClassUnion(decisionValues[classID])))
  }
})
