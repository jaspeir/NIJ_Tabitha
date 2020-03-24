context("Theorems from Greco et al. 2002 DRSA")

# Set-up code:
informationTable = InformationTable$new(informationTable$decisionTable, informationTable$metaData)
classUnions = informationTable$classUnions()

P = names(informationTable$decisionTable)
roughSets = informationTable$roughSets(P)
boundaryRegions = informationTable$boundaryRegions(roughSets)


test_that("Theorem 1 (Rough inclusion)", {

  expect_equal(roughSets$upward_L & classUnions$upward, roughSets$upward_L)
  expect_equal(classUnions$upward & roughSets$upward_U, classUnions$upward)

  expect_equal(roughSets$downward_L & classUnions$downward, roughSets$downward_L)
  expect_equal(classUnions$downward & roughSets$downward_U, classUnions$downward)

})

test_that("Theorem 3 (Identity of boundaries)", {

  classCount = nrow(roughSets$upward_U)

  for (class in 2:classCount) {
    expect_equal(boundaryRegions$upward[class, ], boundaryRegions$downward[class - 1, ])
  }
})
