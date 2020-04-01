test_that("should fail - empty decision table", {
  decisionTable = data.frame()

  expect_error(InformationTable$new(decisionTable))
})


test_that("should fail - too few columns in decision table", {
  decisionTable = tribble(
    ~object, ~decision
  )

  expect_error(InformationTable$new(decisionTable))
})

test_that("should fail - meta-data has incorrect schema", {
  decisionTable = data.frame()
  metaData = tribble(
    ~name, ~type
  )

  expect_error(InformationTable$new(decisionTable, metaData))
})

test_that("should fail - meta-data missing for some columns in the decision table", {
  decisionTable = tribble(
    ~object, ~attribute, ~decision
  )
  metaData = tribble(
    ~name, ~type, ~alpha, ~beta,
    'object', 'object', NA, NA,
    'decision', 'decision', NA, NA,
  )

  expect_error(InformationTable$new(decisionTable, metaData))
})

test_that("should fail - meta-data missing alpha- and beta-parameters for similarity attributes", {
  decisionTable = tribble(
    ~object, ~attribute, ~decision
  )
  metaData = tribble(
    ~name, ~type, ~alpha, ~beta,
    'object', 'object', NA, NA,
    'decision', 'decision', NA, NA,
    'attribute', 'similarity', NA, NA
  )

  expect_error(InformationTable$new(decisionTable, metaData))
})

test_that("should fail - meta-data missing alpha-parameter for similarity attributes", {
  decisionTable = tribble(
    ~object, ~attribute, ~decision
  )
  metaData = tribble(
    ~name, ~type, ~alpha, ~beta,
    'object', 'object', NA, NA,
    'decision', 'decision', NA, NA,
    'attribute', 'similarity', NA, 1.0
  )

  expect_error(InformationTable$new(decisionTable, metaData))
})

test_that("should fail - meta-data missing beta-parameter for similarity attributes", {
  decisionTable = tribble(
    ~object, ~attribute, ~decision
  )
  metaData = tribble(
    ~name, ~type, ~alpha, ~beta,
    'object', 'object', NA, NA,
    'decision', 'decision', NA, NA,
    'attribute', 'similarity', 0.0, NA
  )

  expect_error(InformationTable$new(decisionTable, metaData))
})

test_that("should fail - meta-data missing object-column", {
  decisionTable = tribble(
    ~object, ~attribute, ~decision
  )
  metaData = tribble(
    ~name, ~type, ~alpha, ~beta,
    'object', 'dominance', NA, NA,
    'decision', 'decision', NA, NA,
    'attribute', 'dominance', NA, NA
  )

  expect_error(InformationTable$new(decisionTable, metaData))
})

test_that("should fail - meta-data more than one object-column", {
  decisionTable = tribble(
    ~object, ~attribute, ~decision
  )
  metaData = tribble(
    ~name, ~type, ~alpha, ~beta,
    'object', 'object', NA, NA,
    'decision', 'object', NA, NA,
    'attribute', 'dominance', NA, NA
  )

  expect_error(InformationTable$new(decisionTable, metaData))
})

test_that("default metadata created - dummy dataset", {
  decisionTable = tribble(
    ~object, ~a1, ~decision,
    'X1', 3, 'Pass',
    'X2', 4, 'Pass',
    'X3', 5, 'Fail'
  )

  it = InformationTable$new(decisionTable = decisionTable)
  it$decisionTable$decision
  it$metaData
  it$objects
  P = c('a1')
  domlem = DOMLEM$new(it, P)
  domlem$main()
  domlem$rules

  expect_true(TRUE)
})

test_that("default metadata created - trial dataset", {
  datasetPath = system.file("extdata", "JS_DRSA_Trial_Key_and_Data_Table.xlsx", package = "DRSA", mustWork = TRUE)
  decisionTable = readxl::read_excel(path = datasetPath, sheet = "Input_Data")

  it = InformationTable$new(decisionTable = decisionTable)
  it$metaData$type[it$metaData$name == 'Q3OTH_RESP'] = 'misc'
  it$objects
  it$decisionTable$QF1
  P = it$metaData$name[c(2:10, 12:17)]
  it$classUnions()
  it$roughSets(P)

  domlem = DOMLEM$new(it, P)
  #domlem$main()
  #domlem

  expect_true(TRUE)
})
