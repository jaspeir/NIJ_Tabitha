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
