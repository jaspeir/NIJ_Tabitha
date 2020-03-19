library(tibble)
library(purrr)

#################################
### Dominance-attributes only ###
#################################

library(tibble)

createTestDataset = function(X, Y) {
  n = nrow(X)

  decisionTable = bind_rows(
    bind_cols(
      data.frame(
        object = 1:n,
        decision = NA,
        stringsAsFactors = F
      ),
      X
    ),
    bind_cols(
      data.frame(
        object = (n + 1):(2 * n),
        decision = NA,
        stringsAsFactors = F
      ),
      Y
    )
  )

  metaData = tribble(
    ~name, ~type, ~alpha, ~beta,
    'object', 'object', NA, NA,
    'decision', 'decision', NA, NA
  )
  metaData = bind_rows(
    metaData,
    data.frame(
      name = names(X),
      type = 'dominance',
      alpha = NA,
      beta = NA,
      stringsAsFactors = F
    )
  )

  return(InformationTable$new(decisionTable, metaData))
}

test_that("dominates single", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  it = createTestDataset(r1, r2)

  expect_true(it$dominates(1, 2, "q"))
})

test_that("does not dominate single", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))
  it = createTestDataset(r1, r2)

  expect_false(it$dominates(1, 2, "q"))
})

test_that("dominates two attributes", {
  r1 = tibble(q = (1), r = (0))
  r2 = tibble(q = (1), r = (0))
  it = createTestDataset(r1, r2)

  expect_true(it$dominates(1, 2, c("q", "r")))
})

test_that("does not dominate two attributes", {
  r1 = tibble(q = (1), r = (0))
  r2 = tibble(q = (0), r = (1))
  it = createTestDataset(r1, r2)

  expect_false(it$dominates(1, 2, c("q", "r")))
  expect_false(it$dominates(2, 1, c("q", "r")))
})

test_that("should fail - attribute not in examples", {
  r1 = tibble(q = (1), r = (0))
  r2 = tibble(q = (0), r = (1))
  it = createTestDataset(r1, r2)

  expect_error(it$dominates(1, 2, c("a")))
  expect_error(it$dominates(1, 2, c("q", "r", "s")))
})
