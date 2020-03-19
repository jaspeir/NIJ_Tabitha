library(tibble)

createTestDataset = function(X, Y, alpha, beta) {
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
      type = 'similarity',
      alpha = alpha,
      beta = beta,
      stringsAsFactors = F
    )
  )

  return(InformationTable$new(decisionTable, metaData))
}


test_that("similar single diff 0 - v1", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  it = createTestDataset(r1, r2, alpha = 0.0, beta = 0.0)

  expect_true(it$similar(it$decisionTable[1,], it$decisionTable[2,], c("q")))
})

test_that("similar single diff 0 - v2", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  it = createTestDataset(r1, r2, alpha = 1.0, beta = 0.0)

  expect_true(it$similar(it$decisionTable[1,], it$decisionTable[2,], c("q")))
})

test_that("similar single diff 0 - v3", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  it = createTestDataset(r1, r2, alpha = 0.0, beta = 1.0)

  expect_true(it$similar(it$decisionTable[1,], it$decisionTable[2,], c("q")))
})

test_that("similar single diff 0 - v4", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (1))
  it = createTestDataset(r1, r2, alpha = 1.0, beta = 1.0)

  expect_true(it$similar(it$decisionTable[1,], it$decisionTable[2,], c("q")))
})

test_that("similar single diff nonzero - v1", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))
  it = createTestDataset(r1, r2, alpha = 0.0, beta = 0.0)

  expect_false(it$similar(it$decisionTable[1,], it$decisionTable[2,], c("q")))
})

test_that("similar single diff nonzero - v2", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))
  it = createTestDataset(r1, r2, alpha = 1.0, beta = 0.0)

  expect_true(it$similar(it$decisionTable[1,], it$decisionTable[2,], c("q")))
})

test_that("similar single diff nonzero - v3", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))
  it = createTestDataset(r1, r2, alpha = 0.0, beta = 1.0)

  expect_true(it$similar(it$decisionTable[1,], it$decisionTable[2,], c("q")))
})

test_that("similar single diff nonzero - v4", {
  r1 = tibble(q = (1))
  r2 = tibble(q = (2))
  it = createTestDataset(r1, r2, alpha = 1.0, beta = 1.0)

  expect_true(it$similar(it$decisionTable[1,], it$decisionTable[2,], c("q")))
})
