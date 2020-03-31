library(testthat)
library(DRSA)

P = c("A1", "A2", "A3")
roughSets = warehouseIT$roughSets(P)
boundary = warehouseIT$boundaryRegions(roughSets)

getObjects = function(v) {
  warehouseIT$objects[v]
}

context("Warehouse dataset - class union approximations")

test_that("test downward lower t=1", {
  expect_equal(getObjects(roughSets$downward_L[1,]), c("w1"))
})

test_that("test downward upper t=1", {
  expect_equal(getObjects(roughSets$downward_U[1,]), c("w1", "w3", "w4", "w6", "w7", "w8"))
})

test_that("test boundary of downward union t=1", {
  expect_equal(getObjects(boundary$downward[1,]), c("w3", "w4", "w6", "w7", "w8"))
})

test_that("test upward lower t=2", {
  expect_equal(getObjects(roughSets$upward_L[2,]), c("w2", "w5"))
})

test_that("test upward upper t=2", {
  expect_equal(getObjects(roughSets$upward_U[2,]), c("w2", "w3", "w4", "w5", "w6", "w7", "w8"))
})

test_that("test boundary of upward union t=2", {
  expect_equal(getObjects(boundary$upward[2,]), c("w3", "w4", "w6", "w7", "w8"))
})

test_that("test accuracy of approximation", {
  acc = warehouseIT$accuracyOfApproximation(roughSets)
  expect_equal(round(acc$downward[1], digits = 2), 0.17)
  expect_equal(round(acc$upward[2], digits = 2), 0.29)
})

test_that("test quality of classification", {
  q = warehouseIT$qualityOfApproximation(boundaryRegions = boundary)
  expect_equal(round(q - 0.0000001, digits = 2), 0.37)
})

