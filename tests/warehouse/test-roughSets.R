library(testthat)
library(DRSA)

load("data/warehouse-informationTable.RData")

P = c("A1", "A2", "A3")
roughSets = warehouseIT$roughSets(P)
boundary = warehouseIT$boundaryRegions(roughSets)

getObjects = function(v) {
  warehouseIT$objects[v]
}

context("Warehouse dataset - class union approximations")

test_that("test downward lower t=1", {
  expect_equal(getObjects(roughtSets$downward_L[1,]), c("w1"))
})

test_that("test downward upper t=1", {
  expect_equal(getObjects(roughtSets$downward_U[1,]), c("w1", "w3", "w4", "w6", "w7", "w8"))
})

test_that("test boundary of downward union t=1", {
  expect_equal(getObjects(boundary$downward[1,]), c("w3", "w4", "w6", "w7", "w8"))
})

test_that("test upward lower t=2", {
  expect_equal(getObjects(roughtSets$upward_L[2,]), c("w2", "w5"))
})

test_that("test upward upper t=2", {
  expect_equal(getObjects(roughtSets$upward_U[2,]), c("w2", "w3", "w4", "w5", "w6", "w7", "w8"))
})

test_that("test boundary of upward union t=2", {
  expect_equal(getObjects(boundary$upward[2,]), c("w3", "w4", "w6", "w7", "w8"))
})

