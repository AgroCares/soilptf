# test bulk density functions
require(testthat)

test_that("estimation sptf_bd1", {
  expect_equal(
    sptf_bd1(A_SOM_LOI = c(2,5), 
             A_CLAY_MI = c(10,15)),
    expected = c(1343.75,1176.612),
    tolerance = 0.01)
})
