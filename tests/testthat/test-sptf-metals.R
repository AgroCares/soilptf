# test bulk density functions
require(testthat)

test_that("freundlich coefficient functions for metals returns the correct values", {
  
  
  # Define test data
  A_SOM_LOI <- c(5, 15)
  A_CLAY_MI <- c(20, 10)
  A_PH_WA <- c(5.8, 7)
  
  # run all bulk density functions
  
  expect_equal(
    sptf_fc_zn(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA),
    expected = c(0.001020927 , 0.001493947),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_fc_pb(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA),
    expected = c(0.01217952  , 0.03331450),
    tolerance = 0.01
  )
  
  
  expect_equal(
    sptf_fc_cd(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA),
    expected = c(0.0002479206   , 0.0004340389),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_fc_cu(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA),
    expected = c(0.001999624    , 0.003205305),
    tolerance = 0.01
  )
  

  
})

