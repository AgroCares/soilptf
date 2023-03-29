# test bulk density functions
require(testthat)

test_that("USDA texture classification", {
  
  # input parameters
  dt <- data.table(
    A_CLAY_MI = c(10, 30),
    A_SILT_MI = c(20, 20),
    A_SAND_MI = c(60, 50)
  )
  
  
  textures <- sptf_textureclass(A_CLAY_MI = dt$A_CLAY_MI, A_SILT_MI = dt$A_SILT_MI, A_SAND_MI = dt$A_SAND_MI)
  expect_equal(textures, expected = c( "sandy loam","sandy clay loam"), tolerance = 0.01)

  
})





