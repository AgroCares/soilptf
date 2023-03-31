# test bulk density functions
require(testthat)

test_that("USDA texture classification", {
  
  # input parameters
    A_CLAY_MI = c(10, 30)
    A_SILT_MI = c(20, 20)
    A_SAND_MI = c(15, 20)
  
  
  textures <- sptf_textureclass(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_SAND_MI = A_SAND_MI)
  expect_equal(textures, expected = c( "sandy loam","sandy loam"))

  
})





