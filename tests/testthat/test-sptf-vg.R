# test bulk density functions
require(testthat)

test_that("all hydraulic functions works", {
  
  # input parameters
  dt <- data.table(
    A_SOM_LOI = c(2,5),
    A_CLAY_MI = c(10, 20),
    A_SILT_MI = c(20, 30),
    A_SAND_M50 = c(180, 250), 
    A_DEPTH = c(0.25, 0.35)
  )
  
  
  # VG stands for Van Genuchten (?)
  
  # Wösten, J.H.M , Lilly, A., Nemes, A., Le Bas, C. (1999)
  dt_vg1 <- sptf_vg1(A_CLAY_MI = dt$A_CLAY_MI,A_SILT_MI = dt$A_SILT_MI, A_SOM_LOI = dt$A_SOM_LOI, A_DEPTH = dt$A_DEPTH)
  expect_equal(dt_vg1$ThetaS, expected = c(-408.6516 , -358.1684), tolerance = 0.01)
  expect_equal(dt_vg1$ksat, expected = c(-1746705 , -1261632), tolerance = 0.01)
  
  # Wösten, J. H. M., Veerman, G. ., de Groot, W. J., & Stolte, J. (2001)
  dt_vg2 <- sptf_vg2(A_CLAY_MI = dt$A_CLAY_MI, A_SILT_MI = dt$A_SILT_MI, A_SOM_LOI = dt$A_SOM_LOI, A_SAND_M50 = dt$A_SAND_M50, A_DEPTH = dt$A_DEPTH)
  expect_equal(dt_vg2$ThetaS, expected = c(0.4172326, 0.4800080), tolerance = 0.01)
  expect_equal(dt_vg2$ksat, expected = c(37.33175 , 73.31747), tolerance = 0.01)
  expect_equal(dt_vg2$alfa, expected = c(0.05503836, 0.03926662), tolerance = 0.01)

  
})





