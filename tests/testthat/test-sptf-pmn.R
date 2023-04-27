# test bulk density functions
require(testthat)

test_that("all pmn functions works", {
  
  A_SOM_LOI = c(2,5)
  A_C_OF = c(20, 200)
  A_CLAY_MI = c(10, 20)
  A_N_RT = c(2300, 1000)
  A_PH_CC = c(7, 7.5)
  A_CEC_CO = c(100, 200)
  A_SILT_MI = c(20, 30)
  
  expect_equal(sptf_pmn1(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI , A_N_RT = A_N_RT, A_PH_CC = A_PH_CC), 
               expected = c(88.49, -14.26), tolerance = 0.01
  )
  
  expect_equal(sptf_pmn2(A_C_OF = A_C_OF, A_SILT_MI = A_SILT_MI, A_N_RT = A_N_RT, A_PH_CC = A_PH_CC, t = 7),
               expected = c(54.49848, 231.49470), tolerance = 0.01)

  expect_equal(sptf_pmn3(A_C_OF = A_C_OF,A_N_RT = A_N_RT, A_CLAY_MI = A_CLAY_MI),
               expected = c(141.85684,  54.53614), tolerance = 0.01)

  expect_equal(sptf_pmn4(A_C_OF = A_C_OF,A_N_RT = A_N_RT, A_CLAY_MI = A_CLAY_MI),
               expected = c( 85.75527, 38.72754), tolerance = 0.01)

  expect_equal(sptf_pmn5(A_N_RT = A_N_RT, A_C_OF = A_C_OF, A_CEC_CO = A_CEC_CO, t = 7),
               expected = c(16.71463, 39.15966), tolerance = 0.01)

  expect_equal(sptf_pmn6(A_N_RT = A_N_RT, A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI,  A_SILT_MI = A_SILT_MI, A_PH_CC = A_PH_CC, t = 7, RES = 1),
               expected = c(7.212288, 4.492643), tolerance = 0.01)

  expect_equal(sptf_pmn7(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, t = 7),
               expected = c(54.94377, 497.67854), tolerance = 0.01)

  expect_equal(sptf_pmn8(A_N_RT = A_N_RT, A_CEC_CO = A_CEC_CO, A_SILT_MI = A_SILT_MI, t = 7),
               expected = c(28.16674, 17.23649), tolerance = 0.01)

  expect_equal(sptf_pmn9(A_N_RT = A_N_RT,  CULT = 1, t = 7),
               expected = c(14.122981,  9.844958), tolerance = 0.01)

  expect_equal(sptf_pmn10(A_N_RT = A_N_RT,  t = 7),
               expected = c(4.786352, 3.596433), tolerance = 0.01)
  
  
})





