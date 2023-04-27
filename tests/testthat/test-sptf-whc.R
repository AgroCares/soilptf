# test bulk density functions
require(testthat)
# require(euptf2)

test_that("water holding capacity functions returns the correct values", {
  
  
  # Define test data
  A_SOM_LOI <- c(2, 5)
  A_C_OF <- c(20, 200)
  A_CLAY_MI <- c(10, 20)
  A_SAND_M50 <- c(150, 200)
  A_SILT_MI <- c(23, 10)
  A_SAND_MI <- c(15, 25)
  A_DEPTH <- c(0.3, 1)
  D_BDS <- c(1200, 1100)
  topsoil <- c(10, 20)
  
  
  # run all water holding capacity functions
  expect_equal(
    sptf_whc1(
      A_C_OF = A_C_OF,
      A_SAND_MI = A_SAND_MI,
      A_CLAY_MI = A_CLAY_MI
    ),
    expected = c(NA_real_,NA_real_),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_whc2(
      A_C_OF = A_C_OF,
      A_SAND_MI = A_SAND_MI,
      A_CLAY_MI = A_CLAY_MI
    ),
    expected = c(NA_real_,NA_real_),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_whc3(
      A_SAND_MI = A_SAND_MI,
      A_CLAY_MI = A_CLAY_MI
    ),
    expected = c(0.81 , 0.63),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_whc4(
      A_C_OF = A_C_OF,
      A_SAND_MI = A_SAND_MI,
      A_CLAY_MI = A_CLAY_MI
    ),
    expected = c(NA_real_,NA_real_),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_whc5(
      A_SOM_LOI = A_SOM_LOI,
      A_SILT_MI = A_SILT_MI,
      A_CLAY_MI = A_CLAY_MI
    ),
    expected = c(0.42 , 0.45),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_whc6(
      A_C_OF = A_C_OF,
      A_SAND_MI = A_SAND_MI,
      A_CLAY_MI = A_CLAY_MI
    ),
    expected = c(0.45, 0.69),
    tolerance = 0.01
  )
  
  # expect_equal(
  #   sptf_whc7(
  #     A_SAND_MI = A_SAND_MI,
  #     A_CLAY_MI = A_CLAY_MI,
  #     A_SILT_MI = A_SILT_MI,
  #     D_BDS = D_BDS,
  #     A_C_OF = A_C_OF,
  #     A_DEPTH = c(.15, .15),
  #     mp_wp = c(1500, 1500),
  #     mp_fc = c(33, 33)),
  #     expected = c(,),
  #     tolerance = 0.01
  #   )
  
  expect_equal(
    sptf_whc8(
      A_C_OF = A_C_OF,
      A_SAND_MI = A_SAND_MI,
      A_CLAY_MI = A_CLAY_MI
    ),
    expected = c(0.434,0.578),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_whc9(
      A_CLAY_MI = A_CLAY_MI,
      A_SILT_MI = A_SILT_MI,
      A_C_OF = A_C_OF
    ),
    expected = c(NA_real_,NA_real_),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_whc10(
      A_C_OF = A_C_OF,
      A_CLAY_MI = A_CLAY_MI,
      A_SILT_MI = A_SILT_MI,
      A_SAND_MI = A_SAND_MI
    ),
    expected = c(NA_real_,NA_real_),
    tolerance = 0.01
  )
  
  # nb: check whole fucntion
  # expect_equal(
  #   sptf_whc13(
  #     A_SAND_MI = A_SAND_MI,
  #     A_CLAY_MI = A_CLAY_MI,
  #     D_BDS = D_BDS,
  #     A_C_OF = A_C_OF,
  #     mp_wp = 1500,
  #     mp_fc = 33),
  #     expected = c(NA, NA),
  #     tolerance = 0.01
  #   )
  
  expect_equal(
    sptf_whc14(
      A_CLAY_MI = A_CLAY_MI,
      A_SILT_MI = A_SILT_MI,
      A_SOM_LOI = A_SOM_LOI
    ),
    expected = c(0.394 , 0.468),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_whc15(
      A_CLAY_MI = A_CLAY_MI,
      A_SILT_MI = A_SILT_MI,
      A_SOM_LOI = A_SOM_LOI
    ),
    expected = c(0.42 , 0.48),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_whc16(
      A_CLAY_MI = A_CLAY_MI,
      A_SILT_MI = A_SILT_MI,
      A_SOM_LOI = A_SOM_LOI
    ),
    expected = c(0.4, 0.43),
    tolerance = 0.01
  )
  
  
})

