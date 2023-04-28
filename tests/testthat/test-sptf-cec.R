# test bulk density functions
require(testthat)

test_that("cec functions returns the correct values", {
  
  
  # Define test data
  A_SOM_LOI <- c(20, 5)
  A_CLAY_MI <- c(10, 20)
  A_SAND_M50 <- c(150, 200)
  A_PH_KCL <- c(6, 7.5)
  A_PH_WA <- c(6, 7.5)
  A_PH_CC <- c(6, 7.5)
  A_CN_FR <- c(12, 13)
  A_SILT_MI <- c(23, 10)
  A_C_OF <- c(500, 200)
  A_SAND_MI <- c(15, 25)
  A_DEPTH <- c(0.3, 1)
  D_BDS <- c(1200, 1100)
  topsoil <- c(10, 20)
  A_CACO3_MI = c(3, 7)
  B_LU_PTFCLASS <- c('cropland', 'grassland')
  B_SOILCLASS_USDA <- c('alfisol','andisol')
  
  # run all bulk density functions
  
  expect_equal(
    sptf_cec1(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(350, 175),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec2(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_KCL = A_PH_KCL),
    expected = c(-332.00  , -13.75),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec3(A_CLAY_MI = A_CLAY_MI, A_C_OF = A_C_OF, A_PH_CC = A_PH_CC),
    expected = c(525.92     , NA),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec4(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC),
    expected = c(1291.4  , 773.0),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec5(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(421.9560 , 149.5771),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec6(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_CC = A_PH_CC),
    expected = c(542.25 , 220.00),
    tolerance = 0.01
  )
  
  # add later: test for sandy soils in NL
  expect_equal(
    sptf_cec7(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI =A_CLAY_MI, A_PH_CC = A_PH_CC, B_LU_PTFCLASS =  B_LU_PTFCLASS),
    expected = c(NA_real_, NA_real_),
    tolerance = 0.01
  )
  
  # add later: test for peaty soils in N L
  expect_equal(
    sptf_cec8(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC, A_CN_FR = A_CN_FR, B_LU_PTFCLASS = B_LU_PTFCLASS),
    expected = c(NA_real_, NA_real_),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec9(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_CC =A_PH_CC, B_LU_PTFCLASS =  B_LU_PTFCLASS),
    expected = c(317.9960 , 227.5841),
    tolerance = 0.01
  )
  
  # add later: forest topsoils (0-30cm) in Switzerland
  expect_equal(
    sptf_cec10(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC, B_LU_PTFCLASS = B_LU_PTFCLASS),
    expected = c(NA_real_, NA_real_),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec11(A_C_OF = A_C_OF, A_CLAY_MI =A_CLAY_MI, A_PH_CC = A_PH_CC, B_LU_PTFCLASS =B_LU_PTFCLASS  ),
    expected = c(NA_real_, NA_real_),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec12(A_C_OF = A_C_OF, B_LU_PTFCLASS =B_LU_PTFCLASS ),
    expected = c(NA_real_, NA_real_),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec13(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(190.50 , 143.45),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec14(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(505.5333 , 265.5667),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec15(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(631.0 , 229.5),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec16(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_WA =A_PH_WA ),
    expected = c(251.5377, 153.8962),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec17(A_SOM_LOI = A_SOM_LOI, B_LU_PTFCLASS = B_LU_PTFCLASS),
    expected = c(NA_real_, 122.1),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec18(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(410.2 , 221.7),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec19(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(1887.4  , 853.4),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec20(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA),
    expected = c(1124.336  , 494.459),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec21(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI),
    expected = c(187.3 , 145.0),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec22(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI),
    expected = c(258.28452  , 50.42727),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec23(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC),
    expected = c(NA_real_, NA_real_),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec24(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC),
    expected = c(1106.70  , 597.88),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec25(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC),
    expected = c(105.7188 , 190.4850),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec26(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA, B_SOILCLASS_USDA = B_SOILCLASS_USDA),
    expected = c(142.287 , 4306475.818), # grazy high number: add max
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec27(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_CLIM_CAT1 = c('LLH', 'HLH')),
    expected = c(331.3 , 114.1),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec28(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_CC = A_PH_CC),
    expected = c(636.8 , 269.0),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec29(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI,A_CACO3_MI = A_CACO3_MI, A_PH_CC = A_PH_CC),
    expected = c(467.6 , 190.1),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec30(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(495.6 , 205.1),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec31(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, B_SOILCLASS_USDA = B_SOILCLASS_USDA),
    expected = c(285.0 , 201.4),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec32(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_CC = A_PH_CC),
    expected = c(605.0 , 264.5),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec33(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC),
    expected = c(1765.20  , 814.05),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec34(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC, B_LU_PTFCLASS = B_LU_PTFCLASS),
    expected = c(NA_real_, NA_real_),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec35(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC),
    expected = c(835.5 , 500.5),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec36(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(1887  , 806),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec37(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(1853.95, 838.75),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec38(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(500.00 , 222.25),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec39(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(1770  , 788),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec40(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(1050  , 500),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec41(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(382.6667 , 201.2500),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec42(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI),
    expected = c(430008.2013, 832.6731),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec43(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_CC = A_PH_CC),
    expected = c(461.230 , 264.665),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec44(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_CC = A_PH_CC),
    expected = c(703.1500 , 388.4925),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec45(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA),
    expected = c(804.40 , 277.75),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec46(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(4844, 2008),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec47(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_PTFCLASS = B_LU_PTFCLASS),
    expected = c(154.1  , 90.6),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec48(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(204.8 , 411.8),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec49(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI),
    expected = c(434.075 , 297.435),
    tolerance = 0.01
  )
  
  
  expect_equal(
    sptf_cec40(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(1050  , 500),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec41(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(382.6667 , 201.2500),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec42(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI),
    expected = c(430008.2013    , 832.6731),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec43(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_CC = A_PH_CC),
    expected = c(461.230 , 264.665),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec44(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_CC = A_PH_CC),
    expected = c(703.1500, 388.4925),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec45(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA),
    expected = c(804.40 , 277.75),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec46(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(4844 , 2008),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec47(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, B_LU_PTFCLASS = B_LU_PTFCLASS),
    expected = c(154.1, 90.6),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec48(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(204.8 , 411.8),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec49(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI),
    expected = c(434.075 , 297.435),
    tolerance = 0.01
  )
  
  
  expect_equal(
    sptf_cec50(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_CACO3_MI = A_CACO3_MI, A_PH_CC = A_PH_CC),
    expected = c(317.13 , 331.37),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec51(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA),
    expected = c(-75.70 , 109.85),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec52(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA),
    expected = c(426.37, 382.24),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec53(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA),
    expected = c(519.9941 , 232.5820),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec54(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA, B_LU_PTFCLASS = B_LU_PTFCLASS),
    expected = c(385.420 , 240.625),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec55(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA),
    expected = c(159.5004 , 218.3972),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec56(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA),
    expected = c(2584.3000  , 788.2167),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec57(A_SOM_LOI = A_SOM_LOI, A_SAND_MI = A_SAND_MI, A_PH_WA = A_PH_WA),
    expected = c(83.025 , 110.025),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec58(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_CACO3_MI = A_CACO3_MI),
    expected = c(82.09 , 133.00),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec59(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(177.15 , 233.55),
    tolerance = 0.01
  )
  
  expect_equal(
    sptf_cec60(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(1067  , 528),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec61(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(2133.1  , 941.1),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec62(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_CACO3_MI = A_CACO3_MI),
    expected = c(245.3428 , 178.3788),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec63(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(303.9576 , 313.1119),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec64(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(870.4851 , 569.7069),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec65(A_C_OF = A_C_OF),
    expected = c(4439.3 , 1823.3),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec66(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI =  A_CLAY_MI),
    expected = c(774.6 , 354.6),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec67(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(2030.9  , 874.7),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec68(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(1138  , 508),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec69(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(3421.7 , 1441.7),
    tolerance = 0.01
  )
  
  
  expect_equal(
    sptf_cec70(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(1484.7  , 682.8),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec71(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI),
    expected = c(3504.8 , 1464.3),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec72(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_WA = A_PH_WA),
    expected = c(92.37322 , 105.82678),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec73(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA),
    expected = c(237.435 , 142.500),
    tolerance = 0.01
  )
  # expect_equal(
  #   sptf_cec74(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_PH_WA = A_PH_WA,B_SOILCLASS_USDA = B_SOILCLASS_USDA),
  #   expected = c(1343.750, 1141.657),
  #   tolerance = 0.01
  # )
  expect_equal(
    sptf_cec75(A_C_OF = A_C_OF, A_CLAY_MI = A_CLAY_MI, A_PH_CC = A_PH_CC),
    expected = c(1589.20  , 728.75),
    tolerance = 0.01
  )
  
  
  
  
  
})

