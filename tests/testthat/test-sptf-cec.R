# test bulk density functions
require(testthat)

test_that("cec functions returns the correct values", {
  
  
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
  
  # run all bulk density functions
  
  expect_equal(
    sptf_cec1(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(80, 175),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec2(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec3(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec4(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec5(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec6(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec7(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec8(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec9(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec10(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec11(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec12(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec13(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec14(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec15(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec16(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec17(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec18(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec19(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec20(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec21(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec22(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec23(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec24(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec25(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec26(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec27(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec28(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec29(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec30(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec31(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec32(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec33(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec34(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec35(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec36(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec37(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec38(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec39(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec40(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec41(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec42(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec43(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec44(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec45(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec46(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec47(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec48(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec49(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  
  
  expect_equal(
    sptf_cec40(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec41(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec42(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec43(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec44(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec45(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec46(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec47(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec48(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec49(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  
  
  expect_equal(
    sptf_cec50(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec51(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec52(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec53(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec54(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec55(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec56(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec57(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec58(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec59(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  
  
  expect_equal(
    sptf_cec60(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec61(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec62(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec63(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec64(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec65(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec66(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec67(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec68(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec69(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  
  
  expect_equal(
    sptf_cec70(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec71(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec72(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec73(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec74(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  expect_equal(
    sptf_cec75(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI),
    expected = c(1343.750, 1141.657),
    tolerance = 0.01
  )
  
  
  
  
  
})

