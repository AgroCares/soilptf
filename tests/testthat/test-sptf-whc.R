# test bulk density functions
require(testthat)
# require(euptf2)


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
  expected = c(0.19789  , 0.52965),
  tolerance = 0.01
)

expect_equal(
  sptf_whc2(
    A_C_OF = A_C_OF,
    A_SAND_MI = A_SAND_MI,
    A_CLAY_MI = A_CLAY_MI
  ),
  expected = c(0.21108  , 0.30864),
  tolerance = 0.01
)

expect_equal(
  sptf_whc3(
    A_SAND_MI = A_SAND_MI,
    A_CLAY_MI = A_CLAY_MI,
    mp_wp = 1500,
    mp_fc = 33
  ),
  expected = c(0.1972271 , 0.1654625),
  tolerance = 0.01
)

expect_equal(
  sptf_whc4(
    A_SAND_MI = A_SAND_MI,
    A_CLAY_MI = A_CLAY_MI,
    D_BDS = D_BDS,
    A_DEPTH = c(.16, .16),
    mp_wp = 1500
  ),
  expected = c(159.2110, 146.5118),
  tolerance = 0.01
)

expect_equal(
  sptf_whc5(
    A_SILT_MI = A_SILT_MI,
    A_CLAY_MI = A_CLAY_MI,
    D_BDS = D_BDS,
    A_SOM_LOI = A_SOM_LOI,
    topsoil = 1,
    mp_wp = 1500,
    mp_fc = 33
  ),
  expected = c(0 , 0),
  tolerance = 0.01
)

expect_equal(
  sptf_whc6(
    A_SAND_MI = A_SAND_MI,
    A_CLAY_MI = A_CLAY_MI,
    D_BDS = D_BDS,
    A_C_OF = A_C_OF,
    mp_wp = 1500,
    mp_fc = 33
  ),
  expected = c(0, 0),
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
    A_SAND_MI = A_SAND_MI,
    A_CLAY_MI = A_CLAY_MI,
    D_BDS = D_BDS,
    A_C_OF = A_C_OF,
    mp_wp =  1500,
    mp_fc =  33
  ),
  expected = c(-79.39869,-71.52744),
  tolerance = 0.01
)

expect_equal(
  sptf_whc9(
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    A_C_OF = A_C_OF,
    mp_fc =  33
  ),
  expected = c(0.09564 , 0.06056),
  tolerance = 0.01
)

expect_equal(
  sptf_whc10(
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    A_SAND_MI = A_SAND_MI,
    A_C_OF = A_C_OF,
    D_BDS = D_BDS,
    mp_fc = 33
  ),
  expected = c(0.2158 , 0.4356),
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
    A_SOM_LOI = A_SOM_LOI,
    A_SAND_M50 = A_SAND_M50,
    topsoil = 1,
    mp_wp = 1500,
    mp_fc = 33
  ),
  expected = c(0.1162968 , 0.1336105),
  tolerance = 0.01
)

expect_equal(
  sptf_whc15(
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    A_SOM_LOI = A_SOM_LOI,
    A_SAND_M50 = A_SAND_M50,
    topsoil = 1,
    mp_wp = 1500,
    mp_fc = 33
  ),
  expected = c(0.1202259 , 0.1384599),
  tolerance = 0.01
)

expect_equal(
  sptf_whc16(
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    A_SOM_LOI = A_SOM_LOI,
    A_SAND_M50 = A_SAND_M50,
    mp_wp = 1500,
    mp_fc = 33
  ),
  expected = c(0.1499688, 0.2139534),
  tolerance = 0.01
)
