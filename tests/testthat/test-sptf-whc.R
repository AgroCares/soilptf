# test bulk density functions
require(testthat)
require(euptf2)

# dynamic file location
tloc <- if(length(list.files("../testdata/"))>0){"../testdata/"} else {'tests/testdata/'}

# test for all functions of bulk density
# if not passing test it will indicate what collumn (function) name is not right


# define inputs
dt <- data.table(
  A_SOM_LOI = c(2,5),
  A_C_OF = c(20, 200),
  A_CLAY_MI = c(10, 20),
  A_SILT_MI = c(23, 10),
  A_SAND_MI = c(15, 25),
  A_DEPTH = c(0.3, 1),
  D_BDS = c(20, 30),
  topsoil = c(10, 20)
)

# function inputs that should not be added in data.table (not as vector)
mp_wp = 1500
mp_fc = 33


# run all bulk density functions
dt[, p1 := sptf_whc1(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI)]
dt[, p2 := sptf_whc2(A_C_OF = A_C_OF, A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI)] 
dt[, p3 := sptf_whc3(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI, mp_wp = mp_wp, mp_fc = mp_fc)] 
dt[, p4 := sptf_whc4(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI, D_BDS = D_BDS, A_DEPTH = 30, mp_wp = mp_wp)] 
dt[, p5 := sptf_whc5(A_SILT_MI = A_SILT_MI, A_CLAY_MI = A_CLAY_MI, D_BDS = D_BDS, A_SOM_LOI = A_SOM_LOI, topsoil = 1, mp_wp = mp_wp, mp_fc = mp_fc)] 
dt[, p6 := sptf_whc6(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI, D_BDS = D_BDS, A_C_OF = A_C_OF, mp_wp = mp_wp, mp_fc = mp_fc)] 
dt[, p7 := sptf_whc7(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, D_BDS = D_BDS, A_C_OF = A_C_OF, A_DEPTH = 30, mp_wp = mp_wp, mp_fc = mp_fc)]
dt[, p8 := sptf_whc8(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI,  D_BDS = D_BDS, A_C_OF = A_C_OF, mp_wp = mp_wp, mp_fc = mp_fc)] 
dt[, p9 := sptf_whc9(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_C_OF = A_C_OF,  mp_fc = mp_fc)] 
dt[, p10 := sptf_whc10(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_SAND_MI = A_SAND_MI, A_C_OF = A_C_OF, D_BDS = D_BDS, mp_fc = mp_fc)]
dt[, p11 := 0] #  
dt[, p12 := 0]
dt[, p13 := sptf_whc13(A_SAND_MI = A_SAND_MI, A_CLAY_MI = A_CLAY_MI, D_BDS = D_BDS, A_C_OF = A_C_OF, mp_wp = mp_wp, mp_fc = mp_fc)]
dt[, p14 := sptf_whc14(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_SOM_LOI = A_SOM_LOI, A_SAND_M50 = 150, topsoil = 1, mp_wp = mp_wp, mp_fc = mp_fc)] 
dt[, p15 := sptf_whc15(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_SOM_LOI = A_SOM_LOI, A_SAND_M50 = 150, topsoil = 1, mp_wp = mp_wp, mp_fc = mp_fc)] 
dt[, p16 := sptf_whc16(A_CLAY_MI = A_CLAY_MI, A_SILT_MI = A_SILT_MI, A_SOM_LOI = A_SOM_LOI, A_SAND_M50 = 150, mp_wp = mp_wp, mp_fc = mp_fc)]

# select only collumns with density predictions
cols <- colnames(dt)[grep('^p', names(dt))]
dt <- dt[, ..cols] 

# temp solution: 
dt[is.na(dt), ] = 0

# write to testdata
# dt |> fwrite('tests/testdata/whc.csv')

dt_expected <- fread(paste0(tloc, 'whc.csv'))

# run test_that

test_that("estimation water holding capacity", {
  expect_equal(object = dt, expected = dt_expected, tolerance = 0.01)
})





