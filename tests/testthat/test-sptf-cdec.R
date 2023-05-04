# test carbon decomposition estimation functions
test_that('Carbon decomposition functions work',{
  # fabricate test data
  dt <- data.table(
    A_N_RT = round(
      c(
        seq(from = 50,
            to = 10000,
            length.out = 5),
        seq(from = 10000,
            to = 50,
            length.out = 5)
        )),
    A_C_OF = round(
      seq(from = 1,
          to = 250,
          length = 5)
    )
    )
  
  expect_equal(
    sptf_cdec1(A_C_OF = dt$A_C_OF, A_N_RT = dt$A_N_RT, year = rep(100,10)),
    expected = c(6.927213e-01,  4.157815e+01,  8.292904e+01,  1.238001e+02,
                 1.646756e+02,  7.976791e-01,  4.783101e+01 , 8.292904e+01 ,
                 8.172597e+00 ,-2.013675e+47),
    tolerance = 0.1
  )
  
  expect_equal(
    sptf_cdec2(A_C_OF = dt$A_C_OF, year = rep(100,10)),
               expected = c(0.4617284,  29.0888906,  58.1777811,  86.8049433, 
                            115.4321054,   0.4617284,  29.0888906 , 58.1777811,
                            86.8049433, 115.4321054),
    tolerance = 0.1
  )
})


