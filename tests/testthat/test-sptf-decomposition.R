# test bulk density functions
require(testthat)

test_that("decomposition functions returns the correct values", {
  
  
  # Define test data
  A_C_OF <- c(50, 20)
  A_N_RT <- c(1500, 2500)
  years <- c(5, 25)
  
  # run all bulk density functions
  
  expect_equal(
    sptf_cdec1(A_C_OF = A_C_OF, A_N_RT = A_N_RT, years = years),
    expected = c(2.2, 6.02),
    tolerance = 0.01
  )
  
  # expect_equal(
  #   sptf_cdec2(A_C_OF = A_C_OF, years = years),
  #   expected = c(350, 175),
  #   tolerance = 0.01
  # )

  
  
  
})

