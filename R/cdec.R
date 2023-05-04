# functions to estimate carbon decomposition

#' Calculate the carbon decomposition for peat soils
#'
#' This function calculates the decomposition of peat using first order kinetics.
#'
#' @inheritParams sptf_bd0
#' @param years (numeric) The number of years for which the carbon decomposition need to be estimated.
#'
#' @import data.table
#' 
#' @references Wosten et al. (1997) Bodemkundige vertaalfuncties bij SC-DLO; state of the art. https://edepot.wur.nl/299664
#'
#' @export
sptf_cdec1 <- function(A_C_OF, A_N_RT, years) {
  
  # Check input
  arg.length <- max(length(A_N_RT),length(A_C_OF))
  check_numeric('A_N_RT', A_N_RT, anymissing = FALSE, arg.length = arg.length)
  check_numeric('A_C_OF', A_C_OF, anymissing = FALSE, arg.length = arg.length)
  checkmate::assert_numeric(years, lower = 1, len = arg.length)

  # estimate the potential decomposition rate (Vermeulen en Hendriks, 1996)
  kpot <- 0.016 - 0.00021 * A_C_OF *1000 / A_N_RT
  
  # estimate carbon decomposition
  value <- A_C_OF * (1 - exp(-kpot * years))
  
  # return value
  return(value)
  
}

#' Calculate the carbon decomposition for mineral soils using MINIP approach
#'
#' This function calculates the decomposition of sand and peat soils using first order kinetics.
#'
#' @inheritParams sptf_bd0
#' @inheritParams sptf_cdec1
#'
#' @import data.table
#' 
#' @references Janssen (1984) A simple method for calculating decomposition and accumulation of 'young' soil organic matter.
#'
#' @export
sptf_cdec2 <- function(A_C_OF, years) {
  
  # add visual bindings
  cor_temp = temp = NULL
  
  # Check input
  check_numeric('A_C_OF', A_C_OF, anymissing = FALSE)
  checkmate::assert_numeric(years, lower = 1)
  
  # combine arguments in internal table
  dt <- data.table(A_C_OF = A_C_OF,temp = 12)
  
  # add correction factor for annual temperature
  dt[, cor_temp := ifelse(temp<=-1,0,ifelse(temp<=9,0.1*(temp+1),ifelse(temp<=27,2^((temp-9)/9),4)))]
  
  # estimate C decline via MINIP for other crops, 0-20 cm depth
  dt[, value := A_C_OF * (1-exp(4.7*(((17+cor_temp*years)^-0.6)-(17^-0.6))))]
  
  # select value
  value <- dt[,value]
  
  # return value
  return(value)
  
}

