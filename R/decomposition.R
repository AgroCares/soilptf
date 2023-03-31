# functions to estimate carbon decomposition

#' Calculate the carbon decomposition for peat soils
#'
#' This function calculates the decomposition of peat using first order kinetics.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_N_RT (numeric) The nitrogen content of the soil (mg / kg).
#' @param years (numeric) The years for which the carbon decomposition need to be estimated.
#'
#' @import data.table
#' 
#' @references Wosten et al. (1997) Bodemkundige vertaalfuncties bij SC-DLO; state of the art. https://edepot.wur.nl/299664
#'
#' @export
sptf_cdec <- function(A_C_OF, A_N_RT, years) {
  
  # Check input
  arg.length <- max(length(A_N_RT),length(A_C_OF))
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 10000, len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  
  # estimate the potential decomposition rate (Vermeulen en Hendriks, 1996)
  kpot <- 0.016 - 0.00021 * A_C_OF *1000 / A_N_RT
  
  # estimate carbon decomposition
  value <- A_C_OF * (1 - exp(-kpot * years))
  
  # return value
  return(value)
  
}
