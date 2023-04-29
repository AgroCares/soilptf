# predict metal availability as function of soil properties

#' Calculate the zinc freundlich coefficient determining the adsorption rate of zinc in soils  
#'
#' This function calculates the freundlich coefficient (mol/ l n /kg) for Zn from soil properties
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#'
#' @import data.table
#' 
#' @references Romkens et al. (2004) Derivation of parition relationships to calculate Cd, Cu, Ni, Pb and Zn solubility and activity in soil solutions.
#'
#' @export
sptf_fc_zn <- function(A_SOM_LOI,A_CLAY_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_PH_WA), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 13, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # precit freundlich coefficient coefficient for zinc (R2 = 0.82, n = 1400)
  dt[,value := 10^(-4.51 + 0.39 * log10(A_SOM_LOI) + 0.35 * log10(A_CLAY_MI) + 0.45 * log(A_PH_WA))]
  
  # select value
  value <- dt[,value]
  
  # return pH value
  return(value)
  
}

#' Calculate the copper freundlich coefficient determining the adsorption rate of copper in soils  
#'
#' This function calculates the freundlich coefficient (mol/ l n /kg) for cu from soil properties
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#'
#' @import data.table
#' 
#' @references Romkens et al. (2004) Derivation of parition relationships to calculate Cd, Cu, Ni, Pb and Zn solubility and activity in soil solutions.
#'
#' @export
sptf_fc_cu <- function(A_SOM_LOI,A_CLAY_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_PH_WA), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 13, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # precit freundlich coefficient coefficient for zinc (R2 = 0.62, n = 1400)
  dt[,value := 10^(-3.55 + 0.48 * log10(A_SOM_LOI) + 0.18 * log10(A_CLAY_MI) + 0.16 * log(A_PH_WA))]
  
  # select value
  value <- dt[,value]
  
  # return pH value
  return(value)
  
}

#' Calculate the lead freundlich coefficient determining the adsorption rate of lead in soils  
#'
#' This function calculates the freundlich coefficient (mol/ l n /kg) for Pb from soil properties
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#'
#' @import data.table
#' 
#' @references Romkens et al. (2004) Derivation of parition relationships to calculate Cd, Cu, Ni, Pb and Zn solubility and activity in soil solutions.
#'
#' @export
sptf_fc_pb <- function(A_SOM_LOI,A_CLAY_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_PH_WA), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 13, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # precit freundlich coefficient coefficient for zinc (R2 = 0.57, n = 1400)
  dt[,value := 10^(-2.96 + 0.83 * log10(A_SOM_LOI) + 0.02 * log10(A_CLAY_MI) + 0.25 * log(A_PH_WA))]
  
  # select value
  value <- dt[,value]
  
  # return pH value
  return(value)
  
}

#' Calculate the cadmium freundlich coefficient determining the adsorption rate of cadmium in soils  
#'
#' This function calculates the freundlich coefficient (mol/ l n /kg) for Cd from soil properties
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#'
#' @import data.table
#' 
#' @references Romkens et al. (2004) Derivation of parition relationships to calculate Cd, Cu, Ni, Pb and Zn solubility and activity in soil solutions.
#'
#' @export
sptf_fc_cd <- function(A_SOM_LOI,A_CLAY_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_PH_WA), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 13, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # precit freundlich coefficient coefficient for zinc (R2 = 0.79, n = 1400)
  dt[,value := 10^(-4.85 + 0.58 * log10(A_SOM_LOI) + 0.28 * log10(A_CLAY_MI) + 0.27 * log(A_PH_WA))]
  
  # select value
  value <- dt[,value]
  
  # return pH value
  return(value)
  
}