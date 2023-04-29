# Predicting the shear strength of soil (kg cm-2).
# The shear strength of soil determines its resistance to deformation by tangential (or shear) stress. 
# Soil that has greater shear strength will have more cohesion between particles, and more friction or interlocking to prevent particles sliding over each other.
# Note that the functions here need further analysis and evaluation.

#' Calculate the soil shear strength.
#'
#' This function calculates the soil shear strength via various soil properties
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CACO3_MI (numeric) The carbonate content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Zhang et al. (2018) Estimation of surface shear strength of undisturbed soils in the eastern part of northern China’s wind erosion area
#'
#' @export
sptf_sss1 <- function(A_SOM_LOI,A_CACO3_MI) {
  
  # add visual bindings
  bd = v1 = v2 = v3 = A_C_OF = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CACO3_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (density in g/cm3)
  dt <- data.table(id = 1: arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = 0.5 * A_SOM_LOI * 10,
                   A_CACO3_MI = A_CACO3_MI)
  
  # estimate bulk density (in g/cm3)
  dt[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) * 0.001]
  
  # Estimate soil shear strength using SOM (R2 = 0.59, n = 11 soils, n = 153 samples, China, sand to loamy sand)
  dt[, v1 := 2.448 * A_SOM_LOI + 0.882]
  
  # Estimate sss using CaCO3 (R2 = 0.45, n = 11)
  dt[, v2 := 5.260 * A_CACO3_MI - 4.311]
  
  # Estimate sss using bulk density (R2 = 0.42, n =11)
  dt[, v3 := 29.132 * bd + 34.840]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select output variable
  value <- dt[,value]
  
  # return value
  return(value)
  
}

#' Calculate the soil shear strength.
#'
#' This function calculates the soil shear strength via various soil properties for soils (8-12 cm) in Denmark and Belgium 
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Schjonning (2021) Topsoil shear strength – Measurements and predictions 
#'
#' @export
sptf_sss2 <- function(A_SOM_LOI,A_CLAY_MI) {
  
  # add visual bindings
  NL = bd = pps = v1 = v2 = A_C_OF = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (clay and som in kg / kg)
  dt <- data.table(id = 1: arg.length,
                   A_C_OF = A_SOM_LOI * 10 * 0.5,
                   A_SOM_LOI = A_SOM_LOI * 0.01,
                   A_CLAY_MI = A_CLAY_MI * 0.01)
  
  # add normal load (in kPa, varying from 30-180 kPa in the experiments)
  dt[, NL := 100]
  
  # estimate bulk density (in g / cm3)
  dt[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) * 0.001]
  
  # estimate PPS, preload suction stress, calculated as the product of water saturation, S, and the numerical value of the matric potential
  dt[, pps := 67]
  
  # Estimate soil shear strength across normal loads (R2 = 0.51, n = 931)
  dt[, v1 := 46.5 + 0.60 * NL]
  
  # Estimate sss using bulk density (R2 = 0.91, n = 931), use PSS in hPa
  dt[ ,v2 := -206 + 0.594 * NL + 78* A_CLAY_MI + 0.275 * pps + 123 * bd + 963 * A_SOM_LOI]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select output variable
  value <- dt[,value]
  
  # return value tau (kPa)
  return(value)
  
}

# Predicting soil shear strength
#
#' Calculate the soil shear strength.
#'
#' This function calculates the soil shear strength via various soil properties for agricultural soils in Denmark
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Schjonning (2021) Topsoil shear strength – Measurements and predictions. Using data from Schjonning et al. (2020).
#'
#' @export
sptf_sss3 <- function(A_SOM_LOI,A_CLAY_MI) {
  
  # add visual bindings
  NL = bd = pps = A_C_OF = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (clay and som in kg / kg)
  dt <- data.table(id = 1: arg.length,
                   A_C_OF = A_SOM_LOI * 10 * 0.5,
                   A_SOM_LOI = A_SOM_LOI * 0.01,
                   A_CLAY_MI = A_CLAY_MI * 0.01,
                   value = NA_real_)
  
  # add normal load (in kPa, varying from 30-180 kPa in the experiments)
  dt[, NL := 100]
  
  # estimate bulk density (in g / cm3)
  dt[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) * 0.001]
  
  # estimate PPS, preload suction stress, calculated as the product of water saturation, S, and the numerical value of the matric potential
  dt[, pps := 67]
  
  # Estimate sss using bulk density (R2 = 0.84, n = 1651), use PSS in hPa
  dt[ ,value := -188 + 0.558 * NL + 227* A_CLAY_MI + 0.220 * pps + 114 * bd + 1315 * A_SOM_LOI]
  
  # select output variable
  value <- dt[,value]
  
  # return value tau (kPa)
  return(value)
  
}