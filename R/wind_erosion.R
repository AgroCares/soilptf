# wind erodibility

# Predicting the shear strength of soil (kg cm-2).
# The shear strength of soil determines its resistance to deformation by tangential (or shear) stress. 
# Soil that has greater shear strength will have more cohesion between particles, and more friction or interlocking to prevent particles sliding over each other.

#' Predicting threshold velocity (m / sec)
#'
#' Calculate the threshold velocity for wind erosion for soils in Iran.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_AMWD_DM (numeric) The aggregate mean weight diameter in mm (dry aggregate stability).
#'
#' @import data.table
#' 
#' @references Rezaei et al. (2022) Determination of the threshold velocity of soil wind erosion using a wind tunnel and its prediction for calcareous soils of Iran
#'
#' @export
sptf_tv1 <- function(A_SOM_LOI,A_CLAY_MI,A_AMWD_DM = NA_real_) {
  
  # add visual bindings
  v1 = v2 = v3 = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_AMWD_DM))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_AMWD_DM, len = arg.length)
  
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_AMWD_DM = A_AMWD_DM)
  
  # replace missing AMWD with mean value
  dt[is.na(A_AMWD_DM), A_AMWD_DM := 0.63]
  
  # function to estimate threshold velocity (m/s) in Iran, calibrated n= 50, validated n = 22, 0-3 cm depth.
  
  # ptf1 (R2 = 0.42, n = 22)
  dt[,v1 := 2.73 + 0.23 * A_CLAY_MI]
  
  # ptf2 (R2 = 0.60, n = 22)
  dt[,v2 := 0.084 + 12.35 * A_AMWD_DM]
  
  # ptf3 (R2 = 0.60, n = 22)
  dt[,v3 := 0.027 + 11.75 * A_AMWD_DM + 0.45 * A_SOM_LOI]
 
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Predicting threshold velocity (m / sec)
#'
#' Calculate the threshold velocity for soils sensitive to wind erosion in Hungary
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references unknown
#'
#' @export
sptf_tv2 <- function(A_CLAY_MI,A_SAND_MI) {
  
  # add visual bindings
  B_SOILTYPE = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = 100-A_CLAY_MI - A_SILT_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # add USDA soil classification
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # function to estimate threshold velocity (m/s) in Hungary (n = 215, 0-20 cm soil layer)
  dt[B_SOILTYPE == 'loam', value := 9.7]
  dt[B_SOILTYPE == 'silt loam', value := 10]
  dt[B_SOILTYPE == 'clay loam', value := 11]
  dt[B_SOILTYPE == 'silty clay loam', value := 10.2]
  dt[B_SOILTYPE == 'silty clay', value := 11.5]
  dt[B_SOILTYPE == 'clay', value := 12]
  dt[B_SOILTYPE == 'sandy clay loam', value := 9.8]
  dt[B_SOILTYPE == 'sandy loam', value := 8.7]
  dt[B_SOILTYPE == 'loamy sand', value := 7.3]
  dt[B_SOILTYPE == 'sand', value := 6.5]
  dt[B_SOILTYPE == 'sandy clay', value := 10]
  dt[B_SOILTYPE == 'silt', value := 10.5]
  
  # select output variable
  value <- dt[,value]
  
  # return threshold wind velocity (m / s)
  return(value)
  
}

#' Predicting threshold velocity (m / sec)
#'
#' Calculate the threshold velocity for wind erosion for agicultural soils in Hungary
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Negyesi et al. (2016) Effect of soil parameters on the threshold wind velocity and maximum eroded mass in a dry environment
#'
#' @export
sptf_tv3 <- function(A_CLAY_MI,A_SILT_MI) {
  
  # add visual bindings
  B_SOILTYPE = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = 100-A_CLAY_MI - A_SILT_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # add USDA soil classification
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # function to estimate threshold velocity (m/s) in Hungary (n = 80, 0-20 cm soil layer,R2 = 0.72)
  # A_SILT_MI is in this case a proxy for 0.05-0.02 mm  (its coarse fraction of silt)
  dt[, value := 2.263 + 0.18 * sqrt(A_SILT_MI)]

  # select output variable
  value <- dt[,value]
  
  # return threshold wind velocity (m / s)
  return(value)
  
}


#' Calculate the wind erodibility as a function of MWD.
#'
#' This function calculates the wind erodibility as a function of MWD for soils (0-20cm) in Iran, being the slope of wind erosivity plotted to erosion rate.
#'
#' @param A_AMWD_DM (numeric) The aggregate mean weight diameter in mm (dry aggregate stability).
#'
#' @import data.table
#' 
#' @references Zamani & Mahmoodabadi (2013) Effect of particle-size distribution on wind erosion rate and soil erodibility
#'
#' @export
sptf_erodibility1 <- function(A_AMWD_DM) {
  
  # Check input
  checkmate::assert_numeric(A_AMWD_DM)
  
  # make internal data.table
  dt <- data.table(A_AMWD_DM = A_AMWD_DM)
  
  # ptf1 (R2 = 0.998, n = 3)
  dt[,value := 3.3816 * A_AMWD_DM^-1.7319]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the wind erodible fraction (-)
#'
#' This function calculates the erodible fraction in Argentina
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' 
#' @import data.table
#' 
#' @references Colazo & Buschiazzo (2010) Soil dry aggregate stability and wind erodible fraction in a semiarid environment of Argentina
#'
#' @export
sptf_erodibility2 <- function(A_CLAY_MI,A_SILT_MI,A_C_OF) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = v5 = v6 = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_C_OF),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)

  # make internal data.table (14 sites, cultivated and uncultivated)
  dt <- data.table(id = 1:arg.length,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_SILT_MI = A_SILT_MI)
  
  # estimate 1 and 2 based on OC (R2 ns and 0.86)
  dt[, v1 := 11.4 * A_C_OF^(4.2/A_C_OF)]
  dt[, v2 := 17.7 * A_C_OF^(4.1/A_C_OF)]
  
  # estimate 2 and 3 based on clay content (R2 0.87 - 0.91)
  dt[, v3 := 140 - 23 * log(A_CLAY_MI)]
  dt[, v4 := 859 * A_CLAY_MI^0.75]
  
  # estimate 4 and 5 based on sand and clay ratio (R2 0.87 - 0.94)
  dt[, v5 := 8.3 + 13 * log (A_SAND_MI / A_CLAY_MI)]
  dt[, v6 := 12 + 1.9 * A_SAND_MI / A_CLAY_MI]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3','v4','v5','v6'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select output variable
  value <- dt[,value]
  
  # return threshold wind velocity (m / s)
  return(value)
  
}

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
  bd = v1 = v2 = v3 = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_AMWD_DM))
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
  NL = bd = pps = v1 = v2 = NULL
  
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
  NL = bd = pps = NULL
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