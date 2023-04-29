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
sptf_tv2 <- function(A_CLAY_MI,A_SILT_MI) {
  
  # add visual bindings
  B_SOILTYPE = A_SAND_MI = NULL
  
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
  B_SOILTYPE = A_SAND_MI = NULL
  
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