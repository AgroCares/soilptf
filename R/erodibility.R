# wind erodibility

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
  v1 = v2 = v3 = v4 = v5 = v6 = A_SAND_MI = NULL
  
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

# see also rakkar_2019