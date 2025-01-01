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
sptf_ef1 <- function(A_AMWD_DM) {
  
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
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Colazo & Buschiazzo (2010) Soil dry aggregate stability and wind erodible fraction in a semiarid environment of Argentina
#'
#' @export
sptf_ef2 <- function(A_CLAY_MI,A_SILT_MI,A_C_OF) {
  
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

#' Calculate the wind erodible fraction (ef)
#'
#' This function calculates the erodible fraction in Argentina
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Colazo & Buschiazzo (2010) Soil dry aggregate stability and wind erodible fraction in a semiarid environment of Argentina
#'
#' @export
sptf_ef3 <- function(A_CLAY_MI,A_SILT_MI,A_C_OF) {
  
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
  dt[, v4 := 0.859 * A_CLAY_MI^0.75]
  
  # estimate 4 and 5 based on sand and clay ratio (R2 0.87 - 0.94)
  dt[, v5 := 8.3 + 13 * log (A_SAND_MI / A_CLAY_MI)]
  dt[, v6 := 12 + 1.9 * A_SAND_MI / A_CLAY_MI]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3','v4','v5','v6'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select output variable
  value <- dt[,value]
  
  # return erodible fraction(%)
  return(value)
  
}

#' Calculate the wind erodible fraction (-)
#'
#' This function calculates the erodible fraction in Argentina(main) and Spain
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' @references reference López, M., De Dios Herrero, J. M., Hevia, G., Gracia, R., & Buschiazzo, D. E. (2007). Determination of the wind-erodible fraction of soils using different methodologies. Geoderma, 139(3–4), 407–411. https://doi.org/10.1016/j.geoderma.2007.03.006
#'
#' @export
sptf_ef4 <- function(A_CLAY_MI, A_SAND_MI, A_SOM_LOI) {
  
  # add visual bindings
  v1 = v2 = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI), length(A_SOM_LOI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (14 sites, cultivated and uncultivated)
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI * 10, #convert from % to g/kg
                   A_SAND_MI = A_SAND_MI * 10, #convert from % to g/kg
                   A_SOM_LOI = A_SOM_LOI * 10, #convert from % to g/kg
                   value = NA_real_)
  
  # estimate EF obtained with the rotary sieve(v1) and flat sieve(v2) (%)
  dt[, v1 := 9.98 + 6.91 * A_SAND_MI/A_CLAY_MI + 14.1 / A_SOM_LOI]
  dt[, v2 := 4.77 + 7.43 * A_SAND_MI/A_CLAY_MI + 27.6 / A_SOM_LOI]
  dt[, value := (v1+v2)/2]
  
  # select output variable
  value <- dt[,value]
  
  # return erodible fraction
  return(value)
  
}

#' Calculate the wind erodible fraction (-)
#'
#' This function calculates the erodible fraction in U.S.
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' @references Fryrear, D. W., Krammes, C. A., Williamson, D. L., & Zobeck, T. M. (1994). Computing the wind erodible fraction of soils. Journal of Soil and Water Conservation, 49(2), 183-188.
#' @export
sptf_ef5 <- function(A_CLAY_MI, A_SAND_MI, A_SILT_MI, A_SOM_LOI, A_CACO3_IF) {
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI), length(A_SILT_MI), length(A_SOM_LOI), length(A_CACO3_IF))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_IF, lower = 0, upper = 50, len = arg.length)
  
  # make internal data.table (14 sites, cultivated and uncultivated)
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CACO3_IF = A_CACO3_IF)
  
  # estimate erodible fraction in %
  dt[, value := 29.09 + 0.31 * A_SAND_MI + 0.17 * A_SILT_MI + 0.33 * A_SAND_MI/A_CLAY_MI - 2.59 * A_SOM_LOI - 0.95 * A_CACO3_IF]
  
  # select output variable
  value <- dt[,value]
  
  # return 
  return(value)
  
}

#' Calculate the Erodible fraction(EF)
#'
#' Calculate the erodible fraction for agricultural soils in Fars province, which is located in the south-central region of Iran
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Mina, M., Rezaei, M., Sameni, A., Riksen, M., & Ritsema, C. (2023). Estimating the indices of soil erodibility to wind erosion using pedo- and spectro-transfer functions in calcareous soils. Geoderma, 438, 116612. https://doi.org/10.1016/j.geoderma.2023.116612
#'
#' @export
sptf_ef6 <- function(A_SOM_LOI, A_CLAY_MI, A_C_OF) {
  
  # add visual binding
  B_SOILTYPE =  A_SAND_MI = PR = SS = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI),length(A_CLAY_MI), length(A_C_OF))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (SOC and mineralogy in g/kg)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_C_OF = A_C_OF, 
                   value = NA_real_)
  
  #predict shear strength(kPa)
  dt[, SS := sptf_sss3(A_SOM_LOI,A_CLAY_MI)]
  #convert to kg/cm2
  dt[, SS := SS * 0.0102]
  # estimate penetration (MPa)
  #dt[, PR := pr1(A_C_OF, A_CLAY_MI)]
  # using maximum value (kg/cm2)
  dt[, PR := 5]
  
  # estimate erodible fraction. 
  dt[, value := 98.7 - 6.94 * SS - 4.33 * A_SOM_LOI - 4.13 * PR - 0.09 * A_CLAY_MI]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Erodible fraction(EF)
#'
#' Calculate the erodible fraction for Kassala State in Sudan
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Hassan, A. A., & Mustafa, M. A. (2011). Assessment and mapping of wind erodibility of Aridisols and Entisols in the river Nile State, Sudan. In The 5th Annual Conference-Agricultural and Veterinary Research-February 2014. University of Khartoum
#'
#' @export
sptf_ef7 <- function(A_SOM_LOI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   value = NA_real_)
  
  # estimate EF 
  dt[, value := -22.809 * A_SOM_LOI ^ 2 + 69.59 * A_SOM_LOI + 9.4731]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}
# see also rakkar_2019