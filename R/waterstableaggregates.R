# Aggregate Stability

#' Calculate the percentage water stable aggregates given the pedotransferfunction of Stengel et al. (1984)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Stengel et al. (1984).Factors influencing the variation of some properties of soils in relation to their suitability for direct drilling.
#'
#' @export
sptf_wsa1 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   value = NA_real_)
  
  # estimate percentage of water stable aggregates (R2 = 0.61)
  dt[, value := 11.57 * A_C_OF + 12.75]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the percentage water stable aggregates given the pedotransferfunction of Ekwue (1990)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Ekwue (1990).Organic matter effects on soil strength properties.
#'
#' @export
sptf_wsa2 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   value = NA_real_)
  
  # estimate percentage of water stable aggregates(>0.5 mm) (n = 14, R2 = 0.87) on sandy grassland soils
  dt[, value := 3.32 * A_C_OF -1.44]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of Gulser (2018)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_K_AA (numeric) The potassium level in soil, extrated via ammonium acetate (mg/kg)
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#' 
#' @import data.table
#' 
#' @references Gulser (2018) Predicting Aggregate Stability of Cultivated Soils
#'
#' @export
sptf_wsa3 <- function(A_SOM_LOI,A_CLAY_MI,A_SILT_MI,A_K_AA,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_K_AA),length(A_PH_WA))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_K_AA, lower = 0, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
                            
  # Collect data into a table, potassium in cmol / kg
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_K_AA = A_K_AA *0.1 / 39.0983,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # replace missing K with mean value
  dt[is.na(A_K_AA), A_K_AA := 0.59]
  
  # set mean EC value (dS/m)
  dt[, EC := 0.65]
  
  # Estimate WSA (%)
  dt[, value := 17.996 + 1.57 * A_CLAY_MI - 7.346 * A_PH_WA + 0.494 * A_SAND_MI +4.837 * A_SOM_LOI - 7.965 * A_K_AA + 3.805 * EC]
  
  # return value (%)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

# Predicting the percentage water stable aggregates
#
#' Calculate the percentage water stable aggregates (%)
#'
#' This function calculates theerodible fraction in Argentina
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' 
#' @import data.table
#' 
#' @references Colazo & Buschiazzo (2010) Soil dry aggregate stability and wind erodible fraction in a semiarid environmentvof Argentina
#'
#' @export
sptf_wsa4 <- function(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_C_OF) {
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI)
  
  # estimate 1 and 2 based on OC (R2 0.65 and 0.96)
  dt[,v1 := 85/(1+357 * exp(1.5 * A_C_OF))]
  dt[,v2 := 90/(1+2.9 * exp(0.5 * A_C_OF))]
  
  # estimate 2 and 3 based on clay content (R2 0.93 - 0.92)
  dt[, v3 := 85 * (1.1 - exp(0.015 * A_CLAY_MI))]
  dt[, v4 := 161 * (0.6 - exp(0.03 * A_CLAY_MI))]
  
  # estimate 4 and 5 based on sand and clay ratio (R2 0.96 - 0.96)
  dt[, v5 := 98 + 2.8 * A_SAND_MI / A_CLAY_MI]
  dt[, v6 := 89 + 0.8 * A_SAND_MI / A_CLAY_MI - 0.2 * (A_SAND_MI/A_CLAY_MI)^2]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3','v4','v5','v6'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select output variable
  value <- dt[,value]
  
  # return percentage water stable aggregates (%)
  return(value)
  
}

# Predicting the percentage water stable aggregates
#
#' Calculate the percentage water stable aggregates (%)
#'
#' This function calculates the percentage water stable aggregates for alluvial soils in southern Ohio
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SOM_LOI (numeric) The soil organic matter content (\%).
#' 
#' @import data.table
#' 
#' @references Salchow et al. (1996) Pedotransfer functions for variable alluvial soils in southern Ohio.
#'
#' @export
sptf_wsa5 <- function(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI) {
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(A_SILT_MI),length(A_SOM_LOI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_SOM_LOI * 0.5 * 10,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI)
  
  # estimate bulk density (in g / cm3)
  dt[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) * 0.001]
  
  # derive UDS soil classification
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # estimate percentage of water stable aggregates (%) (R2 = 0.38)
  dt[, value := 0.6867 * A_SAND_MI + 0.3447 * A_SILT_MI + 0.6649 * A_CLAY_MI + 11.911 * A_SOM_LOI -25.50 * bd]
  
  # overwrite averaged estimate with soil type specific ones (R2 = 0.31,0.48,0.10,0.48)
  dt[B_SOILTYPE == 'silty clay loam', value := 1.885 * A_SAND_MI -0.4838 * A_SILT_MI + 1.445 * A_CLAY_MI + 13.588 * A_SOM_LOI -20.30 * bd]
  dt[B_SOILTYPE == 'silty loam', value := 0.7156 * A_SAND_MI +0.6106 * A_SILT_MI + 0.9397 * A_CLAY_MI + 12.568 * A_SOM_LOI -42.78 * bd]
  dt[B_SOILTYPE == 'loam', value := 0.5579 * A_SAND_MI +0.5757 * A_SILT_MI + 0.578 * A_CLAY_MI -2.096 * A_SOM_LOI -6.43 * bd]
  dt[B_SOILTYPE == 'sandy loam', value := -0.3474 * A_SAND_MI -0.0045 * A_SILT_MI -1.357 * A_CLAY_MI +24.39 * A_SOM_LOI +19.05 * bd]
  
  # select output variable
  value <- dt[,value]
  
  # return percentage water stable aggregates (%)
  return(value)
  
}

#' Calculate the water stable aggragates given the pedotransferfunction of le Bissonnais et al. (2007)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_CACO3_MI (numeric) The calcium carbonate content of the soil (\%)
#' 
#' @import data.table
#' 
#' @references le Bissonnais et al. (2007) Erodibility of Mediterranean vineyard soils: relevant aggregate stability methods and significant soil variables
#'
#' @export
sptf_wsa6 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI,A_CACO3_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI),length(A_CACO3_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 15, len = arg.length)
  
  # Collect data into a table (units in g/kg)
  dt <- data.table(id = 1:arg.length,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SILT_MI = A_SILT_MI * 10,
                   A_CACO3_MI = A_CACO3_MI * 10,
                   value = NA_real_)
  
  # estimate Mean Weight Diamater (r = 0.91, n = 68)
  dt[, fsoc := 35.6 + 52.86/(1 + 4.14e5 * exp(-0.67 * A_C_OF))]
  dt[, value := -27.56 + 0.98 * fsoc + 0.41 * (A_CLAY_MI + A_SILT_MI) + 0.13 * A_CACO3_MI ]
  
  # return value (%)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of Chaney and Swift (1984)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Chaney and Swift (1984). The influence of organic matter on aggregate stability in some British soils
#'
#' @export
sptf_mwd1 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table (SOC in %)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   value = NA_real_)
  
  # estimate Mean Weight Diamater (n = 120)
  dt[, value := (A_SOM_LOI + 24) * 31]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of Haynes et al. (1991)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Haynes et al. (1991) Influence of mixed cropping rotations (pasture-arable) on organic matter content, water-stable aggregation and clod porosity in a group of soils
#'
#' @export
sptf_mwd2 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   value = NA_real_)
  
  # Estimate USDA soil type
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # estimate Mean Weight Diamater
  dt[B_SOILTYPE =='sandy loam', value := 0.60 * A_C_OF + 0.65]
  dt[B_SOILTYPE == 'silt loam', value := 1.09 * A_C_OF - 0.86]
  dt[B_SOILTYPE == 'clay loam', value := 0.62 * A_C_OF + 0.27]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of Saidi et al. (2015)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CEC_CO (numeric) The Cation Exchange Capacity (mmol+/kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#' @param A_CACO3_MI (numeric) The calcium carbonate content of the soil (\%)
#' 
#' @import data.table
#' 
#' @references Saidi et al. (2015). 
#'
#' @export
sptf_mwd3 <- function(A_C_OF,A_CEC_CO,A_CLAY_MI,A_SILT_MI, A_PH_WA, A_CACO3_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CEC_CO),length(A_CLAY_MI),length(A_SILT_MI),length(A_PH_WA),length(A_CACO3_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 0, upper = 600, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 15, len = arg.length)
  
  # Collect data into a table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_SOM_LOI = A_C_OF * 0.1 * 2,
                   A_CEC_CO = A_CEC_CO,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_WA = A_PH_WA, 
                   A_CACO3_MI = A_CACO3_MI,
                   value = NA_real_)
  
  # Estimate USDA soil type
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # estimate Mean Weight Diamater in mm (R2 = 0.67 ~ 0.71 ~ 0.91 ~ 0.94)
  dt[grepl('sand',B_SOILTYPE), value := 0.017 * A_C_OF + 0.004 * A_SILT_MI + 0.002 * A_PH_WA + 0.018 * A_CACO3_MI + 0.14 * A_SOM_LOI + 0.0006 * A_CEC_CO + 0.052]
  dt[grepl('silt',B_SOILTYPE), value := 0.007 * A_SILT_MI + 0.001 * A_SAND_MI + 0.05 * A_PH_WA + 2.6e-4 * A_CACO3_MI + 0.04 * A_SOM_LOI + 0.04 * 9.69 + 0.002 * A_CEC_CO - 0.3]
  dt[grepl('clay',B_SOILTYPE), value := 0.00056 * A_CEC_CO]
  dt[grepl('silty clay',B_SOILTYPE), value := 0.005 * A_C_OF + - 0.02 * A_PH_WA + 0.01 * A_CACO3_MI + 0.15 * A_SOM_LOI + 0.04 * 9.69 + 0.008 * A_CEC_CO]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of Annabi et al. (2017)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_CACO3_MI (numeric) The calcium carbonate content of the soil (\%)
#' 
#' @import data.table
#' 
#' @references Annabi et al. (2017) Spatial variability of soil aggregate stability at the scale of an agricultural region in Tunisia
#'
#' @export
sptf_mwd4 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI, A_CACO3_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_SILT_MI),length(A_CACO3_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 15, len = arg.length)
  
  # Collect data into a table (SOC and mineralogy in g/kg)
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SILT_MI = A_SILT_MI * 10,
                   A_CACO3_MI = A_CACO3_MI,
                   value = NA_real_)
  
  # Estimate USDA soil type
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # estimate Mean Weight Diamater in mm (r = 0.624, n = 113)
  dt[, v1 := 0.5227 + 0.0015 * A_CLAY_MI - 0.0121 * A_C_OF]
  dt[, v2 := 0.9535 + 0.0036 * A_SILT_MI - 0.0272 * A_C_OF + 0.0011 * A_CACO3_MI + 0.43 * 9.2]
  dt[, v3 := 0.262 + 0.0052 * A_SILT_MI - 0.0358 * A_C_OF + 0.4784 * 9.2]
  dt[, v4 := 0.5502 + 0.0031 * A_SILT_MI - 0.0214 * A_C_OF + 0.4460 * 9.2]
  
  # take the mean MWD (in mm)
  dt[ value := (v1 + v2 + v3 + v4) /4]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of Ciric et al. (2012)
#'
#' @param A_DENSITY (numeric) The bulk density of the soil (kg / m3)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#' 
#' @import data.table
#' 
#' @references Ciric et al. (2012) Soil dry aggregate size distribution: effects of soil type and land use
#'
#' @export
sptf_mwd5 <- function(A_DENSITY, A_CLAY_MI, A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_DENSITY), length(A_CLAY_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_DENSITY, lower = 500, upper = 2000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  
  # Collect data into a table (density in Mg m-3)
  dt <- data.table(A_DENSITY = A_DENSITY * 0.001,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # estimate Mean Weight Diamater (n = 120)
  # estimate water retention at -33 kPa from clay content
  dt[, value := -4.731 + 7.453 * A_DENSITY + 0.200 * (-14.2099 * A_CLAY_MI^0.5) - 0.570 * A_PH_WA]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of Gao et al. (2019)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Gao et al. (2019). Soil wet aggregate distribution and pore size distribution under different tillage systems after 16 years in the Loess Plateau of China
#'
#' @export
sptf_mwd6 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (SOC in g/kg)
  dt <- data.table(A_C_OF = A_C_OF,
                   value = NA_real_)
  
  # estimate Mean Weight Diamater (n = 6 fields, x treatments, R2 = 0.95)
  dt[, value := 0.2077 + 0.1145 * A_C_OF]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of Hatamvand & Kashani (2021)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_SAND_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Hatamvand & Kashani (2021). Estimating Wet Aggregates Stability from Easily Available Soil Properties in North West of Lake Urmia
#'
#' @export
sptf_mwd7 <- function(A_C_OF,A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_SAND_MI))
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate Mean Weight Diamater
  dt[, value := 0.192 + 0.132 * A_C_OF + 0.007 * A_SAND_MI]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of Hamel et al. (2021)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#' @param A_CACO3_MI (numeric) The calcium carbonate content of the soil (\%)
#' 
#' @import data.table
#' 
#' @references Hamel et al. (2021). Evaluation of soil aggregate stability in Algerian northwestern soils using pedotransfer functions and artificial neural networks
#'
#' @export
sptf_mwd8 <- function(A_SOM_LOI,A_CLAY_MI,A_SILT_MI,A_PH_WA, A_CACO3_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI),length(A_CLAY_MI),length(A_SILT_MI), length(A_PH_WA), length(A_CACO3_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 15, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_WA = A_PH_WA,
                   A_CACO3_MI = A_CACO3_MI,
                   value = NA_real_)
  
  # Estimate USDA soil type
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # set mean EC
  dt[, EC := 10.73]
  
  # estimate MWD (mm)per soil type (n = 1248, 0-20cm)
  dt[grepl('sand',B_SOILTYPE), value := -1.385 + 0.019 * A_CLAY_MI + 0.017 * A_SILT_MI + 0.004 * A_SAND_MI + 0.17 * A_PH_WA - 0.016 * A_CACO3_MI + 0.01* A_SOM_LOI - 0.01 * EC]
  dt[grepl('silt',B_SOILTYPE), value := 1.179 + 0.003 * A_CLAY_MI -0.003 * A_SILT_MI - 0.004 * A_SAND_MI -0.012 * A_PH_WA +0.007 * A_CACO3_MI -0.007* A_SOM_LOI + 0.004 * EC]
  dt[grepl('clay',B_SOILTYPE), value := 0.426 -0.003 * A_CLAY_MI +0 * A_SILT_MI +0.0046 * A_SAND_MI -0.103 * A_PH_WA +0.0037 * A_CACO3_MI +0.003* A_SOM_LOI + 0.033 * EC]
  dt[grepl('silty clay',B_SOILTYPE), value := 0.684 -0.009 * A_CLAY_MI +0.001 * A_SILT_MI +0.002 * A_SAND_MI -0.036 * A_PH_WA -0.004 * A_CACO3_MI +0.002* A_SOM_LOI + 0.006 * EC]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of Clergue et al. (2023)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#' 
#' @import data.table
#' 
#' @references Clergue et al. (2023). Estimating soil aggregate stability with infrared spectroscopy and pedotransfer functions 
#'
#' @export
sptf_mwd9 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI,A_PH_WA, B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_SILT_MI), length(A_PH_WA), length(A_CACO3_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,length = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SILT_MI = A_SILT_MI * 10,
                   A_PH_WA = A_PH_WA,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_)
  
  # Estimate thw MWD (mm) for croplands
  dt[B_LU_PTFCLASS %in% c('agriculture','cropland'), value := 0.7630 + 0.0017 * A_CLAY_MI + 0.0145 * A_C_OF - 0.0719 * A_PH_WA - 0.004 * A_SILT_MI]
  
  # Estimate thw MWD (mm) for graslands
  dt[B_LU_PTFCLASS == 'grasland', value := 1.4536 + 0.0017 * A_CLAY_MI + 0.0145 * A_C_OF - 0.0719 * A_PH_WA - 0.004 * A_SILT_MI]
  
  # Estimate thw MWD (mm) for forests and woodlands
  dt[B_LU_PTFCLASS == 'forest', value := 2.068 + 0.0017 * A_CLAY_MI + 0.0145 * A_C_OF - 0.0719 * A_PH_WA - 0.004 * A_SILT_MI]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diamater given the pedotransferfunction of le Bissonnais et al. (2007)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' 
#' @import data.table
#' 
#' @references le Bissonnais et al. (2007) Erodibility of Mediterranean vineyard soils: relevant aggregate stability methods and significant soil variables
#'
#' @export
sptf_mwd10 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table 
  dt <- data.table(A_C_OF = A_C_OF,
                   value = NA_real_)
  
  # estimate Mean Weight Diamater (r = 0.917, n = 68)
  dt[, value := 0.001 * (exp(4.83 + (2.76 + 7e8 * exp(-1.01 * A_C_OF))) - 1)]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

# see paper of Purushothaman et al. (2022) for 17 PTFs
# see Bhattacharya, https://doi.org/10.1002/agj2.20469
# le bissonnais, gomez, annabi
