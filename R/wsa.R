# Aggregate Stability

#' Calculate the percentage water stable aggregate
#' 
#' Calculate the percentage water stable aggregates for topsoils (0-20cm) in the United Kingdom given the pedotransferfunction of Stengel et al. (1984).
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
  
  # estimate percentage of water stable aggregates (n = 16 x 3 = 48, R2 = 0.61)
  dt[, value := 11.57 * A_C_OF + 12.75]
  
  # return value for WSA (%)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the percentage water stable aggregate
#' 
#' Calculate the percentage water stable aggregates for agricultural soils (0-15 cm) in UK given the pedotransferfunction of Ekwue (1990)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Ekwue (1990). Organic matter effects on soil strength properties.
#'
#' @export
sptf_wsa2 <- function(A_SOM_LOI) {
  
  # add visual bindings
  v1 = v2 = NULL
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table (SOC in %)
  dt <- data.table(id = 1:length(A_SOM_LOI),
                   A_SOM_LOI = A_SOM_LOI,
                   value = NA_real_)
  
  # estimate percentage of water stable aggregates(>0.5 mm) (n = 14, R2 = 0.87) on sandy grassland soils
  dt[, v1 := 3.32 * A_SOM_LOI -1.44]
  dt[, v2 := 3.03 * exp(-0.22 * A_SOM_LOI)]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the percentage water stable aggregate
#' 
#' Calculate the Percentage Water Stable Aggregates for agricultural soils (0-20cm) in Turkey given the pedotransferfunction of Gulser (2018)
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

  # add visual bindings
  EC = A_SAND_MI = NULL
  
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
  
  # replace missing K and soil pH with mean value
  dt[is.na(A_K_AA), A_K_AA := 0.59]
  dt[is.na(A_PH_WA), A_PH_WA := 7.56]
  
  # set mean EC value (dS/m)
  dt[, EC := 0.65]
  
  # Estimate WSA (%) for topsoils (n = 176, R2 = 0.55)
  dt[, value := 17.996 + 1.57 * A_CLAY_MI - 7.346 * A_PH_WA + 0.494 * A_SAND_MI +4.837 * A_SOM_LOI - 7.965 * A_K_AA + 3.805 * EC]
  
  # return value (%)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Predicting the percentage water stable aggregates
#' 
#' Calculate the percentage water stable aggregates (%)
#'
#' This function calculates the erodible fraction in Argentina for agricultural topsoils being cultivated and uncultivated.
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
sptf_wsa4 <- function(A_CLAY_MI,A_SILT_MI,A_C_OF) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = v5 = v6 = A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SAND_MI = 10 * (100 - A_CLAY_MI - A_SILT_MI),
                   A_SILT_MI = A_SILT_MI * 10)
  
  # estimate 1 and 2 based on OC (R2 0.65 and 0.96)
  dt[,v1 := 85/(1+357 * exp(-1.5 * A_C_OF))]
  dt[,v2 := 90/(1+2.9 * exp(-0.5 * A_C_OF))]
  
  # estimate 2 and 3 based on clay content (R2 0.93 - 0.92)
  dt[, v3 := 85 * (1.1 - exp(-0.015 * A_CLAY_MI))]
  dt[, v4 := 162 * (0.6 - exp(-0.03 * A_CLAY_MI))]
  
  # estimate 4 and 5 based on sand and clay ratio (R2 0.96 - 0.96)
  dt[, v5 := 98 - 2.8 * A_SAND_MI / A_CLAY_MI]
  dt[, v6 := 89 + 0.8 * A_SAND_MI / A_CLAY_MI - 0.2 * (A_SAND_MI/A_CLAY_MI)^2]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3','v4','v5','v6'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select output variable
  value <- dt[,value]
  
  # return percentage water stable aggregates (%)
  return(value)
  
}

#' Predicting the percentage water stable aggregates
#' 
#' Calculate the percentage water stable aggregates (%) for variable alluvial soils (0-10cm) in southern Ohio.
#'
#' This function calculates the percentage water stable aggregates for alluvial soils in southern Ohio
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_SOM_LOI (numeric) The soil organic matter content (\%).
#' 
#' @import data.table
#' 
#' @references Salchow et al. (1996) Pedotransfer functions for variable alluvial soils in southern Ohio.
#'
#' @export
sptf_wsa5 <- function(A_CLAY_MI,A_SILT_MI,A_SOM_LOI) {
  
  # add visual bindings
  bd = B_SOILTYPE = A_SAND_MI = A_C_OF = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(A_SILT_MI),length(A_SOM_LOI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_SOM_LOI * 0.5 * 10,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate bulk density (in g / cm3)
  dt[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) * 0.001]
  
  # derive UDS soil classification
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # estimate percentage of water stable aggregates (%) (n = 108, R2 = 0.38)
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

#' Predicting the percentage water stable aggregates
#' 
#' Calculate the water stable aggragates for brown calcareous soils (0-5 cm) within the Mediterranean basin in France given the pedotransferfunction of le Bissonnais et al. (2007)
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
  
  # add visual bindings
  fsoc = NULL
  
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
  
  # set mean CaCO3 level when its value is unknown
  dt[is.na(A_CACO3_MI), A_CACO3_MI := 456.4]
  
  # estimate Aggregate Stability Index MA200 (r = 0.91, n = 68)
  dt[, fsoc := 35.6 + 52.86/(1 + 4.14e5 * exp(-0.67 * A_C_OF))]
  dt[, value := -27.56 + 0.98 * fsoc + 0.41 * (A_CLAY_MI + A_SILT_MI) + 0.13 * A_CACO3_MI ]
  
  # return value (%)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Predicting the percentage water stable aggregates
#' 
#' Calculate the water stable aggragates for agricultural topsoils in Spain given the pedotransferfunction of Canasveras et al. (2010)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#' @param A_CACO3_MI (numeric) The calcium carbonate content of the soil (\%)
#' 
#' @import data.table
#' 
#' @references Canasveras et al. (2010) Estimation of aggregate stability indices in Mediterranean soils by diffuse reflectance spectroscopy
#'
#' @export
sptf_wsa7 <- function(A_SOM_LOI,A_CLAY_MI,A_SILT_MI,A_PH_WA, A_CACO3_MI) {
  
  # add visual bindings
  fe = A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI),length(A_CLAY_MI),length(A_SILT_MI), length(A_PH_WA), length(A_CACO3_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 15, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 10,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SILT_MI = A_SILT_MI * 10,
                   A_SAND_MI = (100 - A_CLAY_MI - A_SILT_MI) * 10,
                   A_PH_WA = A_PH_WA,
                   A_CACO3_MI = A_CACO3_MI * 10,
                   value = NA_real_)
  
  # add mean Fe extractable with citrate-bicarbonate-dithionite
  dt[, fe := 8.3]
  
  # set mean CaCO3 level when its value is unknown
  dt[is.na(A_CACO3_MI), A_CACO3_MI := 310]
  
  # Estimate wsa (g/kg), R2 = 0.37, n = 80 topsoils, Spain
  dt[, value := 44.158 - 0.048 * A_SAND_MI - 0.087 * A_CLAY_MI + 0.002 * A_CACO3_MI +0.431 * A_PH_WA + 0.041 * A_SOM_LOI + 0.756 * fe]
  
  # avoid values outside calibration range
  dt[value < 0 | value > 499, value := NA_real_]
  
  # return value (%)
  value <- dt[, value * 10]
  
  # return value
  return(value)
  
}

#' Predicting the percentage water stable aggregates
#' 
#' Calculate the Percentage Water Stable Aggregates for tiled soils in Ontaria, Canada, given the pedotransferfunction of Perfect et al. (1993)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Perfect et al. (1993) Comparison of functions for characterizing the dry aggregate size distribution of tilled soil
#'
#' @export
sptf_wsa8 <- function(A_SOM_LOI,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # Estimate WSA, using an averaged moisture content of 20% (n = 36, R2 = 0.83)
  dt[, value := 8.17 + 0.84 * A_CLAY_MI + 3.21 * A_SOM_LOI - 0.99 * 15]
  
  # the function is sensitive to unknown W, so set min and max based on measured values
  dt[, value := pmin(45,pmax(8, value))]
  
  # select value (%)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Predicting the percentage water stable aggregates
#' 
#' Calculate the Percentage Water Stable Aggregates for agricultural and forest soils (0-17cm) in Chile given the pedotransferfunction of Rivera & Bonilla (2020)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#' 
#' @import data.table
#' 
#' @references Rivera & Bonilla (2020) Predicting soil aggregate stability using readily available soil properties and machine learning techniques
#'
#' @export
sptf_wsa9 <- function(A_SOM_LOI,A_CLAY_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  
  # Collect data into a table, 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # when pH water is missing update, replace by mean pH
  dt[is.na(A_PH_WA), A_PH_WA := 6.7]
  
  # Estimate WSA (%), R2 = 0.59, n = 109, Chile
  dt[, value := 122.4 + 1.1 * A_SOM_LOI + 0.19 * A_CLAY_MI - 9.1 * A_PH_WA]
  
  
  # avoid predictions outside calibration range
  dt[value < 12.5 | value > 98.7, value := NA_real_]
  
  # return value (%)
  value <- dt[, value]
  
  # return value
  return(value)
  
}


