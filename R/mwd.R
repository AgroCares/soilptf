#' Calculate the Mean Weight Diameter
#' 
#' Calculate the Mean Weight Diameter for agricultural British soils (0-20cm) given the pedotransferfunction of Chaney and Swift (1984)
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references Chaney and Swift (1984). The influence of organic matter on aggregate stability in some British soils
#'
#' @export
sptf_mwd1 <- function(A_SOM_LOI) {
  
  # add visual bindings
  v1 = v2 = v3 = NULL
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table (SOC in %)
  dt <- data.table(id = 1: length(A_SOM_LOI),
                   A_SOM_LOI = A_SOM_LOI,
                   value = NA_real_)
  
  # estimate Mean Weight Diameter (n = 26, R2 = 0.74)
  dt[, v1 := A_SOM_LOI * 24 + 31]
  
  # estimate for Stirling soil series (n = 14)
  dt[, v2 := A_SOM_LOI * 24 + 24]
  
  # estiate for Humbie soil series (n = 14)
  dt[, v3 := A_SOM_LOI * 30 + 49]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # adjust value to scale 0 -3 mm
  dt[,value := value * 0.01]
  
  # return value (mm), varied between 37 and 225 mm.
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diameter
#' 
#' Calculate the Mean Weight Diameter for agricultural top soils (0-20cm) in New Zealand given the pedotransferfunction of Haynes et al. (1991)
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references Haynes et al. (1991) Influence of mixed cropping rotations (pasture-arable) on organic matter content, water-stable aggregation and clod porosity in a group of soils
#'
#' @export
sptf_mwd2 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI) {
  
  # add visual bindings
  B_SOILTYPE = A_SAND_MI = NULL
  
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
  
  # estimate Mean Weight Diameter (n = 3x26 = 26, R2 = 0.44 to 0.59)
  dt[B_SOILTYPE =='sandy loam', value := 0.60 * A_C_OF + 0.65]
  dt[B_SOILTYPE == 'silt loam', value := 1.09 * A_C_OF - 0.86]
  dt[B_SOILTYPE == 'clay loam', value := 0.62 * A_C_OF + 0.27]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diameter
#' 
#' Calculate the Mean Weight Diameter for soils (0-20cm) in Northwestern Algeria given the pedotransferfunction of Saidi et al. (2015)
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Saidi et al. (2015). Using Pedotransfer Functions to Assess Aggregate Stability: Application to the Lower Cheliff Soils, Algeria
#'
#' @export
sptf_mwd3 <- function(A_C_OF,A_CEC_CO,A_CLAY_MI,A_SILT_MI, A_PH_WA, A_CACO3_IF) {
  
  # add visual bindings
  B_SOILTYPE = A_SAND_MI = A_SOM_LOI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CEC_CO),length(A_CLAY_MI),length(A_SILT_MI),length(A_PH_WA),length(A_CACO3_IF))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 0, upper = 600, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CACO3_IF, lower = 0, upper = 15, len = arg.length)
  
  # Collect data into a table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_SOM_LOI = A_C_OF * 0.1 * 2,
                   A_CEC_CO = A_CEC_CO,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_WA = A_PH_WA, 
                   A_CACO3_IF = A_CACO3_IF,
                   value = NA_real_)
  
  # Estimate USDA soil type
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # replace missing pH and CaCo3 with mean values when inputs are missing
  dt[is.na(A_PH_WA), A_PH_WA := 7.32]
  dt[is.na(A_CACO3_IF), A_CACO3_IF := 7.81]
  dt[is.na(A_CEC_CO), A_CEC_CO := 15.8 * 10]
  
  # estimate Mean Weight Diamater in mm (n = 183, R2 = 0.67 ~ 0.71 ~ 0.91 ~ 0.94)
  dt[grepl('sand',B_SOILTYPE), value := 0.017 * A_C_OF + 0.004 * A_SILT_MI + 0.002 * A_PH_WA + 0.018 * A_CACO3_IF + 0.14 * A_SOM_LOI + 0.0006 * A_CEC_CO + 0.052]
  dt[grepl('silt',B_SOILTYPE), value := 0.007 * A_SILT_MI + 0.001 * A_SAND_MI + 0.05 * A_PH_WA + 2.6e-4 * A_CACO3_IF + 0.04 * A_SOM_LOI + 0.04 * 9.69 + 0.002 * A_CEC_CO - 0.3]
  dt[grepl('clay',B_SOILTYPE), value := 0.00056 * A_CEC_CO]
  dt[grepl('silty clay',B_SOILTYPE), value := 0.005 * A_C_OF - 0.02 * A_PH_WA + 0.01 * A_CACO3_IF + 0.15 * A_SOM_LOI + 0.04 * 9.69 + 0.008 * A_CEC_CO]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diameter
#'
#' Calculate the Mean Weight Diameter for agricultural soils (0-10cm) in Tunesia given the pedotransferfunction of Annabi et al. (2017)
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Annabi et al. (2017) Spatial variability of soil aggregate stability at the scale of an agricultural region in Tunisia
#'
#' @export
sptf_mwd4 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI, A_CACO3_IF) {
  
  # add visual binding
  B_SOILTYPE = v1 = v2 = v3 = v4 = A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_SILT_MI),length(A_CACO3_IF))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_IF, lower = 0, upper = 15, len = arg.length)
  
  # Collect data into a table (SOC and mineralogy in g/kg)
  dt <- data.table(id = 1:arg.length,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_SILT_MI - A_CLAY_MI,
                   A_CACO3_IF = A_CACO3_IF)
  
  # Estimate USDA soil type
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # update units to g/kg
  dt[,A_CLAY_MI := A_CLAY_MI * 10]
  dt[,A_SILT_MI := A_SILT_MI * 10]
  dt[,A_CACO3_IF := A_CACO3_IF * 10]
  
  # add median A_CACO3 when the input missing
  dt[is.na(A_CACO3_IF), A_CACO3_IF := 30]
  
  # estimate Mean Weight Diameter in mm (r = 0.624, n = 113). Units for iron adapted because of too high outputs
  dt[, v1 := 0.5227 + 0.0015 * A_CLAY_MI - 0.0121 * A_C_OF]
  dt[, v2 := 0.9535 + 0.0036 * A_SILT_MI - 0.0272 * A_C_OF + 0.0011 * A_CACO3_IF + 0.43 * 9.2 * 0.1]
  dt[, v3 := 0.262 + 0.0052 * A_SILT_MI - 0.0358 * A_C_OF + 0.4784 * 9.2 * 0.1]
  dt[, v4 := 0.5502 + 0.0031 * A_SILT_MI - 0.0214 * A_C_OF + 0.4460 * 9.2 * 0.1]
  
  # Estimate the mean MWD (in mm)
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3','v4'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # set allowable max and min
  dt[,value := pmin(pmax(0.3,value),3)]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diameter
#'
#' Calculate the Mean Weight Diameter for agricultural soils (0-20 cm) in Serbia given the pedotransferfunction of Ciric et al. (2012)
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Ciric et al. (2012) Soil dry aggregate size distribution: effects of soil type and land use
#'
#' @export
sptf_mwd5 <- function(A_C_OF, A_CLAY_MI, A_PH_WA) {
  
  # add visual bindings
  bd = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  
  # Collect data into a table (density in Mg m-3)
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # replace missing pH with mean values when inputs are missing
  dt[is.na(A_PH_WA), A_PH_WA := 7.0]
  
  # estimate bulk density (in g / cm3)
  dt[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) * 0.001]
  
  # estimate Mean Weight Diameter (n = 120, R2 = 0.63)
  # estimate water retention at -33 kPa from clay content
  dt[, value := -4.731 + 7.453 * bd + 0.200 * (-14.2099 + 8.3147 * A_CLAY_MI^0.5) - 0.570 * A_PH_WA]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diameter
#'
#' Calculate the Mean Weight Diameter for agricultural soils (0-20cm) in the Loess Plateau of China given the pedotransferfunction of Gao et al. (2019)
#'
#' @inheritParams sptf_bd0
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
  
  # estimate Mean Weight Diameter (n = 6 fields, x treatments, R2 = 0.95)
  dt[, value := 0.2077 + 0.1145 * A_C_OF]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diameter
#'
#' Calculate the Mean Weight Diameter for agricultural soils (0-10cm) in North West of Lake Urmia in Iraqi given the pedotransferfunction of Hatamvand & Kashani (2021)
#'
#' @inheritParams sptf_bd0
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
  
  # estimate Mean Weight Diameter (n = 100, R2 = 0.83)
  dt[, value := 0.192 + 0.132 * A_C_OF + 0.007 * A_SAND_MI]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diameter
#'
#' Calculate the Mean Weight Diameter for agricultural soils (0-20cm) for Algeria, extended withh data from published results from other countries, given the pedotransferfunction of Hamel et al. (2021)
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Hamel et al. (2021). Evaluation of soil aggregate stability in Algerian northwestern soils using pedotransfer functions and artificial neural networks
#'
#' @export
sptf_mwd8 <- function(A_SOM_LOI,A_CLAY_MI,A_SILT_MI,A_PH_WA, A_CACO3_IF) {
  
  # add visual bindings
  B_SOILTYPE = EC = A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI),length(A_CLAY_MI),length(A_SILT_MI), length(A_PH_WA), length(A_CACO3_IF))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CACO3_IF, lower = 0, upper = 15, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_WA = A_PH_WA,
                   A_CACO3_IF = A_CACO3_IF,
                   value = NA_real_)
  
  # Estimate USDA soil type
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # replace missing pH with mean values when inputs are missing
  dt[is.na(A_PH_WA), A_PH_WA := 7.97]
  dt[is.na(A_CACO3_IF), A_CACO3_IF := 17.73]
  
  # set mean EC
  dt[, EC := 10.73]
  
  # estimate MWD (mm) per soil type (n = 1248, 0-20cm, R2 varies from 0.22 to 0.65)
  dt[grepl('sand',B_SOILTYPE), value := -1.385 + 0.019 * A_CLAY_MI + 0.017 * A_SILT_MI + 0.004 * A_SAND_MI + 0.17 * A_PH_WA - 0.016 * A_CACO3_IF + 0.01* A_SOM_LOI - 0.01 * EC]
  dt[grepl('silt',B_SOILTYPE), value := 1.179 + 0.003 * A_CLAY_MI -0.003 * A_SILT_MI - 0.004 * A_SAND_MI -0.012 * A_PH_WA +0.007 * A_CACO3_IF -0.007* A_SOM_LOI + 0.004 * EC]
  dt[grepl('clay',B_SOILTYPE), value := 0.426 -0.003 * A_CLAY_MI +0 * A_SILT_MI +0.0046 * A_SAND_MI -0.103 * A_PH_WA +0.0037 * A_CACO3_IF +0.003* A_SOM_LOI + 0.033 * EC]
  dt[grepl('silty clay',B_SOILTYPE), value := 0.684 -0.009 * A_CLAY_MI +0.001 * A_SILT_MI +0.002 * A_SAND_MI -0.036 * A_PH_WA -0.004 * A_CACO3_IF +0.002* A_SOM_LOI + 0.006 * EC]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diameter
#'
#' Calculate the Mean Weight Diameter for variable land uses in France (0-20cm) given the pedotransferfunction of Clergue et al. (2023)
#'
#' @inheritParams sptf_bd0
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#' 
#' @import data.table
#' 
#' @references Clergue et al. (2023). Estimating soil aggregate stability with infrared spectroscopy and pedotransfer functions 
#'
#' @export
sptf_mwd9 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI,A_PH_WA, B_LU_PTFCLASS) {
  
  # add visual bindings
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_SILT_MI), length(A_PH_WA))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,len = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SILT_MI = A_SILT_MI * 10,
                   A_PH_WA = A_PH_WA,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_)
  
  # replace missing pH with mean values when inputs are missing
  dt[is.na(A_PH_WA), A_PH_WA := 6.7]
  
  # Estimate thw MWD (mm) for croplands (n = 129, R2 = 0.64)
  dt[B_LU_PTFCLASS %in% c('agriculture','cropland'), value := 0.7630 + 0.0017 * A_CLAY_MI + 0.0145 * A_C_OF - 0.0719 * A_PH_WA - 0.0004 * A_SILT_MI]
  
  # Estimate thw MWD (mm) for graslands (n = 52)
  dt[B_LU_PTFCLASS == 'grasland', value := 1.4536 + 0.0017 * A_CLAY_MI + 0.0145 * A_C_OF - 0.0719 * A_PH_WA - 0.0004 * A_SILT_MI]
  
  # Estimate thw MWD (mm) for forests and woodlands (n = 21)
  dt[B_LU_PTFCLASS == 'forest', value := 2.068 + 0.0017 * A_CLAY_MI + 0.0145 * A_C_OF - 0.0719 * A_PH_WA - 0.0004 * A_SILT_MI]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the Mean Weight Diameter
#'
#' Calculate the Mean Weight Diameter for brown calcareous soils (0-5 cm) within the Mediterranean basin in France given the pedotransferfunction of le Bissonnais et al. (2007)
#'
#' @inheritParams sptf_bd0
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
  
  # estimate Mean Weight Diameter (r = 0.917, n = 68)
  dt[, value := 0.001 * (exp(4.83 + 2.76/(1 + 7e8 * exp(-1.014 * A_C_OF)))-1)]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the Mean Weight Diameter
#'
#' Calculate the Mean Weight Diameter given for soils (0-20cm) in Italy the pedotransferfunction of Bazzoffi et al. (1994)
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Bazzoffi et al. (1994) Statistical models for predicting aggregate stability from intrinsic soil components.  
#'
#' @export
sptf_mwd11 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # Estimate thw MWD (mm) for 15 soils, 0-20cm, Italy (R2 = 0.63)
  dt[, value := exp(-3.4347 + 0.8413 * log(A_CLAY_MI) + 0.8858 * log(A_C_OF))]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Predicting the the Mean Weight Diameter
#'
#' Calculate the Mean Weight Diameter (mm) for agricultural topsoils (0-30cm) in India given the pedotransferfunction of Bhattacharya et al. (2021)
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Bhattacharya et al. (2021) Prediction of mean weight diameter of soil using machine learning approaches
#'
#' @export
sptf_mwd12 <- function(A_SOM_LOI,A_CLAY_MI,A_SILT_MI) {
  
  # add visual bindings
  bd = A_C_OF = A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SILT_MI),length(A_SOM_LOI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_SOM_LOI * 0.5,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_SILT_MI = A_SILT_MI)
  
  # estimate bulk density (in g / cm3)
  dt[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) * 0.001]
  
  # estimate Mean Weight Diameter (mm) (R2 = 0.65, n = 120)
  dt[, value := 3.7 + 0.0329 * A_CLAY_MI - 0.0317 * A_SILT_MI - 0.03 * A_SAND_MI + 0.019 * bd + 0.4267 * A_C_OF]
  
  # select output variable
  value <- dt[,value]
  
  # return MWD (mm)
  return(value)
  
}

#' Predicting the the Mean Weight Diameter for soils in Morroco
#'
#' Calculate the Mean Weight Diameter (mm) for agricultural soils (0-40 cm) in Morroco given the pedotransferfunction of Bouslihim et al. (2021)
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Bouslihim et al. (2021) Machine learning approaches for the prediction of soil aggregate stability
#'
#' @export
sptf_mwd13 <- function(A_CLAY_MI,A_SOM_LOI) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SOM_LOI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI)
  
  # estimate Mean Weight Diameter (mm) for SP1 (R2 =  0.59, n =  77)
  dt[, v1 := 0.577 + 0.176 * A_SOM_LOI + 0.012 * A_CLAY_MI]
  
  # estimate Mean Weight Diameter (mm) for SP2 (R2 = 0.35, n =  114)
  dt[, v2 := 0.673 + 0.171 * A_SOM_LOI + 0.009 * A_CLAY_MI]
  
  # estimate Mean Weight Diameter (mm) for SPRS1 (R2 = 0.52, n =  77)
  dt[, v3 := 0.577 + 0.176 * A_SOM_LOI + 0.012 * A_CLAY_MI]
  
  # estimate Mean Weight Diameter (mm) for SPRS2 (R2 = 0.36, n =  114)
  dt[, v4 := 0.673 + 0.171 * A_SOM_LOI + 0.009 * A_CLAY_MI]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2', 'v3','v4'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select output variable
  value <- dt[,value]
  
  # return MWD (mm)
  return(value)
  
}

#' Predicting the the Mean Weight Diameter for soils
#'
#' Calculate the Mean Weight Diameter for agricultural soils (0-10cm) in southern Spain given the pedotransferfunction of Canasveras et al. (2010)
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Canasveras et al. (2010) Estimation of aggregate stability indices in Mediterranean soils by diffuse reflectance spectroscopy
#'
#' @export
sptf_mwd14 <- function(A_SOM_LOI,A_CLAY_MI,A_SILT_MI,A_PH_WA, A_CACO3_IF) {
  
  # add visual bindings
  fe = A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI),length(A_CLAY_MI),length(A_SILT_MI), length(A_PH_WA), length(A_CACO3_IF))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CACO3_IF, lower = 0, upper = 15, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 10,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SILT_MI = A_SILT_MI * 10,
                   A_SAND_MI = (100 - A_CLAY_MI - A_SILT_MI) * 10,
                   A_PH_WA = A_PH_WA,
                   A_CACO3_IF = A_CACO3_IF * 10,
                   value = NA_real_)
  
  # add mean Fe extractable with citrate-bicarbonate-dithionite
  dt[, fe := 8.3]
  
  # Estimate MWD (mm), R2 = 0.52, n = 80 topsoils, Spain
  dt[, value := 2.573 - 0.001 * A_SAND_MI - 0.003 * A_CLAY_MI + 0.001 * A_CACO3_IF -0.119 * A_PH_WA + 0.012 * A_SOM_LOI + 0.042 * fe]
  
  # avoid values outside calibration range
  dt[value < 0.1 | value > 2.8, value := NA_real_]
  
  # return value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Predicting the the Mean Weight Diameter for soils
#'
#' Calculate the Mean Weight Diameter for agricultural topsoils (0-10cm) in Belgium given the pedotransferfunction of Shi et al. (2020)
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Shi et al. (2020). Vis-NIR spectroscopic assessment of soil aggregate stability and aggregate size distribution in the Belgian Loam Belt  
#'
#' @export
sptf_mwd15 <- function(A_C_OF,A_PH_WA) {
  
  # add visual bindings
  swc = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_PH_WA))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # set mean moisture content at sampling (%)
  dt[, swc := 16.55]
  
  # estimate MWD (mm) for 83 topsoils (0-10 cm) in Belgium (R2 = 0.85, n = 85)
  dt[,value := 1.2 + 0.527 * A_C_OF - 0.174 * A_PH_WA - 0.015 * swc]
  
  # return MWD value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Predicting the the Mean Weight Diameter for soils
#'
#' Calculate the Mean Weight Diamater for agricultural topsoils (0-28cm) in France.
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' 
#' @import data.table
#' 
#' @references Chenu et al. (2000) Organic Matter Influence on Clay Wettability and Soil Aggregate Stability
#' 
#' @export
sptf_mwd16 <- function(A_C_OF) {
  
  # add visual bindings
  swc = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)

  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF,
                   value = NA_real_)
  
  # estimate MWD (mm) for 15 topsoils from forst and cropped fields, loamy soils (n = 15, R2 = 0.6654)
  dt[,value := 0.514 * A_C_OF + 0.0697]
  
  # return MWD value (mm)
  value <- dt[, value]
  
  # return value
  return(value)
  
}
# see paper of Purushothaman et al. (2022) for 17 PTFs
# see Bhattacharya, https://doi.org/10.1002/agj2.20469
# le bissonnais, gomez, annabi