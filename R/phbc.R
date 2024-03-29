#' Calculate the soil pH buffering capacity
#'
#' This function calculates the capacity of soils to buffer pH changes.
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references McBratney et al. (2002). From pedotransfer functions to soil inference systems. 
#' 
#' @details pHBC determined by titration according to method of Aitken & Moody (1994)
#' 
#' @export
sptf_phbc1 <- function(A_C_OF, A_CLAY_MI) {
  
  # add visual bindings
  A_REST_MI = value = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with SOC in %, percentage 20-2000 um as the remaining, slib + sand)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_REST_MI = 100 - A_CLAY_MI,
                   value = NA_real_)
  
  # estimate pH buffer capacity (R2 = 0.79, n = 85)
  dt[,value := 6.38 - 0.08 * A_CLAY_MI + 2.63 * A_C_OF - 0.23 * A_REST_MI + 0.02 * A_CLAY_MI * A_REST_MI + 0.17 * A_REST_MI * A_C_OF]
  
  # select value
  value <- dt[,value]
  
  # return pHBC (mmol H+ kg-1 pH-1)
  return(value)
  
}

#' Calculate the soil pH buffering capacity
#'
#' This function calculates the capacity of soils (0-10cm) in Australia to buffer pH changes in response to changes in SOC.
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references Wong et al. (2013) Development of buffer methods and evaluation of pedotransfer functions to estimate pH buffer capacity of highly weathered soils
#'
#' @details pHBC determined by titration according to method of Aitken & Moody (1994)
#' 
#' @export
sptf_phbc2 <- function(A_C_OF) {
  
  # add visual binding
  value = NULL
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   value = NA_real_)
  
  # estimate pH buffer capacity (R2 = 0.89, n = 89 topsoils (0-10 cm) Western Australia)
  dt[,value := 10 * (0.73 + 0.66 * A_C_OF)]
  
  # select value
  value <- dt[,value]
  
  # return pHBC (mmol H+ kg-1 pH-1)
  return(value)
  
}

#' Calculate the soil pH buffering capacity
#'
#' This function calculates the capacity of soils to buffer pH changes in response to changes in SOC.
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Wong et al. (2013) Development of buffer methods and evaluation of pedotransfer functions to estimate pH buffer capacity of highly weathered soils
#'
#' @details pHBC determined by titration according to method of Aitken & Moody (1994). Datasets derived from literature.
#' 
#' @export
sptf_phbc3 <- function(A_C_OF,A_CLAY_MI) {
  
  # add visual bindings
  v1 = v2 = v3a = v3b = v4a = v4b = v4c = value = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(id = 1: length(A_C_OF),
                   A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI)
  
  # estimate pH buffer capacity (R2 = 0.87), Noble's Dalrymple Shire
  dt[,v1 := 10 * (0.42 + 1.24 * A_C_OF)]
  
  # estimate pH buffer capacity (R2 = 0.83), Hochman et al. (1995, 1992)
  dt[,v2 := 10 * (1.27 + 0.42 * A_C_OF)]
  
  # estimate pH buffer capacity (R2 = 0.85 - 0.87), Aitken et al. (1998)
  dt[, v3a := 10 * (2.55 + 1.30 * A_C_OF)]
  dt[, v3b := 10 * (0.033 * A_CLAY_MI + 1.23 * A_C_OF + 1.87)]
  
  # estimate pH buffer capacity (R2 = 0.90 - 0.93), Gillman granite and metamorphic soils
  dt[, v4a := 10 * (0.28 + 1.65 * A_C_OF)]
  dt[, v4b := 10 * (0.023 * A_CLAY_MI + 1.29 * A_C_OF + 1.16)]
  dt[, v4c := 10 * (0.044 * A_CLAY_MI + 1.82 * A_C_OF - 1.01)]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3a','v3b','v4a','v4b','v4c'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select value
  value <- dt[,value]
  
  # return pHBC (mmol H+ kg-1 pH-1)
  return(value)
  
}

#' Calculate the soil pH buffering capacity
#'
#' This function calculates the capacity of soils in New Zealand to buffer pH changes in response to changes in SOC.
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Curtin & Trolove (2013) Predicting pH buffering capacity of New Zealand soils from organic matter content and mineral characteristics
#'
#' @details pHBC determined by titration according to method of Aitken & Moody (1994) using CaOH2. 
#' 
#' @export
sptf_phbc4 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with SOC and clay in kg/kg)
  dt <- data.table(id = 1: length(A_C_OF),
                   A_C_OF = A_C_OF * 0.001,
                   A_CLAY_MI = A_CLAY_MI * 0.01,
                   value = NA_real_)
  
  # estimate pH buffer capacity (R2 = 0.83, n = 30)
  dt[,value := 10 * (1 + 102 * A_C_OF + 5.8 * A_CLAY_MI)]
  
  # select value
  value <- dt[,value]
  
  # return pHBC (mmol H+ kg-1 pH-1)
  return(value)
  
}

#' Calculate the soil pH buffering capacity
#'
#' This function calculates the capacity of soils in Georgia to buffer pH changes in response to changes in SOC.
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Weaver et al. (2004) Mapping Soil pH Buffering Capacity of Selected Fields in the Coastal Plain
#'
#' @details pHBC determined by titration according to method of Aitken & Moody (1994) using CaOH2. 
#' 
#' @export
sptf_phbc5 <- function(A_C_OF) {
  
  # add visual bindings
  bd = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)

  # make internal data.table
  dt <- data.table(id = 1: length(A_C_OF),
                   A_C_OF = A_C_OF,
                   value = NA_real_)
  
  # estimate bulk density (in kg/m3)
  dt[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF)]
  
  # estimate pH buffer capacity  (with SOC given in %) (R2 = 0.62, n = 136 topsoils). Note unit is dpH / kg CaCO3 ha-1
  dt[,value := 0.0087 * exp(-2.16 * A_C_OF * 0.1)]
  
  # convert unit: kg CaCO3 ha-1 per unit pH
  dt[, value := 1/ value]
  
  # convert 1 kg CaCO3 / ha to mmol H+ / kg per unit pH
  dt[, value := value * (1000000 / (0.15 * bd * 100 * 100)) * 2/100.0869]
  
  # set values outside calibration range to NA
  dt[A_C_OF > 20, value := NA_real_]
  
  # select value
  value <- dt[,value]
  
  # return pHBC (mmol H+ kg-1 pH-1)
  return(value)
  
}

#' Calculate the soil pH buffering capacity
#'
#' This function calculates the capacity of soils to buffer pH changes in agricultral topsoil in Queensland (Australia) in response to changes in SOC.
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Aitken (1990) Lime Requirement of Acidic Queensland Soils. I. Relationships between Soil Properties and pH Buffer Capacity 
#'
#' @details pHBC determined by titration according to method of Aitken & Moody (1994) using CaOH2. 
#' 
#' @export
sptf_phbc6 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (SOC in %)
  dt <- data.table(id = 1: arg.length,
                   A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate pH buffer capacity (R2 = 0.83, n = 40)
  dt[,value := 10 * (0.9548 * A_C_OF + 0.0111 * A_CLAY_MI)]
  
  # select value
  value <- dt[,value]
  
  # return pHBC (mmol H+ kg-1 pH-1)
  return(value)
  
}

#' Calculate the soil pH buffering capacity
#'
#' This function calculates the capacity of soils to buffer pH changes in response to changes in SOC, clay and pH.
#'
#' @inheritParams sptf_bd0
#'  
#' @import data.table
#' 
#' @references Owusu et al. (1995) Comparative study of selected lime requirement methods for some acid Ghanaian soils
#'
#' @details pHBC determined by titration according to using CaOH2 titration method. 
#' 
#' @export
sptf_phbc7 <- function(A_C_OF,A_CLAY_MI,A_PH_WA) {
  
  # add visual bindings
  bd = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(id = 1: length(A_C_OF),
                   A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # give mean pH when input is missing
  dt[is.na(A_PH_WA), A_PH_WA := 4.9]
  
  # estimate bulk density (in kg/m3)
  dt[, bd := (1617 - 77.4 * log(A_C_OF*10) - 3.49 * A_C_OF*10)]
  
  # estimate pH buffer capacity  (R2 = 0.92, n = x=6 topsoils, 0-22cm, in Ghana). 
  # Note unit is mmol+/kg per unit pH
  dt[,value := 4.2 - 1.1 * A_PH_WA + 1.7 * A_C_OF + 0.05 * A_CLAY_MI]
  
  # convert 1 ton CaCO3 / ha to mmol H+ / kg per unit pH // CaOH2
  dt[, value := value * 1000* (1000000 / (0.22 * bd * 100 * 100)) * 2/74.09]
  
  # select value
  value <- dt[,value]
  
  # return pHBC (mmol H+ kg-1 pH-1)
  return(value)
  
}

#' Calculate the soil pH buffering capacity
#'
#' This function calculates the capacity of soils to buffer pH changes in response to changes in SOC and clay.
#'
#' @inheritParams sptf_bd0
#'  
#' @import data.table
#' 
#' @references Joret et al. (1934) Líappreciation des besoins en chaux des sols de limon díaprËs leur Ètat de saturation en bases Èchangeables.
#'
#' @details pHBC determined by titration according to using CaOH2 titration method. 
#' 
#' @export
sptf_phbc8 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(id = 1: length(A_C_OF),
                   A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate pH buffer capacity  Note unit is cmol+/kg per unit pH
  dt[,value := 0.11 * (A_CLAY_MI + 5 * A_C_OF * 2)]
  
  # select value
  value <- dt[,value*10]
  
  # return pHBC (mmol H+ kg-1 pH-1)
  return(value)
  
}

# klei < 2 um, silt 2-50 um en zand > 50 um

