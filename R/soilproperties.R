#' Calculate the soil pH buffering capacity
#'
#' This function calculates the capacity of soils to buffer pH changes.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references McBraney et al. (2002). From pedotransfer functions to soil inference systems. 
#'
#' @export
sptf_phbc <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with SOC in %, percentage 20-2000 um as the remaining, slib + sand)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_REST_MI = 100 - A_CLAY_MI)
  
  # estimate pH buffer capacity (R2 = 0.79, n = 85)
  dt[,value := 6.38 - 0.08 * A_CLAY_MI + 2.63 * A_C_OF - 0.23 * A_REST_MI + 0.02 * A_CLAY_MI * A_REST_MI + 0.17 * A_REST_MI * A_C_OF]
  
  # return pHBC (mmol H+ kg-1 pH-1)
  return(value)
  
}

# klei < 2 um, silt 2-50 um en zand > 50 um

#' Calculate the pH-water value from pH-KCL
#'
#' This function calculates the pH extracted with water from the pH-KCL.
#'
#' @param A_PH_KCL (numeric) The acidity of the soil, pH in KCL (-)
#'
#' @import data.table
#' 
#' @references Wosten et al. (1997) Bodemkundige vertaalfuncties bij SC-DLO; state of the art. https://edepot.wur.nl/299664
#'
#' @export
sptf_ph1 <- function(A_SOM_LOI, A_PH_KCL) {
  
  # Check input
  checkmate::assert_numeric(A_PH_KCL, lower = 2, upper = 10, len = arg.length)
  
  # estimate pH water from pH-KCL for peat soils (Finke, 1996)
  value <- 1.3235 + 0.8581 * A_PH_KCL
  
  # return pH value
  return(value)
  
}

#' Calculate the pH-water value from pH-KCL
#'
#' This function calculates the pH extracted with water from the pH-KCL.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_PH_KCL (numeric) The acidity of the soil, pH in KCL (-).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Wosten et al. (1997) Bodemkundige vertaalfuncties bij SC-DLO; state of the art. https://edepot.wur.nl/299664
#'
#' @export
sptf_ph2 <- function(A_SOM_LOI, A_PH_KCL,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_PH_KCL))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_PH_KCL, lower = 2, upper = 10, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_KCL = A_PH_KCL)
  
  # estimate pH water from pH-KCL for sand soils with <15% humus (Finke, 1996)
  dt[A_SOM_LOI < 15 & A_CLAY_MI <= 20,value := 0.9843 + 0.9003 * A_PH_KCL + 0.00995 * A_SOM_LOI]
  
  # estiamte pH water from pH-KCL for clay soils
  dt[A_CLAY_MI > 20, value := 2.189 + 0.7748 * A_PH_KCL]
  
  # return pH value
  return(value)
  
}

# The influence of organic matter on aggregate stability in some British soils
# K. CHANEY, R.S. SWIFT

