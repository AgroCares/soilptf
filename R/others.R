# other functions, not yet sufficient to be added in a separate script

#' Calculate the crop yield of cereals via the meta-regression of global dataset from Oldfield et al. (2019)
#'
#' This function calculates the crop yield of cereals.
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references Oldfield et al. (2019).Global meta-analysis of the relationship between soil organic matter and crop yields
#'
#' @export
sptf_yield1 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # default mean inputs
  lat = 30.49
  aridity = 0.76
  ph = 7.15
  irrigation = 0
  ndose = 118
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # meta-regression model
  dt[,value := -1.61 + 0.179 * A_C_OF - 0.46 * (0.1 * A_C_OF)^2 + irrigation * 0.75 + 0.053 * ph + 0.16 * aridity + 
               0.013 * A_CLAY_MI + ndose * 0.018 -0.000039 * ndose^2 + 0.0039 * 0.1 * A_C_OF * ndose]
  
  # set value outside calibration range to NA
  dt[A_C_OF > 27, value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}


#' Calculate the penetration resistance using ptfs of da Silva and Kay (1997)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Kay et al. (1997) Sensitivity of soil structure to changes in organic carbon content:Predictions using pedotransfer functions Cites in: da Silva et al. (1997)  Management versus inherent properties effects on bulk density and relative compaction. Soil Tillage Res.
#'
#' @export
pr1 <- function(A_C_OF, A_CLAY_MI) {
  
  # add visual bindings
  theta = bd = pc = pd = pe = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate bulk density in g/cm3
  dt[,bd := 0.001 * (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF)]
  
  # moisture content, %, can be derived via ptf for field capacity
  dt[ ,theta := 0.18]
  
  # estimate penetration resistance
  dt[, pc := exp(-3.67 + 0.765 * 0.1 * A_C_OF - 0.145 * A_CLAY_MI)]
  dt[, pd := -0.481 + 0.208 * 0.1 * A_C_OF - 0.124 * A_CLAY_MI]
  dt[, pe := 3.85 + 0.0963 * A_CLAY_MI]
  dt[, value := pc * theta^pd * bd^pe]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the pH-water value from pH-KCL
#'
#' This function calculates the pH extracted with water from the pH-KCL.
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references Wosten et al. (1997) Bodemkundige vertaalfuncties bij SC-DLO; state of the art. https://edepot.wur.nl/299664
#'
#' @export
sptf_ph1 <- function(A_PH_KCL) {
  
  # Check input
  checkmate::assert_numeric(A_PH_KCL, lower = 2, upper = 10)
  
  # estimate pH water from pH-KCL for peat soils (Finke, 1996)
  value <- 1.3235 + 0.8581 * A_PH_KCL
  
  # return pH value
  return(value)
  
}

#' Calculate the pH-water value from pH-KCL
#'
#' This function calculates the pH extracted with water from the pH-KCL.
#'
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Wosten et al. (1997) Bodemkundige vertaalfuncties bij SC-DLO; state of the art. https://edepot.wur.nl/299664
#'
#' @export
sptf_ph2 <- function(A_SOM_LOI, A_PH_KCL,A_CLAY_MI) {
  
  # add visual binding
  value = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_PH_KCL))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_PH_KCL, lower = 2, upper = 10, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_KCL = A_PH_KCL,
                   value = NA_real_)
  
  # estimate pH water from pH-KCL for sand soils with <15% humus (Finke, 1996)
  dt[A_SOM_LOI < 15 & A_CLAY_MI <= 20,value := 0.9843 + 0.9003 * A_PH_KCL + 0.00995 * A_SOM_LOI]
  
  # estiamte pH water from pH-KCL for clay soils
  dt[A_CLAY_MI > 20, value := 2.189 + 0.7748 * A_PH_KCL]
  
  # select value
  value <- dt[,value]
  
  # return pH value
  return(value)
  
}



# The influence of organic matter on aggregate stability in some British soils
# K. CHANEY, R.S. SWIFT

# minansy (2011)
# plot(ph,6.01 + 1.384*(2.285 - 4.819/(1 + exp(-3.935 + 0.608 *ph) + 0.092 * log(0.2))))
# lines(ph,-0.05+0.9*ph + 0.14 * log(0.2))
              