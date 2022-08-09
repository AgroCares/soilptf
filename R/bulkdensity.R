# Functions for bulk soil density

#' Calculate the bulk density given the pedotransferfunction in Dutch Fertilizer Recommendation
#'
#' This function calculates the bulk density of Dutch soils given texture and organic matter content.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references CBAV, 2022; CBGV, 2022
#'
#' @export
sptf_bd1 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  soiltype = dens.sand = dens.clay = cf = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_
  )
  
  # estimate soil density (CBAV, 2019)
  # https://www.handboekbodemenbemesting.nl/nl/handboekbodemenbemesting/Bodem/Volumegewicht-grond.htm
  # https://edepot.wur.nl/413891
  
  # calculate soil texture dependent density (kg / m3)
  dt[, dens.sand := (1 / (0.02525 * A_SOM_LOI + 0.6541)) * 1000]
  dt[, dens.clay :=  (0.00000067*A_SOM_LOI^4 - 0.00007792*A_SOM_LOI^3 + 0.00314712*A_SOM_LOI^2 - 0.06039523*A_SOM_LOI + 1.33932206) * 1000]
  
  # fraction clay correction
  dt[, cf := pmin(1, A_CLAY_MI/25)]
  
  # clay dependent density
  dt[, value := cf * dens.clay + (1-cf) * dens.sand]
  
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the bulk density given the pedotransferfunction of Curtis & Post (1964).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Curtis & Post (1964) Estimating Bulk Density from Organic-Matter Content in Some Vermont Forest Soils.
#'
#' @export
sptf_bd2 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)

  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 10^(0.09963 - 0.00064*log10(A_SOM_LOI) - 0.22302 * log10(OM)^2)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Adams (1973).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Adams (1973) THE EFFECT OF ORGANIC MATTER ON THE BULK AND TRUE DENSITIES OF SOME UNCULTIVATED PODZOLIC SOILS.
#'
#' @export
sptf_bd3 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 100 / (A_SOM_LOI / 0.224 + (100 - A_SOM_LOI/1.27))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Alexander (1980).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Alexander (1980) Bulk Densities of California Soils in Relation to Other Soil Properties.
#'
#' @export
sptf_bd4 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.66 - 0.308 * A_C_OF^0.5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Alexander (1980).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Alexander (1980) Bulk Densities of California Soils in Relation to Other Soil Properties.
#'
#' @export
sptf_bd5 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.72 - 0.294 * A_C_OF^0.5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Federer (1983).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Federer (1983) Nitrogen Mineralization and Nitrification: Depth Variation in Four New England Forest Soils
#'
#' @export
sptf_bd6 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := exp(-2.31 - 1.079 * log(A_SOM_LOI/100) - 0.113 * log(A_SOM_LOI/100)^2)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Huntington et al. (1989).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Huntington et al. (1989) Carbon, organic matter and bulk density relationships in a forested spodosol.
#'
#' @export
sptf_bd7 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := exp(0.263 - 0.147 * log(A_C_OF) - 0.103 * log(A_C_OF)^2)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Huntington et al. (1989).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Huntington et al. (1989) Carbon, organic matter and bulk density relationships in a forested spodosol.
#'
#' @export
sptf_bd8 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := exp(-2.39 - 1.316 * log(A_SOM_LOI) - 0.167 * log(A_SOM_LOI)^2)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Manrique & Jones (1991).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Manrique & Jones (1991) Bulk Density of Soils in Relation to Soil Physical and Chemical Properties.
#'
#' @export
sptf_bd9 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.66 - 0.318 * A_C_OF^0.5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Manrique & Jones (1991).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Manrique & Jones (1991) Bulk Density of Soils in Relation to Soil Physical and Chemical Properties.
#'
#' @export
sptf_bd10 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.51 - 0.113 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}



in kaur 2002, zit meerder funs
ruehlmann_2009 data uit duisland
# check ptfs from han 2006
# hong 2013, BDmin = 1.017 + 0.0032 ∗ Sand + 0.054 ∗ log(depth)
# nanko_2014 bevat ptf


