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


#' Calculate the bulk density given the pedotransferfunction of Federer et al. (1993)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Federer et al. (1993) The organic fraction - bulk density relationship and the expression of nutrient content in forest soils.
#'
#' @export
sptf_bd11 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.111 * 1.450/(1.450 * A_SOM_LOI/100 + 0.111 * (1-A_SOM_LOI/100))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Tamminen & Starr (1994)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Tamminen & Starr (1994) Bulk density of forested mineral soils.
#'
#' @export
sptf_bd12 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.565 - 0.2298 * A_SOM_LOI^0.5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Tamminen & Starr (1994)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Tamminen & Starr (1994) Bulk density of forested mineral soils.
#'
#' @export
sptf_bd13 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.735 - 0.2678* A_SOM_LOI^0.5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the bulk density given the pedotransferfunction of Tomasalla & Hodnett (1998).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Tomasalla & Hodnett (1998) Estimating soil water retention characteristics from limited data in Brazilian Amazonia.
#'
#' @export
sptf_bd14 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.578 - 0.054 * A_C_OF - 0.006 * A_SILT_MI - 0.004 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Bernoux et al. (1998)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Bernoux et al. (1998) Bulk Densities of Brazilian Amazon Soils Related to Other Soil Properties
#'
#' @export
sptf_bd15 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.398 - 0.042 * A_C_OF - 0.0047 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Post & Kwon (2000)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Post & Kwon (2000) Soil carbon sequestration and land-use change: processes and potential
#'
#' @export
sptf_bd16 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.244 * 1.640 / (1.640 * A_SOM_LOI/100 + 0.244 * (1 - A_SOM_LOI/100))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the bulk density given the pedotransferfunction of Leonaviciute (2000).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Leonaviciute (2000). Predicting soil bulk and particle densities by pedotransfer functions from existing soil data in Lithuania.
#'
#' @export
sptf_bd17 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.70398 - 0.00313 * A_SILT_MI + 0.00261 * A_CLAY_MI - 0.11245 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Leonaviciute (2000).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Leonaviciute (2000). Predicting soil bulk and particle densities by pedotransfer functions from existing soil data in Lithuania.
#'
#' @export
sptf_bd18 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.99915 - 0.00592 * log(A_SILT_MI) + 0.7712 * log(A_CLAY_MI) + 0.09371 * log(A_SAND_MI) - 0.08415 * log(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Leonaviciute (2000).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Leonaviciute (2000). Predicting soil bulk and particle densities by pedotransfer functions from existing soil data in Lithuania.
#'
#' @export
sptf_bd19 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.073 + 0.03273 * log(A_SILT_MI) + 0.03875 * log(A_CLAY_MI) + 0.07888 * log(A_SAND_MI) - 0.05431 * log(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Leonaviciute (2000).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Leonaviciute (2000). Predicting soil bulk and particle densities by pedotransfer functions from existing soil data in Lithuania.
#'
#' @export
sptf_bd20 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.06727 + 0.01074 * log(A_SILT_MI) + 0.0807 * log(A_CLAY_MI) + 0.08759 * log(A_SAND_MI) + 0.0565 * log(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Kaur et al. (2002)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Kaur et al. (2002). A pedo-transfer function (PTF) for estimating soil bulk density from basic soil data and its comparison with existing PTFs.
#'
#' @export
sptf_bd21 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := exp(0.313 - 0.191 * A_C_OF + 0.02102 * A_CLAY_MI - 0.000476 * A_CLAY_MI^2 - 0.00432 * A_SILT_MI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Tremblay et al. (2002)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Tremblay et al. (2002). Prediction of organic carbon content in upland forest soils of Quebec, Canada.
#'
#' @export
sptf_bd22 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.120 * 1.4/(1.4 * A_SOM_LOI/100 + 0.120 * (1-A_SOM_LOI / 100))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Prevost (2004)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Prevost (2004).Predicting Soil Properties from Organic Matter Content following Mechanical Site Preparation of Forest Soils.
#'
#' @export
sptf_bd23 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := exp(-1.81 - .892 * log(A_SOM_LOI/100) - 0.092 * log(A_SOM_LOI)^2)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Prevost (2004)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Prevost (2004).Predicting Soil Properties from Organic Matter Content following Mechanical Site Preparation of Forest Soils.
#'
#' @export
sptf_bd24 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.159 * 1.561 / (1.561 * A_SOM_LOI/100 + 0.159 * (1 - A_SOM_LOI/100))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Heuscher et al. (2005)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_H2O_T105 (numeric) The volumetric moisture content of the soil (\%)
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Heuscher et al. (2005) Using Soil Physical and Chemical Properties to Estimate Bulk Density.
#'
#' @export
sptf_bd25 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI,A_H2O_T105,B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_H2O_T105),length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_H2O_T105, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (A_C_OF in units %, and DEPTH in units cm)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_H2O_T105 = A_H2O_T105,
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.685 - 0.198 * A_C_OF^0.5 - 0.0133 * A_H2O_T105 + 0.0079 * A_CLAY_MI + 0.00014 * B_DEPTH - 0.0007 * A_SILT_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Perie & Quimet (2007)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Perie & Quimet (2007). Organic carbon, organic matter and bulk density relationships in boreal forest soils
#'
#' @export
sptf_bd26 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := -1.977 + 4.105 * A_SOM_LOI/100 - 1.229 * log(A_SOM_LOI/100) - 0.103 * log(A_SOM_LOI/100)^2]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Perie & Quimet (2007)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Perie & Quimet (2007). Organic carbon, organic matter and bulk density relationships in boreal forest soils
#'
#' @export
sptf_bd27 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (0.111 * 1.767)/(1.767 * A_SOM_LOI/100 + (1 - A_SOM_LOI/100) * 0.111)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Tranter et al. (2007)
#'
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Tranter et al. (2007). Building and testing conceptual and empirical models for predicting soil bulk density
#'
#' @export
sptf_bd28 <- function(A_SAND_MI, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_SAND_MI), length(B_DEPTH))
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (depth set in units cm)
  dt <- data.table(A_SAND_MI = A_SAND_MI, 
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.35 + 0.0045 * A_SAND_MI + (44.7 - A_SAND_MI)^2 * 6e-5 + 0.06 * log10(B_DEPTH)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Tranter et al. (2007)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Tranter et al. (2007). Building and testing conceptual and empirical models for predicting soil bulk density
#'
#' @export
sptf_bd29 <- function(A_C_OF,A_SAND_MI, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_SAND_MI), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (depth set in units cm)
  dt <- data.table(A_SAND_MI = A_SAND_MI, 
                   A_C_OF = A_C_OF * 0.1,
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.2 + 0.0021 * A_SAND_MI - 0.143 * A_C_OF/100 + (A_SAND_MI - 47.95)^2 + 6e-5 - 0.043 * log10(B_DEPTH)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Benites et al. (2007)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Benites et al. (2007). Pedotransfer functions for estimating soil bulk density from existing soil survey reports in Brazil.
#'
#' @export
sptf_bd30 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.5688 - 0.0005 * A_CLAY_MI - 0.0090 * A_C_OF]
  
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


