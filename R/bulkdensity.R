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
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI))
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
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
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
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI))
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
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
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
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
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
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
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
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI))
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
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI),length(A_H2O_T105),length(B_DEPTH))
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
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
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

#' Calculate the bulk density given the pedotransferfunction of Benites et al. (2007)
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Benites et al. (2007). Pedotransfer functions for estimating soil bulk density from existing soil survey reports in Brazil.
#'
#' @export
sptf_bd31 <- function(A_CLAY_MI) {
  
  # Check input
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_CLAY_MI = A_CLAY_MI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.5224 - 0.0005 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Ruehlmann & Korschens (2009)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Ruehlmann & Korschens (2009) Calculating the Effect of Soil Organic Matter Concentration on Soil Bulk Density
#'
#' @export
sptf_bd32 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (2.684 - 140.943 * 0.006) * exp(-0.006 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Keller & Hakansson (2010)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (#%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Keller & Hakansson (2010) Estimation of reference bulk density from soil particle size distribution and soil organic matter content.
#'
#' @export
sptf_bd33 <- function(A_SOM_LOI, A_CLAY_MI, A_SAND_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.31 + 0.012 * A_CLAY_MI + 0.0103 * A_SAND_MI - 0.00018 * A_CLAY_MI^2 - 
                8e-5 * A_SAND_MI^2 - 6e-4 * A_SILT_MI * A_SOM_LOI - 6e-4 * A_SAND_MI * A_SOM_LOI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Han et al. (2012)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Han et al. (2012).Pedotransfer Functions for Estimating Soil Bulk Density in China 
#'
#' @export
sptf_bd34 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := exp(0.5379 - 0.0653 * A_SOM_LOI^0.5)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Han et al. (2012)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Han et al. (2012).Pedotransfer Functions for Estimating Soil Bulk Density in China 
#'
#' @export
sptf_bd35 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.167 * 1.526 / (1.526 * A_SOM_LOI/100 + 0.167 * (1 - A_SOM_LOI/100))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hollis et al. (2012)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Hollis et al. (2012). Empirically-derived pedotransfer functions for predicting bulk density in European soils.
#'
#' @export
sptf_bd36 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.69794 + 0.750636 * exp(-0.230355 * A_C_OF) + 0.0008687 * A_SAND_MI - 0.0005164 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hollis et al. (2012)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Hollis et al. (2012). Empirically-derived pedotransfer functions for predicting bulk density in European soils.
#'
#' @export
sptf_bd37 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.4903 - 0.33293 * log(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hollis et al. (2012)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Hollis et al. (2012). Empirically-derived pedotransfer functions for predicting bulk density in European soils.
#'
#' @export
sptf_bd38 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.80806 + 0.82384 * exp(-0.27993 * A_C_OF) + 0.0014065 * A_SAND_MI - 0.0010299 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Suuster et al. (2011)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_H2O_T105 (numeric) The volumetric moisture content of the soil (\%)
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Suuster et al. (2011) Soil bulk density pedotransfer functions of the humus horizon in arable soils.
#'
#' @export
sptf_bd39 <- function(A_C_OF, A_CLAY_MI, A_H2O_T105,B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_H2O_T105),length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_H2O_T105, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (A_C_OF in units %, and DEPTH in units cm)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_H2O_T105 = A_H2O_T105,
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.76 - 0.45/A_H2O_T105 - 0.004 * B_DEPTH - 0.08 * A_C_OF + 0.00004 * A_CLAY_MI^2 + 0.01 * A_CLAY_MI - 0.0002 * A_H2O_T105 / A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hong et al. (2013)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Hong et al. (2013) Predicting and mapping soil available water capacity in Korea.
#'
#' @export
sptf_bd40 <- function(A_SOM_LOI, A_SAND_MI,B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_SAND_MI),length(B_DEPTH))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (DEPTH in units cm)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_SAND_MI = A_SAND_MI,
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 100/(A_SOM_LOI/0.224 + (100-A_SOM_LOI)/ (1.017 + 0.0032 * A_SAND_MI + 0.054 * log10(B_DEPTH)))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hong et al. (2013)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Hong et al. (2013) Predicting and mapping soil available water capacity in Korea. 
#'
#' @export
sptf_bd41 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.02 - 0.156 * log(A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Al-Qinna & Jaber (2013) 
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Al-Qinna & Jaber (2013). PREDICTING SOIL BULK DENSITY USING ADVANCED PEDOTRANSFER FUNCTIONS IN AN ARID ENVIRONMENT
#'
#' @export
sptf_bd42 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.654 - 0.163 * log10(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Al-Qinna & Jaber (2013) 
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Al-Qinna & Jaber (2013). PREDICTING SOIL BULK DENSITY USING ADVANCED PEDOTRANSFER FUNCTIONS IN AN ARID ENVIRONMENT
#'
#' @export
sptf_bd43 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.397 + 0.553 * exp(-0.740 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Al-Qinna & Jaber (2013) 
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Al-Qinna & Jaber (2013). PREDICTING SOIL BULK DENSITY USING ADVANCED PEDOTRANSFER FUNCTIONS IN AN ARID ENVIRONMENT.
#'
#' @export
sptf_bd44 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 67.086 /(1 + exp(3.809 + 0.128 * A_C_OF))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Al-Qinna & Jaber (2013) 
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Al-Qinna & Jaber (2013). PREDICTING SOIL BULK DENSITY USING ADVANCED PEDOTRANSFER FUNCTIONS IN AN ARID ENVIRONMENT.
#'
#' @export
sptf_bd45 <- function(A_C_OF, A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.398 - 0.138 * A_C_OF + 0.008 * A_SAND_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Al-Qinna & Jaber (2013) 
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Al-Qinna & Jaber (2013). PREDICTING SOIL BULK DENSITY USING ADVANCED PEDOTRANSFER FUNCTIONS IN AN ARID ENVIRONMENT.
#'
#' @export
sptf_bd46 <- function(A_C_OF, A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.228 - 0.155 * log10(A_C_OF) + 0.008 * A_SAND_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Al-Qinna & Jaber (2013) 
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Al-Qinna & Jaber (2013). PREDICTING SOIL BULK DENSITY USING ADVANCED PEDOTRANSFER FUNCTIONS IN AN ARID ENVIRONMENT.
#'
#' @export
sptf_bd47 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
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
  dt[, value := 1.724 + 0.175 *(0.027 * A_SAND_MI - 0.016 * A_CLAY_MI - 0.02 * A_SILT_MI - 0.787 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Nanko et al. (2014)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Nanko et al. (2014). A pedotransfer function for estimating bulk density of forest soil in Japan affected by volcanic ash.
#'
#' @export
sptf_bd48 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1/(0.882 + 0.133 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Nanko et al. (2014).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Nanko et al. (2014). A pedotransfer function for estimating bulk density of forest soil in Japan affected by volcanic ash. 
#'
#' @export
sptf_bd49 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 100 / (A_SOM_LOI/0.140 + (100- A_SOM_LOI)/1.152)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of De Vos et al. (2005).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references De Vos et al. (2005). Predictive Quality of Pedotransfer Functions for Estimating Bulk Density of Forest Soils. 
#'
#' @export
sptf_bd50 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.7749 - 0.1725 * A_SOM_LOI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of De Vos et al. (2005).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references De Vos et al. (2005). Predictive Quality of Pedotransfer Functions for Estimating Bulk Density of Forest Soils. 
#'
#' @export
sptf_bd51 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 100 / (A_SOM_LOI/0.312 + (100- A_SOM_LOI)/1.661)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Jeffrey (1970)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Jeffrey (1970). A Note on the use of Ignition Loss as a Means for the Approximate Estimation of Soil Bulk Density
#'
#' @export
sptf_bd52 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.482-0.6786 * log10(A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Harrison & Bocock (1981)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Harrison & Bocock (1981) Estimation of Soil Bulk-Density from Loss-on-Ignition Values.
#'
#' @export
sptf_bd53 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.558 - 0.728 * log10(A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Harrison & Bocock (1981)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Harrison & Bocock (1981) Estimation of Soil Bulk-Density from Loss-on-Ignition Values.
#'
#' @export
sptf_bd54 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.729-0.769 * log10(A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Harrison & Bocock (1981)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Harrison & Bocock (1981) Estimation of Soil Bulk-Density from Loss-on-Ignition Values.
#'
#' @export
sptf_bd55 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 2.089-1.156 * log10(A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Harrison & Bocock (1981)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Harrison & Bocock (1981) Estimation of Soil Bulk-Density from Loss-on-Ignition Values.
#'
#' @export
sptf_bd56 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.392-0.556 * log10(A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Harrison & Bocock (1981)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Harrison & Bocock (1981) Estimation of Soil Bulk-Density from Loss-on-Ignition Values.
#'
#' @export
sptf_bd57 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.8 - 0.772 * log10(A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Grigal (1989)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Grigal (1989). Bulk density of surface soils and peat in the north central united states
#'
#' @export
sptf_bd58 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.073 + 2.369 * exp(-0.0730 * A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Grigal (1989)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Grigal (1989). Bulk density of surface soils and peat in the north central united states
#'
#' @export
sptf_bd59 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.669 + 0.941 * exp(-0.240 * A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Grigal (1989)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Grigal (1989). Bulk density of surface soils and peat in the north central united states
#'
#' @export
sptf_bd60 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 4.258 * exp(-0.047 * A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Grigal (1989)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Grigal (1989). Bulk density of surface soils and peat in the north central united states
#'
#' @export
sptf_bd61 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.043 + 4.258 * exp(-0.047 *A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Honeysett & Ratkowsky (1989).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Honeysett & Ratkowsky (1989). The use of ignition loss to estimate bulk density of forest soils. 
#'
#' @export
sptf_bd62 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1/(0.564 + 0.0556 * A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Honeysett & Ratkowsky (1989).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Honeysett & Ratkowsky (1989). The use of ignition loss to estimate bulk density of forest soils. 
#'
#' @export
sptf_bd63 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.801 - 0.397 * ln(A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Cienciala et al. (2006)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Cienciala et al. (2006). Forest topsoil organic carbon content in Southwest Bohema region 
#'
#' @export
sptf_bd64 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 100 / (A_SOM_LOI/0.244 + (100 - A_SOM_LOI)/ 1.41)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Aguilera et al. (2013)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Aguilera et al. (2013). Managing soil carbon for climate change mitigation and adaptation in Mediterranean cropping systems: A meta-analysis 
#'
#' @export
sptf_bd65 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units g/kg)
  dt <- data.table(A_C_OF = A_C_OF, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.8403 - 0.443 * log10(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Howard et al. (1995)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Howard et al. (1995) The carbon content of soil and its geographical distribution in Great Britain 
#' 
#'
#' @export
sptf_bd66 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.3 - 0.275 * log(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Song et al. (2005)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Song et al. (2005) Topsoil organic carbon storage of China and its loss by cultivation 
#'
#' @export
sptf_bd67 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units g/kg)
  dt <- data.table(A_C_OF = A_C_OF, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.377 * exp(-0.0048 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Song et al. (2005)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Song et al. (2005) Topsoil organic carbon storage of China and its loss by cultivation 
#'
#' @export
sptf_bd68 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units g/kg)
  dt <- data.table(A_C_OF = A_C_OF, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.3565 * exp(-0.0046 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Qiao et al. (2019)
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Qiao et al. (2019) Development of pedotransfer functions for predicting the bulk density in the critical zone on the Loess Plateau, China
#'
#' @export
sptf_bd69 <- function(A_CLAY_MI, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(B_DEPTH))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (depth set in units cm)
  dt <- data.table(A_CLAY_MI = A_CLAY_MI, 
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.68 + 0.001 * B_DEPTH - 2.249 / A_CLAY_MI - 0.086/B_DEPTH]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Wang et al. (2014)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#' @param B_ALTITUDE (numeric) The altitude (m)
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#' @param B_SLOPE_ASPECT (numeric) The slope aspect (degrees)
#'
#' @import data.table
#' 
#' @references Wang et al. (2014) Prediction of Bulk Density of Soils in the Loess Plateau Region of China
#'
#' @export
sptf_bd70 <- function(A_C_OF,A_CLAY_MI, A_SILT_MI,B_DEPTH, B_ALTITUDE,B_SLOPE_DEGREE,B_SLOPE_ASPECT) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI), length(A_SILT_MI),
                    length(B_DEPTH),length(B_ALTITUDE),
                    length(B_SLOPE_DEGREE),length(B_SLOPE_ASPECT))
  
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_ALTITUDE, lower = 0, upper = 10000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_SLOPE_DEGREE, lower = 0, upper = 90, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_SLOPE_ASPECT, lower = 0, upper = 360, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (depth set in units cm)
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI, 
                   A_SILT_MI = A_SILT_MI,
                   B_DEPTH = B_DEPTH * 100,
                   B_ALTITUDE = B_ALTITUDE,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   B_SLOPE_ASPECT = B_SLOPE_ASPECT,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.795 + 0.003 * A_CLAY_MI - 0.005 * A_SILT_MI - 0.01 * A_C_OF - 0.000022 * B_ALTITUDE - 0.003 * B_SLOPE_DEGREE + 0.000001 * B_SLOPE_ASPECT]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Wang et al. (2014)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#'
#' @import data.table
#' 
#' @references Wang et al. (2014) Prediction of Bulk Density of Soils in the Loess Plateau Region of China
#'
#' @export
sptf_bd71 <- function(A_C_OF,A_CLAY_MI, A_SILT_MI,B_SLOPE_DEGREE) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI), length(A_SILT_MI),
                    length(B_SLOPE_DEGREE))
  
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_SLOPE_DEGREE, lower = 0, upper = 90, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (depth set in units cm)
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI, 
                   A_SILT_MI = A_SILT_MI,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.8284 + 0.0429 * log10(A_CLAY_MI) + 0.0205 * A_CLAY_MI^0.5 - 0.0125 * cos(A_CLAY_MI) -
       0.0061 * A_SILT_MI + 0.0001 * A_SILT_MI * B_SLOPE_DEGREE - 0.0098 * B_SLOPE_DEGREE - 0.0071 * A_C_OF -
       0.0505 * A_C_OF^0.5 + 0.0002 * A_C_OF^2]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Katterer et al (2006)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Katterer et al (2006) Pedotransfer functions for estimating plant available water and bulk density in Swedish agricultural soils 
#'
#' @export
sptf_bd72 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.6 - 0.119 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Katterer et al (2006)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Katterer et al (2006) Pedotransfer functions for estimating plant available water and bulk density in Swedish agricultural soils 
#'
#' @export
sptf_bd73 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.63 - 0.125 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Katterer et al (2006)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Katterer et al (2006) Pedotransfer functions for estimating plant available water and bulk density in Swedish agricultural soils 
#'
#' @export
sptf_bd74 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.76 - 0.145 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Katterer et al (2006)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Katterer et al (2006) Pedotransfer functions for estimating plant available water and bulk density in Swedish agricultural soils 
#'
#' @export
sptf_bd75 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.63 - 0.111 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Katterer et al (2006)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Katterer et al (2006) Pedotransfer functions for estimating plant available water and bulk density in Swedish agricultural soils 
#'
#' @export
sptf_bd76 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.1 * exp(-0.036 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Katterer et al (2006)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Katterer et al (2006) Pedotransfer functions for estimating plant available water and bulk density in Swedish agricultural soils 
#'
#' @export
sptf_bd77 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.64 - 0.148 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Brahim et al. (2012)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#'
#' @import data.table
#' 
#' @references Brahim et al. (2012). Pedotransfer functions to estimate soil bulk density for Northern Africa: Tunisia case
#'
#' @export
sptf_bd78 <- function(A_C_OF,A_CLAY_MI, A_SAND_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI), length(A_SAND_MI),
                    length(A_PH_WA))
  
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 10, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (OC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI, 
                   A_SAND_MI = A_SAND_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.65 - 0.117 * A_C_OF - 0.0042 * A_CLAY_MI - 0.0036 * A_SAND_MI + 0.031 * A_PH_WA]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Brahim et al. (2012)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#'
#' @import data.table
#' 
#' @references Brahim et al. (2012). Pedotransfer functions to estimate soil bulk density for Northern Africa: Tunisia case
#'
#' @export
sptf_bd79 <- function(A_C_OF,A_SILT_MI, A_SAND_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_SILT_MI), length(A_SAND_MI),
                    length(A_PH_WA))
  
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 10, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (OC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_SILT_MI = A_SILT_MI, 
                   A_SAND_MI = A_SAND_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.9 - 0.08 * A_C_OF + 0.007 * A_SAND_MI + 0.007 * A_SILT_MI + 0.05 * A_PH_WA]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Brahim et al. (2012)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_CACO3_MI (numeric) The calcium carbonate content of the soil (\%)
#'
#' @import data.table
#' 
#' @references Brahim et al. (2012). Pedotransfer functions to estimate soil bulk density for Northern Africa: Tunisia case
#'
#' @export
sptf_bd80 <- function(A_C_OF,A_CLAY_MI, A_CACO3_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI), length(A_CACO3_MI))
  
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 20, any.missing = FALSE,len = arg.length)
   
  # Collect data into a table (OC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI, 
                   A_CACO3_MI = A_CACO3_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.9 - 0.08 * A_C_OF + 0.0031 * A_CLAY_MI - 0.0023 * A_CACO3_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Abeldaki (2016)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Abeldaki (2016) Evaluation of pedotransfer functions for predicting soil bulk density for U.S. soils 
#'
#' @export
sptf_bd81 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.449 * exp(-0.03 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Apka et al. (2016)
#'
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Apka et al. (2016). Enhancing pedotransfer functions with environmental data for estimating bulk density and effective cation exchange capacity in a data-sparse situation
#'
#' @export
sptf_bd82 <- function(A_SAND_MI,A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SAND_MI),length(A_SILT_MI))
  
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)

  # Collect data into a table (OC in %)
  dt <- data.table(A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.777 + 0.00263 * A_SAND_MI - 0.0439 * log10*A_SILT_MI + 0.00208* A_SILT_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Apka et al. (2016)
#'
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Apka et al. (2016). Enhancing pedotransfer functions with environmental data for estimating bulk density and effective cation exchange capacity in a data-sparse situation
#'
#' @export
sptf_bd83 <- function(A_SAND_MI,A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SAND_MI),length(A_SILT_MI))
  
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.172 + 0.00250 * A_SAND_MI - 0.0341 * log10*A_SILT_MI + 0.000877* A_SILT_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Beutler et al. (2016)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Beutler et al. (2016) Bulk Density Prediction for Histosols and Soil Horizons with High Organic Matter Content
#'
#' @export
sptf_bd84 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (clay in g / kg)
  dt <- data.table(A_C_OF = A_C_OF, 
                   A_CLAY_MI = A_CLAY_MI * 10,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (1.6179 - 0.018 * (A_CLAY_MI +1)^0.46 - 0.0398 * A_C_OF^0.55)^1.33]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the bulk density given the pedotransferfunction of Beutler et al. (2016)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Beutler et al. (2016) Bulk Density Prediction for Histosols and Soil Horizons with High Organic Matter Content
#'
#' @export
sptf_bd85 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (4.0899 - 2.3978 * A_C_OF^0.06)^3.85]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Botula et al. (2015)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Botula et al. (2015). Hierarchical Pedotransfer Functions to Predict Bulk Density of Highly Weathered Soils in Central Africa
#'
#' @export
sptf_bd86 <- function(A_C_OF,A_SAND_MI,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SAND_MI),length(A_CLAY_MI),length(A_C_OF))
  
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (all units in %)
  dt <- data.table(A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI, 
                   A_C_OF = A_C_OF * 0.1,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.64581 - 0.00362 * A_CLAY_MI - 0.01580 * A_C_OF - 0.0016 * A_SAND_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}
# in kaur 2002, zit meerder funs
# ruehlmann_2009 data uit duisland



