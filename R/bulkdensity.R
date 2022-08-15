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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75,  len = arg.length)
  
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
  dt[, value := 10^(0.09963 - 0.00064*log10(A_SOM_LOI) - 0.22302 * log10(A_SOM_LOI)^2)]
  
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
  
  # Collect data into a table (unit in g/g)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 0.01, value = NA_real_)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_H2O_T105, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
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
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
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
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm)
  dt <- data.table(A_SAND_MI = A_SAND_MI, 
                   A_C_OF = A_C_OF * 0.1,
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.2 + 0.0021 * A_SAND_MI - 0.143 * A_C_OF/100 + (A_SAND_MI - 47.95)^2 * 6e-5 - 0.043 * log10(B_DEPTH)]
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100,len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100)
  
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
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_H2O_T105, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
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
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
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
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
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
  dt[, value := 1.801 - 0.397 * log(A_SOM_LOI)]
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  checkmate::assert_numeric(B_ALTITUDE, lower = 0, upper = 10000, len = arg.length)
  checkmate::assert_numeric(B_SLOPE_DEGREE, lower = 0, upper = 90, len = arg.length)
  checkmate::assert_numeric(B_SLOPE_ASPECT, lower = 0, upper = 360, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_SLOPE_DEGREE, lower = 0, upper = 90, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 10, len = arg.length)
  
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
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 10, len = arg.length)
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 20, len = arg.length)
   
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
  
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)

  # Collect data into a table (OC in %)
  dt <- data.table(A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.777 + 0.00263 * A_SAND_MI - 0.0439 * log10(A_SILT_MI) + 0.00208* A_SILT_MI]
  
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
  
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.172 + 0.00250 * A_SAND_MI - 0.0341 * log10(A_SILT_MI) + 0.000877* A_SILT_MI]
  
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
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
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
  
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
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

#' Calculate the bulk density given the pedotransferfunction of Minasny & Hartemink (2011)
#'
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m).
#'
#' @import data.table
#' 
#' @references Minasny & Hartemink (2011) Predicting soil properties in the tropics
#'
#' @export
sptf_bd87 <- function(A_SOM_LOI,A_SAND_MI,B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_SAND_MI),length(B_DEPTH),length(A_SOM_LOI))
  
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (depth is in cm)
  dt <- data.table(A_SAND_MI = A_SAND_MI,
                   B_DEPTH = B_DEPTH * 100, 
                   A_SOM_LOI = A_SOM_LOI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 100/(A_SOM_LOI/0.224 + (100 - A_SOM_LOI)/(0.935 + 0.049*log10(B_DEPTH) + 0.0055 * A_SAND_MI + 0.000065 * (A_SAND_MI - 38.96)^2))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Shiri et al. (2017)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_CACO3_MI (numeric) The calcium carbonate content of the soil (\%)
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#'
#' @import data.table
#' 
#' @references Shiri et al. (2017) Modeling soil bulk density through a complete data scanning procedure: Heuristic alternatives
#'
#' @export
sptf_bd88 <- function(A_C_OF,A_CLAY_MI,A_CACO3_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_CACO3_MI),length(A_PH_WA))
  
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 20, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 10, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI, 
                   A_PH_WA = A_PH_WA,
                   A_CACO3_MI = A_CACO3_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := -0.247 * A_C_OF * atan(A_CLAY_MI/(A_CACO3_MI + 7.00216)) + 
                A_C_OF * atan(A_PH_WA)/(A_CACO3_MI + 10.505) + 1.53433]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Calhoun et al. (2001)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Calhoun et al. (2001) Predicting bulk density of Ohio Soils from Morphology, Genetic Principles, and Laboratory Characterization Data
#'
#' @export
sptf_bd89 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_SILT_MI))
  
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
   
  # Collect data into a table (all units in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.673 - 0.0071 * A_C_OF - 0.0017 * A_SILT_MI - 0.003 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Dexter (2004)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Dexter (2004). Soil physical quality Part I. Theory, effects of soil texture, density, and organic matter, and effects on root growth
#'
#' @export
sptf_bd90 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1 / (0.59 + 0.00163 * A_CLAY_MI + 0.0253 * A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Drew (1973)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Drew (1973) Bulk Density Estimation Based on Organic Matter Content of Some Minnesota Soils
#'
#' @export
sptf_bd91 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1 / (0.6268 + 0.0361 * A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Eschner et al. (2004)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Dexter (2004). Soil physical quality Part I. Theory, effects of soil texture, density, and organic matter, and effects on root growth
#'
#' @export
sptf_bd92 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.8014 - 0.8491 * log10(A_SOM_LOI+2) + 0.0026 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Kobal et al. (2011)
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Kobal et al. (2011) PEDOTRANSFER FUNCTIONS FOR BULK DENSITY ESTIMATION OF FOREST SOILS
#'
#' @export
sptf_bd93 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.4842 - 0.1424 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the bulk density given the pedotransferfunction of Rawls et al. (2004)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Rawls et al. (2004)
#'
#' @export
sptf_bd94 <- function(A_SOM_LOI,A_SAND_MI,A_CLAY_MI) {
  
  # add visual binding
  w = x = y = z = NULL
  
  # Check input
  arg.length <- max(length(A_SAND_MI),length(A_CLAY_MI),length(A_SOM_LOI))
  
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (all units in %)
  dt <- data.table(A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI, 
                   A_SOM_LOI = A_SOM_LOI,
                   x = -1.2141 + 4.23123 * A_SAND_MI * 0.01,
                   y = -1.70126 + 7.55319 * A_CLAY_MI * 0.01,
                   z = -1.55601 + 0.507094 * A_SOM_LOI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, w := -0.0771892 + 0.256629*x + 0.256704*x^2-0.140911*x^3 - 0.0237361 * y - 0.098737 * x * y -
            0.140381*y^2 + 0.0140902*x*y^2 + 0.0287001*y^3]
  dt[, value := 1.36411 + 0.185628 * (0.0845397 + 0.701658 * w - 0.614038*w^2 -1.18871*w^3+
                0.0991862*y-0.301816*w*y-0.153337*w^2*y-0.072242*y^2 + 0.392736*w*y^2 + 0.0886315*y^3-
                0.601301*z + 0.651673*w*z-1.37484*w^2*z + 0.298823*y*z-0.192686*w*z*y +
                0.0815752*y^2*z-0.0450214*z^2-0.179529*w*z^2-0.0797412*y*z^2 + 0.00942183*z^3)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Reidy et al. (2016)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Reidy et al. (2016). Pedotransfer functions for Irish soils – estimation of bulk density (b) per horizon type
#'
#' @export
sptf_bd95 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.705925 - 0.342497 * A_C_OF^0.5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Saini (1966)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references  Saini (1966). Organic Matter as a Measure of Bulk Density of Soil
#'
#' @export
sptf_bd96 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.53 - 0.05 * A_SOM_LOI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Sevastas et al. (2018) 
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Sevastas et al. (2018). Predicting bulk density using pedotransfer functions for soils in the Upper Anthemountas basin, Greece
#'
#' @export
sptf_bd97 <- function(A_C_OF, A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 2.268 - 0.179 * log(A_SAND_MI) - 0.345 * log(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Sevastas et al. (2018)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Sevastas et al. (2018). Predicting bulk density using pedotransfer functions for soils in the Upper Anthemountas basin, Greece
#'
#' @export
sptf_bd98 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 2.039 - 0.563 * A_C_OF + 0.103 * A_C_OF^2]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Williams (1970)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Williams (1970).Relationships Between the Composition of Soils and Physical Measurements Made on Them
#'
#' @export
sptf_bd99 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.37 - 0.076 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Williams (1970)
#'
#' @param A_N_RT (numeric) The nitrogen content of the soil (mg / kg).
#'
#' @import data.table
#' 
#' @references Williams (1970).Relationships Between the Composition of Soils and Physical Measurements Made on Them
#'
#' @export
sptf_bd100 <- function(A_N_RT) {
  
  # Check input
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 10000)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_N_RT = A_N_RT * 0.001 * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.42 - 0.78 * A_N_RT]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Williams (1970) 
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Williams (1970).Relationships Between the Composition of Soils and Physical Measurements Made on Them
#'
#' @export
sptf_bd101 <- function(A_C_OF, A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.36 + 0.001 * A_SAND_MI - 0.076 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Wu et al. (2003)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Wu et al. (2003) Distribution and storage of soil organic carbon in China
#'
#' @export
sptf_bd102 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.2901 - 0.1229 * log(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Wu et al. (2003)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Wu et al. (2003) Distribution and storage of soil organic carbon in China
#'
#' @export
sptf_bd103 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.3774 * exp(-0.0413 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Yang et al. (2007)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Yang et al. (2007). Storage, patterns and environmental controls of soil organic carbon in China
#'
#' @export
sptf_bd104 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.29 + 1.2033 * exp(-0.075 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Zinke et al. (1986).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Zinke et al. (1986).Wordlwide organic soil carbon and nitrogen data.
#'
#' @export
sptf_bd105 <- function(A_C_OF, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm, and OC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.446 - 0.000645 * B_DEPTH - 0.344 * log10(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Zinke et al. (1986).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Zinke et al. (1986).Wordlwide organic soil carbon and nitrogen data.
#'
#' @export
sptf_bd106 <- function(A_C_OF, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm, and OC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.242 - 0.000201 * B_DEPTH - 0.356 * log10(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Zinke et al. (1986).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Zinke et al. (1986).Wordlwide organic soil carbon and nitrogen data.
#'
#' @export
sptf_bd107 <- function(A_C_OF, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm, and OC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.413 - 0.000799 * B_DEPTH - 0.156 * log10(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Zinke et al. (1986).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Zinke et al. (1986).Wordlwide organic soil carbon and nitrogen data.
#'
#' @export
sptf_bd108 <- function(A_C_OF, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm, and OC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.386 - 0.000543 * B_DEPTH - 0.305 * log10(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Zinke et al. (1986).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Zinke et al. (1986).Wordlwide organic soil carbon and nitrogen data.
#'
#' @export
sptf_bd109 <- function(A_C_OF, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm, and OC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.478 - 0.00206 * B_DEPTH - 0.488 * log10(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Zinke et al. (1986).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Zinke et al. (1986).Wordlwide organic soil carbon and nitrogen data.
#'
#' @export
sptf_bd110 <- function(A_C_OF, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm, and OC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.231 - 0.00147 * B_DEPTH - 0.203 * log10(A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Chen et al. (2018)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Chen et al. (2018). Building a pedotransfer function for soil bulk density on regional dataset and testing its validity over a larger area
#'
#' @details This pedotransferfunction is an ensemble of six different PTFs. The best one of this study is saved as \code{\link{sptf_bd112}}. 
#' 
#' @export
sptf_bd111 <- function(A_C_OF) {
  
  # add visual bindings
  value1 = value2 = value3 = value4 = value5 = value6 = NULL
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   value1 = NA_real_,
                   value2 = NA_real_,
                   value3 = NA_real_,
                   value4 = NA_real_,
                   value5 = NA_real_,
                   value6 = NA_real_,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value1 := 1.658 - 0.228 * A_C_OF]
  dt[, value2 := 1.419 -0.197 * log10(A_C_OF)]
  dt[, value3 := 0.721 + 0.855 * exp(-0.172 * A_C_OF)]
  dt[, value4 := 1/(0.635 + 0.059 * A_C_OF)]
  dt[, value5 := exp(0.347 -0.092 * log(A_C_OF) -0.021 * log(A_C_OF)^2)]
  dt[, value6 := 100 / (1.724 * A_C_OF / -5.259 + (100 - 1.724 * A_C_OF)/1.724)]
  dt[, value := (value1 + value2 + value3 + value5 + value6 + value6)/6]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Chen et al. (2018)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Chen et al. (2018). Building a pedotransfer function for soil bulk density on regional dataset and testing its validity over a larger area
#'
#' @export
sptf_bd112 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1/(0.635 + 0.059 * A_C_OF)]

  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Souza et al. (2016)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-) 
#' 
#' @import data.table
#' 
#' @references Souza et al. (2016) Pedotransfer functions to estimate bulk density from soil properties and environmental covariates: Rio Doce basin
#'
#' @export
sptf_bd113 <- function(A_C_OF,A_CLAY_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI), length(A_PH_WA))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 10, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.9039322 - 0.0044017 * A_CLAY_MI - 0.069520 * A_C_OF + 0.1249228 * A_PH_WA]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Urbancic et al. (2011)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Urbancic et al. (2011) PEDOTRANSFER FUNCTIONS FOR BULK DENSITY ESTIMATION OF FOREST SOILS
#'
#' @export
sptf_bd114 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (units in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.4842 - 0.1424 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hallet et al. (1998)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Hallet et al. (1998) Derivation and evaluation of a set of pedogenically-based empirical algorithms for predicting bulk density in British soils
#'
#' @export
sptf_bd115 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.47 + 0.61 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hossain et al. (2015)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Hossain et al. (2015) Bulk density of mineral and organic soils in the Canada’s arctic and sub-arctic
#'
#' @export
sptf_bd116 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.701 + 0.952 * exp(-0.29 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hossain et al. (2015)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Hossain et al. (2015) Bulk density of mineral and organic soils in the Canada’s arctic and sub-arctic
#'
#' @export
sptf_bd117 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 00.074 + 2.632 * exp(-0.076 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hossain et al. (2015)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Hossain et al. (2015) Bulk density of mineral and organic soils in the Canada’s arctic and sub-arctic
#'
#' @export
sptf_bd118 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.071 + 1.322 * exp(-0.071 * A_C_OF)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Merry et al. (2005)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Merry et al. (2005). Used in Sevastas et al. (2018) Predicting bulk density using pedotransfer functions for soils in the Upper Anthemountas basin, Greece
#'
#' @export
sptf_bd119 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.608 - 0.0872 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Men et al. (2018).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Men et al. (2018) Investigation on Pedotransfer function for estimating soil bulk density in Hebei province
#'
#' @export
sptf_bd120 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.386 - 0.078 * A_C_OF + 0.001 * A_SILT_MI + 0.001 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Palladino et al. (2022).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_CACO3_MI (numeric) The calcium carbonate content of the soil (\%)
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#' @param B_ALTITUDE (numeric) The altitude (m)
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#'
#' @import data.table
#' 
#' @references Palladino et al. (2022).Developing pedotransfer functions for predicting soil bulk density in Campania 
#'
#' @export
sptf_bd121 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI, A_CACO3_MI, A_PH_WA, B_ALTITUDE, B_SLOPE_DEGREE) {
  
  # add visual binding
  B_ROCKS_FR = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI),
                    length(A_CACO3_MI), length(A_PH_WA),length(B_ALTITUDE),
                    length(B_SLOPE_DEGREE))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 20, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 10, len = arg.length)
  checkmate::assert_numeric(B_SLOPE_DEGREE, lower = 0, upper = 90, len = arg.length)
  checkmate::assert_numeric(B_ALTITUDE, lower = 0, upper = 10000, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_CACO3_MI = A_CACO3_MI,
                   A_PH_WA = A_PH_WA,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   B_ALTITUDE = B_ALTITUDE,
                   B_ROCKS_FR = 0,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.124 + 0.002 * A_SAND_MI + 0.004 * A_CLAY_MI - 0.048 * A_C_OF +
                0.002 * A_PH_WA + 0.007 * A_CACO3_MI - 1.09e-5 * B_ALTITUDE - 
                0.002 * B_SLOPE_DEGREE + 0.005 * B_ROCKS_FR]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Palladino et al. (2022).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Palladino et al. (2022).Developing pedotransfer functions for predicting soil bulk density in Campania 
#'
#' @export
sptf_bd122 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.131 + 0.00299 * A_SAND_MI + 0.0051158 * A_CLAY_MI - 0.0388105 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Palladino et al. (2022).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#'
#' @import data.table
#'
#' @details This pedotransferfunction is an ensemble of nine different PTFs, trained for the different regions. 
#' The best one of this study is saved as \code{\link{sptf_bd122}}. 
#' 
#' @references Palladino et al. (2022).Developing pedotransfer functions for predicting soil bulk density in Campania 
#'
#' @export
sptf_bd123 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI) {
  
  # set visual bindings
  v1 = v2 = v3 =v4 = v5 = v6 = v7 = v8 = v9 = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %) 
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3 for for all land systems classes in Campaania
  dt[, v1 := 1.232 - 0.001 * A_SAND_MI + 0.003 * A_CLAY_MI - 0.036 * A_C_OF]
  dt[, v2 := 1.115 + 0.001 * A_SAND_MI + 0.006 * A_CLAY_MI - 0.036 * A_C_OF]
  dt[, v3 := 1.668 - 0.004 * A_SAND_MI - 0.003 * A_CLAY_MI - 0.046 * A_C_OF]
  dt[, v4 := 1.360 + 0.003 * A_SAND_MI + 0.003 * A_CLAY_MI - 0.048 * A_C_OF]
  dt[, v5 := 0.458 + 0.011 * A_SAND_MI + 0.016 * A_CLAY_MI - 0.011 * A_C_OF]
  dt[, v6 := 0.988 + 0.004 * A_SAND_MI + 0.006 * A_CLAY_MI - 0.022 * A_C_OF]
  dt[, v7 := 1.329 + 0.003 * A_SAND_MI + 0.000 * A_CLAY_MI - 0.044 * A_C_OF]
  dt[, v8 := 1.261 + 0.002 * A_SAND_MI + 0.001 * A_CLAY_MI - 0.030 * A_C_OF]
  dt[, v9 := 1.165 + 0.004 * A_SAND_MI + 0.001 * A_CLAY_MI - 0.035 * A_C_OF]
  dt[, value := (v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9)/9]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Gosselink et al. (1984)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Gosselink et al. (1984). Relationship of organic carbon and mineral content to bulk density in Louisiana Marsch Soils
#'
#' @export
sptf_bd124 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.013 + 0.024 * 100 / A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Gosselink et al. (1984)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Gosselink et al. (1984). Relationship of organic carbon and mineral content to bulk density in Louisiana Marsch Soils
#'
#' @export
sptf_bd125 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.039 + 0.0193 * 100 / A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Foldal et al. (2020).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Foldal et al. (2020). Deriving regional pedotransfer functions to estimate soil bulk density in Austria
#'
#' @export
sptf_bd126 <- function(A_C_OF, A_SILT_MI, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_SILT_MI), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm, and OC in g/kg)
  dt <- data.table(A_C_OF = A_C_OF, 
                   A_SILT_MI = A_SILT_MI,
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := pmax(0.3,1.650 - 0.0022 * B_DEPTH - 0.3157 * A_C_OF^0.5 + 0.0028 * A_SILT_MI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Foldal et al. (2020).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Foldal et al. (2020). Deriving regional pedotransfer functions to estimate soil bulk density in Austria
#'
#' @export
sptf_bd127 <- function(A_C_OF, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm, and OC in g/kg)
  dt <- data.table(A_C_OF = A_C_OF, 
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := pmax(0.5,1.873 - 0.0021 * B_DEPTH - 0.3042 * A_C_OF^0.5)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the bulk density given the pedotransferfunction of Foldal et al. (2020).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Foldal et al. (2020). Deriving regional pedotransfer functions to estimate soil bulk density in Austria
#'
#' @export
sptf_bd128 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units g/kg)
  dt <- data.table(A_C_OF = A_C_OF, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := pmax(0.4,1.523 - 0.3199 * A_C_OF^0.5)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Foldal et al. (2020).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Foldal et al. (2020). Deriving regional pedotransfer functions to estimate soil bulk density in Austria
#'
#' @export
sptf_bd129 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm, and OC in g/kg)
  dt <- data.table(A_C_OF = A_C_OF, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := pmax(0.3, 1.698 - 0.2737 * A_C_OF^0.5 + 0.0009 * A_SILT_MI - 0.002 * A_CLAY_MI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Foldal et al. (2020).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Foldal et al. (2020). Deriving regional pedotransfer functions to estimate soil bulk density in Austria
#'
#' @export
sptf_bd130 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units g/kg)
  dt <- data.table(A_C_OF = A_C_OF, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.682 - 0.2738 * A_C_OF^0.5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Foldal et al. (2020).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Foldal et al. (2020). Deriving regional pedotransfer functions to estimate soil bulk density in Austria
#'
#' @export
sptf_bd131 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units g/kg)
  dt <- data.table(A_C_OF = A_C_OF, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.720 - 0.2306 * A_C_OF^0.5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Foldal et al. (2020).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Foldal et al. (2020). Deriving regional pedotransfer functions to estimate soil bulk density in Austria
#'
#' @export
sptf_bd132 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units g/kg)
  dt <- data.table(A_C_OF = A_C_OF, 
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.693 - 0.2635 * A_C_OF^0.5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Nussbaum et al. (2016).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#' @param B_SLOPE_DEGREE (numeric) The slope of the field (degrees)
#'
#' @import data.table
#' 
#' @references Nussbaum et al. (2016). Pedotransfer function to predict density of forest soils in Switzerland
#'
#' @export
sptf_bd133 <- function(A_C_OF, B_DEPTH, B_SLOPE_DEGREE) {
  
  # add visual binding
  B_ROCKS_FR = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(B_DEPTH),length(B_SLOPE_DEGREE))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_SLOPE_DEGREE, lower = 0, upper = 90, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF, 
                   B_DEPTH = B_DEPTH,
                   B_SLOPE_DEGREE = B_SLOPE_DEGREE,
                   B_ROCKS_FR = 0,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.948 - 0.002 * A_C_OF + 0.257 * B_DEPTH^0.5 - 0.025 * B_SLOPE_DEGREE - 0.002 * B_ROCKS_FR]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hoogsteen et al. (2020)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Hoogsteen et al. (2020). Monitoring soil organic matter on grassland farms: An exploratory analysis
#'
#' @export
sptf_bd134 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table (SOM in units g/kg)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 10, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.56 * exp(-0.004 * A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hoogsteen et al. (2020)
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Hoogsteen et al. (2020). Monitoring soil organic matter on grassland farms: An exploratory analysis
#'
#' @export
sptf_bd135 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table (SOM in units g/kg)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 10, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.65 * exp(-0.006 * A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Khodaverdiloo et al. (2020).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Khodaverdiloo et al. (2020). Recalibration of existing pedotransfer functions to estimate soil bulk density at a regional scale
#'
#' @export
sptf_bd136 <- function(A_C_OF) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = NULL
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (units in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # add multiple estimates from this study
  dt[, v1 := 1.42 * exp(-0.08 * A_C_OF)]
  dt[, v2 := 1.34 -0.22 * exp(0.23 * A_C_OF)]
  dt[, v3 := 1.55 - 0.25 * A_C_OF^0.5]
  dt[, v4 := exp(0.46 -0.15 * (A_C_OF * 1.724)^0.5)]
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (v1 + v2 + v3 + v4) / 4]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Sun et al. (2019)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#'  
#' @import data.table
#' 
#' @details This pedotransferfunction is an ensemble of 6 different PTFs, trained with different soil properties. 
#' The best one of this study is saved as \code{\link{sptf_bd138}}. 
#' 
#' @references Sun et al. (2019) Comparison of estimated soil bulk density using proximal soil sensing and pedotransfer functions
#'
#' @export
sptf_bd137 <- function(A_SOM_LOI, A_SILT_MI,A_PH_WA,B_DEPTH) {
  
  # add visual bindings
  v1 = v2 =v3 = v4 =v5 =v6 = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_SILT_MI),length(A_PH_WA),length(B_DEPTH))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 10, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (OS in g/kg, depth in cm)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 10, 
                   A_SILT_MI = A_SILT_MI,
                   A_PH_WA = A_PH_WA,
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # add estimates of bulk density
  dt[, v1 := 1.59 - 0.012 * A_SOM_LOI]
  dt[, v2 := 1.58 - 0.0058 * A_SILT_MI]
  dt[, v3 := 0.87 + 0.11 * A_PH_WA]
  dt[, v4 := 1.17 + 0.0093 * B_DEPTH]
  dt[, v5 := 1.76 - 0.011 * A_SOM_LOI - 0.0043 * A_SILT_MI]
  dt[, v6 := 1.4 - 0.0061 * A_SILT_MI + 0.0094 * B_DEPTH]
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (v1 + v2 + v3 + v4 + v5 + v6) / 6]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Sun et al. (2019)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'  
#' @import data.table
#' 
#' @references Sun et al. (2019) Comparison of estimated soil bulk density using proximal soil sensing and pedotransfer functions
#'
#' @export
sptf_bd138 <- function(A_SOM_LOI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
 
  # Collect data into a table (OS in g/kg)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 10, 
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.76 - 0.011 * A_SOM_LOI - 0.0043 * A_SILT_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Pellegrini (2007)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Pellegrini (2007). A new pedotransfer function for estimating soil bulk density
#'
#' @export
sptf_bd139 <- function(A_SOM_LOI, A_CLAY_MI, A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.6776 - 1.2e-5 * A_CLAY_MI^2 - 0.00045 * A_SAND_MI + 0.2351 * A_SOM_LOI^0.5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Patil et al (2012).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Patil et al (2012). Estimation of bulk density of waterlogged soils from basic properties
#'
#' @export
sptf_bd140 <- function(A_SOM_LOI, A_CLAY_MI, A_SAND_MI,A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 10^(-1.2628 + 0.01409 * A_SAND_MI + 0.0134 * A_SILT_MI + 0.01516 * A_CLAY_MI - 0.0675 * A_SOM_LOI * 1.724)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Mello (2007).
#'
#' @param A_C_OF (numeric) The organic C content of the soil (g / kg).
#' @param A_N_RT (numeric) The total N content of the soil (mg / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Mello (2007). Estimativas dos estoques de carbono dos solos nos Estados de Rondonia e Mato Grosso anteriores a inervencao antropica.
#'
#' @export
sptf_bd141 <- function(A_C_OF, A_N_RT,A_CLAY_MI, A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI),length(A_N_RT))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 10000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_N_RT = A_N_RT * 0.001 * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.28 + 0.0017 * A_SAND_MI - 0.098 * A_C_OF + 0.58 * A_N_RT - 0.0023 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Makovnikova et al. (2017).
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Makovnikova et al. (2017). Comparison of different models for predicting soil bulk density. Case study – Slovakian agricultural soils
#'
#' @export
sptf_bd142 <- function(A_CLAY_MI, A_SAND_MI,A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set SOC in units %)
  dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 3.1482 - 0.0118 * A_CLAY_MI - 0.017 * A_SAND_MI - 0.0152 * A_SILT_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Makovnikova et al. (2017).
#'
#' @param A_C_OF (numeric) The organic C content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Makovnikova et al. (2017). Comparison of different models for predicting soil bulk density. Case study – Slovakian agricultural soils
#'
#' @export
sptf_bd143 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI,A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set SOC in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 2.662 - 0.0076 * A_CLAY_MI - 0.0102 * A_SILT_MI - 0.0108 * A_SAND_MI - 0.0855 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Ramos et al. (2022).
#'
#' @param A_C_OF (numeric) The organic C content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Ramos et al. (2022).Pedotransference functions for prediction of density in soils of Piauí, Brazil
#'
#' @export
sptf_bd144 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set units in g/kg)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SAND_MI = A_SAND_MI * 10,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.875 - 0.0009 * A_CLAY_MI - 0.0002 * A_SAND_MI - 0.0622 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Ramos et al. (2022).
#'
#' @param A_C_OF (numeric) The organic C content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @import data.table
#' 
#' @details This pedotransferfunction is an ensemble of four different PTFs given variable soil types. The best one (general ptf) of this study is saved as \code{\link{sptf_bd144}}. 
#' 
#' @references Ramos et al. (2022).Pedotransference functions for prediction of density in soils of Piauí, Brazil
#'
#' @export
sptf_bd145 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI,A_SILT_MI) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set units in g/kg)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_SAND_MI = A_SAND_MI * 10,
                   A_SILT_MI = A_SILT_MI * 10,
                   value = NA_real_)
  
  # estimate soil density by soil group
  dt[, v1 := 1.128 - 0.0006 * A_SAND_MI - 0.0629 * A_C_OF]
  dt[, v2 := 1.807 - 0.0008 * A_SAND_MI + 0.0973 * A_C_OF]
  dt[, v3 := 1.563 + 0.0003 * A_SAND_MI]
  dt[, v4 := 1.642 + 0.0003 * A_SAND_MI - 0.1131 * A_C_OF + 0.0003 * A_SILT_MI]
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (v1 + v2 + v3 + v4) / 4]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Katuwal et al. (2020)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @details This pedotransferfunction is an ensemble of four different PTFs. The MLR model with other soil properties from this study is saved as \code{\link{sptf_bd147}}. 
#'
#' @references Katuwal et al. (2020). Predicting the dry bulk density of soils across Denmark: Comparison of single-parameter, multi-parameter, and vis–NIR based models
#'
#' @export
sptf_bd146 <- function(A_C_OF) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = NULL
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate BD via multiple SOC funs
  dt[, v1 := 1.62 - 0.1 * A_C_OF]
  dt[, v2 := 1.7 - 0.22 * A_C_OF^0.5]
  dt[, v3 := 1.62 * exp(-0.07 * A_C_OF)]
  dt[, v4 := 1.46 - 0.08 * log(A_C_OF)]
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (v1 + v2 + v3 + v4)/4]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Katuwal et al. (2020).
#'
#' @param A_C_OF (numeric) The organic C content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'  
#' @import data.table
#' 
#' @references Katuwal et al. (2020). Predicting the dry bulk density of soils across Denmark: Comparison of single-parameter, multi-parameter, and vis–NIR based models
#' 
#' @export
sptf_bd147 <- function(A_C_OF, A_CLAY_MI, A_SAND_MI,A_SILT_MI, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI),length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (set units in g/100 g, depth in cm)
  # silt should be coarse silt
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.901 - 0.002 * A_CLAY_MI - 0.004* A_SILT_MI - 0.004 * A_SAND_MI - 0.094  * A_C_OF + 0.001 * B_DEPTH]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Marcolin & Klein (2011).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Marcolin & Klein (2011). Determinação da densidade relativa do solo por uma função de pedotransferência para a densidade do solo máxima 
#'
#' @export
sptf_bd148 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (units in g/kg) 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 10, 
                   A_CLAY_MI = A_CLAY_MI * 10,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 2.03133855 - 0.00320878 * A_SOM_LOI - 0.00076508 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of  Chari et al. (2021). 
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#'
#' @import data.table
#' 
#' @details This pedotransferfunction is an ensemble of four different PTFs. The MLR model with other soil properties from this study is saved as \code{\link{sptf_bd147}}. 
#'
#' @references Chari et al. (2021). Predicting bulk density using pedotransfer functions for soils in Sistan plain
#'
#' @export
sptf_bd149 <- function(A_C_OF) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = v5 = NULL
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate BD via multiple SOC funs
  dt[, v1 := 1.4856 - 0.0942 * A_C_OF]
  dt[, v2 := 1.5386 - 0.1452 * A_C_OF^0.5]
  dt[, v3 := 1.4869 * exp(-0.0664 * A_C_OF)]
  dt[, v4 := 1.3985 - 0.04974 * log(A_C_OF)]
  dt[, v5 := 1.4936 - 0.1226 * A_C_OF + 0.0206 * A_C_OF^2]
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (v1 + v2 + v3 + v4 + v5)/5]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Yanti et al. (2021).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param B_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @import data.table
#' 
#' @references Yanti et al. (2021). Development of pedotransfer functions for predicting soil bulk density: A case study in Indonesian small island
#'
#' @export
sptf_bd150 <- function(A_C_OF, B_DEPTH) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(B_DEPTH))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 2, len = arg.length)
  
  # Collect data into a table (depth set in units cm, and OC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   B_DEPTH = B_DEPTH * 100,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.2684 - 0.0011 * B_DEPTH - 0.1774 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Sakin et al. (2011)
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Sakin et al. (2011). Bulk density of Harran plain soils in relation to other soil properties 
#'
#' @export
sptf_bd151 <- function(A_CLAY_MI, A_SAND_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.845 + 0.00381 * A_CLAY_MI + 0.00614 * A_SILT_MI + 0.00428 * A_SAND_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Sakin et al. (2011).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Sakin et al. (2011). Bulk density of Harran plain soils in relation to other soil properties 
#'
#' @export
sptf_bd152 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.64 - 0.0876 * A_SOM_LOI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Rubinic et al. (2019).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Rubinic et al. (2019). Prediction of bulk density in Croatian forest Pseudogleys based on contents of soil organic matter and clay 
#'
#' @export
sptf_bd153 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.16703 + 0.0129861 * A_CLAY_MI - 0.038885 * A_SOM_LOI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Gomes et al. (2017).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#' 
#' @import data.table
#' 
#' @references Gomes et al. (2017). The use of Pedotransfer functions and the estimation of carbon stock in the Central Amazon region
#'
#' @export
sptf_bd154 <- function(A_C_OF, A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_PH_WA))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 10, len = arg.length)
  
  # Collect data into a table (both in g/kg)
  dt <- data.table(A_C_OF = A_C_OF, 
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 0.609 - 0.015 * A_C_OF + 0.145 * A_PH_WA]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Gomes et al. (2017).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Gomes et al. (2017). The use of Pedotransfer functions and the estimation of carbon stock in the Central Amazon region
#'
#' @export
sptf_bd155 <- function(A_C_OF, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (both in g/kg)
  dt <- data.table(A_C_OF = A_C_OF, 
                   A_SILT_MI = A_SILT_MI * 10,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.419 - 0.029 * A_C_OF + 0.0002 * A_SILT_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Perreault et al. (2022)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Perreault et al. (2022) Development of Pedotransfer Functions to Predict Soil Physical Properties in Southern Quebec (Canada)
#'
#' @export
sptf_bd156 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1.5525 - 0.0195 * A_CLAY_MI - 0.1623 * A_C_OF]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Perreault et al. (2022)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Perreault et al. (2022) Development of Pedotransfer Functions to Predict Soil Physical Properties in Southern Quebec (Canada)
#'
#' @export
sptf_bd157 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := pmin(1.9,pmax(0.8, 0.6123 + 0.0067 * A_SILT_MI - 0.1123 * A_CLAY_MI - 0.1925 * A_C_OF))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Perreault et al. (2022)
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Perreault et al. (2022) Development of Pedotransfer Functions to Predict Soil Physical Properties in Southern Quebec (Canada)
#'
#' @export
sptf_bd158 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI) {
  
  # add visual binding
  B_ROCKS_FR = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   B_ROCKS_FR = 7.79,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := pmin(1.9,pmax(0.8,-0.0217 + 0.0076 * A_SILT_MI - 0.0747 * A_CLAY_MI - 0.3998 * A_C_OF + 0.1069 * B_ROCKS_FR))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Hoekstra & Poelman (1982).
#'
#' @param A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Hoekstra & Poelman (1982). Dichtheid van gronden gemeten aan de meest voorkomende bodemeenheden in Nederland.
#'
#' @export
sptf_bd159 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1 /(0.625 + 0.05 * A_C_OF + 0.0015 * A_CLAY_MI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Athira et al. (2019).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Athira et al. (2019). Influence of soil organic matter on bulk density in Coimbatore soils
#'
#' @export
sptf_bd160 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 2.19953 - 1.2794 * A_SOM_LOI + 0.21325 * A_SOM_LOI^2 + 0.02372 * A_SOM_LOI^3]
  dt[A_SOM_LOI > 2.5, value := 0.63]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Athira et al. (2019).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @details This pedotransferfunction is an ensemble of three different PTFs. The best one of this study is saved as \code{\link{sptf_bd160}}. 
#' 
#' @references Athira et al. (2019). Influence of soil organic matter on bulk density in Coimbatore soils
#'
#' @export
sptf_bd161 <- function(A_SOM_LOI) {
  
  # add visual bindings
  v1 = v2 = v3 = NULL
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate density with three ptf
  dt[, v1 := 2.19953 - 1.2794 * A_SOM_LOI + 0.21325 * A_SOM_LOI^2 + 0.02372 * A_SOM_LOI^3]
  dt[, v2 := 2.25226 - 1.43213 * A_SOM_LOI + 0.32843 * A_SOM_LOI^2]
  dt[, v3 := 1.73261 - 0.47684 * A_SOM_LOI]
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (v1 + v2 + v3)/3]
  dt[A_SOM_LOI > 2.5, value := 0.63]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Sakin et al (2012).
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Sakin et al (2012). Organic carbon organic matter and bulk density relationships in arid-semi arid soils in Southeast Anatolia region 
#'
#' @export
sptf_bd162 <- function(A_C_OF) {
  
  # add visual bindings
  v1 = v2 = NULL
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate BD via three ptf
  dt[,v1 := pmax(0.1,A_C_OF - 4.0862) / - 2.7624]
  dt[,v2 := exp((A_C_OF - 1.4342)/-3.584)]
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := (v1 + v2) / 2]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Tanveera et al (2012).
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Tanveera et al (2012). Relation of Soil bulk Density with Texture, Total organic matter content and Porosity in the Soils of Kandi Area of Kashmir valley, India
#'
#' @export
sptf_bd163 <- function(A_C_OF) {
  
  # add visual bindings
  v1 = v2 = NULL
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)

  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := pmin(1.6,1.224 * A_C_OF^-0.137)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Shaykewich and Zwarich (1968)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Shaykewich and Zwarich (1968). Relationships Between Soil Physical Constants and Soil Physical Components of Some Manitoba Soils
#'
#' @export
sptf_bd164 <- function(A_SOM_LOI, A_CLAY_MI, A_SAND_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  # SAND is here Very Fine Sand
  dt[, value := 1.77 - 0.0016 * A_SAND_MI - 0.0017 * A_SILT_MI - 0.0047 * A_CLAY_MI - 0.07 * A_SOM_LOI + 0.0008 * A_SOM_LOI * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Soane (1975).
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Soane (1975) Studies on some soil physical properties in relation to cultivations and traffic. Cites in: Soane (1990).
#'
#' @export
sptf_bd165 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := pmax(1.25,1.86 - 0.055 * A_SOM_LOI)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Taulya et al. (2005).
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Taulya et al. (2005) Validation of pedotransfer functions for soil bulk density estimation on a Lake Victoria Basin soilscape
#'
#' @export
sptf_bd166 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
  
  # Collect data into a table (in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := pmax(0.9,exp(0.265 - 0.145 * log(A_C_OF) -0.1 * log(A_C_OF)^2))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Poelman (1975)
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Poelman (1975) Dichtheid van de vaste delen van rivierkleigronden. In: https://edepot.wur.nl/299664.
#'
#' @export
sptf_bd167 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 100/ (A_SOM_LOI/1.47 + A_CLAY_MI/2.88 + (100 - A_SOM_LOI - A_CLAY_MI)/2.66)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Van der Sluijs (1988).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Van der Sluijs (1988) De poriënindex: een karakteristiek voor vergelijking van de pakking van gronden. 
#'
#' @export
sptf_bd168 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3 for A horizont
  dt[, value := 0.597 + 0.035 * A_SOM_LOI + 0.0009 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Van der Sluijs (1988).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Van der Sluijs (1988) De poriënindex: een karakteristiek voor vergelijking van de pakking van gronden. 
#'
#' @export
sptf_bd169 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3 for C horizont
  dt[, value := 0.628 + 0.0155 * A_SOM_LOI + 0.0024 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Van der Sluijs (1988).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Van der Sluijs (1988) De poriënindex: een karakteristiek voor vergelijking van de pakking van gronden. 
#'
#' @export
sptf_bd170 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3 for A horizont
  dt[, value := 0.618 + 0.023 * A_SOM_LOI + 0.0007 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Van der Sluijs (1988).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Van der Sluijs (1988) De poriënindex: een karakteristiek voor vergelijking van de pakking van gronden. 
#'
#' @export
sptf_bd171 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3 for C horizont
  dt[, value := 0.572 + 0.0053 * A_SOM_LOI + 0.0039 * A_CLAY_MI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Van der Sluijs (1988).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Van der Sluijs (1988) De poriënindex: een karakteristiek voor vergelijking van de pakking van gronden. 
#'
#' @export
sptf_bd172 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3 for A en B2horizont
  dt[, value := 0.637 + 0.0257 * A_SOM_LOI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Van der Sluijs (1988).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Van der Sluijs (1988) De poriënindex: een karakteristiek voor vergelijking van de pakking van gronden. 
#'
#' @export
sptf_bd173 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3 for A en B2horizont
  dt[, value := 0.598 + 0.0355 * A_SOM_LOI]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Van Wallenburg (1988).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Van Wallenburg (1988) De bodemdichtheid van koopveen-, weideveen- en waardveengronden in relatie met bodemkenmerken.
#'
#' @export
sptf_bd174 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in kg / m3
  dt[, value := 1457 - 578 * log10(A_SOM_LOI)]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Van Wallenburg (1988).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Van Wallenburg (1988) De bodemdichtheid van koopveen-, weideveen- en waardveengronden in relatie met bodemkenmerken.
#'
#' @export
sptf_bd175 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in kg / m3
  dt[, value := 1726 - 693 * log10(A_SOM_LOI)]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Van Wallenburg (1988).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#'
#' @import data.table
#' 
#' @references Van Wallenburg (1988) De bodemdichtheid van koopveen-, weideveen- en waardveengronden in relatie met bodemkenmerken.
#'
#' @export
sptf_bd176 <- function(A_SOM_LOI) {
  
  # Check input
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data into a table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, value = NA_real_)
  
  # estimate soil density in kg / m3
  dt[, value := 1251 - 564 * log10(A_SOM_LOI)]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Wosten (1997).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_SAND_M50 (numeric) The median of the sandfraction (um)
#'
#' @import data.table
#' 
#' @references Wosten (1997).Pedotransfer functions to evaluate soil quality.
#'
#' @export
sptf_bd177 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI, A_SAND_M50) {
  
  # add visual bindings
  A_LEEM_MI = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_M50))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_M50, lower = 0, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_LEEM_MI = A_CLAY_MI + 0.3 * A_SILT_MI,
                   A_SAND_M50 = A_SAND_M50,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1 / (-1.984 + 0.01841 * A_SOM_LOI + 0.032 * 1 + 0.00003576 * A_LEEM_MI^2 +
                       67.5 / A_SAND_M50 + 0.424 * log(A_SAND_M50))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Wosten (1997).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Wosten (1997).Pedotransfer functions to evaluate soil quality. 
#'
#' @export
sptf_bd178 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := 1/(0.603 + 0.003975 * A_CLAY_MI + 0.00207 * A_SOM_LOI^2 + 0.01781 * log(A_SOM_LOI))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Melendez (2017).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#'
#' @import data.table
#' 
#' @references Melendez (2017) PREDICTING SOIL BULK DENSITY USING HIERARCHICAL PEDOTRANSFER FUNCTIONS AND ARTIFICIAL NEURAL NETWORKS
#'
#' @export
sptf_bd179 <- function(A_SOM_LOI, A_C_OF = NA, A_CLAY_MI = NA, A_SILT_MI = NA, A_SAND_MI = NA, A_PH_WA = NA) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = v5 = v6 = v7 = id = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_C_OF),
                    length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI),
                    length(A_PH_WA))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 10, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI, 
                   A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # estimate SOM or A_C_OF when one is given
  dt[is.na(A_SOM_LOI), A_SOM_LOI := A_C_OF * 1.725]
  dt[is.na(A_C_OF), A_C_OF := A_SOM_LOI / 1.725]
  
  # estimate the bulk density with multiple ptf
  dt[, v1 := (1 - (0.708 - 0.004 * A_SAND_MI + 0.076 * log10(A_CLAY_MI))) * 2.65 * 0.959]
  dt[, v2 := 100 / ((A_SOM_LOI/0.181) + ((100-A_SOM_LOI)/1.586))]
  dt[, v3 := 1.7 - 0.089 * A_C_OF - 0.004 * A_SILT_MI - 0.003 * A_CLAY_MI]
  dt[, v4 := exp(0.332 - 0.087 * A_C_OF + 0.006 * A_CLAY_MI - 0.0001 * A_CLAY_MI^2  - 0.003 * A_SILT_MI)]
  dt[, v5 := 0.238 + (1.142 * exp(-0.127 * A_C_OF) + 0.003 * A_SAND_MI - 0.0007 * A_CLAY_MI)]
  dt[, v6 := 0.574 + 0.003 * A_CLAY_MI - 0.075 * A_C_OF + 0.097 * A_PH_WA + 0.005 * A_SAND_MI]
  dt[, v7 := 0.639 - 0.075 * A_C_OF - 0.0006 * A_CLAY_MI - 0.004 * A_SAND_MI + 0.119 * A_PH_WA]
  
  # estimate soil density in Mg m-3 = ton m-3
  dt <- melt(dt, id.vars = 'id',measure.vars = patterns("^v"))
  dt <- dt[,list(value = mean(value, na.rm=T)), by = id]
   
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Melendez (2017).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Melendez (2017) PREDICTING SOIL BULK DENSITY USING HIERARCHICAL PEDOTRANSFER FUNCTIONS AND ARTIFICIAL NEURAL NETWORKS
#'
#' @export
sptf_bd180 <- function(A_SOM_LOI,A_SAND_MI,A_CLAY_MI) {
  
  # add visual binding
  w = x = y = z = NULL
  
  # Check input
  arg.length <- max(length(A_SAND_MI),length(A_CLAY_MI),length(A_SOM_LOI))
  
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE,len = arg.length)
  
  # Collect data into a table (all units in %)
  dt <- data.table(A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI, 
                   A_SOM_LOI = A_SOM_LOI,
                   x = -1.2141 + 4.23123 * A_SAND_MI * 0.01,
                   y = -1.70126 + 7.55319 * A_CLAY_MI * 0.01,
                   z = -1.55601 + 0.507094 * A_SOM_LOI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, w := -0.0771892 + 0.256629*x + 0.256704*x^2-0.140911*x^3 - 0.0237361 * y - 0.098737 * x * y -
       0.140381*y^2 + 0.0140902*x*y^2 + 0.0287001*y^3]
  dt[, value := 1.303 + 0.223 * (0.088 + 3.085 * w + 1.075*w^2 -4.223*w^3+
                                        0.491*y+1.473*w*y+0.29*w^2*y+0.333*y^2 + 0.606*w*y^2 + 0.084*y^3-
                                        0.335*z + 0.844*w*z-1.308*w^2*z + 0.113*y*z-0.09*w*z*y +
                                        0.139*y^2*z-0.219*z^2-0.254*w*z^2-0.058*y*z^2 + 0.04*z^3)]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the bulk density given the pedotransferfunction of Abdel (2019).
#'
#' @param A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Abdel (2019). Linear Regression Models to EstimatevExchangeable Sodium Percentage and Bulk Density of Salt Affected Soils in Sahl El-Hossinia, El-Sharkia Governorate, Egypt
#'
#' @export
sptf_bd181 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, any.missing = FALSE,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (OM in g/kg, unit SOM differs from paper because otherwise negative numbers occcur)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 0.1, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # estimate soil density in Mg m-3 = ton m-3
  dt[, value := pmin(1.7,pmax(1.3,1.817 - 0.730 * A_SOM_LOI - 0.002 * A_CLAY_MI - 0.001 * A_SILT_MI))]
  
  # convert to kg / m3
  dt[, value := value * 1000]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}



