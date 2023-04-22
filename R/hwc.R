# predict concentration of Hot Water Carbon

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Ros (2011) Predicting soil Nitrogen Supply.
#'
#' @export
sptf_hwc1 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # experimental data from thesis Ros (2011), n = 100, R2 = 0.83, 0-30 cm soil
  dt[, value := exp(4.16433 + 0.88913 * log(A_C_OF) -0.1334 * log(A_CLAY_MI))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Ros (2011) Predicting soil Nitrogen Supply. Unpublished data from pot experiment and incubation experiment.
#'
#' @export
sptf_hwc2 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF, value = NA_real_)
  
  # experimental data from thesis Ros (2011), n = 47, R2 = 0.55, 0-30 cm soil
  dt[, value := exp(3.7473 + 0.9543 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#'
#' @import data.table
#' 
#' @references Ros et al. (2011) N-advies op nieuwe leest. NMI-report 1248.N.07, 65 pp.
#'
#' @export
sptf_hwc3 <- function(A_C_OF,A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # experimental data from Ros et al. (2011), n = 34, R2 = 0.54, 0-30 cm soil
  dt[,value := exp(4.7291 + 0.7980 * log(A_C_OF))]
  
  # experimental data from Ros et al. (2011), n = 34, R2 = 0.64, 0-30 cm soil
  dt[!is.na(A_PH_CC), value := exp(7.634 + 0.6312 * log(A_C_OF) -1.3548 * log(A_PH_CC))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for grassland soils in the Netherlands.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Van Eekeren & Bokhorst (2009) Beoordeling bodemkwaliteit zandgrond. Een inventarisatie van bodemindicatoren voor de veehouderij. ZvZ-rapport 7, 61 pp.
#'
#' @export
sptf_hwc4 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # experimental data from Van Eekeren & Bokhorst (2009), n = 20, R2 = 0.37, 0-30 cm soil
  dt[,value := exp(4.7593 + 0.7041 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands. Experimental data from 2019.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_P_AL (numeric) The phosphorus content of the soil, extracted in ammonium lactate (mg P2O5/100g)
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#'
#' @import data.table
#' 
#' @references Van Balen et al. (2015) Effecten bodem- en structuurverbeteraars. Onderzoek op klei- en zandgrond 2010-2015. PPO-report 693.
#'
#' @export
sptf_hwc5 <- function(A_C_OF,A_P_AL,A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_P_AL), length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_P_AL, lower = 10, upper = 250, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_P_AL = A_P_AL,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # experimental data from Van Balen et al. (2015), n = 51, R2 = 0.93, 0-30 cm soil
  dt[,value := exp(-3.74515 + 1.57893 * log(A_C_OF) + 0.38658 * log(A_P_AL) + 1.93596 * log(A_PH_CC))]
  
  # experimental data from Van Balen et al. (2015), n = 51, R2 = 0.92, 0-30 cm soil
  dt[is.na(A_PH_CC) & !is.na(A_P_AL),value := exp(1.78782 + 1.1213 * log(A_C_OF) + 0.24771 * log(A_P_AL))]
  
  # experimental data from Van Balen et al. (2015), n = 51, R2 = 0.91, 0-30 cm soil
  dt[is.na(A_PH_CC) & is.na(A_P_AL),value := exp(2.88539 + 1.06946 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands. Data from various years.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_P_AL (numeric) The phosphorus content of the soil, extracted in ammonium lactate (mg P2O5/100g)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Van Balen et al. (2015) Effecten bodem- en structuurverbeteraars. Onderzoek op klei- en zandgrond 2010-2015. PPO-report 693.
#'
#' @export
sptf_hwc6 <- function(A_C_OF,A_P_AL,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_P_AL), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_P_AL, lower = 10, upper = 250, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_P_AL = A_P_AL,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # experimental data from Van Balen et al. (2015), n = 120, R2 = 0.94, 0-30 cm soil
  dt[,value := exp(2.724 + 1.0069 * log(A_C_OF) + 0.12095 * log(A_P_AL) -0.0356 * log(A_CLAY_MI))]
  
  # experimental data from Van Balen et al. (2015), n = 120, R2 = 0.94, 0-30 cm soil
  dt[is.na(A_CLAY_MI) & !is.na(A_P_AL),value := exp(2.29256 + 1.05986 * log(A_C_OF) + 0.17284 * log(A_P_AL))]
  
  # experimental data from Van Balen et al. (2015), n = 120, R2 = 0.94, 0-30 cm soil
  dt[!is.na(A_CLAY_MI) & is.na(A_P_AL),value := exp(3.36622 + 0.96466 * log(A_C_OF) -0.06135 * log(A_CLAY_MI))]
  
  # experimental data from Van Balen et al. (2015), n = 120, R2 = 0.93, 0-30 cm soil
  dt[is.na(A_PH_CC) & is.na(A_CLAY_MI),value := exp(2.97662 + 1.05171 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands. Data from various years.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Fujita & Ros (2023) Evaluation of pedotransfer functions for soil indicators of BLN. NMI report 1819.N.20. Unpublished data of praktijknetwerk.
#'
#' @export
sptf_hwc7 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # experimental data from Fujita & Ros (2023), n = 32, R2 = 0.42, 0-30 cm soil
  dt[,value := exp(6.03315 + 0.21292 * log(A_C_OF) -0.16702 * log(A_CLAY_MI))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands. 
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Hanegraaf et al. (2009). Verkenning van bodemsensoren voor de landbouw. Interreg project Bodembreed, NMI project 1382, 93 pp.
#'
#' @export
sptf_hwc8 <- function(A_C_OF) {
  
  # Check input
  arg.length <- max(length(A_C_OF))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)

  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   value = NA_real_)
  
  # experimental data from Hanegraaf et al. (2009), n = 20, R2 = 0.65, 0-30 cm soil
  dt[,value := exp(4.5138 + 0.6997 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands. Data from various years.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#'
#' @import data.table
#' 
#' @references Hanegraaf et al. (2012) Rekenen met de kwaliteit van organische stof: kengetallen en rekenregels. NMI project 1314.N.09,
#'
#' @export
sptf_hwc9 <- function(A_C_OF,A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # experimental data from Hanegraaf et al. (2012), n = 104, R2 = 0.63, 0-30 cm soil
  dt[,value := exp(6.15815 + 0.71438 * log(A_C_OF) -0.89933 * log(A_PH_CC))]
  
  # experimental data from Hanegraaf et al. (2012), n = 104, R2 = 0.61, 0-30 cm soil
  dt[is.na(A_PH_CC),value := exp(4.51805 + 0.75414 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands. Data from various years.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#'
#' @import data.table
#' 
#' @references Hanegraaf et al. (2009). De afbraaksnelheid van organische stof in Drenthe. NMI project 972.N.09, 93 pp.
#'
#' @export
sptf_hwc10 <- function(A_C_OF,A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # experimental data from Hanegraaf et al. (2009), n = 149, R2 = 0.81, 0-30 cm soil
  dt[,value := exp(4.86 + 0.97704 * log(A_C_OF) -0.79092 * log(A_PH_CC))]
  
  # experimental data from Hanegraaf et al. (2009), n = 170, R2 = 0.83, 0-30 cm soil
  dt[is.na(A_PH_CC),value := exp(3.55037 + 0.99216 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands. Data from various years.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Van Eekeren & Bokhorst (2009) Beoordeling bodemkwaliteit zandgrond. Een inventarisatie van bodemindicatoren voor de veehouderij. ZvZ-rapport 7, 61 pp. Unpublished data.
#'
#' @export
sptf_hwc11 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # experimental data from Van Eekeren & Bokhorst (2009), n = 20, R2 = 0.69, 0-30 cm soil
  dt[,value := exp(4.5884 + 0.7674 * log(A_C_OF) -0.1793 * log(A_CLAY_MI))]
  
  # experimental data from Van Eekeren & Bokhorst (2009), n = 20, R2 = 0.65, 0-30 cm soil
  dt[is.na(A_CLAY_MI),value := exp(4.794 + 0.6406 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the HWC content
#'
#' This function calculates the hot water extractable carbon content for agricultural soils in the Netherlands. Data from various sources.
#' 
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Fujita & Ros (2023) Evaluation of pedotransfer functions for soil indicators of BLN. NMI report 1819.N.20. Unpublished data of praktijknetwerk.
#'
#' @export
sptf_hwc12 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # experimental data from Fujita & Ros (2023), n = 1033, R2 = 0.79, 0-30 cm soil
  dt[,value := exp(4.97245 + 0.60919 * log(A_C_OF) -0.72936 * log(A_CLAY_MI) + 0.19816 * log(A_CLAY_MI)*log(A_C_OF))]
  
  # experimental data from Fujita & Ros (2023), n = 1033, R2 = 0.83, 0-30 cm soil
  dt[is.na(A_CLAY_MI),value := exp(3.50813 + 1.0037 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}