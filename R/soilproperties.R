# functions to estimate soil properties

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 6.5 from SOM and clay content.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Wosten et al. (1997) Bodemkundige vertaalfuncties bij SC-DLO; state of the art. https://edepot.wur.nl/299664
#'
#' @export
sptf_cec1 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 0.01,
                   A_CLAY_MI = A_CLAY_MI * 0.01)
  
  # function for B-horizont of podzols / haplaquods (p30, inputs are in g/g)
  # other soils follow second one (here approxied by clay content)
  dt[, value := ifelse(A_CLAY_MI <= 5, 25 * A_SOM_LOI + 5 * A_CLAY_MI, 15 * A_SOM_LOI + 5 * A_CLAY_MI)]
    
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at 8.2
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_KCL (numeric) The acidity of the soil, pH in KCL (-)
#' 
#' @import data.table
#' 
#' @references Goselink & van Erp (1999). Cited in NMI-report 655.99.
#'
#' @export
sptf_cec2 <- function(A_SOM_LOI, A_CLAY_MI, A_PH_KCL) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_KCL = A_PH_KCL)
  
  # function for CEC from Goselink en van Erp (CEC in meq / 100 g x 10 = mmol+/kg)
  dt[, value := 10 * (0.4 * A_CLAY_MI + 0.01 * A_SOM_LOI * (66 - A_PH_KCL - 246))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at 8.2
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#' 
#' @import data.table
#' 
#' @references Van Erp et al. (2001) Actual Cation Exchange Capacity of Agricultural soils and its relationship with pH and content of organic carbon and clay
#'
#' @export
sptf_cec3 <- function(A_C_OF, A_CLAY_MI, A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in g/kg)
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_PH_CC = A_PH_CC)
  
  # derive the relationship between contribution of clay and oc to CEC at pH 8.1 (R2 = 0.80)
  dt[, D_PH := -1083.15 + 909.5273 * A_PH_CC - 295.814 * A_PH_CC^2 + 45.23841 * A_PH_CC^3 -2.89521 * A_PH_CC^4 + 0.005491 * A_PH_CC^6]
  
  # function for CEC (R2 = 0.89, n = 39, Netherlands, arable soils), originally cmol kg-1
  dt[, value := 10 * (0.0624 * A_CLAY_MI + A_C_OF * (0.295 - D_PH))]
  
  # function is valid between pH range 4.5 to 7.3
  dt[A_PH_CC < 4.5, value := NA_real_]
  dt[A_PH_CC > 7.3, value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 6.5
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#' 
#' @import data.table
#' 
#' @references Helling et al. (1964) Contribution of organic matter and clay to soil cation exchange capacity as affected by the pH of the saturating soil solution. 
#'
#' @export
sptf_cec4 <- function(A_C_OF, A_CLAY_MI, A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %). pH should be pH of soil solution, approximated by 0.01M caCl2 here
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC)
  
  # CHECK CORG VS SOM (Bussink says SOM, de Vries CO)
  
  # function for CEC at 6.5 (ammonium acetate buffered)
  dt[, value := (0.44 * A_PH_CC + 3) * A_CLAY_MI + (5.1 * A_PH_CC - 5.9) * A_C_OF]
  
  # function is valid for non calcareous soils
  dt[A_PH_CC > 8, value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Bril (unknown). Cited in Bussink & van de Zande (2008) Achtergrondinformatie over CEC en CEC-inbedding in bemestingsadviezen 
#'
#' @export
sptf_cec5 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI)
  
  # function for CEC at 8.2 (BaCl2) in mmol+/kg
  dt[, value := 10 * (4.2 * A_SOM_LOI^0.7 + 0.4 * A_SOM_LOI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#'  
#' @import data.table
#' 
#' @references Houba (unknown). Cited in Bussink & van de Zande (2008) Achtergrondinformatie over CEC en CEC-inbedding in bemestingsadviezen 
#'
#' @export
sptf_cec6 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI, A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_CC),length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SLIB_MI = A_CLAY_MI + 0.3 * A_SILT_MI,
                   A_PH_CC = A_PH_CC)
  
  # function for CEC at 8.2 (BaCl2) in mmol+/kg
  dt[, value := 10 * (0.25 * A_SLIB_MI + 2 * A_SOM_LOI * (0.25 * A_PH_CC - 0.25))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for sandy agricultural soils
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Ros & Reijneveld (2015) Bodemadvies voor gips en kalkmeststoffen. NMI-report 1588.N.15.
#'
#' @export
sptf_cec7 <- function(A_SOM_LOI, A_CLAY_MI, A_PH_CC,B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_CC),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,length = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   B_LU_PTFCLASS = B_LU_PTFCLASS
                   )
  
  # function for CEC at 8.2 (BaCl2) in mmol+/kg (n = 26313,R2 = 88%) applicable for sandy soils
  dt[, value := 2.376225 *log(A_SOM_LOI) + 3.796920 * log(A_PH_CC) -0.839837 * log(A_SOM_LOI)*log(A_PH_CC) -3.687682]
  
  # add corrections for cropland or grassland
  dt[B_LU_PTFCLASS == 'cropland', value := value + 0]
  dt[B_LU_PTFCLASS == 'grassland', value := value -0.075286]
  
  # re-transform to original scale
  dt[, value := exp(value)]
  
  # only applicable for sandy agricultural soils
  dt[A_CLAY_MI > 5 | A_SOM_LOI > 20 | B_LU_PTFCLASS %in% c('forest','nature'), value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for peaty agricultural soils
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#' @param A_CN_FR (numeric) The carbon-to-nitrogen ratio of the soil organic matter (-)
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Ros & Reijneveld (2015) Bodemadvies voor gips en kalkmeststoffen. NMI-report 1588.N.15.
#'
#' @export
sptf_cec8 <- function(A_SOM_LOI, A_CLAY_MI, A_PH_CC,A_CN_FR,B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_CC),length(B_LU_PTFCLASS),length(A_CN_FR))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CN_FR, lower = 6, upper = 40, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,length = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   A_CN_FR = A_CN_FR,
                   B_LU_PTFCLASS = B_LU_PTFCLASS
  )
  
  # function for CEC at 8.2 (BaCl2) in mmol+/kg (n = 2253,R2 = 80%) applicable for peat soils
  dt[, value := 52.8375535 * A_PH_CC + 0.0364237 * A_CLAY_MI^2 -2.6484246 * A_CN_FR + 2.26144422 * A_SOM_LOI * A_PH_CC - 153.705956]
  
  # add corrections for cropland or grassland
  dt[B_LU_PTFCLASS == 'cropland', value := value + 0]
  dt[B_LU_PTFCLASS == 'grassland', value := value - 2.29645895 * A_SOM_LOI]
  
  # only applicable for peat agricultural soils
  dt[A_SOM_LOI <= 20 | B_LU_PTFCLASS %in% c('forest','nature'), value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for clay agricultural soils
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Ros & Reijneveld (2015) Bodemadvies voor gips en kalkmeststoffen. NMI-report 1588.N.15.
#'
#' @export
sptf_cec9 <- function(A_SOM_LOI, A_CLAY_MI, A_PH_CC,B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_CC),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,length = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   B_LU_PTFCLASS = B_LU_PTFCLASS
  )
  
  # function for CEC at 8.2 (BaCl2) in mmol+/kg (n = 17984,R2 = 93%) applicable for clay soils
  dt[, value := 25.0383 * A_SOM_LOI + 87.621 * log(A_CLAY_MI) + 13.9793 * A_PH_CC -12.5268 * A_SOM_LOI * log(A_CLAY_MI) -2.2392 * A_SOM_LOI * A_PH_CC +
                -1.4664 * log(A_CLAY_MI) * A_PH_CC + 2.1823 * A_SOM_LOI * log(A_CLAY_MI) * A_PH_CC - 205.5489]
  
  # only applicable for sandy agricultural soils
  dt[A_CLAY_MI <=5 | A_SOM_LOI > 20 | B_LU_PTFCLASS %in% c('forest','nature'), value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

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

