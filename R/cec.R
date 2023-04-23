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
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI)
  
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
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_KCL))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_KCL, lower = 2, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_KCL = A_PH_KCL,
                   value = NA_real_)
  
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
  
  # add visual bindigns
  D_PH = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in g/kg)
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
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
#' This function calculates the CEC at pH 6.5 for agricultural and natural soils in Winconsin (USA).
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
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %). pH should be pH of soil solution, approximated by 0.01M caCl2 here
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC)
  
  # function for CEC at 6.5 (ammonium acetate buffered, n = 60, R2 = 0.99)
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
#' This function calculates the CEC at pH 8.2 for agricultural soils in the Netherlands.
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
#' This function calculates the CEC at pH 8.2 for agricultural soils in the Netherlands.
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
  
  # add visual bindings
  A_SLIB_MI = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_PH_CC))
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
#' This function calculates the CEC at pH 8.2 for sandy agricultural soils in the Netherlands.
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
  checkmate::assert_character(B_LU_PTFCLASS,len = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_
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
#' This function calculates the CEC at pH 8.2 for peaty agricultural soils in the Netherlands.
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
  checkmate::assert_character(B_LU_PTFCLASS,len = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   A_CN_FR = A_CN_FR,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_)
  
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
  checkmate::assert_character(B_LU_PTFCLASS,len = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_)
  
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

#' Calculate the CEC
#'
#' This function calculates the CEC at pH of sample for forest topsoils (0-30cm) in Switzerland
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Soly et al. (2020) A Critical Evaluation of the Relationship Between the Effective Cation Exchange Capacity and Soil Organic Carbon Content in Swiss Forest Soils
#'
#' @export
sptf_cec10 <- function(A_C_OF, A_CLAY_MI, A_PH_CC,B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_CC),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,len = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_)
  
  # function effective CEC (NH4CL) for topsoil forests (n=1189, R2 = 0.85)
  dt[, value := -194.3107 + 1.49668 * A_C_OF + 4.10418 * A_CLAY_MI + 42.71964 * A_PH_CC]
  
  # only applicable for forest soils
  dt[!B_LU_PTFCLASS %in% c('forest','nature'), value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH of sample for forest soils (including all horizons) in Switzerland
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Soly et al. (2020) A Critical Evaluation of the Relationship Between the Effective Cation Exchange Capacity and Soil Organic Carbon Content in Swiss Forest Soils
#'
#' @export
sptf_cec11 <- function(A_C_OF, A_CLAY_MI, A_PH_CC,B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_CC),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,len = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_)
  
  # function effective CEC (NH4CL) for topsoil forests (n=2326, R2 = 0.84)
  dt[, value := -173.20087 + 1.5237 * A_C_OF + 4.41812 * A_CLAY_MI + 37.06488 * A_PH_CC]
  
  # only applicable for forest soils all horizons
  dt[!B_LU_PTFCLASS %in% c('forest','nature'), value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 for tropical forest soils in Brazil
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Aprile & Lorandi (2012) Evaluation of Cation Exchange Capacity (CEC) in Tropical Soils Using Four Different Analytical Methods
#'
#' @export
sptf_cec12 <- function(A_C_OF, B_LU_PTFCLASS) {
  
  # add visual bindings
  bd = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,len = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_
                   )
  
  # kg / m3 uitrekent = g/dm3 => 1 dm3 grond heeft een massa van van 1315 g
  # 1 kg heeft dus een volume van 0.7604563 dm3
  # 1 kg grond heeft 20 g C/kg grond = 20 g C/0.7604563 dm3 = 
  
  # convert A_C_OF from g/kg to g/dm3
  dt[,bd := 1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF]
  dt[,A_C_OF := A_C_OF * bd / 1000]
  
  # function (n = 444, R2 = 0.87) using Corg in g/dm3 = kg / m3
  dt[, value := (A_C_OF - 4.5663)/0.3436]
  
  # only applicable for forest soils all horizons
  dt[!B_LU_PTFCLASS %in% c('forest','nature'), value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of agricultural soils (0-22 cm) in Quebec.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Martel et al. (1978) Cation-exchange capacity of clay-rich soils in relation to organic matter, mineral composition, and surface are.
#'
#' @export
sptf_cec13 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC at 7 (NH4Ac) in mmol+/kg (R2 = 0.83, n = 22)
  dt[, value := 10 * (3.98 + 0.565 * A_SOM_LOI + 0.377 * A_CLAY_MI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of agricultural and forest top soils (0-22 cm) in Ohio.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references wilding & Rutledge (1966) Cation-Exchange capacity as a function of organic matter, total clay, and various clay fractions in a soil toposequence.
#'
#' @export
sptf_cec14 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = v5 = v6 = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC at 7 (NH4Ac) in mmol+/kg (in total n = 169, with R2 values ranging from 0.58 to 0.79)
  dt[, v1 := 10 * (6.7 + 2.11 * A_SOM_LOI + 0.173 * A_CLAY_MI)]
  dt[, v2 := 10 * (-2.4 + 0.539 * A_CLAY_MI + 2.9 * A_SOM_LOI)]
  dt[, v3 := 10 * (5.5 + 1.92 * A_SOM_LOI + 0.632 * A_CLAY_MI)]
  dt[, v4 := 10 * (-1.1 + 0.793 * A_CLAY_MI -2.05 * A_SOM_LOI)]
  dt[, v5 := 10 * (16.5 + 1.54 * A_SOM_LOI + 0.351 * A_CLAY_MI)]
  dt[, v6 := 10 * (-8.5 + 0.831 * A_CLAY_MI +3.83 * A_SOM_LOI)]
  
  # Estimate mean value, and include only topsoils (v1, v3, v5 are A hroizons, v2, v4 and v6 are B horizons)
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v3','v5'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 of agricultural soils in Maryland.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Wright & Foss (1972) Contributions of clay and organic matter to the cation exchange capacity of Maryland soils
#'
#' @export
sptf_cec15 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC at 8.2 (BaCl2) in mmol+/kg (n = 57, R2 = 0.693)
  dt[, value := 10 * (0.+0 + 2.95 * A_SOM_LOI + 0.41 * A_CLAY_MI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for agricultural soils in US
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#'  
#' @import data.table
#' 
#' @references McLean & Owen (1969) Effects of pH on the contributions of organic matter and clay to soil cation exchange capacities.
#'
#' @export
sptf_cec16 <- function(A_SOM_LOI, A_CLAY_MI, A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_
                   )
  
  # estimate CEC via KCL method (n = 12, R2 varies between 0.81 - 0.86)
  dt[A_PH_WA <= 5, value := 10 * (-2.408 + 0.27 * A_SOM_LOI + 0.37 * A_CLAY_MI)]
  dt[A_PH_WA > 5 & A_PH_WA <= 5.5, value := 10 * (-0.57 + 0.7 * A_SOM_LOI + 0.35 * A_CLAY_MI)]
  dt[A_PH_WA > 5.5 & A_PH_WA <= 6.0, value := 10 * (-1.37 + 1.17 * A_SOM_LOI + 0.47 * A_CLAY_MI)]
  dt[A_PH_WA > 6.0, value := 10 * (1.08 + 1.46 * A_SOM_LOI + 0.40 * A_CLAY_MI)]
  
  # convert KCL to BaCl2 method (R2 = 0.985)
  dt[, value := (value - 0.67)/1.06]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 for natural grassland soils in China.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Liu et al. (2020) Soil organic matter and silt contents determine soil particle surface electrochemical properties across a long-term natural restoration grassland
#'
#' @export
sptf_cec17 <- function(A_SOM_LOI, B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,len = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table (ASOM in g/kg)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 10,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_
                   )
  
  # function to estimate CEC for topsoils (0-10 cm) of grasslands with different ages (n = 5, R2 = 0.919) 
  dt[, value := 10 * (0.14 * A_SOM_LOI + 5.21)]
  
  # only applicable for natural grassland soils
  dt[!B_LU_PTFCLASS %in% c('grassland','nature'), value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of agricultural soils in Florida.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Yuan et al. (1967) Relative contribution of organic and clay fractions to cation-exchange capacity of sandy soils from several soil groups.
#'
#' @export
sptf_cec18 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC at pH 7 in mmol+/kg (n = 83, R2 = 0.913)
  dt[, value := 10 * (-0.68 + 1.73 * A_SOM_LOI + 0.71 * A_CLAY_MI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8 of agricultural soils in Winconsin.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Helling et al. (1964) Contribution of organic matter and clay to soil cation-exchange capacity as affected by the pH of the saturating solution.
#'
#' @export
sptf_cec19 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC at pH 8 in mmol+/kg (n = 60, R2 = 0.92)
  dt[, value := 10 * (-0.66 + 3.66 * A_C_OF + 0.64 * A_CLAY_MI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of forest topsoils (0-30 cm) in Nigeria.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#' 
#' @import data.table
#' 
#' @references Olorunfemi et al. (2016) Modeling cation exchange capacity and soil water holding capacity from basic soil properties.
#'
#' @export
sptf_cec20 <- function(A_C_OF, A_CLAY_MI,A_PH_WA) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = v5 = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(id = 1: arg.length,
                   A_C_OF = A_C_OF * 0.1,
                   A_PH_WA = A_PH_WA,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC at pH 7 in mmol+/kg (n = 35, R2 = 0.58 to 0.711)
  dt[, v1 := 10 * (2.17 + 3.072 * A_C_OF)]
  dt[, v2 := 10 * (2.17 + 0.0230 * A_CLAY_MI + 2.802 * A_C_OF)]
  dt[, v3 := 10 * (-11.62 + 2.379 * A_PH_WA + 2.819 * A_C_OF)]
  dt[, v4 := 10 * (-15.37 + 3.173 * A_PH_WA + 0.0890 * A_CLAY_MI)]
  dt[, v5 := 10 * (-13.93 + 2.645 * A_PH_WA + 0.0446 * A_CLAY_MI + 2.267 * A_C_OF)]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3','v4','v5'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of agricultural soils (variable depth) in Sudan.
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Adam et al. (2021) Predicting Soil Cation Exchange Capacity in Entisols with Divergent Textural Classes: The Case of Northern Sudan Soils.
#'
#' @export
sptf_cec21 <- function(A_CLAY_MI,A_SILT_MI) {
  
  # add visual bindings
  A_SAND_MI = B_SOILTYPE = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table 
  dt <- data.table(id = 1:arg.length,
                   A_SILT_MI = A_SILT_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = 100 - A_SILT_MI - A_CLAY_MI,
                   value = NA_real_)
  
  # derive USDA soiltype
  dt[, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
  # use general function for CEC at pH 7 (NH4Ac) in mmol+/kg (n = 301, R2 = 0.95)
  dt[, value := 10 * (-1.53 -0.03 * A_SILT_MI + 0.84 * A_CLAY_MI)]
  
  # overwrite for soiltype scl (n = 126, R2 = 0.94)
  dt[B_SOILTYPE == 'sandy clay loam', value := 10 * (0.78 + 0.28 * A_SILT_MI + 0.89 * A_CLAY_MI)]
  
  # overwrite for soiltype sl (n = 101, R2 = 0.75)
  dt[B_SOILTYPE == 'sandy loam', value := 10 * (4.6 + 0.51 * A_SILT_MI + 0.24 * A_CLAY_MI)]
  
  # overwrite for soiltype ls (n = 53, R2 = 0.91)
  dt[B_SOILTYPE == 'loamy sand', value := 10 * (3.13 -0.10 * A_SILT_MI + 0.55 * A_CLAY_MI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}


#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 of loess soils (0-20cm) in Argentina
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Hepper et al. (2006) Clay mineralogy, cation exchange capacity and specific surface area of loess soils with different volcanic ash contents
#'
#' @export
sptf_cec22 <- function(A_SOM_LOI,A_CLAY_MI,A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI),length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table 
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_SILT_MI = A_SILT_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   value =  NA_real_)
  
  # use general function for CEC at pH 7 (NH4Ac) in mmol+/kg (n = 24, R2 = 0.3818)
  dt[, value := exp(14.6657 + 0.4294 * log(A_SOM_LOI) - 4.3786 * log(A_CLAY_MI) - 2.8604 * log(A_SILT_MI) + 1.1985 * log(A_CLAY_MI)*log(A_SILT_MI))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of forest and nature non-calcareous soils in continental USA.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#' 
#' @import data.table
#' 
#' @references Seybold et al. (2005) Predicting Cation Exchange Capacity for Soil Survey Using Linear Models
#'
#' @export
sptf_cec23 <- function(A_C_OF,A_CLAY_MI,A_PH_CC) {
  
  # add visual bindings
  v1 = v2 = v3 = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 12, len = arg.length)
  
  # make internal data.table (assuming that Corg equals total C, units in \%)
  dt <- data.table(id = 1: arg.length,
                   A_C_OF = A_C_OF * 0.1,
                   A_PH_CC = A_PH_CC,
                   A_CLAY_MI = A_CLAY_MI)
  
  # use general function for CEC at pH 7 (NH4Ac) for Oa, Oe and Oi horizons, in mmol+/kg
  
  # prediction 1. Oa horizon, n = 283, R2 = 0.52
  dt[, v1 := 2.12 * A_C_OF + 9.992 * A_PH_CC - 10.684]
  # prediction 2. Oe horizon, n = 286, R2 = 0.63
  dt[, v2 := 2.03 * A_C_OF + 3.396 * A_PH_CC - 2.939]
  # prediction 3. Oi horizon, n = 300, R2 = 0.43
  dt[, v3 := 1.314 * A_C_OF + 24.047]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # change unit from cmol+/kg to mmol+/kg
  dt[,value := value * 10]
  
  # add filter from the paper
  dt[A_PH_CC > 7 | A_C_OF <= 8 | A_C_OF > 71, value := NA_real_]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of forest and nature soils in continental USA.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#' 
#' @import data.table
#' 
#' @references Seybold et al. (2005) Predicting Cation Exchange Capacity for Soil Survey Using Linear Models
#'
#' @export
sptf_cec24 <- function(A_C_OF,A_CLAY_MI,A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 12, len = arg.length)
  
  # make internal data.table (assuming that Corg equals total C, units in \%)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_PH_CC = A_PH_CC,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # use general function for CEC at pH 7 (NH4Ac) for A horizons, in mmol+/kg
  
    # prediction 1. combination 1 (n = 133, R2 = 0.42)
    dt[A_C_OF > 8 & A_PH_CC <= 7,value := 1.823 * A_C_OF + 0.398 * A_CLAY_MI + 15.54]
    
    # prediction 2. combination 2 (n = 275, R2 = 0.77)
    dt[A_C_OF > 8 & A_C_OF <= 14.5 & A_PH_CC > 7, value := exp(1.316 * log(A_C_OF) + 1.063 * log(A_CLAY_MI) - 3.211)]
    
    # prediction 3. combination 3 (n = 30, R2 = 0.78)
    dt[A_C_OF > 8 & A_C_OF > 14.5 & A_PH_CC > 7, value := 4.314 * A_C_OF -26.492]
    
  # select output variable
  value <- dt[,value * 10]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of soils in continental USA where SOC is lower than 80 g/kg.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#' 
#' @import data.table
#' 
#' @references Seybold et al. (2005) Predicting Cation Exchange Capacity for Soil Survey Using Linear Models
#'
#' @export
sptf_cec25 <- function(A_C_OF,A_CLAY_MI,A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 12, len = arg.length)
  
  # make internal data.table (assuming that Corg equals total C, units in \%)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_PH_CC = A_PH_CC,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # use most common mineralogy group for CEC at pH 7 (NH4Ac), in mmol+/kg
  
  # superactive stratification (n = 12685, R2 = 0.90)
  dt[,value := 10 * exp(0.039 * log(A_C_OF) + 0.901 * log(A_CLAY_MI) + 0.131)]
 
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of soils in continental USA per taxonomic order stratification group.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#' @param B_SOILCLASS_USDA The soil type class according to the USDA Soil Taxonomy (https://en.wikipedia.org/wiki/USDA_soil_taxonomy)
#' 
#' @import data.table
#' 
#' @references Seybold et al. (2005) Predicting Cation Exchange Capacity for Soil Survey Using Linear Models
#'
#' @export
sptf_cec26 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI,A_PH_WA,B_SOILCLASS_USDA) {
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_SILT_MI),length(A_PH_WA),length(B_SOILCLASS_USDA))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_character(B_SOILCLASS_USDA,len = arg.length)
  checkmate::assert_subset(B_SOILCLASS_USDA,choices = c('alfisol','andisol','aridisol','entisol',
                                                        'gelisol','inceptisol','mollisol','oxisol',
                                                        'spodosol','ultisol','vertisol','histosol',
                                                        NA_character_))
  
  # make internal data.table (assuming that Corg equals total C, units in \%)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_PH_WA = A_PH_WA,
                   A_SILT_MI = A_SILT_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # use most common mineralogy group for CEC at pH 7 (NH4Ac), in mmol+/kg
  
  # alfisols (n = 4129, R2 = 0.73 | n = 3206, R2 = 0.72)
  dt[B_SOILCLASS_USDA == 'alfisol' & A_C_OF <= 0.3, value := exp(0.911 * log(A_CLAY_MI) - 0.308)]
  dt[B_SOILCLASS_USDA == 'alfisol' & A_C_OF > 0.3, value := exp(0.158 * log(A_C_OF) + 0.805 * log(A_CLAY_MI) +0.216)]
  
  # andisols (assuming no variation in wilting point, mean = 13.5%, n = 1181, R2 = 0.77)
  dt[B_SOILCLASS_USDA == 'andisol' & is.na(A_PH_WA), A_PH_WA := 6.2]
  dt[B_SOILCLASS_USDA == 'andisol', value := exp(0.088 * log(A_C_OF) + 0.885 * 13.5 + 0.867 * log(A_PH_WA) - 0.985)]
  
  # aridisols (n = 4114, R2 = 0.75)
  dt[B_SOILCLASS_USDA == 'aridisol', value := exp(0.042 * log(A_C_OF) + 0.828 * log(A_CLAY_MI) + 0.236)]
  
  # entisols (n = 1910, R2 = 0.85)
  dt[B_SOILCLASS_USDA == 'entisol', value := exp(0.078 * log(A_C_OF) + 0.873 * log(A_CLAY_MI) + 0.084)]
  
  # gelisols (n = 97, R2 = 0.72)
  dt[B_SOILCLASS_USDA == 'gelisol', value := exp(0.359 * log(A_C_OF) + 0.49 * log(A_CLAY_MI) + 1.05)]
  
  # inceptisols (n = 1921, R2 = 0.71)
  dt[B_SOILCLASS_USDA == 'inceptisol', value := exp(0.134 * log(A_C_OF) + 0.794 * log(A_CLAY_MI) + 0.239)]
  
  # Mollisols (n = 3284, R2 = 0.79 | n = 8132, R2 = 0.74)
  dt[B_SOILCLASS_USDA == 'mollisol' & A_C_OF <= 0.3, value := exp(0.932 * log(A_CLAY_MI) - 0.174)]
  dt[B_SOILCLASS_USDA == 'mollisol' & A_C_OF > 0.3, value := exp(0.113 * log(A_C_OF) + 0.786 * log(A_CLAY_MI) +0.475)]
  
  # oxisols (n = 781, R2 = 0.67)
  dt[B_SOILCLASS_USDA == 'oxisol' & is.na(A_SILT_MI), A_SILT_MI := 21.4]
  dt[B_SOILCLASS_USDA == 'oxisol', value := 2.738 * A_C_OF + 0.103 * A_CLAY_MI + 0.123 * A_SILT_MI - 2.531]
  
  # Spodosols (n = 243, R2 = 0.71)
  dt[B_SOILCLASS_USDA == 'spodosol', value := exp(0.045 * log(A_CLAY_MI)+ 0.798 * A_CLAY_MI - 0.029)]
  
  # Ultisols (n = 499, R2 = 0.76)
  dt[B_SOILCLASS_USDA == 'ultisol' & is.na(A_SILT_MI), A_SILT_MI := 5.2]
  dt[B_SOILCLASS_USDA == 'ultisol', value := exp(0.184 * log(A_C_OF) + 0.57 * log(A_CLAY_MI) + 0.365 * log(A_SILT_MI) - 0.906)]
  
  # Vertisols (n = 2109, R2 = 0.55)
  dt[B_SOILCLASS_USDA == 'vertisol', value := exp(0.059 * log(A_C_OF) + 0.86 * log(A_CLAY_MI) + 0.312)]
  
  # Histosols (n = 60, R2 = 0.78)
  dt[B_SOILCLASS_USDA == 'histosol', value := exp(0.319 * log(A_C_OF) + 0.497 * log(A_CLAY_MI) + 1.075)]
  
  # combination of histosol and gelisol
  dt[B_SOILCLASS_USDA %in% c('histosol','gelisol'),value := 0.5 * value + 0.5 * exp(0.346 * log(A_C_OF) + 0.49 * log(A_CLAY_MI) + 1.064)]
  
  # combination of alifsols 2 and inceptisols
  dt[(B_SOILCLASS_USDA == 'alfisol' & A_C_OF > 0.3) | B_SOILCLASS_USDA == 'inceptisol', value := 0.5 * value + 
       0.5 * exp(0.141 * log(A_C_OF) + 0.797 * log(A_CLAY_MI) + 0.235)]
  
  # convert unit from cmol to mmol+/kg
  dt[,value := 10 * value]
  
  # select output variable
  value <- dt[,value]
  
  # return value
  return(value)
  
}


#' Calculate the CEC
#'
#' This function calculates the effective CEC at pH of sample for agricultural topsoils (0-20cm) in Africa
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The clay content of the soil (\%).
#' @param B_CLIM_CAT1 (character) The climatic region (options: lowland humid (LLH), highland humid (HLH), subhumid (SH) and non humid (NH))
#'  
#' @import data.table
#' 
#' @references Asadu et al. (1997) A COMPARISON OF THE CONTRIBUTIONS OF CLAY, SILT, AND ORGANIC MATTER TO THE EFFECTIVE CEC OF SOILS OF SUBSAHARAN AFRICA.
#'
#' @export
sptf_cec27 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI,B_CLIM_CAT1) {
  
  # discussion note given in the article
  # Soil Survey Laboratory Methods (1996) defines effective CEC as the sum of NH4OAc extractable bases plus KCl extractable Al. 
  # The extraction of Al with KCl, an unbuffered salt, is done close to the pH of the soil. 
  # Effective CEC by this procedure measures the active exchange sites at the existing soil pH.
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(B_CLIM_CAT1))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_character(B_CLIM_CAT1,len = arg.length)
  checkmate::assert_subset(B_CLIM_CAT1, choices = c('LLH', 'HLH', 'SH', 'NH',NA_character_))
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   B_CLIM_CAT1 = B_CLIM_CAT1,
                   value = NA_real_
  )
  
  # function effective CEC, NH4-Ac and KCl (n=905, R2 = 0.48 | n = 708, R2 = 0.26 | n = 281, R2= 0.42| n = 237, R2 = 0.42)
  dt[B_CLIM_CAT1 == 'LLH', value := 0.34 + 0.06 * A_CLAY_MI + 0.13 * A_SILT_MI + 1.46 * A_SOM_LOI]
  dt[B_CLIM_CAT1 == 'SH', value := 2.51 + 0.10 * A_CLAY_MI + 0.04 * A_SILT_MI + 0.13 * A_SOM_LOI]
  dt[B_CLIM_CAT1 == 'NH', value := 1.63 + 0.10 * A_CLAY_MI + 0.16 * A_SILT_MI + 1.36 * A_SOM_LOI]
  dt[B_CLIM_CAT1 == 'HLH', value := 2.71 + 0.08 * A_CLAY_MI + 0.03 * A_SILT_MI + 1.36 * A_SOM_LOI]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the effective CEC at pH of sample for agricultural Danish soils
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#'  
#' @import data.table
#' 
#' @references Krogh et al. (2000) Cation Exchange Capacity Pedotransfer Functions For Danish Soils
#'
#' @export
sptf_cec28 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI, A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_CC),length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # function for CEC at 8.1 (NH4Ac plus KCL) in mmol+/kg (n = 1340, R2 > 0.90)
  dt[, value := 0.95 + 2.9 * A_SOM_LOI + 0.53 * A_CLAY_MI]
  dt[!is.na(A_SILT_MI), value := 0.89 + 2.82 * A_SOM_LOI + 0.48 * A_CLAY_MI + 0.09 * A_SILT_MI]
  dt[!is.na(A_PH_CC), value := -1.9 + 2.85 * A_SOM_LOI + 0.51 * A_CLAY_MI + 0.58 * A_PH_CC]
  
  # non-linear equation
  # dt[!is.na(A_PH_CC), value := 0.5 * value + 0.5* (0.74 + 1.73 * A_SOM_LOI + 0.53 * A_CLAY_MI - 0.09 * A_SOM_LOI^2 + 0.31 * A_SOM_LOI * A_PH_CC - 0.01 * A_SOM_LOI * A_CLAY_MI)]
    
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the effective CEC at pH of sample for agricultural Danish topsoils
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_CACO3_MI (numeric) The carbonate content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#'  
#' @import data.table
#' 
#' @references Krogh et al. (2000) Cation Exchange Capacity Pedotransfer Functions For Danish Soils
#'
#' @export
sptf_cec29 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI, A_CACO3_MI, A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_CC),length(A_SILT_MI),length(A_CACO3_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 15, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_CACO3_MI = A_CACO3_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # function for CEC at 8.1 (NH4Ac plus KCL) in mmol+/kg
  
  # topsoils in MCC regions 1 + 2 + 3 (n = 711, R2 = 0.88)
  dt[A_CLAY_MI <= 10, value := 0.58 + 2.77 * A_SOM_LOI + 0.64 * A_CLAY_MI]
  
  # topsoils in MCC regions 4 + 5 + 6 (n = 629, R2 = 0.84)
  dt[A_CLAY_MI > 10, value := 1.09 + 3.04 * A_SOM_LOI + 0.52 * A_CLAY_MI]
  
  # update for organic soils (n = 68, R2 = 0.71)
  dt[A_SOM_LOI > 10, value := 21.11 + 1.88 * A_SOM_LOI]
  
  # update for calcareous soils (n = 136, R2 = 0.77)
  dt[!is.na(A_CACO3_MI) & A_CACO3_MI > 0.1, value := -0.04 + 2.13 * A_SOM_LOI + 0.42 * A_CLAY_MI]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the effective CEC at sample pH at various depths of agricultural soils in New Jersey
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Drake & Motto (1982) An analysis of the effect of clay and organic matter content on the cation exchange capacity of New Jersey Soils.
#'
#' @export
sptf_cec30 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC 170 profiles, 49 soils (in total 1041 observations), R2 = 0.59, n = 274
  dt[, value := 10 * (2.66 + 2.17 * A_SOM_LOI + 0.35 * A_CLAY_MI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the effective CEC at sample pH at topsoil of agricultural soils in New Jersey.  
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param B_SOILCLASS_USDA The soil type class according to the USDA Soil Taxonomy (https://en.wikipedia.org/wiki/USDA_soil_taxonomy)
#'  
#' @import data.table
#' 
#' @references Drake & Motto (1982) An analysis of the effect of clay and organic matter content on the cation exchange capacity of New Jersey Soils.
#'
#' @details
#' Default is the topsoil averaged value. IF the USDA soiltype is known, then the value is updated.
#' 
#' @export
sptf_cec31 <- function(A_SOM_LOI, A_CLAY_MI,B_SOILCLASS_USDA = NA) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(B_SOILCLASS_USDA))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_character(B_SOILCLASS_USDA,len = arg.length)
  checkmate::assert_subset(B_SOILCLASS_USDA,choices = c('alfisol','andisol','aridisol','entisol',
                                                        'gelisol','inceptisol','mollisol','oxisol',
                                                        'spodosol','ultisol','vertisol','histosol',
                                                        NA_character_))
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   B_SOILCLASS_USDA = B_SOILCLASS_USDA,
                   value = NA_real_)
  
  # function for CEC 170 profiles, R2 = 0.74
  dt[, value := 10 * (1.79 + 2.11 * A_SOM_LOI + 0.39 * A_CLAY_MI)]
  
  # update when approriate (n = 282, R2 = 0.2 | n = 45, R2 = 0.6 | n = 243, R2 = 0.74 | n = 471, R2 = 0.63)
  dt[B_SOILCLASS_USDA == 'alfisol', value := 10 * (9 + 0.19 * A_CLAY_MI + 0.88 * A_SOM_LOI)]
  dt[B_SOILCLASS_USDA == 'entisol', value := 10 * (0.9 + 0.33 * A_CLAY_MI + 3.01 * A_SOM_LOI)]
  dt[B_SOILCLASS_USDA == 'inceptisol', value := 10 * (3.2 + 0.29 * A_CLAY_MI + 2.44 * A_SOM_LOI)]
  dt[B_SOILCLASS_USDA == 'ultisol', value := 10 * (1.1 + 0.40 * A_CLAY_MI + 2.12 * A_SOM_LOI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 for agricultural Chinese topsoils
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#'  
#' @import data.table
#' 
#' @references Liao & Zhu (2015) Development of ensemble pedotransfer functions for cation exchange capacity of soils of Qingdao in China
#'
#' @export
sptf_cec32 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI, A_PH_CC) {
  
  # add visual binding
  A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_CC),length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # function for CEC at pH 7 (NH4Ac), 100 topsoils (0-20 cm), R2 = 0.80
  dt[, value := -7.9 - 0.01 * A_SAND_MI + 0.01 * A_SILT_MI + 0.52 * A_CLAY_MI + 2.75 * A_SOM_LOI + 1.44 * A_PH_CC]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#' 
#' @import data.table
#' 
#' @references Liao & Zhu (2015) Development of ensemble pedotransfer functions for cation exchange capacity of soils of Qingdao in China. 
#' 
#' @details
#' Authors also applied ptf from Manrique et al. (1991). Predicting cationexchange capacity from soil physical and chemical properties. 
#' 
#'
#' @export
sptf_cec33 <- function(A_C_OF, A_CLAY_MI, A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC)
  
  # function for CEC (n = 12000, R2 = 50)
  dt[, value := -6.82 + 0.36 * A_CLAY_MI + 3.38 * A_C_OF + 1.79 * A_PH_CC]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 for forest and natural grassland soils in eastern Canada
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Meyer et al. (1994).  Cation exchange capacities of upland soils in eastern Canada. 
#'
#' @export
sptf_cec34 <- function(A_C_OF, A_CLAY_MI, A_PH_CC,B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,len = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # function for CEC for forest soils (n = 1723, R2 = 0.72)
  dt[B_LU_PTFCLASS %in% c('nature', 'forest'), value := -7.00 + 0.29 * A_CLAY_MI + 0.82 * A_C_OF + 1.4 * A_PH_CC]
  
  # function for CEC for forest soils (n = 344, R2 = 0.79)
  dt[B_LU_PTFCLASS %in% c('grasslands'), value := -8.30 + 0.24 * A_CLAY_MI + 2.14 * A_C_OF + 1.3 * A_PH_CC]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#' 
#' @import data.table
#' 
#' @references Scheinost (1995). Pedotransfer-Funktionen zum Wasser-und Stoffhaushalt einer Bodenlandschaft. FAM-Bericht 5, Shaker, Aachen. Cited in Liao & Zhu (2015).
#'
#' @export
sptf_cec35 <- function(A_C_OF, A_CLAY_MI, A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # function for CEC(n = , R2 = )
  dt[, value := 0.05 + 0.55 * A_CLAY_MI + 0.26 * A_C_OF * A_PH_CC]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Foth & Ellis (1997). Soil fertility, 2nd edn. Lewis CRC Press LLC, Boston, USA. pp. 57. Cited in Liao & Zhu (2015).
#'
#' @export
sptf_cec36 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC(n = , R2 = )
  dt[, value := 3.20 + 0.20 * A_CLAY_MI + 3.67 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 in agricultural soils in Indiana.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Franzmeier et al. (1990). Relation of cation exchange capacity to clay and organic carbon contents of Indiana soils. 
#'
#' @export
sptf_cec37 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC(n = 2725, R2 = 0.79)
  dt[, value := 0.515 + 0.588 * A_CLAY_MI + 3.58 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 for chernozemic soils in Saskatchewan province of Canada.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Arnaud & Sephton (1972). Contribution of clay and organic matter to cation-exchange capacity of chemozemic soils. 
#'
#' @export
sptf_cec38 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # add visual bindings
  v1 = v2 = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC with NH4OAc for A horizon only (n = 111, R2 = 79)
  dt[, v1 := 2.23 * A_SOM_LOI + 0.57 * A_CLAY_MI]
  dt[, v2 := 2.30 * A_SOM_LOI + 0.57 * A_CLAY_MI - 0.01 * A_SOM_LOI * A_CLAY_MI]
  
  # calculate the mean and update unit from cmol/kg to mmol/kg
  dt[, value := (v1 + v2) * 0.5 * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Brady (1990). The nature and properties of soils, 1Oth edn. Macmillan Co, New York, NY. Cited in Liao & Zhu (2015).
#'
#' @export
sptf_cec39 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI)
  
  # function for CEC(n = , R2 = )
  dt[, value := 0.50 * A_CLAY_MI + 3.44 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references MacDonald (1998). Development of pedotransfer functions of southern Ontario Soils. Cited in Liao & Zhu (2015).
#'
#' @export
sptf_cec40 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC(n = , R2 = )
  dt[, value := 0.50 * A_CLAY_MI + 2.00 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 for forests soils in Mississippi, Ohio, Wisconsin, North Carolina and Florida.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Miller (1970). Inter-regional predictability of cation-exchange capacity by multiple regression. 
#'
#' @export
sptf_cec41 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # add visual bindings
  v1 = v2 = v3 = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI)
  
  # function for CEC for Mississippi soils (n = 125, R2 = 0.86)
  dt[, v1 := -1.07 + 0.62 * A_CLAY_MI + 1.78 * A_SOM_LOI]
  
  # function for CEC for Florida soils (n = 83, R2 = 0.91)
  dt[, v2 := -0.68 + 0.71 * A_CLAY_MI + 1.73 * A_SOM_LOI]
  
  # function for CEC for Wisconsin soils (n = 60, R2 = 0.91)
  dt[, v3 := -1.05 + 0.60 * A_CLAY_MI + 2.81 * A_SOM_LOI * 0.5]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 for agricultural soils in Cameroon
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#'  
#' @import data.table
#' 
#' @references Enang et al. (2022) Pedotransfer functions for cation exchange capacity estimation in highly weathered soils of the tropical highlands of NW Cameroon 
#'
#' @export
sptf_cec42 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI) {
  
  # add visual bindings
  v1 = v2 = v3 = v4 = v5 = A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)

  # make internal data.table (oc in \%)
  dt <- data.table(id = 1:arg.length,
                   A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI)
  
  # function for CEC at pH 7 (NH4Ac) for A horizon (n = 79, R2 = 0.75; n = 79, R2 = 0.52)
  dt[, v1 := 36.914 * exp(-0.027 * A_SAND_MI)]
  dt[, v2 := 5.6719 * exp(0.2109 * A_C_OF)]
  
  # function for CEC at pH 7 for AB horizon (R2 = 0.731, n = 26)
  dt[, v3 := 37.402 * exp(-0.032 * A_SAND_MI)]
  
  # function for CEC at pH 7 for B horizon (R2 = 0.754, n = 77)
  dt[, v4 := -0.0653 * A_C_OF^2 + 2.9006 * A_C_OF + 2.9563]
  
  # function for CEC at pH 7 for C horizon (R2 = 0.39, n = 62)
  dt[, v5 := -0.2734 * A_C_OF^2 + 4.5877 * A_C_OF + 4.4277]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3','v4','v5'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the effective CEC at sample pH for agricultural soils at various depths in Nigeria
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#'  
#' @import data.table
#' 
#' @references Apka et al. (2016) Enhancing pedotransfer functions with environmental data for estimating bulk density and effective cation exchange capacity in a data-sparse situation  
#'
#' @export
sptf_cec43 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI, A_PH_CC) {
  
  # add visual bindings
  A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_CC),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table (soc in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # function for CEC at pH 7 (NH4Ac) at various depths (all data, n = 2124, R2 = 0.577)
  dt[, value := -15.681 + 0.118 * A_CLAY_MI + 4.097 * A_PH_CC * 0.124 - A_SAND_MI + 0.0887 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the effective CEC at sample pH for agricultural topsoils in Nigeria
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#'  
#' @import data.table
#' 
#' @references Apka et al. (2016) Enhancing pedotransfer functions with environmental data for estimating bulk density and effective cation exchange capacity in a data-sparse situation  
#'
#' @export
sptf_cec44 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI, A_PH_CC) {
  
  # add visual bindings
  A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_CC),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table (soc in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # function for CEC at pH 7 (NH4Ac) for the topsoil only (n = 627, R2 = 0.812)
  dt[, value := -22.612 + 0.212 * A_CLAY_MI + 5.0295 * A_PH_CC -0.110 * A_SAND_MI + 0.136 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 at agricultural sites in Finland
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-)
#' 
#' @import data.table
#' 
#' @references Raty et al. (2021) Estimating cation exchange capacity and clay content from agricultural soil testing data.
#'
#' @export
sptf_cec45 <- function(A_C_OF, A_CLAY_MI, A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # function for CEC(n = 114, R2 = 0.89)
  dt[A_C_OF <= 10, value := 4.89 + 1.67 * A_C_OF + 0.247 * A_CLAY_MI]
  dt[A_C_OF > 10, value := 70.10 + 1.30 * A_C_OF - 9.11 * A_PH_WA]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}


' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 at agricultural sites at various depths in Tanzania
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Mtama et al. (2018) Pedotransfer Functions for Cation Exchange Capacity, Available Water Holding Capacity and Soil Organic Carbon for Representative Soils of Southern Highland Zone of Tanzania
#'
#' @export
sptf_cec46 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC (n = 20, R2 = 0.93)
  dt[, value := 9.6 * A_C_OF + 0.44 * A_CLAY_MI]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the effective CEC at pH of sample for agricultural topsoils (0-20cm) in Africa
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The clay content of the soil (\%).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Asadu et al. (1997) Contributions of organic matter, clay and silt to the effective CEC of soils of different land use history
#'
#' @export
sptf_cec47 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI,B_LU_PTFCLASS) {
  
  # discussion note given in the article
  # Soil Survey Laboratory Methods (1996) defines effective CEC as the sum of NH4OAc extractable bases plus KCl extractable Al. 
  # The extraction of Al with KCl, an unbuffered salt, is done close to the pH of the soil. 
  # Effective CEC by this procedure measures the active exchange sites at the existing soil pH.
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,len =arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_
  )
  
  # function effective CEC, NH4-Ac and KCl (n=36, R2 = 0.35)
  dt[, value := 2.76 + 0.08 * A_SOM_LOI + 0.07 * A_CLAY_MI + 0.45 * A_SILT_MI]
  
  # overwrite when more data is known (n = 7, R2 = 0.85 |n = 5, R2 = 0.97 | n = 10, R2 = 0.10)
  # dt[B_LU_PTFCLASS == 'cropland', value := -4.63 + 0.83 * A_SOM_LOI + 0.20 * A_CLAY_MI + 0.56 * A_SILT_MI]
  # dt[B_LU_PTFCLASS == 'forest', value := 14.52 -1.32 * A_SOM_LOI - 0.01 * A_CLAY_MI -0.18 * A_SILT_MI]
  # dt[B_LU_PTFCLASS == 'nature', value := 3.33 + 0.08 * A_SOM_LOI + 0.02 * A_CLAY_MI + 0.06 * A_SILT_MI]

  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}


#' Calculate the CEC
#'
#' This function calculates the CEC at pH 6.5 for agricultural fields at two depths in Central Iran.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Salehi et al. (2008) Developing Soil Cation Exchange Capacity Pedotransfer Functions using Regression and Neural Networks and the Effect of Soil Partitioning on the Accuracy and Precision of Estimation
#'
#' @export
sptf_cec48 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function to estimate CEC (n = 20, R2 - 0.67)
  dt[, value := -51.2 + 22.8 * A_CLAY_MI + 1.4 * A_SOM_LOI]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the effective CEC at pH 7 for agricultural topsoils (0-30cm) in Iraqi
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The clay content of the soil (\%).
#'  
#' @import data.table
#' 
#' @references Fattah & Karim (2021) PERFORMANCE OF LINEAR MODELS IN PREDICTING CATION EXCHANGE CAPACITY OF CALCAREOUS SOILS
#'
#' @export
sptf_cec49 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI) {
  
  # add visual bindings
  v1 = v2 = A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(id = 1: arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   value = NA_real_)
  
  # function effective CEC, NH4-Ac and KCl (n=200, R2 = 0.90)
  dt[, v1 := 68.313 - 0.560 * A_SAND_MI - 0.592 * A_SILT_MI + 1.312 * A_SOM_LOI]
  dt[, v2 := 9.094 + 0.032 * A_SAND_MI + 0.592 * A_CLAY_MI + 1.312 * A_SOM_LOI]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for calcareous soils in Iran, Texas, USA, Cuba and Kenya
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_CACO3_MI (numeric) The carbonate content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#'  
#' @import data.table
#' 
#' @references Razzaghi et al. (2021) Evaluating models to estimate cation exchange capacity of calcareous soils 
#'
#' @export
sptf_cec50 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI, A_CACO3_MI, A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_CC),length(A_SILT_MI),length(A_CACO3_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 15, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_CACO3_MI = A_CACO3_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # function for CEC at 8.2 (NH4Ac) (n = 38, R2 = 0.64)
  dt[, value := 29.9 + 0.298 * A_CLAY_MI - 0.389 * A_CACO3_MI]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for various land uses in Iran
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#'  
#' @import data.table
#' 
#' @references Khaledian et al. (2017). Modeling soil cation exchange capacity in multiple countries.
#'
#' @export
sptf_cec51 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI, A_PH_WA) {
  
  # add visual bindings
  A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_WA),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_WA = A_PH_WA)
  
  # function for CEC at 8.2 (NH4Ac) (n = 65, R2 = 0.52)
  dt[, value := 9.47 + 0.48 * A_CLAY_MI - 0.24 * A_SILT_MI - 035 * A_SAND_MI + 3.13 * A_PH_WA - 0.233 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for various land uses in Iraq
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#'  
#' @import data.table
#' 
#' @references Khaledian et al. (2017). Modeling soil cation exchange capacity in multiple countries.
#'
#' @export
sptf_cec52 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI, A_PH_WA) {
  
  # add visual bindings
  A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_WA),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_WA = A_PH_WA)
  
  # function for CEC at 8.2 (NH4Ac) (n = 40, R2 = 0.91)
  dt[, value := 39.964 + 0.339 * A_CLAY_MI - 0.453 * A_SILT_MI - 0.296 * A_SAND_MI + 0.964 * A_PH_WA + 0.475 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for various land uses in Spain
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#'  
#' @import data.table
#' 
#' @references Khaledian et al. (2017). Modeling soil cation exchange capacity in multiple countries.
#'
#' @export
sptf_cec53 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI, A_PH_WA) {
  
  # add visual bindings
  A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_WA),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_WA = A_PH_WA)
  
  # function for CEC at 8.2 (NH4Ac) (n = 40, R2 = 0.82)
  dt[, value := 25.219 + 0.132 * A_CLAY_MI +216.7 * A_SILT_MI - 0.054 * A_SAND_MI -2.445 * A_PH_WA + 0.865 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for various land uses in USA, Spain, Iran and Iraq.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'   
#' @import data.table
#' 
#' @references Khaledian et al. (2017). Modeling soil cation exchange capacity in multiple countries.
#'
#' @export
sptf_cec54 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI, A_PH_WA,B_LU_PTFCLASS) {
  
  # add visual bindings
  A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_WA),length(A_SILT_MI),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,len =arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table (SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_WA = A_PH_WA,
                   B_LU_PTFCLASS = B_LU_PTFCLASS,
                   value = NA_real_)
  
  # function for CEC at 8.2 (NH4Ac) (n = 170, R2 = 0.71)
  dt[, value := 27.842 + 0.706 * A_CLAY_MI -0.092 * A_SILT_MI - 0.169 * A_SAND_MI -2.277 * A_PH_WA + 0.278 * A_C_OF]
  
  # make it land use specific: agriculture (n = 88, R2 = 0.88)
  dt[B_LU_PTFCLASS %in% (c('agriculture','cropland')), value := 41.025 + 1.091 * A_CLAY_MI + 0.512 * A_SILT_MI + 0.159 * A_SAND_MI - 10.087 * A_PH_WA + 0.494 * A_C_OF]
  
  # make it land use specific: forest (n = 37, R2 = 0.62)
  dt[B_LU_PTFCLASS =='forest', value := 10.973 + 0.427 * A_CLAY_MI -0.379 * A_SILT_MI -0.436 * A_SAND_MI +4.313 * A_PH_WA + 0.0696 * A_C_OF]
  
  # make it land use specific: pasture (n = 45, R2 = 0.75)
  dt[B_LU_PTFCLASS %in% (c('agriculture','grassland')), value := -60.880 + 1.365 * A_CLAY_MI + 0.992 * A_SILT_MI + 0.864 * A_SAND_MI - 2.453 * A_PH_WA + 0.282 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for various land uses in USA, Spain, Iran and Iraq.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#'   
#' @import data.table
#' 
#' @references Khaledian et al. (2017). Modeling soil cation exchange capacity in multiple countries.
#'
#' @export
sptf_cec55 <- function(A_C_OF, A_CLAY_MI, A_SILT_MI, A_PH_WA) {
  
  # add visual bindings
  v1 = v2 = A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_WA),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table (SOC in %)
  dt <- data.table(id = 1:arg.length,
                   A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # function GLM for CEC at 8.2 (NH4Ac) (n = 170, R2 = 0.71)
  dt[, v1 := 27.842 + 0.706 * A_CLAY_MI -0.092 * A_SILT_MI - 0.169 * A_SAND_MI -2.277 * A_PH_WA + 0.278 * A_C_OF]
  
  # function GLM polynomial for CEC at 8.2 (NH4Ac) (n = 170, R2 = 0.80)
  dt[, v2 := -25.82 + 1.0 * A_CLAY_MI - 0.037 * A_SAND_MI + 0.543 * A_PH_WA + 0.405 * A_C_OF + 0.0023 * A_CLAY_MI^2 + 
              0.0058 * A_SILT_MI^2 + 0.0057 * A_SAND_MI^2 - 0.158 * A_PH_WA^2 - 0.015 * A_C_OF^2 + 4.337 * exp(A_C_OF/A_SAND_MI)^2]
 
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for calcareous soils (0-20cm) in Mexico
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#'  
#' @import data.table
#' 
#' @references Bell & van Keulen (1995). Soil pedotransfer functions for 4 Mexican soils. Cited in Razzaghi et al. (2021).
#'
#' @export
sptf_cec56 <- function(A_SOM_LOI, A_CLAY_MI,A_PH_WA) {
  
  # add visual bindings
  v1 = v2 = v3 = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table (SOM in g/kg)
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI * 10,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA)
  
  # function for CEC at 8.2 (NH4Ac) (n = 148, R2 = 0.96; R2 = 0.94)
  dt[, v1 := 42.8 - 5.36 * A_PH_WA + 0.3 * A_SOM_LOI * 10 - 2.04 * A_CLAY_MI + 0.36 * A_CLAY_MI * A_PH_WA]
  dt[, v2 := -10 + 0.163 * A_SOM_LOI * A_PH_WA - 0.0209 * A_SOM_LOI * A_CLAY_MI + 0.131 * A_CLAY_MI * A_PH_WA]
  
  # overwrite when pH is missing
  dt[!is.na(A_PH_WA), v3 := 5.79 + 0.1 * A_CLAY_MI * A_PH_WA]
  dt[is.na(A_PH_WA), v3 := 2.24 + 0.774 * A_CLAY_MI + 0.0807 * A_SOM_LOI]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for agricultural soils (0-20cm) in Mexico
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_SAND_MI (numeric) The sand content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#'  
#' @import data.table
#' 
#' @references Bell (1993). Organic matter, soil properties and wheat production in the high Valley of Mexico
#'
#' @export
sptf_cec57 <- function(A_SOM_LOI, A_SAND_MI,A_PH_WA) {
  
  # add visual bindings
  v1 = v2 = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_SAND_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table (SOM in g/kg)
  dt <- data.table(id = 1:arg.length,
                   A_SOM_LOI = A_SOM_LOI * 10,
                   A_SAND_MI = A_SAND_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # function for CEC at 8.2 (NH4Ac) (n = 68, R2 = 0.62)
  dt[, v1 := -1.91 +1.84 * A_PH_WA - 0.06 * A_SAND_MI]
 
  # same function references in Bell & van Keulen (1995) (R2 = 0.03, n =37)
  dt[, v2 := -6.16 + 2.58 * A_PH_WA - 0.063 * A_SAND_MI]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}
#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for calcareous soils in Iran
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_CACO3_MI (numeric) The carbonate content of the soil (\%).
#'  
#' @import data.table
#' 
#' @references Asadzadeh et al. (2019). Predicting cationic exhcnage capacity in calcareous soils of East Azerbaijen province, northwest Iran. Cited in Razzaghi et al. (2021).
#'
#' @export
sptf_cec58 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI, A_CACO3_MI) {
  
  # add visual bindings
  A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_CACO3_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 15, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_CACO3_MI = A_CACO3_MI,
                   value = NA_real_)
  
  # function for CEC at 8.2 (NH4Ac) (n = 417, R2 = 0.85)
  dt[, value := 0.027 + 0.811 * A_CLAY_MI - 0.045 * A_SAND_MI + 0.168 * A_SOM_LOI - 0.091 * A_CACO3_MI]
  
  # update when input is missing (n = 417, R2 = 0.83)
  dt[is.na(A_CACO3_MI), value := 0.112 + 0.878 * A_CLAY_MI + 0.127 * A_CLAY_MI / A_SOM_LOI]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for soil in Lower Namoi Valley, Australia. 
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references McBratney et al. (2002). From pedotransfer functions to soil inference systems. 
#'
#' @export
sptf_cec59 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC (n = 1930, R2 =  0.74), using data of McGarry et al. (1989)
  dt[, value := -29.25 + 8.14 * A_CLAY_MI + 0.25 * A_CLAY_MI * A_C_OF]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for calcareous soils in various countries across all continents
#' 
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Rehman et al. (2019). Comparison of cation exchange capacity estimated from Vis-NIR spectral relfectance data and a pedotransfer function. Cited in Razzaghi et al. (2021).
#'
#' @export
sptf_cec60 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI)
  
  # function for CEC (n = 235, R2 =  0.53)
  dt[, value := 0.60 + 0.61 * A_CLAY_MI + 2.00 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for calcareous soils in Iran
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Khodaverdiloo et al. (2018). Performance of soil cation exchange capacity pedotransfer function as affected by the inputs and database size. Cited in Razzaghi et al. (2021).
#'
#' @export
sptf_cec61 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC (n = 1141, R2 = 0.60)
  dt[, value := 7.71 + 0.26 * A_CLAY_MI + 4.06 * A_C_OF]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for calcareous soils in Iran
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_CACO3_MI (numeric) The carbonate content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Kashi et al. (2014) Estimation of Soil Infiltration and Cation Exchange Capacity Based on Multiple Regression, ANN (RBF, MLP), and ANFIS Models.
#'
#' @export
sptf_cec62 <- function(A_C_OF, A_CLAY_MI,A_SILT_MI,A_CACO3_MI) {
  
  # add visual bindings
  bd = sar = ec = A_SAND_MI = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI),length(A_CACO3_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_CACO3_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SAND_MI = 100 - A_CLAY_MI - A_SILT_MI,
                   A_CACO3_MI = A_CACO3_MI,
                   value = NA_real_)
  
  # estimate bulk density (in g/cm3)
  dt[, bd := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF) * 0.001]
  
  # set sodium absorption rate (SAR) to mean value
  dt[, sar := 5.67]
  
  # set electric conductivity (EC, dS/m) to mean value
  dt[, ec := 5.25]
  
  # function for CEC NH4Ac pH 8.2 (n = 200, 0-30cm, R2 =  0.80)
  dt[, value := 18.87 - 6.16 * bd - 0.019 * sar + 0.016 * A_SAND_MI + 0.026 * A_SILT_MI + 0.076 * A_CLAY_MI - 0.038 * ec]
  
  # when carbonate is known adapt
  dt[, value := value - 0.07 * fifelse(is.na(A_CACO3_MI),19,A_CACO3_MI)]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC for soils in Italy
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Bazzoffi et al. (1994) Statistical models for predicting aggregate stability from intrinsic soil components.  
#'
#' @export
sptf_cec63 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC (n = 15, R2 =  0.62)
  dt[, value := exp(2.2334 + 0.2485 * log(A_CLAY_MI) + 0.1556 * log(A_C_OF)) * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC for agricultural soils (0-20cm) in New Zealand
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Curtin & Trolove (2013) Predicting pH buffering capacity of New Zealand soils from organic matter content and mineral characteristics
#'
#' @export
sptf_cec64 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (clay in g/kg)
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI * 10,
                   value = NA_real_)
  
  # function for CEC (n = 34, R2 =  0.66)
  dt[, value := exp(-0.6274 + 0.643 * log(A_C_OF) + 0.2384 * log(A_CLAY_MI)) * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the effective CEC at pH 7 for agricultural topsoils (0-30cm) in Iran.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'  
#' @import data.table
#' 
#' @references Rashidi & Seilsepour (2008) Modeling of soil cation exchange capacity based on soil organic carbon
#'
#' @export
sptf_cec65 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 100)

  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   value = NA_real_)
  
  # function effective CEC, NH4-Ac and KCl (n= 57, R2 =  0.73)
  dt[, value := 7.93 + 8.72 * A_C_OF]
 
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC for soils in Iraq
#'
#' @param A_SOM_LOI (numeric) The organi cmatter content of the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Majed et al. (1991) Relative contribution of clay and organic fraction to the cation exchange capacity of northern Iraqi Soils. Cited in Fattah et al. (2021)
#'
#' @export
sptf_cec66 <- function(A_SOM_LOI, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC (n = 65, R2 =  0.77)
  dt[, value := (10.96 + 0.45 * A_CLAY_MI + 3.10 * A_SOM_LOI) * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC for soils in Iran
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Sarmadian et al. (2009) Modeling of Some Soil Properties Using Artificial Neural Network and Multivariate Regression in Gorgan Province, North Iran
#'
#' @export
sptf_cec67 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC (n = 125 , R2 =  0.63)
  dt[, value := (1.91 + 0.318 * A_CLAY_MI + 3.96 * A_C_OF) * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC for soils in New Zealand
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Parfitt et al. (1995) Contribution of organic matter and clay minerals to the cation exchange capacity of soils
#'
#' @export
sptf_cec68 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (both in kg/kg)
  dt <- data.table(A_C_OF = A_C_OF * 0.001,
                   A_CLAY_MI = A_CLAY_MI*0.01,
                   value = NA_real_)
  
  # function for CEC for A horizons (n = 347 , R2 =  0.7, manually estimated)
  dt[, value := (221 * A_C_OF + 33 * A_CLAY_MI) * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 for soils in India, variable land uses
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Somani (1983). The computation of the cation exchange capacity of soils from clay and organic matter content. Cited in Meyer (1994).
#'
#' @export
sptf_cec69 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (both in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC NH4OAc for unknown horizons (n = 42 , R2 =  0.83)
  dt[, value := (2.57 + 0.36 * A_CLAY_MI + 6.72 * A_C_OF) * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 for forage soils in Quebec.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Martel and Laverdiere (1976). Facteurs qui influencent la teneur de la matibre organique et les propietes d'echange cationique des horizons Ap des sols de grande culture du Quebec. Cited in Meyer (1994).
#'
#' @export
sptf_cec70 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (both in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC NH4OAc for A horizons (n = 17 , R2 =  0.88)
  dt[, value := (7.06 + 0.291 * A_CLAY_MI + 2.77 * A_C_OF) * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for various land use soils in Ireland
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' 
#' @import data.table
#' 
#' @references Curtin and Smillie (1976). Estimation of components of soil cation exchange capacity from measurements of specific surface and organic matter. Cited in Meyer (1994).
#'
#' @export
sptf_cec71 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (both in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # function for CEC BaCl2 for A horizons (n = 51 , R2 =  0.58)
  dt[, value := (0.13 + 0.385 * A_CLAY_MI + 6.93 * A_C_OF) * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8.2 for various land use soils in Ireland
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#' 
#' @import data.table
#' 
#' @references Tranter et al. (2009). Using distance metrics to determine the appropriate domain of pedotransfer function predictions
#'
#' @export
sptf_cec72 <- function(A_C_OF, A_CLAY_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table (both in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # function for CEC BaCl2 for A horizons (n = 92 , R2 =  0.69)
  dt[, value := (3.7 - 0.28 * A_PH_WA + 2.6 * log10(A_C_OF) + 0.28 * A_CLAY_MI) * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the effective CEC at pH 7 for agricultural topsoils (0-30cm) in Germany
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#'   
#' @import data.table
#' 
#' @references deumlich et al. (2015) Characterization of cation exchange capacity (CEC) for agricultural land-use areas
#'
#' @export
sptf_cec73 <- function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI,A_PH_WA) {
  
  # add visual bindings
  v1 = v2 = A_SLIB_MI = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(id = 1: arg.length,
                   A_SOM_LOI = A_SOM_LOI,
                   A_SLIB_MI = A_CLAY_MI + 0.3 * A_SILT_MI,
                   A_PH_WA = A_PH_WA,
                   value = NA_real_)
  
  # function effective CEC, NH4-Ac and KCl (n = 1533, R2 unknown)
  dt[, v1 := -2.84 + 0.33 * A_SLIB_MI + 1.75 * A_SOM_LOI + 0.6 * A_PH_WA]
  dt[, v2 := 0.5 * A_CLAY_MI + 0.05 * A_SILT_MI]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of soils in continental USA per taxonomic order stratification group.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' @param A_PH_WA (numeric) The acidity of the soil, pH in water (-).
#' @param B_SOILCLASS_USDA The soil type class according to the USDA Soil Taxonomy (https://en.wikipedia.org/wiki/USDA_soil_taxonomy)
#' 
#' @import data.table
#' 
#' @references Manrique et al. (1990) Estimation of exchangeable bases and base saturation from soil physical and chemical data
#'
#' @export
sptf_cec74 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI,A_PH_WA,B_SOILCLASS_USDA) {
  
  # add visual bindings
  v1 = v2 = v3 = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_CLAY_MI),length(A_SILT_MI),length(A_PH_WA),length(B_SOILCLASS_USDA))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_character(B_SOILCLASS_USDA,len = arg.length)
  checkmate::assert_subset(B_SOILCLASS_USDA,choices = c('alfisol','andisol','aridisol','entisol',
                                                        'gelisol','inceptisol','mollisol','oxisol',
                                                        'spodosol','ultisol','vertisol','histosol',
                                                        NA_character_))
  
  # make internal data.table (assuming that Corg equals total C, units in \%)
  dt <- data.table(id = 1:arg.length,
                   A_C_OF = A_C_OF * 0.1,
                   A_PH_WA = A_PH_WA,
                   A_SILT_MI = A_SILT_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # add general eqation (R2 0.58, R2 = 0.63)
  dt[, v1 := -4.043 + 0.071 * A_PH_WA^3]
  dt[, v2 := -50.231 + 9.689 * A_PH_WA + 0.251 * A_CLAY_MI]
  
  # add CEC for horizon B (R2 = 0.63)
  dt[, v3 := -4.916 + 0.075 * A_PH_WA^3]
  
  # Estimate mean value
  dt <- melt(dt,id.vars = 'id',measure.vars = c('v1','v2','v3'))
  dt <- dt[,list(value = mean(value,na.rm=T)),by='id']
  
  # alfisols (R2 = 0.45 - 0.53)
  dt[B_SOILCLASS_USDA == 'alfisol', value := -0.328 + 0.059 * A_PH_WA^3]
  dt[B_SOILCLASS_USDA == 'alfisol', value := (value -38.366 + 7.422 * A_PH_WA + 0.297 * A_CLAY_MI) * 0.5]
  
  # aridisols (R2 = 0.35)
  dt[B_SOILCLASS_USDA == 'aridisol', value := -85.477 + 18.839 * A_PH_WA + 0.421 * A_CLAY_MI]
  
  # entisols (R2 = 0.44)
  dt[B_SOILCLASS_USDA == 'entisol', value := -48.564 + 9.553 * A_PH_WA]
  
  # inceptisols (R2 = 0.55)
  dt[B_SOILCLASS_USDA == 'inceptisol', value := -6.227 + 0.073*A_PH_WA^3]
  
  # Mollisols (nR2 = 0.54)
  dt[B_SOILCLASS_USDA == 'mollisol' , value := -62.51 + 11.013 * A_PH_WA +0.436 * A_CLAY_MI]

  # convert unit from cmol to mmol+/kg
  dt[,value := 10 * value]
  
  # select output variable
  value <- dt[,value]
  
  # return value
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#' 
#' @import data.table
#' 
#' @references Thiere et al. (1986). Analyse des Zusammenhanges von Austauschkapazit€at (T-Wert), Kornung, organischer Bodensubstanz und Bodenreaktion fur verbreitete € Substrat- und Horizontgruppen. Cited in Liao & Zhu (2015).
#'
#' @export
sptf_cec75 <- function(A_C_OF, A_CLAY_MI, A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table (with clay and Corg in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC)
  
  # function for CEC(n = , R2 = )
  dt[, value := -3.30 + 0.46 * A_CLAY_MI + 3.06 * A_C_OF + 0.77 * A_PH_CC]
  
  # update unit from cmol/kg to mmol/kg
  dt[, value := value * 10]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}
#
# montecillo, tropical
# sinoga, spain
# soares 2008 tropics
# https://doi.org/10.1080/00103628309367409, Fpilippine
# https://doi.org/10.2136/sssaj1962.03615995002600030021x Kamprath

# cec after removal of som, discussion dabkowska_2001_cation
# deumlich bevat interpretatie data CEC

