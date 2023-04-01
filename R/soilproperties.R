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
  
  # function for CEC at 6.5 (ammonium acetate buffered, n = 60)
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

#' Calculate the CEC
#'
#' This function calculates the CEC at pH of sample for forest topsoils (0-30cm) in Switzerland
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Soly et al. (2020) A Critical Evaluation of the Relationship Between the Effective Cation Exchange Capacity and Soil Organic Carbon Content in Swiss Forest Soils
#'
#' @export
sptf_cec10 <- function(A_SOM_LOI, A_CLAY_MI, A_PH_CC,B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_CC),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,length = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   B_LU_PTFCLASS = B_LU_PTFCLASS
  )
  
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
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Soly et al. (2020) A Critical Evaluation of the Relationship Between the Effective Cation Exchange Capacity and Soil Organic Carbon Content in Swiss Forest Soils
#'
#' @export
sptf_cec11 <- function(A_SOM_LOI, A_CLAY_MI, A_PH_CC,B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_CC),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 12, len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,length = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   B_LU_PTFCLASS = B_LU_PTFCLASS
  )
  
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
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param B_LU_PTFCLASS (character) The land use category (options: agriculture, grassland, cropland, forest, nature)
#'  
#' @import data.table
#' 
#' @references Aprile & Lorandi (2012) Evaluation of Cation Exchange Capacity (CEC) in Tropical Soils Using Four Different Analytical Methods
#'
#' @export
sptf_cec12 <- function(A_SOM_LOI, B_LU_PTFCLASS) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI),length(B_LU_PTFCLASS))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_character(B_LU_PTFCLASS,length = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table (A_C_OF in g/kg; SOM in \%)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_SOM_LOI * 0.5 * 10,
                   B_LU_PTFCLASS = B_LU_PTFCLASS
                   )
  
  # kg / m3 uitrekent = g/dm3 => 1 dm3 grond heeft een massa van van 1315 g
  # 1 kg heeft dus een volume van 0.7604563 dm3
  # 1 kg grond heeft 20 g C/kg grond = 20 g C/0.7604563 dm3 = 
  
  # convert A_C_OF from g/kg to g/dm3
  dt[,D_DENSITY := 1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF]
  dt[,A_C_OF := A_C_OF * D_DENSITY / 1000]
  
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
                   A_CLAY_MI = A_CLAY_MI)
  
  # function for CEC at 7 (NH4Ac) in mmol+/kg
  dt[, value := 10 * (3.98 + 0.565 * A_SOM_LOI + 0.377 * A_CLAY_MI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of agricultural and forest top soils (0-22 cm) in Ohio
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
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI)
  
  # function for CEC at 7 (NH4Ac) in mmol+/kg (in total n = 169, with R2 values ranging from 0.58 to 0.79)
  dt[, v1 := 10 * (6.7 + 2.11 * A_SOM_LOI + 0.173 * A_CLAY_MI)]
  dt[, v2 := 10 * (-2.4 + 0.539 * A_CLAY_MI + 2.9 * A_SOM_LOI)]
  dt[, v3 := 10 * (5.5 + 1.92 * A_SOM_LOI + 0.632 * A_CLAY_MI)]
  dt[, v4 := 10 * (-1.1 + 0.793 * A_CLAY_MI -2.05 * A_SOM_LOI)]
  dt[, v5 := 10 * (16.5 + 1.54 * A_SOM_LOI + 0.351 * A_CLAY_MI)]
  dt[, v6 := 10 * (-8.5 + 0.831 * A_CLAY_MI +3.83 * A_SOM_LOI)]
  
  # include only topsoils (v1, v3, v5 are A hroizons, v2, v4 and v6 are B horizons)
  dt[, value := (v1 + v3 + v5 ) / 3]
  
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
                   A_CLAY_MI = A_CLAY_MI)
  
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
sptf_cec16 <- function(A_SOM_LOI, A_CLAY_MI, A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_WA = A_PH_WA
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
  checkmate::assert_character(B_LU_PTFCLASS,length = arg.length)
  checkmate::assert_subset(B_LU_PTFCLASS, choices = c('agriculture', 'grassland', 'cropland', 'forest', 'nature'))
  
  # make internal data.table (ASOM in g/kg)
  dt <- data.table(A_SOM_LOI = A_SOM_LOI * 10,
                   B_LU_PTFCLASS = B_LU_PTFCLASS
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
                   A_CLAY_MI = A_CLAY_MI)
  
  # function for CEC at pH 7 in mmol+/kg (n = 83, R2 = 0.913)
  dt[, value := 10 * (-0.68 + 1.73 * A_SOM_LOI + 0.71 * A_CLAY_MI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 8 of agricultural soils in Wisconsin.
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
                   A_CLAY_MI = A_CLAY_MI)
  
  # function for CEC at pH 8 in mmol+/kg (n = 60, R2 = 0.92)
  dt[, value := 10 * (-0.66 + 3.66 * A_C_OF + 0.64 * A_CLAY_MI)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of forest topsoils (0-30 cm) in Nigeria
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
sptf_cec19 <- function(A_C_OF, A_CLAY_MI,A_PH_WA) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI),length(A_PH_WA))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_WA, lower = 2, upper = 12, len = arg.length)
  
  # make internal data.table (with SOC in %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_PH_WA = A_PH_WA,
                   A_CLAY_MI = A_CLAY_MI)
  
  # function for CEC at pH 7 in mmol+/kg (n = 35, R2 = 0.58 to 0.711)
  dt[, v1 := 10 * (2.17 + 3.072 * A_C_OF)]
  dt[, v2 := 10 * (2.17 + 0.0230 * A_CLAY_MI + 2.802 * A_C_OF)]
  dt[, v3 := 10 * (-11.62 + 2.379 * A_PH_WA + 2.819 * A_C_OF)]
  dt[, v4 := 10 * (-15.37 + 3.173 * A_PH_WA + 0.0890 * A_CLAY_MI)]
  dt[, v5 := 10 * (-13.93 + 2.645 * A_PH_WA + 0.0446 * A_CLAY_MI + 2.267 * A_C_OF)]
  
  # take the mean estimated CEC
  dt[, value := (v1 + v2 + v3 + v4 + v5) / 5]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mmol+ / kg)
  return(value)
  
}

#' Calculate the CEC
#'
#' This function calculates the CEC at pH 7 of agricultural soils (variable depth) in Sudan
#'
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_SILT_MI (numeric) The silt content of the soil (\%).
#' 
#' @import data.table
#' 
#' @referencesAdam et al. (2021) Predicting Soil Cation Exchange Capacity in Entisols with Divergent Textural Classes: The Case of Northern Sudan Soils.
#'
#' @export
sptf_cec20 <- function(A_CLAY_MI,A_SILT_MI) {
  
  # Check input
  arg.length <- max(length(A_CLAY_MI),length(A_SILT_MI))
   checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table 
  dt <- data.table(A_SILT_MI = A_SILT_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = 100 - A_SILT_MI - A_CLAY_MI)
  
  # derive USDA soiltype
  dt[num_obs >= 2, B_SOILTYPE := sptf_textureclass(A_CLAY_MI = A_CLAY_MI,A_SILT_MI = A_SILT_MI,A_SAND_MI = A_SAND_MI)]
  
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

