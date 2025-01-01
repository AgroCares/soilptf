# Functions for MB (microbial biomass) 

#' Calculate the MB in arable land given the pedo transfer function of Oberholzer and Scheid, 2007 
#' 
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references not available; cited by Greiner, L., Nussbaum, M., Papritz, A., Fraefel, M., Zimmermann, S., Schwab, P., Grêt‐Regamey, A., & Keller, A. (2018). Assessment of soil multi-functionality to support the sustainable use of soil resources on the Swiss Plateau. Geoderma Regional, 14, e00181. https://doi.org/10.1016/j.geodrs.2018.e00181
#'
#' @export
sptf_mb1 <- function(A_SOM_LOI, A_CLAY_MI, A_SAND_MI, A_PH_CC) {
  
  # add visal bindings
  value = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI), length(A_SAND_MI), length(A_PH_CC))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10,len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  
  # idem for soil pH
  dt[is.na(A_PH_CC), A_PH_CC := median(c(4.3,5.2,7.7,4.9,5.4,5.1,6.1,5.6,5.4,5.6,5.5,5.3,6.1,6.3,5))]
  
  # Calculate MB (mg/kg dry soil)
  dt[, value := exp(3.58 + 0.82 * log(A_SOM_LOI) + 0.15 * A_PH_CC + 0.31 * log(A_CLAY_MI) + 0.005 * A_SAND_MI)]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the MB in grassland given the pedo transfer function of Oberholzer and Scheid, 2007 
#' 
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references not available; cited by Greiner, L., Nussbaum, M., Papritz, A., Fraefel, M., Zimmermann, S., Schwab, P., Grêt‐Regamey, A., & Keller, A. (2018). Assessment of soil multi-functionality to support the sustainable use of soil resources on the Swiss Plateau. Geoderma Regional, 14, e00181. https://doi.org/10.1016/j.geodrs.2018.e00181
#'
#' @export
sptf_mb2 <- function(A_SOM_LOI, A_CLAY_MI, A_PH_CC) {
  
  # add visal bindings
  value = NULL
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(A_CLAY_MI), length(A_PH_CC))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10,len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  
  # idem for soil pH
  dt[is.na(A_PH_CC), A_PH_CC := median(c(4.3,5.2,7.7,4.9,5.4,5.1,6.1,5.6,5.4,5.6,5.5,5.3,6.1,6.3,5))]
  
  # Calculate MB (mg/kg dry soil)
  dt[, value := exp(3.61 + 0.92 * log(A_SOM_LOI) + 0.28 * A_PH_CC + 0.17 * log(A_CLAY_MI))]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the MB in agricultural land treated by tillage and additional SOM given the pedo transfer function of Rui et al. (2016) at experimental site in Australia
#' 
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references Rui, Y., Murphy, D. V., Wang, X., & Hoyle, F. C. (2016). Microbial respiration, but not biomass, responded linearly to increasing light fraction organic matter input: Consequences for carbon sequestration. Scientific Reports, 6(1). https://doi.org/10.1038/srep35496
#'
#' @export
sptf_mb3 <- function(A_C_OF) {
  
  # add visal bindings
  mbc = D_BDS = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF, 
                   value = NA_real_)
  
  # Calculate microbial biomass carbon (kg C/ha)
  #dt[, mbc := (1.7725 * A_C_OF ^ 2 - 11.01 * A_C_OF + 17.853) * A_C_OF]
  
  # set depth as 10cm(0.2m)
  depth = 0.1
  
  # use bulk density (kg / m3)
  dt[,D_BDS := 1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF]
  
  #should be converted to mg/kg using bulk density
  #dt[, value := mbc / 10000 /(D_BDS * 0.1) * 1000000]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the MB in Mulun National Natural Reserve in southwest China
#' 
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references Qian, Z., Li, Y., Du, H., Wang, K., & Li, D. (2023). Increasing plant species diversity enhances microbial necromass carbon content but does not alter its contribution to soil organic carbon pool in a subtropical forest. Soil Biology & Biochemistry, 187, 109183. https://doi.org/10.1016/j.soilbio.2023.109183
#'
#' @export
sptf_mb4 <- function(A_C_OF) {
  
  # add visal bindings
  mbc = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF, 
                   value = NA_real_)
  
  # Calculate microbial biomass carbon (g/kg)
  dt[, value := (A_C_OF - 23.5) / 25.2]
  dt[value < 0, value := 0]
  
  # convert to (mg/kg)
  dt[, value := value * 1000]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the MB of agricultural land with different management practices in Tibetan Plateau, China
#' 
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references Li, J. H., Hou, Y. L., Zhang, S. X., Li, W. J., Xu, D. H., Knops, J. M. H., & Shi, X. (2018). Fertilization with nitrogen and/or phosphorus lowers soil organic carbon sequestration in alpine meadows. Land Degradation & Development, 29(6), 1634–1641. https://doi.org/10.1002/ldr.2961
#'
#' @export
sptf_mb5 <- function(A_C_OF) {
  
  # add visal bindings
  mbc = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF, 
                   value = NA_real_)
  
  # convert SOC(g/kg) to SOC (%)
  dt[, A_C_OF := A_C_OF * 0.1]
  
  # Calculate microbial biomass carbon (g/kg)
  dt[, value := (log(A_C_OF) - 0.44) / 0.22]
  dt[value < 0, value := 0]
  
  # convert to (mg/kg)
  dt[, value := value * 1000]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}