# Functions for water holding capacity (WHC)

#' Calculate the water holding capacity given the pedotransferfunction of Bagdal et al.2022 for non-calcareous soil
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @references Bagdal et al.(2022) Carbon-sensitive pedotransfer functions for plant available water
#'
#' @export
sptf_whc1 <- function(A_C_OF, A_SAND_MI, A_CLAY_MI) {

  # add visual bindings
  theta_pwp = theta_fc = NULL
  
  # Check input
  arg.length <-max(length(A_C_OF), length(A_SAND_MI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_
                  )
  
  # to be deleted
  
  # convert mm / 100mm to fraction (cm3/cm3)
  dt[, value := NA_real_]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the water holding capacity given the pedotransferfunction of Bagdal et al.2022 for calcareous soil
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @references Bagdal et al.(2022) Carbon-sensitive pedotransfer functions for plant available water
#'
#' @export
sptf_whc2 <- function(A_C_OF, A_SAND_MI, A_CLAY_MI) {
  
  # add visual bindings
  theta_wp = theta_fc = NULL
  
  # Check input
  arg.length <-max(length(A_C_OF), length(A_SAND_MI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_
                  )
  
  # to be deleted
  
  # convert mm / 100mm to fraction (cm3/cm3)
  dt[, value := NA_real_]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the water holding capacity given the pedotransferfunction of Saxton et al.1986
#'
#' @inheritParams sptf_bd0
#'
#' @details
#' mp_fc must be larger than 10
#'
#' @import data.table
#'
#' @references Saxton et al.(1986) Estimating Generalized Soil-water Characteristics from Texture
#'
#' @export
sptf_whc3 <-  function(A_SAND_MI,A_CLAY_MI) {
    
    # Add visual bindings
    theta = theta_sat = theta_res = theta_fc = alfa = n = A = B = theta_wp =NULL
    
    # set default parameters for this function
    mp_whc = 0
    
    # check inputs
    arg.length <-max(length(A_SAND_MI), length(A_CLAY_MI))
    checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
    checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)

    # Collect data into a table (set in units %)
    dt <- data.table(A_SAND_MI = A_SAND_MI,
                     A_CLAY_MI = A_CLAY_MI,
                     value = NA_real_)
    
    # Calculate parameter values
    dt[, A := 100 * exp(-4.396 - 0.0715 * A_CLAY_MI - 0.000488 * A_SAND_MI ^ 2 - 0.00004285 * A_SAND_MI ^ 2 * A_CLAY_MI)]
    dt[, B := -3.140 - 0.00222 * A_CLAY_MI ^ 2 - 0.00003484 * A_SAND_MI ^ 2 * A_CLAY_MI]
    
    # Calculate volumetric water content at saturation (cm3/cm3)
    dt[, theta := exp(log(1 * 10^mp_whc / A) / B)]
    
    # select value
    value <- dt[, theta]
    
    # return value
    return(value)
    
  }

#' Calculate the waterholding capacity given the pedotransferfunction of Oosterveld and Chang (1980)
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @details
#' For this function A_DEPTH should be between 0.08 and 0.18
#'
#'
#' @references Oosterveld and Chang (1980) Empirical relations between laboratory determinations of soil texture and moisture retention
#'
#' @export
sptf_whc4 <-  function(A_C_OF,A_CLAY_MI,A_SAND_MI) {
    
  # Add visual bindings
  theta_sat = theta_res = theta_fc = alfa = n = theta_wp = mp_fc = NULL
  
  # set default parameters for this function
  A_DEPTH = 0.15
  mp_wp = 1500
  
  # Check input
  arg.length <-max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_DEPTH, lower = 0.08, upper = 0.180) 
  
  # Collect data into a table (set in units %, depth in cm)
  dt <- data.table(A_C_OF = A_C_OF,
                   A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_DEPTH = A_DEPTH * 100,
                   value = NA_real_
                  )
    
  # to be deleted
  
  # convert mm / 100mm to fraction (cm3/cm3)
  dt[, value := NA_real_]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
    
  }


#' Calculate the waterholding capacity given the pedotransferfunction of Wösten et al. 1999
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @references Wösten et al. (1999) Development and use of a database of hydraulic properties of European soils
#'
#' @export
sptf_whc5 <-  function(A_SOM_LOI, A_CLAY_MI, A_SILT_MI) {
    
    # Add visual bindings
    theta_sat = theta_res = theta_fc = alfa = n = theta_wp = NULL
    A_SAND_MI = A_C_OF = D_BDS = theta = NULL
    
    # set default parameters for this function
    # boolean argument for top soil (1) or sub-soil (0)
    A_DEPTH = 0.15
    topsoil = 1 
    mp_whc = 0
    
    # Check input
    arg.length <-max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SAND_MI))
    checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
    checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
    checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
    
    # Collect data into a table (set in units %)
    dt <- data.table(A_SILT_MI = A_SILT_MI,
                     A_CLAY_MI = A_CLAY_MI,
                     A_SOM_LOI = A_SOM_LOI,
                     A_C_OF = A_SOM_LOI * 0.5 * 10,
                     topsoil = topsoil,
                     value = NA_real_ )
    
    # add density (g / cm3)
    dt[,D_BDS := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF)*0.001]
    
    # Estimate water retention parameters
    dt[, theta_res    := 0.01]
    dt[, theta_sat    := 0.7919 + 0.001691 * A_CLAY_MI - 0.29619 * D_BDS - 0.000001491 * A_SILT_MI ^ 2
       + 0.0000821 * A_SOM_LOI ^ 2 + 0.02427 / A_CLAY_MI + 0.01113 / A_SILT_MI + 0.01472 * log(A_SILT_MI)
       - 0.0000733 * A_SOM_LOI * A_CLAY_MI - 0.000619 * D_BDS * A_CLAY_MI
       - 0.001183 * D_BDS * A_SOM_LOI - 0.0001664 * topsoil * A_SILT_MI]
    
    dt[,  alfa      := exp(
      -14.96 + 0.03135 * A_CLAY_MI + 0.0351 * A_SILT_MI + 0.646 * A_SOM_LOI
      + 15.29 * D_BDS - 0.192 * topsoil - 4.671 * D_BDS ^ 2 - 0.000781 * A_CLAY_MI ^ 2
      - 0.00687 * A_SOM_LOI ^ 2 + 0.0449 / A_SOM_LOI + 0.0663 * log(A_SILT_MI)
      + 0.1482 * log(A_SOM_LOI) - 0.04546 * D_BDS * A_SILT_MI - 0.4852 * D_BDS * A_SOM_LOI
      + 0.00673 * topsoil * A_CLAY_MI
    )]
    
    dt[, n         := 1 + exp(
      -25.23 - 0.02195 * A_CLAY_MI + 0.0074 * A_SILT_MI - 0.1940 * A_SOM_LOI
      + 45.5 * D_BDS - 7.24 * D_BDS ^ 2 + 0.0003658 * A_CLAY_MI ^ 2
      + 0.002885 * A_SOM_LOI ^ 2 - 12.81 / D_BDS - 0.1524 / A_SILT_MI - 0.01958 / A_SOM_LOI
      - 0.2876 * log(A_SILT_MI) - 0.0709 * log(A_SOM_LOI) - 44.6 * log(D_BDS)
      - 0.02264 * D_BDS * A_CLAY_MI + 0.0896 * D_BDS * A_SOM_LOI
      + 0.00718 * topsoil * A_CLAY_MI
    )]
    
    # Calculate volumetric water content at whc (cm3/cm3)
    dt[, theta := pF_curve(head=-1*10^mp_whc, theta_res, theta_sat, alfa, n)]
    
    # Calculate water holding capacity (cm3/cm3)
    dt[, value :=  theta]
    
    # return value
    value <- dt[, value]
    
    # return value
    return(value)
    
  }



#' Calculate the water holding capacity given the pedotransferfunction of Vereecken et al. 1989
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @references Vereecken et al. (1989) Estimating the soil moisture retention characteristics from texture, bulk density and carbon content
#'
#' @export
sptf_whc6 <-  function(A_C_OF,A_CLAY_MI, A_SAND_MI) {
    
    # Add visual bindings
    theta_sat = theta_res = theta_fc = alfa = n = theta_wp =NULL
    D_BDS = NULL
    
    # set default parameters for this function
    mp_whc = 0
    
    # Check input
    arg.length <-max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI))
    checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
    checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
    checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
    
    # Collect data into a table (set in units %)
    dt <- data.table(A_SAND_MI = A_SAND_MI,
                     A_CLAY_MI = A_CLAY_MI,
                     A_C_OF = A_C_OF * 0.1,
                     value = NA_real_
                    )
    
    # add density
    dt[,D_BDS := (1617 - 77.4 * log(A_C_OF * 10) - 3.49 * A_C_OF * 10)*0.001]
    
    # Estimate water retention parameters
    dt[, theta_sat := 0.81 - 0.283 * D_BDS + 0.001 * A_CLAY_MI]
    dt[, theta_res := 0.015 + 0.005 * A_CLAY_MI + 0.014 * A_C_OF]
    dt[, alfa := exp(-2.486 + 0.025 * A_SAND_MI - 0.351 * A_C_OF - 2.617 * D_BDS - 0.023 * A_CLAY_MI)]
    dt[, n := exp(0.053 - 0.009 * A_SAND_MI - 0.013 * A_CLAY_MI + 0.00015 * A_SAND_MI^2)]
    
    # Calculate volumetric water content at field capacity (cm3/cm3) and wilting point
    # note that the van Genuchten model structure is different here
    dt[, value := theta_res + (theta_sat - theta_res) / (1 + abs(alfa * (-1 * 10^mp_whc)) ^ n)]

    # return value
    value <- dt[, value]
    
    # return value
    return(value)
    
  }

#' Calculate the water holding capacity given the pedotransferfunction of Szabo et al.2021
#' The RF-model based prediction can be calculated using the R package euptf2 (https://github.com/tkdweber/euptf2/)
#' Note YF 20220928: It may be better to copy-paste codes and RF model results from euptf2,
#' # so that we have no dependency on euptf2 package?
#' Note YF 20220929: The function 'euptfFun' does not work when the data frame has only 1 row ?!
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @references Szabó et al.(2021) Updated European hydraulic pedotransfer functions with communicated uncertainties in the predicted variables (euptfv2)
#'
#' @export
sptf_whc7 <-function(A_C_OF,A_CLAY_MI,A_SILT_MI, A_SAND_MI) {
    
    # Add visual bindings
    theta_sat = theta_res = theta_fc = alfa = n = theta_wp =NULL
    D_BDS = . = NULL
    
    # set default parameters for this function
    A_DEPTH = 0.15
    mp_wp = 1500
    mp_fc = 33
    
    # Check input
    arg.length <-max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI))
    checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
    checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
    checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
    checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
    
    # Collect data into a table (set in units %, and depth in cm)
    dt <- data.table(A_C_OF = A_C_OF * 0.1,
                    A_CLAY_MI = A_CLAY_MI,
                    A_SILT_MI = A_SILT_MI,
                    A_SAND_MI = A_SAND_MI,
                    A_DEPTH = A_DEPTH * 100,
                    value = NA_real_
                  )
    
    # add density (with Corg in g/kg as input)
    dt[,D_BDS := 1617 - 77.4 * log(A_C_OF * 10) - 3.49 * A_C_OF * 10]
    
    # Estimate water retention parameters, using functions of the R package 'euptf2'
    # store necessary predictor as data frame
    dt2 <- dt[,.(DEPTH_M = A_DEPTH,
                 USSAND = A_SAND_MI,
                 USSILT = A_SILT_MI,
                 USCLAY = A_CLAY_MI,
                 OS = A_C_OF,
                 BD = D_BDS)]
    dt2 <- as.data.frame(dt2)
    
    # estimate water retention parameters
    # (This works only then dt2 has more than 1 rows!!)
    # euptfres <- as.data.table(euptf2::euptfFun(# this funtion is disabled to avoid importing euptf2 This function should be included in soilptf to avoid dependenc
    #   ptf = "PTF07",
    #   predictor = dt2,
    #   target = c("VG")
    # ))
    #euptfres = NULL
    #cols <- c("THS_PTF07", "THR_PTF07", "ALP_PTF07", "N_PTF07")
    #cols2 <- c("theta_sat", "theta_res", "alfa", "n")
    #setnames(euptfres, cols, cols2)
    #dt <- cbind(dt, euptfres[, mget(cols2)])
    
    
    # Calculate volumetric water content at field capacity (cm3/cm3)
    #dt[, theta_fc := pF_curve(mp_fc * 10, theta_res, theta_sat, alfa, n)]
    
    # Calculate volumetric water content at wilting point (cm3/cm3)
    #dt[, theta_wp := pF_curve(mp_wp * 10, theta_res, theta_sat, alfa, n)]
    
    # Calculate water holding capacity (cm3/cm3)
    #dt[, value :=  theta_fc - theta_wp]
    
    # # Estimate water holding capacity directly using 'euptf2'
    # # This gives different values from those calculated from water retention parameters???
    # # Plant available water (FC - WP), with field capacity at -330 cm
    # dt[, awc_330 := euptf2::euptfFun(ptf = "PTF03",
    #                                  predictor = dt2,
    #                                  target = c("AWC"))$AWC_PTF03]
    # # Plant available water (FC - WP), with field capacity at -100 cm
    # dt[, awc_100 := euptf2::euptfFun(ptf = "PTF03",
    #                                  predictor = dt2,
    #                                  target = c("AWC_2"))$AWC_2_PTF03]
    
    # return value
    value <- dt[, value]
    
    # return value
    return(value)
    
  }

#' Calculate the water holding capacity given the pedotransferfunction of Weynants et al. 2009
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @references Weynants et al.(2009) Revisiting Vereecken Pedotransfer Functions: Introducing a Closed-Form Hydraulic Model
#'
#' @export
sptf_whc8 <- function(A_C_OF,A_CLAY_MI,A_SAND_MI) {
  
  # Add visual bindings
  theta_sat = theta_res = alfa =n = theta_fc = theta_wp =  NULL
  D_BDS = NULL
  
  # set default parameters for this function
  A_DEPTH = 0.15
  mp_whc = 0
  
  # Check input
  arg.length <-max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table (set in units %)
  dt <- data.table(A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_C_OF = A_C_OF * 0.1,
                   value = NA_real_
                  )
  
  # add density (with Corg in g/kg as input) as g/cm3
  dt[,D_BDS := (1617 - 77.4 * log(A_C_OF * 10) - 3.49 * A_C_OF * 10) * 0.001]
  
  # Calculate water retention parameters
  dt[, theta_sat := 0.6355 + 0.0013 * A_CLAY_MI - 0.1631 * D_BDS]
  dt[, theta_res := 0]
  dt[, alfa := exp(-4.3003 - 0.0097 * A_CLAY_MI + 0.0138 * A_SAND_MI - 0.0992 * A_C_OF)]
  dt[, n := exp(-1.0846 - 0.0236 * A_CLAY_MI - 0.0085 * A_SAND_MI
                + 1.3699 * 10 ^ (-4) * A_SAND_MI ^ 2) + 1]
  
  # Calculate volumetric water content at saturation (cm3/cm3)
  dt[, value := pF_curve(head=-1*10^mp_whc, theta_res, theta_sat, alfa, n)]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the water holding capacity given the pedotransferfunction of Tomasella & Hodnett 1998
#'
#' @inheritParams sptf_bd0
#'
#' @details
#' mp_fc must be either 10 or 33
#'
#' @import data.table
#'
#' @references Tomasella & Hodnett (1998) Estimating soil water retention characteristics from limited data in Brazilian Amazonia. Soil Sci. 163, 190-202.
#'
#' @export
sptf_whc9 <- function(A_C_OF,A_CLAY_MI, A_SILT_MI) {
  
  # add visual bindings
  theta_wp = a= d = b= theta_fc = mp = NULL
  
  # set default parameters for this function
  mp_fc = 33
  
  # Check input
  arg.length <-max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_subset(mp_fc, choices = c(10, 33), empty.ok = FALSE)
  
  # Collect data into a table (set in units %)
  dt <- data.table(
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    A_C_OF = A_C_OF * 0.1,
    value = NA_real_
  )
  
  # to be updated
  
  # Calculate water holding capacity (cm3/cm3)
  dt[, value :=  (23.839 + 0.530 * A_SILT_MI + 0.255 * A_CLAY_MI)*0.01]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the water holding capacity given the pedotransferfunction of Rawls et al.1982
#'
#' @inheritParams sptf_bd0
#'
#' @details
#' mp_fc must be either 10 or 33
#'
#' @import data.table
#'
#' @references Rawls et al.(1982) Estimation of soil water properties. Trans. ASAE 25, 1316–1320.
#'
#' @export
sptf_whc10 <-function(A_C_OF,A_CLAY_MI,A_SILT_MI,A_SAND_MI) {
    
  # add visual bindings
  mp = theta_wp = a = b = d = e = f = theta_fc = theta_wp = NULL
  D_BDS = A_SOM_LOI = NULL
  
  # set default parameters for this function
  mp_fc = 33
  
  # Check input
  arg.length <-max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_subset(mp_fc, choices = c(10, 33), empty.ok = FALSE)
  
    # Collect data into a table (set in units %)
    dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                     A_SILT_MI = A_SILT_MI,
                     A_SAND_MI = A_SAND_MI,
                     A_C_OF = A_C_OF * 0.1,
                     A_SOM_LOI = A_C_OF * 0.1 * 2,
                     value = NA_real_)
    
    # add density (with Corg in g/kg as input)
    dt[,D_BDS := (1617 - 77.4 * log(A_C_OF * 10) - 3.49 * A_C_OF * 10) * 0.001]
    
    # to be updated
    
    # Calculate water holding capacity (cm3/cm3)
    dt[, value := 0.7899 - 0.0037 * A_SAND_MI + 0.01 * A_SOM_LOI -0.1315 * D_BDS]
    
    # return value
    value <- dt[, value]
    
    # return value
    return(value)
    
  }

#' Calculate the water holding capacity given the pedotransferfunction of Campbell & Shiozawa 1992
#' Note: Calculation of whc parameters may be wrong. The calculated water content is out of normal range. Original literature can not be found on web.
#'
#' @inheritParams sptf_bd0
#'
#' @details
#' mp_fc must be either 10 or 33
#'
#' @import data.table
#'
#' @references Campbell & Shiozawa (1992) Prediction of hydraulic properties of soils using particle-size distribution and bulk density data
#'
#' @export
sptf_whc11 <- function(A_C_OF,A_CLAY_MI,A_SILT_MI) {
  
  # add visual bindings
  theta_sat = theta_res = theta_fc = alfa = n = d_g = sigma_g = psi_es = lambda = psi_b=  theta_wp = NULL
  D_BDS = NULL
  
  # set default parameters for this function
  mp_whc = 0
  
  # Check input
  arg.length <-max(length(A_C_OF), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(mp_whc, any.missing = FALSE)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_
                  )
  
  # add density (with Corg in g/kg as input)
  dt[,D_BDS := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF)*0.001]
  
  # Calculate water holding capacity (cm3/cm3)
  dt[, value := calc_soil_porosity(D_BDS)]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}



#' Calculate the water holding capacity given the pedotransferfunction of Rawls & Brakensiek 1985
#'
#' Note: Calculation of whc parameters may be wrong. The calculated water content is out of normal range. Original literature cannot be found on web.
#'
#' @inheritParams sptf_bd0
#'
#' @details
#' mp_fc must be either 10 or 33
#'
#'
#' @import data.table
#'
#' @references Rawls & Brakensiek (1985) Prediction of Soil Water Properties for Hydrologic Modelling
#'
#' @export
sptf_whc12 <- function(A_C_OF,A_CLAY_MI,A_SAND_MI) {
  
  # Add visual bindings
  theta_sat = theta_res = theta_fc = alfa = n = por = psi_b = lambda =  theta_wp = NULL
  D_BDS = NULL
  
  # set default parameters for this function
  mp_fc = 2.0
  mp_wp = 4.2
  
  # Check input
  arg.length <-max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(mp_wp, any.missing = FALSE)
  checkmate::assert_subset(mp_fc, choices = c(2, 4.2))
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   value = NA_real_
                  )
  
  # add density (with Corg in g/kg as input) in units g/cm3
  dt[,D_BDS := (1617 - 77.4 * log(A_C_OF) - 3.49 * A_C_OF)*0.001]
  
  # Calcaulte parameters of water retention curve of Brooks and Corey (1964)
  dt[, value := calc_soil_porosity(D_BDS)]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the water holding capacity given the pedotransferfunction of Tian et al. 2021
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @references Tian et al. (2021) New pedotransfer functions for soil water retention curves that better account for bulk density effects
#'
#' @export
sptf_whc13 <- function(A_C_OF,A_CLAY_MI,A_SAND_MI) {
  
  # Add visual bindings
  theta_sat = theta_res = theta_fc = alfa = n = A_LOAM_MI = Dichtheid = theta_wp =  NULL
  D_BDS = NULL
  
  # Check input
  arg.length <-max(length(A_C_OF), length(A_CLAY_MI),length(A_SAND_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length)

  # Collect data into a table (set in units %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1,
                   A_SAND_MI = A_SAND_MI,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_
                  )
  
  # add density (with Corg in g/kg as input)
  dt[,D_BDS := (1617 - 77.4 * log(A_C_OF *10) - 3.49 * A_C_OF * 10)*0.001]
  
  # Calculate water retention parameters (exc. OC; Eq. 13-16)
  dt[, value := -0.3311 * D_BDS + 0.8916]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the water holding capacity given the pedotransferfunction of Wösten 1997
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @references Wösten, J. H. M. (1997). Chapter 10 Pedotransfer functions to evaluate soil quality. In: Developments in Soil Science, Volume 25:221-245, Elsevier
#'
#' @export
sptf_whc14 <- function(A_SOM_LOI,A_CLAY_MI,A_SILT_MI) {
  
  # Add visual bindings
  theta_sat = theta_res = theta_fc = alfa = n = A_LOAM_MI = theta_wp = Dichtheid = NULL
  
  # set default parameters for this function
  A_SAND_M50 = 150
  topsoil = 1
  mp_fc = 2.0
  mp_wp = 4.2
  
  # Check input
  arg.length <-max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(mp_fc, any.missing = FALSE)
  checkmate::assert_numeric(mp_wp, any.missing = FALSE)
  
  # Collect data into a table with loam content (< 50 um)
  dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_SAND_M50 = A_SAND_M50,
                   topsoil = topsoil,
                   A_LOAM_MI = (A_CLAY_MI + A_SILT_MI),
                   value = NA_real_  )
  
  # For sandy soils
  dt[A_CLAY_MI < 8, Dichtheid := 1 / (
    -1.984 + 0.01841 * A_SOM_LOI + 0.032 * topsoil + 0.00003576 * A_LOAM_MI ^ 2 +
      67.5 / A_SAND_M50 + 0.424 * log(A_SAND_M50)
  )]
  dt[A_CLAY_MI < 8, theta_res    := 0.01]
  dt[A_CLAY_MI < 8, theta_sat    := -13.6 - 0.01533 * A_LOAM_MI + 0.0000836 * A_LOAM_MI ^ 2 - 0.0973 / A_LOAM_MI +
       0.708 / Dichtheid - 0.00703 * A_SAND_M50 + 225.3 / A_SAND_M50 + 2.614 * log(A_SAND_M50) +
       0.0084 / A_SOM_LOI + 0.02256 * log(A_SOM_LOI) + 0.00718 * Dichtheid * A_LOAM_MI]
  dt[A_CLAY_MI < 8, alfa      := exp(
    146.9 - 0.0832 * A_SOM_LOI - 0.395 * topsoil - 102.1 * Dichtheid +
      22.61 * Dichtheid ^ 2  - 70.6 / Dichtheid - 1.872 / A_LOAM_MI - 0.3931 * log(A_LOAM_MI)
  )]
  dt[A_CLAY_MI < 8, n         := exp(
    1092 + 0.0957 * A_LOAM_MI + 1.336 * A_SAND_M50 - 13229 / A_SAND_M50 - 0.001203 * A_SAND_M50 ^ 2 -
      234.6 * log(A_SAND_M50) - 2.67 / Dichtheid - 0.115 / A_SOM_LOI - 0.4129 * log(A_SOM_LOI)
    - 0.0721 * Dichtheid * A_LOAM_MI
  ) + 1]
  
  # For clay and loamy soils
  dt[A_CLAY_MI >= 8, Dichtheid := 1 / (0.603 + 0.003975 * A_CLAY_MI + 0.00207 * A_SOM_LOI ^ 2 + 0.01781 * log(A_SOM_LOI))]
  dt[A_CLAY_MI >= 8, theta_res  := 0.01]
  dt[A_CLAY_MI >= 8, theta_sat  := 0.8085 - 0.2617 * Dichtheid - 0.038 * topsoil + 0.00001046 * A_CLAY_MI ^ 2 +
       0.01287 * log(A_SOM_LOI) + 0.000789 * A_CLAY_MI * topsoil]
  dt[A_CLAY_MI >= 8, alfa     := exp(
    11 - 2.298 * Dichtheid ^ 2 - 12.41 / Dichtheid + 0.838 * A_SOM_LOI +
      0.343 / A_SOM_LOI + 2.03 * log(A_SOM_LOI) - 1.263 * Dichtheid * A_SOM_LOI
  )]
  dt[A_CLAY_MI >= 8, n         := exp(
    -0.34 + 1.224 / Dichtheid - 0.7952 * log(A_CLAY_MI) - 0.3201 * log(A_SOM_LOI) +
      0.0651 * Dichtheid * A_SOM_LOI
  ) + 1]
  
  # return value
  value <- dt[, theta_sat]
  
  # return value
  return(value)
}

#' Calculate the water holding capacity given the pedotransferfunction of Wösten et al.2001
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @references Wösten, J. H. M., Veerman, G. ., de Groot, W. J., & Stolte, J. (2001). Waterretentie en doorlatendheidskarakteristieken van boven- en ondergronden in Nederland: de Staringreeks. Alterra Rapport, 153, 86. https://edepot.wur.nl/43272
#'
#' @export
sptf_whc15 <- function(A_SOM_LOI,A_CLAY_MI,A_SILT_MI) {
  
  # Add visual bindings
  theta_sat = theta_res = theta_fc = alfa = n = A_LOAM_MI = theta_wp =  Dichtheid = NULL
  
  # set default parameters for this function
  A_SAND_M50 = 150
  topsoil = 1
  mp_fc = 2.0
  mp_wp = 4.2
  
  # Check input
  arg.length <-max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(mp_fc, any.missing = FALSE)
  checkmate::assert_numeric(mp_wp, any.missing = FALSE)
  
  # Collect data into a table, with loam content (< 50 um)
  dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_SAND_M50 = A_SAND_M50,
                   topsoil = topsoil,
                   A_LOAM_MI = (A_CLAY_MI + A_SILT_MI),
                   value = NA_real_
                  )
  
  # For sandy soils
  dt[A_CLAY_MI < 8, Dichtheid := 1 / (
    -7.58 + 0.01791 * A_SOM_LOI + 0.0326 * topsoil - 0.00338 * A_SAND_M50 +
      0.00003937 * A_LOAM_MI ^ 2 +
      157.7 * (1 / A_SAND_M50) + 1.522 * log(A_SAND_M50)
  )]
  dt[A_CLAY_MI < 8, theta_res    := 0.01]
  dt[A_CLAY_MI < 8, theta_sat    := -35.7 - 0.1843 * Dichtheid - 0.03576 *
       A_SAND_M50 + 0.0000261 * A_SAND_M50 ^ 2 - 0.0564 * (1 / A_LOAM_MI) +
       0.008 * (1 / A_SOM_LOI) + 496 * (1 / A_SAND_M50) + 0.02244 * log(A_SOM_LOI) +
       7.56 * log(A_SAND_M50)]
  dt[A_CLAY_MI < 8, alfa      := exp(
    13.66 - 5.91 * Dichtheid - 0.172 * topsoil + 0.003248 * A_SAND_M50 -
      11.89 * (1 / Dichtheid) - 2.121 * (1 /
                                           A_LOAM_MI) - 0.3742 * log(A_LOAM_MI)
  )]
  dt[A_CLAY_MI < 8, n         := exp(
    -1.057 + 0.1003 * A_SOM_LOI + 1.119 * Dichtheid + 0.000764 * A_LOAM_MI ^
      2 -
      0.1397 * (1 / A_SOM_LOI) - 57.2 * (1 /
                                           A_SAND_M50) - 0.557 * log(A_SOM_LOI) - 0.02997 * Dichtheid * A_LOAM_MI
  ) + 1]
  
  # For clay and loamy soils
  dt[A_CLAY_MI >= 8, Dichtheid := 1 / (0.6117 + 0.003601 * A_CLAY_MI + 0.002172 *
                                         A_SOM_LOI ^ 2 + 0.01715 * log(A_SOM_LOI))]
  dt[A_CLAY_MI >= 8, theta_res  := 0.01]
  dt[A_CLAY_MI >= 8, theta_sat  := 0.6311 + 0.003383 * A_CLAY_MI - 0.09699 *
       Dichtheid ^ 2 - 0.00204 * Dichtheid * A_CLAY_MI]
  dt[A_CLAY_MI >= 8, alfa     := exp(
    -19.13 + 0.812 * A_SOM_LOI + 23.4 * Dichtheid - 8.16 * Dichtheid ^ 2 +
      0.423 * (1 / A_SOM_LOI) + 2.388 * log(A_SOM_LOI) -
      1.338 * Dichtheid * A_SOM_LOI
  )]
  dt[A_CLAY_MI >= 8, n         := exp(
    -0.235 + 0.972 * (1 / Dichtheid) - 0.7743 * log(A_CLAY_MI) - 0.3154 * log(A_SOM_LOI) +
      0.0678 * Dichtheid * A_SOM_LOI
  ) + 1]
  
  # select value
  value <- dt[, theta_sat]
  
  # return value
  return(value)
}


#'  the water holding capacity given the pedotransferfunction of Wösten et al.2001 (Table 3), for each soil class
#'
#' @inheritParams sptf_bd0
#'
#' @import data.table
#'
#' @references Wösten, J. H. M., Veerman, G. ., de Groot, W. J., & Stolte, J. (2001). Waterretentie en doorlatendheidskarakteristieken van boven- en ondergronden in Nederland: de Staringreeks. Alterra Rapport, 153, 86. https://edepot.wur.nl/43272
#'
#' @export
sptf_whc16 <- function(A_SOM_LOI,A_CLAY_MI,A_SILT_MI) {
  
  # Add visual bindings
  theta_sat = theta_res = theta_fc = alfa = n = A_LOAM_MI = theta_wp = NULL
  SEL1 = CF1 = CF2 = NULL
  
  # set default parameters for this function
  A_SAND_M50 = 150
  topsoil = 1
  mp_fc = 2.0
  mp_wp = 4.2
  
  # Check input
  arg.length <-max(length(A_SOM_LOI), length(A_CLAY_MI),length(A_SILT_MI))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(mp_fc, any.missing = FALSE)
  checkmate::assert_numeric(mp_wp, any.missing = FALSE)
  
  # load table of parameter values
  bouwsteen_tb <- soilptf::sptf_bouwsteen |> setDT()
  
  # Collect data into a table, with loam content (< 50 um)
  dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   A_SAND_M50 = A_SAND_M50,
                   A_LOAM_MI = (A_CLAY_MI + A_SILT_MI),
                   value = NA_real_)
  
  dt[A_CLAY_MI <= 8, CF1 := 0]
  dt[A_CLAY_MI > 8, CF1 := 1]
  
  dt[A_SOM_LOI > 15, CF2 := 1]
  dt[A_SOM_LOI <= 15, CF2 := 0]
  
  # comment YF: B6 is missing: from the source table the definition is not clear
  dt[, SEL1 := "B20"]
  dt[CF1 == 0 &
       CF2 == 0 & A_LOAM_MI >= 00 &
       A_LOAM_MI < 10 & A_SAND_M50 < 210, SEL1 := "B1"]
  dt[CF1 == 0 &
       CF2 == 0 & A_LOAM_MI >= 10 &
       A_LOAM_MI < 18 & A_SAND_M50 < 210, SEL1 := "B2"]
  dt[CF1 == 0 &
       CF2 == 0 & A_LOAM_MI >= 18 &
       A_LOAM_MI < 33 & A_SAND_M50 < 210, SEL1 := "B3"]
  dt[CF1 == 0 &
       CF2 == 0 & A_LOAM_MI >= 33 &
       A_LOAM_MI < 50 & A_SAND_M50 < 210, SEL1 := "B4"]
  dt[CF1 == 0 &
       CF2 == 0 &
       A_LOAM_MI <= 50 &
       A_SAND_M50 > 210 & A_SAND_M50 <= 2000, SEL1 := "B5"]
  dt[CF1 == 1 & CF2 == 0 & A_CLAY_MI >= 8  &
       A_CLAY_MI < 12, SEL1 := "B7"]
  dt[CF1 == 1 & CF2 == 0 & A_CLAY_MI >= 12 &
       A_CLAY_MI < 18, SEL1 := "B8"]
  dt[CF1 == 1 & CF2 == 0 & A_CLAY_MI >= 18 &
       A_CLAY_MI < 25, SEL1 := "B9"]
  dt[CF1 == 1 & CF2 == 0 & A_CLAY_MI >= 25 &
       A_CLAY_MI < 35, SEL1 := "B10"]
  dt[CF1 == 1 & CF2 == 0 & A_CLAY_MI >= 35 &
       A_CLAY_MI < 50, SEL1 := "B11"]
  dt[CF1 == 1 & CF2 == 0 &
       A_CLAY_MI >= 50 & A_CLAY_MI <= 100, SEL1 := "B12"]
  dt[CF1 == 0 & CF2 == 0 & A_LOAM_MI >= 50 &
       A_LOAM_MI < 85, SEL1 := "B13"]
  dt[CF1 == 0 & CF2 == 0 &
       A_LOAM_MI >= 85 & A_LOAM_MI <= 100, SEL1 := "B14"]
  dt[CF1 == 0 & CF2 == 1 &
       A_SOM_LOI >= 15  & A_SOM_LOI < 25, SEL1 := "B15"]
  dt[CF1 == 0 & CF2 == 1 &
       A_SOM_LOI >= 25  & A_SOM_LOI <= 100, SEL1 := "B16"]
  dt[CF1 == 1 & CF2 == 1 &
       A_SOM_LOI >= 15  & A_SOM_LOI < 35, SEL1 := "B17"]
  dt[CF1 == 1 & CF2 == 1 &
       A_SOM_LOI >= 35  & A_SOM_LOI <= 70, SEL1 := "B18"]
  
  # merge table
  dt <- merge(dt,bouwsteen_tb,by.x = "SEL1",by.y = "bouwsteen",all.x = T,all.y = F)
  setnames(dt,
           c("thres", "thsat", "alpha"),
           c("theta_res", "theta_sat", "alfa"))
  
  # Calculate water holding capacity (cm3/cm3)
  dt[, value :=  theta_sat]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Water retention curve
#'
#' This function compute water content at given pressure head, using Van Genuchten water retention curve
#'
#' @param head (numeric)  suction pressure ([L] or cm of water)
#' @param thetaR (numeric) residual water content (cm3/cm3)
#' @param thetaS (numeric) saturated water content (cm3/cm3)
#' @param alfa (numeric)  related to the inverse of the air entry suction, alfa > 0 (1/cm)
#' @param n (numeric)  a measure of the pore-size distribution, n>1, dimensionless
#'
#' @return theta (numeric) water content (cm3/cm3)
#'
#' @export
pF_curve <- function(head, thetaR, thetaS, alfa, n) {
  theta <- thetaR + (thetaS - thetaR) / (1 + abs(alfa * head) ^ n) ^ (1 - 1 / n)
  
  return(theta)
}



#' Estimate soil pore space (= theta_sat) (cm3/cm3)
#'
#' @inheritParams sptf_bd0
#' @param method (CHAR) The method to estimate soil density. "Heinen" or "average"
#'
#' @export
calc_soil_porosity <- function(D_BDS,
                               A_SAND_MI = NULL,
                               A_CLAY_MI = NULL,
                               A_SILT_MI = NULL,
                               A_SOM_LOI = NULL,
                               method = "average") {
  # check inputs
  arg.length <- max(length(D_BDS))
  checkmate::assert_subset(method, choices = c('Heinen', 'average'))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length,null.ok = TRUE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length,null.ok = TRUE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, len = arg.length,null.ok = TRUE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, len = arg.length,null.ok = TRUE)
  checkmate::assert_numeric(D_BDS, lower = 0, upper = 2000, len = arg.length)
  
    if (method == "Heinen") {
      # Calulate soil density (g/cm3), according to Heinen 2006
      # Heinen, M., (2006) Application of a widely used denitrification model to Dutch datasets. Geoderma 133, 464e473.
      
      # Convert sand/clay/silt fraction to % in the whole soil (instead of % in mineral soil)
      # (The original ref should be checked to see if this conversion is really necessary)
      A_SAND_MI <- A_SAND_MI / 100 * (100 - A_SOM_LOI)
      A_CLAY_MI <- A_CLAY_MI / 100 * (100 - A_SOM_LOI)
      A_SILT_MI <- A_SILT_MI / 100 * (100 - A_SOM_LOI)
      
      soildens <-
        1 / (A_SOM_LOI / 147 +  A_CLAY_MI / 275 +  (A_SILT_MI + A_SAND_MI) / 266)
      
    }
    
    if (method == "average") {
      # Average soil density (cm3), Nasta et al. 2021
      soildens <-
        2.65 # average particle density (cm3), Nasta et al. 2021
    }
    
    
    # calculate soil pore space (= theta_sat) (cm3/cm3)
    theta_sat <- 1 - D_BDS / soildens
    
    return(theta_sat)
  }