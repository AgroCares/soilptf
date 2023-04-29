# Functions for PMN (potentially mineralizable Nitrogen)

#' Calculate the PMN given the pedo transfer function of Narteh and Sahrawat (1997) 
#' 
#' @inheritParams sptf_bd0
#'
#' @import data.table
#' 
#' @references Narteh and Sahrawat (1997) Potentially Mineralizable Nitrogen in West African Lowland Rice Soils. Geoderma, 76, 145-154
#'
#' @export
sptf_pmn1 <- function(A_C_OF, A_CLAY_MI, A_N_RT, A_PH_CC) {
  
  # add visal bindings
  A_CN_FR = A_FE_DTPA = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI), length(A_N_RT), length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000,len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, len = arg.length)
  checkmate::assert_numeric(A_N_RT, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_N_RT = A_N_RT,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # compute C:N ratio and replace by mean value when data is missing
  dt[, A_CN_FR := A_C_OF / (A_N_RT/1000)]
  dt[is.na(A_CN_FR), A_CN_FR := 13.55]
  
  # adjust Ntotal from CN ratio
  dt[is.na(A_N_RT), A_N_RT := A_C_OF * 1000/ A_CN_FR]
  
  # Convert to the units of the predictors in the literature:
  # organic C (g/kg), DTPA Fe (mg/kg), clay (g/kg), total N (mg/kg)
  dt[, A_CLAY_MI := A_CLAY_MI * 10]
  
  # DTPA Fe (mg/kg) data is not available, but this predictor is not significantly correlated to the response varialle.
  # Therefore, we will use the median value of DTPA Fe (N=15)
  dt[, A_FE_DTPA := median(c(339, 63, 178, 178, 112, 264, 295, 340, 486, 485, 415, 236, 284, 24, 166))]
  
  # idem for soil pH
  dt[is.na(A_PH_CC), A_PH_CC := median(c(4.3,5.2,7.7,4.9,5.4,5.1,6.1,5.6,5.4,5.6,5.5,5.3,6.1,6.3,5))]
  
  # Calculate PMN (mg/kg), 30 dC for 14 days
  
  # Note YF 20221104: the calculated PMN does not match the measured PMN, ca. x10 too large!!
  # It seems that the unit of the explanatory variables are wrong (e.g. A_C_OF is %?)
  # dt[, value := -14.33 + 67.68 * A_C_OF + 0.8 * A_FE_DTPA +
  #      24.64 * A_PH_CC - 0.15 * A_CLAY_MI - 7.61 * A_CN_FR - 0.07 * A_N_RT]
  
  # Regression coefficient calculated by YF,
  # using the data (N=15) in table 2
  # see: dev/narteh1997_regression.R
  dt[, value := -19.28679 + 6.07373 * A_C_OF + 0.07522 * A_FE_DTPA +
       23.85534 * A_PH_CC -0.02653 * A_CLAY_MI -6.71265 * A_CN_FR -0.06066 * A_N_RT]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the PMN given the pedo transfer function of Zou et al (2018) 
#' 
#' @inheritParams sptf_bd0
#' @param t (numeric) Length of incubation period (days). The empirical data is ~35 days, thus it should not exceed this value too much.
#'
#' @import data.table
#' 
#' @references Zou et al. (2018) Pedo-Transfer Functions to Estimate Kinetic Parameters for Anaerobic Soil Nitrogen Mineralization. Open Journal of Soil Science, 8, 75-86. doi: 10.4236/ojss.2018.82006.
#'
#' @export
sptf_pmn2 <- function(A_C_OF, A_SILT_MI, A_N_RT, A_PH_CC, t = 7) {
  
  # add visual bindings
  f = k = A_CN_FR = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_SILT_MI), length(A_N_RT), length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 50000, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 13, len = arg.length)
  checkmate::assert_numeric(t, lower = 0, upper = 35)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF, 
                   A_SILT_MI = A_SILT_MI,
                   A_N_RT = A_N_RT,
                   A_PH_CC = A_PH_CC, # predictor pH in the literature is pH_H2O
                   t = t,
                   value = NA_real_)
  
  # calculate CN ratio
  dt[, A_CN_FR := A_C_OF / (A_N_RT * 0.001)]
  dt[is.na(A_CN_FR), A_CN_FR := 7.49]
  dt[is.na(A_N_RT), A_N_RT := A_C_OF * A_CN_FR * 1000]
  
  # add missing value for pH
  dt[is.na(A_PH_CC), A_PH_CC := 4.93]
  
  # convert unit to match the predictors' unit
  dt[, A_N_RT :=  A_N_RT * 0.1 * 0.001] # mg/kg to %
  dt[, A_C_OF := A_C_OF * 0.1] # g/kg to %
  
  # mineralization rate constants for soil resistant N (d-1)
  kr <- 1.5 * 10^-6
  
  # Calculate parameters of 1st order kinetics model
  # fraction of soil active N pool (range 0-1)
  dt[, f := 0.029 + 0.021 * A_C_OF + 0.00076 * A_SILT_MI - 0.001 * A_PH_CC^2]
  # mineralization rate constants for soil active N (d-1)
  # THe observed k value is ca. 0.15, while it can  be ~0.4 (very high!!!) for dutch agricultural soils.
  # In the dataset of Zou, highest TN is 0.29% (=2900 mgN/kg),
  # while in Dutch dataset, TN ranges up to ca. 15000 mg/kg. (median 2355 mgN/kg)
  dt[, k := 0.15 + 0.25 * A_N_RT - 0.44 / A_PH_CC]
  
  # Calculate Nmin (mg/kg) for t days at 35 dC
  dt[, value := A_N_RT * 10^4 * 
       (f * (1 - exp(-k * t)) + (1-f) * (1 - exp(-kr * t)))]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the PMN given the pedo transfer function of Ros & Fujita 2020 for Dutch soils
#' 
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Ros & Fujita (2020) FACTSHEET actools: Potential Mineralizable N Index v.0.4.
#'
#' @export
sptf_pmn3 <- function(A_C_OF,A_N_RT, A_CLAY_MI) {

  # add visual bindings
  A_CN_FR = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_N_RT), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 50000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)

  # Collect data into a table
  dt <- data.table(id = 1:arg.length,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_C_OF * 1000 / A_N_RT,
                   A_CLAY_MI = A_CLAY_MI, 
                   value = NA_real_)
  
  # estimate A_N_RT from missing data assuming mean CN ratio (derived from historical datasets)
  dt[is.na(A_CN_FR) & A_CLAY_MI <= 10, A_CN_FR := 16.8]
  dt[is.na(A_CN_FR) & A_CLAY_MI > 10, A_CN_FR := 12.2]
  dt[is.na(A_N_RT), A_N_RT := A_C_OF * A_CN_FR * 1000]
  
  # Calculate Nmin (mg/kg) for 7 days at 40 dC
  dt[, value := exp(-3.440931 + 1.1012449 * log(A_N_RT) + -0.055858 * log(A_CLAY_MI))]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the PMN given the pedo transfer function of Ros & Fujita 2020 for global soils
#' 
#' @inheritParams sptf_bd0
#' 
#' @import data.table
#' 
#' @references Ros & Fujita (2020) FACTSHEET actools: Potential Mineralizable N Index v.0.4.
#'
#' @export
sptf_pmn4 <- function(A_C_OF,A_N_RT, A_CLAY_MI) {
  
  # add visual bindings
  A_CN_FR = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF),length(A_N_RT), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 50000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # Collect data into a table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_C_OF * 1000 / A_N_RT,
                   A_CLAY_MI = A_CLAY_MI, 
                   value = NA_real_)
  
  # estimate A_N_RT from missing data assuming mean CN ratio (derived from historical datasets)
  # mean CN ratio from Cleveland and Liptzin (2007) and Lal (2008)
  dt[is.na(A_CN_FR), A_CN_FR := 12.3]
  dt[is.na(A_N_RT), A_N_RT := A_C_OF * A_CN_FR * 1000]
  
  # Calculate Nmin (mg/kg) for 7 days at 40 dC
  dt[, value := exp(-2.333 + 0.897 * log(A_N_RT) + -0.069 * log(A_CLAY_MI))]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the PMN given the pedo transfer function of Rasiah (1995), based on dataset of Stanford and Smith (1972)
#' 
#' @inheritParams sptf_bd0
#' @param t (numeric) Length of incubation period (days). 
#' 
#' @import data.table
#' 
#' @references Rasiah (1995). Comparison of pedotransfer functions to predict nitrogen-mineralization parameters of one- and two-pool models. Communications in Soil Science and Plant Analysis 26, 1873–1884.
#' @references Stanford and Smith (1972) Nitrogen mineralization potentials of soils. Soil Sci. Soc. Amer. J. 36:465-472.
#' 
#' @export
sptf_pmn5 <- function(A_N_RT, A_C_OF, A_CEC_CO, t = 7) {
  
  # add visual bindings
  TN = OC = CEC = wk = N0 = k0 = NULL
  
  # Check input
  arg.length <- max(length(A_N_RT), length(A_C_OF), length(A_CEC_CO))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 50000, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 0, upper = 800, len = arg.length)
  checkmate::assert_numeric(t, lower = 0, upper = 100)
  
  # Collect data into a table
  dt <- data.table(A_N_RT = A_N_RT,
                   A_C_OF = A_C_OF, 
                   A_CEC_CO = A_CEC_CO, 
                   value = NA_real_)
  
  # Calculate total N as %
  dt[, TN :=  A_N_RT * 0.1 * 10^-3] # mg/kg to %
  # Calculate organic C as % 
  dt[, OC := A_C_OF * 0.1]  # g/kg to %
  # calculate CEC as mol/kg
  #dt[, CEC := A_CEC_CO * 10^-3] # mmol/kg to mol/kg
  dt[, CEC := A_CEC_CO * 0.1] # mmol/kg to cmol/kg
  # convert t (day) to wk (week)
  dt[, wk := t / 7]
  
  # Calculate Nmin (mg/kg) based on 1-pool model (dataset: Stanford and Smith 1972)
  # mineralizable N
  # This becomes too large for Dutch soil??
  dt[, N0 := 1895.68 *  TN + 5.59 * CEC - 52.96 * CEC * TN - 16.33]
  dt[N0 < 0, N0 := 0]
  # rate constant
  # this becomes often negative value for Dutch soil?! ---> the range of CEC is too low (Dutch median: 0.1 mol/kg). 
  # it's probably cmol/kg, instead of mol/kg (as described in table caption)
  dt[, k0 := 0.0713 - 0.00065 *  CEC - 0.0358 * OC + 0.236 * TN + 0.0023 * OC * CEC - 0.0196 * CEC * TN]
  dt[k0 < 0, k0 := 0] 
  
  dt[, value := N0 * (1 - exp(-k0 * wk))]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the PMN given the pedo transfer function of Rasiah (1995), based on Cabrera & Kissel (1988)
#' 
#' @inheritParams sptf_bd0
#' @param t (numeric) Length of incubation period (days). 
#' @param RES (numeric) whether residue is not removed (1) or removed (0)
#' 
#' @import data.table
#' 
#' @references Rasiah (1995). Comparison of pedotransfer functions to predict nitrogen-mineralization parameters of one- and two-pool models. Communications in Soil Science and Plant Analysis 26, 1873–1884.
#'
#' @export
sptf_pmn6 <- function(A_N_RT, A_C_OF, A_CLAY_MI, A_SILT_MI, A_PH_CC, t = 7, RES = 1) {
  
  # add visual bindings
  TN=OC=wk=N1=Nr=k1=kr=NULL
  
  # Check input
  arg.length <- max(length(A_N_RT), length(A_C_OF), length(A_CLAY_MI), length(A_SILT_MI), length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 50000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 13, len = arg.length)
  checkmate::assert_numeric(t, lower = 0, upper = 100)
  checkmate::assert_numeric(RES,lower = 0, upper = 1)
  
  # Collect data into a table
  dt <- data.table(A_N_RT = A_N_RT,
                   A_C_OF = A_C_OF, 
                   A_CLAY_MI = A_CLAY_MI, 
                   A_SILT_MI = A_SILT_MI,
                   A_PH_CC = A_PH_CC,
                   t = t,
                   RES = RES,
                   value = NA_real_)
  
  # Calculate total N as %
  dt[, TN :=  A_N_RT * 0.1 * 10^-3] # mg/kg to %
  # Calculate organic C as % 
  dt[, OC := A_C_OF * 0.1]  # g/kg to %
  # convert t (day) to wk (week)
  dt[, wk := t / 7]
  
  # Calculate Nmin (mg/kg) based on 2-pool model
  # The N1 and k1 are small for dutch soil, resulting in very low PMN.
  # the original dataset is poor in total N (0.05 - 0.13% -> 500-1300 mg/kg), 
  # compared to dutch soil (median 2533 mgN/kg)
  dt[, N1 := 206.98 * TN + 37.73 * RES + 14.15 * A_PH_CC - 0.232 * A_CLAY_MI - 114.03]
  dt[N1 < 0, N1 := 0] 
  dt[, Nr := 5.345 + 19.46 * TN]
  dt[Nr < 0, Nr := 0] 
  dt[, k1 := 0.00477 + 0.0536 * TN + 0.0007 * A_CLAY_MI - 0.00028 * A_SILT_MI - 0.00119 * A_CLAY_MI * TN]
  dt[k1 < 0, k1 := 0] 
  dt[, kr := 0.6118 + 0.00184 * A_CLAY_MI - 0.00176 * A_SILT_MI + 1.729 * TN - 0.00392 * A_CLAY_MI * TN] # this values are too large!?
  dt[kr < 0, kr := 0] 
  dt[, value := N1 * (1 - exp(-k1 * wk)) + Nr * (1 - exp(-kr * wk))]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the PMN given the pedo transfer function of Haer & Benbi (2003)
#' 
#' @inheritParams sptf_bd0
#' @param t (numeric) Length of incubation period (days). 
#' 
#' @import data.table
#' 
#' @references Haer and Benbi (2003) Modeling Nitrogen Mineralization Kinetics in Arable Soils of Semiarid India. Arid Land Research Management, 17, 153-168
#'
#' @export
sptf_pmn7 <- function(A_C_OF, A_CLAY_MI, t = 7) {
  
  # add visual bindings
  k = N0 = NULL
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(t, lower = 0, upper = 100)

  # Collect data into a table (set SOC to %)
  dt <- data.table(A_C_OF = A_C_OF * 0.1, 
                   A_CLAY_MI = A_CLAY_MI, 
                   t = t,
                   value = NA_real_)
  
    # Calculate Nmin (mg/kg) based on 1-pool model
  dt[, k := 0.04] # there was no PTF for k, thus the average value of 15 soils is used 
  dt[, N0 := -5.34 + 128.16 * A_C_OF - 1.3 * A_CLAY_MI * A_C_OF]
  dt[, value := N0 * (1 - exp(-k * t))]
  
  # return value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the PMN given the pedo transfer function of Rasiah (1995), based on dataset of Herlihy (1979)
#' 
#' @inheritParams sptf_bd0
#' @param t (numeric) Length of incubation period (days). 
#' 
#' @import data.table
#' 
#' @references Herlihy (1979) Nitrogen mineralization in soils of varying texture, moisture and organic matter. Plant and Soil. 53:255-267.
#'
#' @export
sptf_pmn8 <- function(A_N_RT, A_CEC_CO, A_SILT_MI, t = 7) {
  
  # add visual bindings
  TN = CEC = wk = N0 = k0 = NULL
  
  # Check input
  arg.length <- max(length(A_N_RT), length(A_CEC_CO), length(A_SILT_MI))
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 50000, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 0, upper = 800, len = arg.length)
  checkmate::assert_numeric(t, lower = 0, upper = 100)
  
  # Collect data into a table
  dt <- data.table(A_N_RT = A_N_RT,
                   A_CEC_CO = A_CEC_CO, 
                   A_SILT_MI = A_SILT_MI,
                   value = NA_real_)
  
  # Calculate total N as %
  dt[, TN :=  A_N_RT * 0.1 * 10^-3] # mg/kg to %
  # calculate CEC as mol/kg
  dt[, CEC := A_CEC_CO * 10^-3] # mmol/kg to mol/kg
  #dt[, CEC := A_CEC_CO * 0.1] # mmol/kg to cmol/kg
  # convert t (day) to wk (week)
  dt[, wk := t / 7]
  
  # Calculate Nmin (mg/kg) based on 1-pool model 
  # mineralizable N (mg/kg)
  dt[, N0 := 12.01 - 34.55 * CEC + 5.22 * A_SILT_MI + 2272 * TN]
  # rate constant (1/week)
  # The regression model was NA. Average value of 6 soils are used.
  dt[, k0 := 0.04533333]

  dt[, value := N0 * (1 - exp(-k0 * wk))]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}

#' Calculate the PMN given the pedo transfer function of Rasiah (1995), based on dataset of Campbel et al (1984)
#' 
#' @inheritParams sptf_bd0
#' @param CULT (integer) whether cultivated (0) or virgin soil (0)
#' @param t (numeric) Length of incubation period (days). 
#' 
#' @import data.table
#' 
#' @references Campbel et al (1984)  Mineralization rate constants and their use for estimating nitrogen mineralization in some Canadian prairie soils. Canadian J. Soil Sci. 64:333-343.
#'
#' @export
sptf_pmn9 <- function(A_N_RT, CULT = 1, t = 7) {
  
  # add visual bindings
  TN = wk = N0 = k0 = NULL
  
  # Check input
  arg.length <- max(length(A_N_RT), length(CULT))
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 50000, len = arg.length)
  checkmate::assert_numeric(t, lower = 0, upper = 100)
  
  # Collect data into a table
  dt <- data.table(A_N_RT = A_N_RT,
                   CULT = CULT, 
                   value = NA_real_)
  
  # Calculate total N as %
  dt[, TN :=  A_N_RT * 0.1 * 10^-3] # mg/kg to %
  # convert t (day) to wk (week)
  dt[, wk := t / 7]
  
  # Calculate Nmin (mg/kg) based on 1-pool model 
  # mineralizable N (mg/kg)
  dt[, N0 := 177.81 + 290.61 * TN - 119.93 * CULT]
  # rate constant (1/week)
  # The regression model was NA. Average value of 33 soils (for 35 dC) are used. Column 6 of Table 1
  #dt[, k0 := 0.109091]
  # average values, separately calculated for cultivated and virgin
  dt[CULT == 1, k0 := 0.120177778]
  dt[CULT == 0, k0 := 0.095786667]
  
  
  dt[, value := N0 * (1 - exp(-k0 * wk))]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the PMN given the pedo transfer function of Rasiah (1995), based on dataset of Simard & N'dayegamiye (1993)
#' 
#' @inheritParams sptf_bd0
#' @param t (numeric) Length of incubation period (days). 
#' 
#' @import data.table
#' 
#' @references Simard & N'dayegamiye (1993) Nitrogen -mineralization potential of meadow soils. Can. J. Soil Sci. 73:27-38
#'
#' @export
sptf_pmn10 <- function(A_N_RT, t = 7) {
  
  # add visual bindings
  TN = wk = N0 = k0 = NULL
  
  # Check input
  arg.length <- max(length(A_N_RT))
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 50000, len = arg.length)
  checkmate::assert_numeric(t, lower = 0, upper = 100)
  
  # Collect data into a table
  dt <- data.table(A_N_RT = A_N_RT,
                   value = NA_real_)
  
  # Calculate total N as %
  dt[, TN :=  A_N_RT * 0.1 * 10^-3] # mg/kg to %
  # convert t (day) to wk (week)
  dt[, wk := t / 7]
  
  # Calculate Nmin (mg/kg) based on 1-pool model 
  # mineralizable N (mg/kg)
  dt[, N0 := 65.73 + 224.4 * TN ]
  # rate constant (1/week)
  # The regression model was NA. Average value of 20 soils. Table 2
  dt[, k0 := 0.041645]
  
  
  dt[, value := N0 * (1 - exp(-k0 * wk))]
  
  # select value
  value <- dt[, value]
  
  # return value
  return(value)
  
}


#' Calculate the Potentially Mineralizable N (PMN) in mg/kg
#'
#' This function calculates the PMN content for agricultural soils in the Netherlands. PMN refers to anearobic mineralizable N pool, 40 degrees for 7 days.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Ros (2011) Predicting soil Nitrogen Supply.
#'
#' @export
sptf_pmn11 <- function(A_C_OF, A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # experimental data from thesis Ros (2011), n = 100, R2 = 0.44, 0-30 cm soil
  dt[, value := exp(0.61003 + 0.98365 * log(A_C_OF) -0.05396 * log(A_CLAY_MI))]
  
  # experimental data from thesis Ros (2011), n = 100, R2 = 0.44, 0-30 cm soil
  dt[is.na(A_CLAY_MI), value := exp(0.4691 + 1.0077 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the Potentially Mineralizable N (PMN) in mg/kg
#'
#' This function calculates the PMN content for agricultural soils in the Netherlands. PMN refers to anearobic mineralizable N pool, 40 degrees for 7 days.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Ros (2011) Predicting soil Nitrogen Supply. Unpublished data from pot experiment and incubation experiment.
#'
#' @export
sptf_pmn12 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF, value = NA_real_)
  
  # experimental data from thesis Ros (2011), n = 47, R2 = 0.52, 0-30 cm soil
  dt[, value := exp(1.19921 + 0.38323 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}


#' Calculate the Potentially Mineralizable N (PMN) in mg/kg
#'
#' This function calculates the PMN content for agricultural soils in the Netherlands. PMN refers to anearobic mineralizable N pool, 40 degrees for 7 days.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Van Eekeren & Bokhorst (2009) Beoordeling bodemkwaliteit zandgrond. Een inventarisatie van bodemindicatoren voor de veehouderij. ZvZ-rapport 7, 61 pp.
#'
#' @export
sptf_pmn13 <- function(A_C_OF) {
  
  # Check input
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   value = NA_real_)
  
  # experimental data from Van Eekeren & Bokhorst (2009), n = 20, R2 = 0.37, 0-30 cm soil
  dt[,value := exp(2.6409 + 0.5214 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the Potentially Mineralizable N (PMN) in mg/kg
#'
#' This function calculates the PMN content for agricultural soils in the Netherlands. PMN refers to anearobic mineralizable N pool, 40 degrees for 7 days.
#' Experimental data from 2019.
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
sptf_pmn14 <- function(A_C_OF,A_P_AL,A_PH_CC) {
  
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
  
  # experimental data from Van Balen et al. (2015), n = 51, R2 = 0.683, 0-30 cm soil
  dt[,value := exp(-15.4676 + 2.0614 * log(A_C_OF) + 0.6355 * log(A_P_AL) + 5.4224 * log(A_PH_CC))]
  
  # experimental data from Van Balen et al. (2015), n = 51, R2 = 0.54, 0-30 cm soil
  dt[is.na(A_PH_CC) & !is.na(A_P_AL),value := exp(0.02961 + 0.77965 * log(A_C_OF) + 0.2465 * log(A_P_AL))]
  
  # experimental data from Van Balen et al. (2015), n = 51, R2 = 0.53, 0-30 cm soil
  dt[is.na(A_PH_CC) & is.na(A_P_AL),value := exp(1.12183 + 0.72807 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the Potentially Mineralizable N (PMN) in mg/kg
#'
#' This function calculates the PMN content for agricultural soils in the Netherlands. PMN refers to anearobic mineralizable N pool, 40 degrees for 7 days.
#' Data from various years.
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
sptf_pmn15 <- function(A_C_OF,A_P_AL,A_CLAY_MI) {
  
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
  
  # experimental data from Van Balen et al. (2015), n = 120, R2 = 0.64, 0-30 cm soil
  dt[,value := exp(-1.17455 + 0.97101 * log(A_C_OF) + 0.26496 * log(A_P_AL) +0.37920 * log(A_CLAY_MI))]
  
  # experimental data from Van Balen et al. (2015), n = 120, R2 = 0.29, 0-30 cm soil
  dt[is.na(A_CLAY_MI) & !is.na(A_P_AL),value := exp(3.42059 + 0.40691 * log(A_C_OF) -0.28764 * log(A_P_AL))]
  
  # experimental data from Van Balen et al. (2015), n = 120, R2 = 0.62, 0-30 cm soil
  dt[!is.na(A_CLAY_MI) & is.na(A_P_AL),value := exp(0.23228 + 0.87849 * log(A_C_OF) + 0.32279 * log(A_CLAY_MI))]
  
  # experimental data from Van Balen et al. (2015), n = 120, R2 = 0.25, 0-30 cm soil
  dt[is.na(A_P_AL) & is.na(A_CLAY_MI),value := exp(2.28216 + 0.42048 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}


#' Calculate the Potentially Mineralizable N (PMN) in mg/kg
#'
#' This function calculates the PMN content for agricultural soils in the Netherlands. PMN refers to anearobic mineralizable N pool, 40 degrees for 7 days.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#'
#' @import data.table
#' 
#' @references Hanegraaf et al. (2009). Verkenning van bodemsensoren voor de landbouw. Interreg project Bodembreed, NMI project 1382, 93 pp.
#'
#' @export
sptf_pmn16 <- function(A_C_OF) {
  
  # Check input
  arg.length <- max(length(A_C_OF))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   value = NA_real_)
  
  # experimental data from Hanegraaf et al. (2009), n = 20, R2 = 0.15, 0-30 cm soil
  dt[,value := exp(3.4881 + 0.3987 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the Potentially Mineralizable N (PMN) in mg/kg
#'
#' This function calculates the PMN content for agricultural soils in the Netherlands. PMN refers to anearobic mineralizable N pool, 40 degrees for 7 days.
#' Data from various years.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#'
#' @import data.table
#' 
#' @references Hanegraaf et al. (2012) Rekenen met de kwaliteit van organische stof: kengetallen en rekenregels. NMI project 1314.N.09,
#'
#' @export
sptf_pmn17 <- function(A_C_OF,A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # experimental data from Hanegraaf et al. (2012), n = 104, R2 = 0.31, 0-30 cm soil
  dt[,value := exp(2.8779 + 0.632 * log(A_C_OF) -0.7644 * log(A_PH_CC))]
  
  # experimental data from Hanegraaf et al. (2012), n = 104, R2 = 0.30, 0-30 cm soil
  dt[is.na(A_PH_CC),value := exp(1.4894 + 0.6646 * log(A_C_OF))]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}

#' Calculate the Potentially Mineralizable N (PMN) in mg/kg
#'
#' This function calculates the PMN content for agricultural soils in the Netherlands. PMN refers to anearobic mineralizable N pool, 40 degrees for 7 days.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_PH_CC (numeric) The acidity of the soil, pH in CaCl2 (-)
#'
#' @import data.table
#' 
#' @references Hanegraaf et al. (2009). De afbraaksnelheid van organische stof in Drenthe. NMI project 972.N.09, 93 pp.
#'
#' @export
sptf_pmn18 <- function(A_C_OF,A_PH_CC) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_PH_CC))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 12, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)
  
  # experimental data from Hanegraaf et al. (2009), n = 149, R2 = 0.21, 0-30 cm soil
  dt[,value := exp(-0.01683 + 0.5636 * log(A_C_OF) +0.30884 * log(A_PH_CC))]
  
  # experimental data from Hanegraaf et al. (2009), n = 170, R2 = 0.11, 0-30 cm soil
  dt[is.na(A_PH_CC),value := exp(1.18616 + 0.41434 * log(A_C_OF))]
  
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
sptf_pmn19 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # experimental data from Van Eekeren & Bokhorst (2009), n = 20, R2 = 0.20, 0-30 cm soil
  dt[,value := exp(1.56777 + 0.42086 * log(A_C_OF) +0.02016 * log(A_CLAY_MI))]
  
  # experimental data from Van Eekeren & Bokhorst (2009), n = 20, R2 = 0.25, 0-30 cm soil
  dt[is.na(A_CLAY_MI),value := exp(1.5447 + 0.4351 * log(A_C_OF))]
  
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
sptf_pmn20 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # experimental data from Fujita & Ros (2023), n = 820, R2 = 0.31, 0-30 cm soil
  dt[,value := exp(0.53598 + 0.90292 * log(A_C_OF) +0.33166 * log(A_CLAY_MI) -0.08935 * log(A_CLAY_MI)*log(A_C_OF))]
  
  # experimental data from Fujita & Ros (2023), n = 820, R2 = 0.08, 0-30 cm soil
  dt[is.na(A_CLAY_MI),value := exp(0.10723 + 1.50209 * log(A_C_OF) -0.14623 * log(A_C_OF)^2)]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}


#' Empirical relationship to convert Nmin to 7 days (mgN/kg/7 days)
#' 
#' @param Nmin (numeric) anaerobic N mineralization rate for t-day incubation (mgN/kg)
#' @param t (numeric) incubation period (days)
correct_incubationperiod <- function(Nmin, t){
  
  b0 <- 2.495
  b1 <- 0.5131
  
  cor_factor <- exp(b0 + b1 * log(7)) / exp(b0 + b1 * log(t))
  Nmin_7d <- Nmin * cor_factor
  
  return(Nmin_7d)
  
}



