# function for hydraulic properties

#' This function calculates different kind of Water Retention Indices given the continuous pedotransferfunctions of Wosten et al. (1999)
#' These include : 'wilting point','field capacity','water holding capacity','plant available water' and 'Ksat'
#' 
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_DEPTH (numeric) The depth of the sampled soil layer (m)
#'
#' @references Wösten, J.H.M , Lilly, A., Nemes, A., Le Bas, C. (1999) Development and use of a database of hydraulic properties of European soils. Geoderma 90 (3-4): 169-185.
#'
#' @import data.table  
#'
#' @export
sptf_vg1 <- function(A_CLAY_MI,A_SILT_MI,A_SOM_LOI,A_DEPTH) {
  
  # add visual bindings
  A_DENSITY = ThetaR = ThetaS = alfa = n = ksat = id = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SILT_MI), length(A_SOM_LOI), length(B_DEPTH))
  check_numeric('A_CLAY_MI', A_CLAY_MI, FALSE, arg.length)
  check_numeric('A_SILT_MI', A_SILT_MI, FALSE, arg.length)
  check_numeric('A_SOM_LOI', A_SOM_LOI, FALSE, arg.length)
  check_numeric('A_DEPTH', A_DEPTH, FALSE, arg.length)
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   TOPSOIL = fifelse(A_DEPTH <= 50, 1,0),
                   A_DENSITY = NA_real_,
                   ThetaR = NA_real_,
                   ThetaS = NA_real_, 
                   alfa = NA_real_, 
                   ksat = NA_real_,
                   n = NA_real_)
  
  # Estimate of bulk density (source: Handboek Bodem en Bemesting )
  dt[, A_DENSITY := sptf_bd1(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]

  # Continue pedotransferfunctie Wosten 1999 (in PFT manual), see Wosten 2001 (based on HYPRES dataset)
  dt[, ThetaR := 0.01]
  dt[, ThetaS := 0.7919+0.001691*A_CLAY_MI-0.29619*A_DENSITY-0.000001491*A_SILT_MI^2+0.0000821*A_SOM_LOI^2+
                 0.02427/A_CLAY_MI+0.01113/A_SILT_MI+0.01472*log(A_SILT_MI)-0.0000733*A_SOM_LOI*A_CLAY_MI-0.000619*A_DENSITY*A_CLAY_MI-
                 0.001183*A_DENSITY*A_SOM_LOI-0.0001664*TOPSOIL*A_SILT_MI]
  dt[,  alfa := exp(-14.96+0.03135*A_CLAY_MI+0.0351*A_SILT_MI+0.646*A_SOM_LOI+15.29*A_DENSITY-0.192*TOPSOIL-
                     4.671*A_DENSITY^2-0.000781*A_CLAY_MI^2-0.00687*A_SOM_LOI^2+0.0449/A_SOM_LOI+0.0663*log(A_SILT_MI)+
                     0.1482*log(A_SOM_LOI)-0.04546*A_DENSITY*A_SILT_MI-0.4852*A_DENSITY*A_SOM_LOI+0.00673*TOPSOIL*A_CLAY_MI)]
  dt[, n := 1 + exp(-25.23-0.02195*A_CLAY_MI+0.0074*A_SILT_MI-0.1940*A_SOM_LOI+45.5*A_DENSITY-7.24*A_DENSITY^2+
                    0.0003658*A_CLAY_MI^2+0.002885*A_SOM_LOI^2-12.81/A_DENSITY-0.1524/A_SILT_MI-0.01958/A_SOM_LOI-
                    0.2876*log(A_SILT_MI)-0.0709*log(A_SOM_LOI)-44.6*log(A_DENSITY)-0.02264*A_DENSITY*A_CLAY_MI+
                    0.0896*A_DENSITY*A_SOM_LOI+0.00718*TOPSOIL*A_CLAY_MI)]
  dt[, ksat := 7.755 + 0.0352 * A_SILT_MI + 0.93 * TOPSOIL - 0.967 * A_DENSITY^2 - 0.000484 * A_CLAY_MI^2 -
               0.000322 * A_SILT_MI^2 + 0.001 / A_SILT_MI - 0.0748 / A_SOM_LOI - 0.643 * log(A_SILT_MI) -
               0.01398 * A_DENSITY * A_CLAY_MI - 0.1673 * A_DENSITY * A_SOM_LOI + 0.2986 * TOPSOIL * A_CLAY_MI -
               0.03305 * TOPSOIL * A_SILT_MI]
  
  # Copied from the old version of waterretention.R. THe source information need to be verified.
  dt[A_SOM_LOI>25 & ksat < 0, ksat := 5] 
  
  # order dt
  setorder(dt, id)
  
  # select the output of the van Genucjten function
  value <- dt[, list(ThetaR, ThetaS, alfa, n, ksat)]
  
  # return
  return(value)
  
}


#' Estimate water retention curve parameters based on Wosten 2001
#'
#' This function estimates water retention curve parameters using Pedo transfer function of Wosten (2001)
#' 
#' @param A_CLAY_MI (numeric) The clay (<2um) content of the soil (\%) 
#' @param A_SILT_MI (numeric) The silt content of the soil (\%) 
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param A_SAND_M50 (numeric) the size of sand fraction (um)
#' @param A_DEPTH (numeric) The depth of the sampled soil layer (m)
#' 
#' @references Wösten, J. H. M., Veerman, G. ., de Groot, W. J., & Stolte, J. (2001). Waterretentie en doorlatendheidskarakteristieken van boven- en ondergronden in Nederland: de Staringreeks. Alterra Rapport, 153, 86. https://doi.org/153
#'
#' @export 
sptf_vg1 <- function(A_CLAY_MI, A_SILT_MI, A_SOM_LOI, A_SAND_M50, A_DEPTH){
  
  # add visual bindings
  A_DENSITY = ThetaR = ThetaS = Ksat = alfa = l = n =  id = NULL
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_SILT_MI), length(A_SOM_LOI), length(A_SAND_M50), length(B_DEPTH))
  check_numeric('A_CLAY_MI', A_CLAY_MI, FALSE, arg.length)
  check_numeric('A_SILT_MI', A_SILT_MI, FALSE, arg.length)
  check_numeric('A_SOM_LOI', A_SOM_LOI, FALSE, arg.length)
  check_numeric('A_SAND_M50', A_SAND_M50, FALSE, arg.length)
  check_numeric('A_DEPTH', A_DEPTH, FALSE, arg.length)
  
  # Collect data in a table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   TOPSOIL = fifelse(A_DEPTH <= 50, 1,0),
                   A_LOAM_MI = A_SILT_MI + A_CLAY_MI,
                   A_SAND_M50 = A_SAND_M50,
                   ThetaR = NA_real_,
                   ThetaS = NA_real_, 
                   Ksat = NA_real_, 
                   alfa = NA_real_, 
                   l = NA_real_, 
                   n = NA_real_)
  
  # sand soils 
  dt[A_CLAY_MI<8, A_DENSITY := 1/(-7.58+0.01791*A_SOM_LOI+0.0326*TOPSOIL-0.00338*A_SAND_M50+0.00003937*A_LOAM_MI^2+
                                157.7*(1/A_SAND_M50)+1.522*log(A_SAND_M50))]
  dt[A_CLAY_MI<8, ThetaR    := 0.01]
  dt[A_CLAY_MI<8, ThetaS    := -35.7-0.1843*A_DENSITY - 0.03576*A_SAND_M50+0.0000261*A_SAND_M50^2-0.0564*(1/A_LOAM_MI)+
                              0.008*(1/A_SOM_LOI)+496*(1/A_SAND_M50)+0.02244*log(A_SOM_LOI)+7.56*log(A_SAND_M50)]
  dt[A_CLAY_MI<8, Ksat      := exp(45.8-14.34*A_DENSITY+0.001481*A_LOAM_MI^2-27.5*(1/A_DENSITY)-
                                 0.891*log(A_LOAM_MI)-0.34*log(A_SOM_LOI))]
  dt[A_CLAY_MI<8, alfa      := exp(13.66-5.91*A_DENSITY-0.172*TOPSOIL+0.003248*A_SAND_M50-
                                 11.89*(1/A_DENSITY)-2.121*(1/A_LOAM_MI)-0.3742*log(A_LOAM_MI))]
  dt[A_CLAY_MI<8, l         := (2*exp(-76.4-0.097*A_LOAM_MI+59.6*A_DENSITY+0.0332*A_SAND_M50-13.45*A_DENSITY^2+
                                    0.001127*A_LOAM_MI^2+0.00431*A_SOM_LOI^2-0.0000399*A_SAND_M50^2+40.8*(1/A_DENSITY)+
                                    2.364*(1/A_LOAM_MI)+1.014*log(A_LOAM_MI))-2)/(1+
                                                                            exp(-76.4-0.097*A_LOAM_MI+59.6*A_DENSITY+0.0332*A_SAND_M50-13.45*A_DENSITY^2+
                                                                                  0.001127*A_LOAM_MI^2+0.00431*A_SOM_LOI^2-0.0000399*A_SAND_M50^2+40.8*(1/A_DENSITY)+
                                                                                  2.364*(1/A_LOAM_MI)+1.014*log(A_LOAM_MI)))]
  dt[A_CLAY_MI<8, n         := exp(-1.057+0.1003*A_SOM_LOI+1.119*A_DENSITY+0.000764*A_LOAM_MI^2 -
                                 0.1397*(1/A_SOM_LOI)-57.2*(1/A_SAND_M50)-0.557*log(A_SOM_LOI)-0.02997*A_DENSITY*A_LOAM_MI)+1]
  
  # clay and loamy soils
  dt[A_CLAY_MI>=8, A_DENSITY := 1/(0.6117+0.003601*A_CLAY_MI+0.002172*A_SOM_LOI^2+0.01715*log(A_SOM_LOI))]
  dt[A_CLAY_MI>=8, ThetaR    := 0.01]
  dt[A_CLAY_MI>=8, ThetaS    := 0.6311+0.003383*A_CLAY_MI-0.09699*A_DENSITY^2-0.00204*A_DENSITY*A_CLAY_MI]
  dt[A_CLAY_MI>=8, Ksat      := exp(-42.6+8.71*A_SOM_LOI+61.9*A_DENSITY-20.79*A_DENSITY^2-
                                  0.2107*A_SOM_LOI^2-0.01622*A_CLAY_MI*A_SOM_LOI-5.382*A_DENSITY*A_SOM_LOI)]
  dt[A_CLAY_MI>=8,  alfa     := exp(-19.13+0.812*A_SOM_LOI+23.4*A_DENSITY-8.16*A_DENSITY^2+
                                  0.423*(1/A_SOM_LOI)+2.388*log(A_SOM_LOI)-1.338*A_DENSITY*A_SOM_LOI)]
  dt[A_CLAY_MI>=8,  l        := (exp(0.102+0.0222*A_CLAY_MI-0.043*A_DENSITY*A_CLAY_MI)-1)*10/(1+
                                                                                    exp(0.102+0.0222*A_CLAY_MI-0.043*A_DENSITY*A_CLAY_MI))]
  dt[A_CLAY_MI>=8, n         := exp(-0.235+0.972*(1/A_DENSITY)-0.7743*log(A_CLAY_MI)-0.3154*log(A_SOM_LOI)+
                                  0.0678*A_DENSITY*A_SOM_LOI)+1 ]
  
  # order dt
  setorder(dt, id)
  
  # select the relevant columns
  value <- dt[, list(A_DENSITY, ThetaR,  ThetaS, alfa, n, ksat = Ksat, l)]
  
  # return output
  return(value)
}
