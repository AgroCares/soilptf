# Functions for bulk soil density

#' Calculate the bulk density
#'
#' This function calculates the bulk density of the soil based on texture and organic matter
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references CBAV, 2022; CBGV, 2022
#'
#' @export
bd_nl_cbav <- function(A_SOM_LOI, A_CLAY_MI) {
  
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
  
  # calculate soil texture dependent density
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