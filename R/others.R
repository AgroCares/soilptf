# other functions, not yet sufficient to be added in a separate script

#' Calculate the crop yield of cereals via the meta-regression of global dataset from Oldfield et al. (2019)
#'
#' This function calculates the crop yield of cereals.
#'
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#' 
#' @references Oldfield et al. (2019).Global meta-analysis of the relationship between soil organic matter and crop yields
#'
#' @export
sptf_yield1 <- function(A_C_OF,A_CLAY_MI) {
  
  # Check input
  arg.length <- max(length(A_C_OF), length(A_CLAY_MI))
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, len = arg.length)
  
  # default mean inputs
  lat = 30.49
  aridity = 0.76
  ph = 7.15
  irrigation = 0
  ndose = 118
  
  # make internal data.table
  dt <- data.table(A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)
  
  # meta-regression model
  dt[,value := -1.61 + 0.179 * A_C_OF - 0.46 * (0.1 * A_C_OF)^2 + irrigation * 0.75 + 0.053 * ph + 0.16 * aridity + 
               0.013 * A_CLAY_MI + ndose * 0.018 -0.000039 * ndose^2 + 0.0039 * 0.1 * A_C_OF * ndose]
  
  # select output variable
  value <- dt[,value]
  
  # return value (mg / kg)
  return(value)
  
}
