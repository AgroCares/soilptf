# functions to check function inputs
#' Get value of a parameter
#'
#' This function retrieves the value of a parameter for a given column name in sptf_parameters
#'
#' @param this.parameter (character) Quoted name of the parameter code
#' @param this.column (character) Quoted name of the column in sptf_parameters
#' @param check.num (boolean) If TRUE, check whether the parameter is of data_type 'num' or 'int' in sptf_parameters
#'
#' @details
#' Vectors of parameter codes are also possible
get_parval <- function(this.parameter, this.column, check.num = FALSE) {
  # add visual binding
  out = code = test.pass = data_type = NULL
  
  # load sptf_parameters
  sptf_parameters <- soilptf::sptf_parameters
  
  # check inputs
  checkmate::assert_subset(this.parameter, choices = sptf_parameters$code)
  checkmate::assert_subset(this.column, choices = names(sptf_parameters))
  checkmate::assert_subset(check.num, choices = c(TRUE, FALSE))
  
  # check numeric if required
  if(check.num) {
    test.pass <- checkmate::test_subset(this.parameter,
                                        choices = sptf_parameters[data_type %in%
                                                                   c('num', 'int'), code])
    
    # raise error on fail
    if(!test.pass) {
      stop(paste0(this.parameter, ' is not a parameter of data_type "num" or "int"'))
    }
  }
  
  # get value
  out <- sptf_parameters[code %in% this.parameter, get(this.column)]
  
  return(out)
}
#' Get minimum value of a parameter
#'
#' This function retrieves the value lowest acceptable value of a numeric parameter
#'
#' @param this.parameter (character) Quoted name of the parameter code
#'
#' @examples
#' get_minval(this.parameter = "A_CLAY_MI")
#'
#' @details
#' This function retrieves the value in column `value_min` of `sptf_parameters`
#' Vectors of parameter codes are also possible
get_minval <- function(this.parameter) {
  # add visual binding
  data_type = code = NULL
  
  # load sptf_parameters
  sptf_parameters <- soilptf::sptf_parameters
  
  # get value
  out <- get_parval(this.parameter = this.parameter, this.column = "value_min",
                    check.num = TRUE)
  
  return(out)
}
#' Get maximum value of a parameter
#'
#' This function retrieves the value highest acceptable value of a numeric parameter
#'
#' @param this.parameter (character) Quoted name of the parameter code
#'
#' @examples
#' get_maxval(this.parameter = "A_CLAY_MI")
#'
#' @details
#' This function retrieves the value in column `value_max` of `sptf_parameters`
#' Vectors of parameter codes are also possible
get_maxval <- function(this.parameter) {
  # add visual binding
  data_type = code = NULL
  
  # load sptf_parameters
  sptf_parameters <- soilptf::sptf_parameters
  
  # get value
  out <- get_parval(this.parameter = this.parameter, this.column = "value_max",
                    check.num = TRUE)
  
  return(out)
}
# Functions to facilitate working with pandex

#' Get options for enum parameters
#'
#' This function is a wrapper to easily get a vector of options for a parameter of data_type enum
#'
#' @param this.parameter (character) Quoted name of the parameter
#'
#' @examples
#' enum_opts(this.parameter = "B_GWL_CLASS")
#'
#' @details This function wraps unlist(strsplit(sptf_parameters[code == this.parameter, options]))
#' to return a vector of allowed values for parameters of data_type 'enum'. If the
#' data is numeric or integer, options will be given as numeric/integer instead of strings
enum_opts <- function(this.parameter) {
  # add visual binding
  data_type = enum = code = NULL
  
  # load sptf_parameters
  sptf_parameters <- soilptf::sptf_parameters
  
  # check input
  checkmate::assert_subset(this.parameter,
                           choices = sptf_parameters[enum == TRUE, code])
  
  # retrieve options for parameter
  opts <- unlist(strsplit(sptf_parameters[code == this.parameter, options],
                          split = '||', fixed = TRUE))
  
  # make numeric if parameter is of data_type int or num
  if(this.parameter %in% sptf_parameters[data_type %in% c('num'), code]) {
    opts <- as.numeric(opts)
  }
  if(this.parameter %in% sptf_parameters[data_type %in% c('int'), code]) {
    opts <- as.integer(opts)
  }
  
  # return
  return(opts)
}
# functions to check funtion inputs with pandex and checkmate

#' Check numeric sptf_parameters
#' 
#' @param this.parameter.name (character) Quoted name of the variable to check
#' @param this.parameter.value (numeric) Value or vector of values to check
#' @param anymissing (boolean) are missing values allowed (TRUE) or not (FALSE)
#' 
check_numeric <- function(this.parameter.name, this.parameter.value, anymissing = FALSE){
  checkmate::assert_numeric(this.parameter.value,
                            lower = soilptf::get_minval(this.parameter.name),
                            upper = soilptf::get_maxval(this.parameter.name),
                            any.missing = anymissing)
}

#' Check enum parameters
#' 
#' @param this.parameter.name (character) Quoted name of the variable to check
#' @param this.parameter.value (numeric) Value or vector of values to check
check_enum <- function(this.parameter.name, this.parameter.value){
  checkmate::assert_subset(this.parameter.value,
                           choices = soilptf::enum_opts(this.parameter.name))
}