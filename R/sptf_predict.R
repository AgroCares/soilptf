# predict function
ptf <- function(country = 'NL',type = 'bulkdensity',...){
  
  # combine all input objects
  obj <- list(...)
  
  # read in internal table
  ptf.mods <- soilptf::sptf_bulkdensity
  ptf.mods[,c('reference','url') := NULL]
  
  # add filter
  d1 <- 5
  
  out <- list(obj)
  return(out)
}
