# make internal table for bulk density

  # load internal packages
  require(data.table)
  require(usethis)
  
  # load the csv file with pdtf for bulk density
  d1 <- fread('dev/sptf_bulkdensity.csv')

  # make copy for package table
  sptf_bulkdensity <- copy(d1)
  
  # save updated crop table
  usethis::use_data(sptf_bulkdensity,overwrite = TRUE)
  