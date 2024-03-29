# make internal table for bulk density

  # load internal packages
  require(data.table)
  require(usethis)
  
# sptf_bulkdensity =============================================================
# load the csv file with pdtf for bulk density
  d1 <- fread('dev/sptf_bulkdensity.csv',na.strings='', dec=',')

  # make copy for package table
  sptf_bulkdensity <- copy(d1)
  
  # save updated crop table
  usethis::use_data(sptf_bulkdensity,overwrite = TRUE)
  
  # make table for all countries (https://datahub.io/core/country-list#r)
  
    # get information where to find the location where csv data is stored
    # library("jsonlite")
    # json_file <- 'https://datahub.io/core/country-list/datapackage.json'
    # json_data <- fromJSON(paste(readLines(json_file), collapse=""))
    # json_data$resources$path
    
    # download data
    # d2 <- fread('https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv', encoding = 'UTF-8')

# sptf_countries ===============================================================
# loaddata
  d2 <- fread('dev/sptf_countries.csv',encoding = 'UTF-8')
  
  # update column names
  setnames(d2,c('country_name','country_code','continent_code'))
  
  # set countries to lower case
  d2[, country_name := tolower(country_name)]

  # set NA for north-america to NAm
  d2[is.na(continent_code)|continent_code =='NA', continent_code := 'NAm']
  d2[continent_code=='SA', continent_code := 'SAm']
  
  # make copy for package table
  sptf_countries <- copy(d2)
  
  # save updated crop table
  usethis::use_data(sptf_countries,overwrite = TRUE)
  
# sptf_soilproperties ==========================================================
# load data
  d3 <- fread('dev/sptf_soilproperties.csv',encoding = 'UTF-8')
  
  # set empty continentcode to NA
  d3[continent_code=='', continent_code := NA_character_]
  
  # make copy for package table
  sptf_soilproperties <- copy(d3)
  
  # save updated crop table
  usethis::use_data(sptf_soilproperties,overwrite = TRUE)
  
          
  
# sptf_parameters ==============================================================
  sptf_parameters <- fread('dev/sptf_parameters.csv',encoding = 'UTF-8')
  usethis::use_data(sptf_parameters, overwrite = TRUE)

  # document
  document()
  