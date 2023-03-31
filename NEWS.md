# Changelog soilptf

# Version 0.1.0.9000 2023-03-15
## Added
* `sptf_parameters` a table with data on parameters used within the package
* functions to facilitate checkmates with `sptf_parameters` (`check_enum()`, 
`check_numeric`, `get_minval()`, `get_maxval()`, `enum_opts()`)
* manuals for whc and pmn functions

## Changed
* checkmates to use `sptf_parameters` instead of hardcoding min and maxvalues in
each function
* export whc and pmn ptf's in NAMESPACE
* `sptf_whc4` now takes A_DEPTH in m instead of cm 


## Version 0.1.0  2022-08-15
First start of pedotransferfunction for bulk density

### Added
* 181 pedotransfer functions from scientific literature
* an internal package table `sptf_bulkdensity` showing background information of all the ptfs collected
* a prediction function `sptf_predict` to estimate bulk density for a soil sample
* a function `sptf_textureclass` to estimate USDA soil texture classification from soil mineralogy
* an internal package table `sptf_countries` showing allowed country codes