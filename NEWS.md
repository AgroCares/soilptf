# Changelog soilptf

## Version 0.2.1  2023-xx-xx

### Added
* 12 pedotransfer functions for HWC
* 10 pedotransfer functions for PMN
* 1 function for carbon decomposition
* 4 partition functions for freundlich coefficients for metals
* 7 predict functions: `ptf_cec_all`,`ptf_phbc_all`,`ptf_mwd_all`,`ptf_wsa_all`,`ptf_hwc_all`,`ptf_sss_all`,and `ptf_cdec_all` 

## Version 0.2.0  2023-04-14

### Added
* 6 extra pedotransfer functions for bulk density
* 74 pedotransfer functions for CEC
* 15 pedotransfer functions of Mean Weight Diameter of soil aggregates
* 8 pedotransfer functions for pH buffer capacity
* 9 pedotransfer functions for Water Stable Aggregates
* 3 pedotransfer functions for threshold velocity for wind erosion
* 3 pedotransfer functions for soil shear strength
* 2 pedotransfer functions for soil erodibility
* documentation on table `sptf_bouwsteen`

### Changed
* various scripts from previous branches that were not fully checked

# Version 0.1.1 2023-04-11
## Fixed
* package being uninstallable due to dependency on EUptf2

## Changed
* `sptf_whc7` does not work due to missing dependency (EUptf2)

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
* `sptf_whc4` and `sptf_whc7` now takes A_DEPTH in m instead of cm 


## Version 0.1.0  2022-08-15
First start of pedotransferfunction for bulk density

### Added
* 181 pedotransfer functions from scientific literature
* an internal package table `sptf_bulkdensity` showing background information of all the ptfs collected
* a prediction function `sptf_predict` to estimate bulk density for a soil sample
* a function `sptf_textureclass` to estimate USDA soil texture classification from soil mineralogy
* an internal package table `sptf_countries` showing allowed country codes