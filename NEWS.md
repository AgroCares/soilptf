# Changelog soilptf

## Version 0.5.2  2023-05-xx

### Added
* in dev, update critical thresholds for metals

## Version 0.5.1  2023-05-09

# Fixed
* NAm and SAm as abbreviation for North and South America causes errors due to confusion with NA, issue #3
* replace `assert_int` with `assert_integer` for years in `sptf_cdec1`, issue #26
* ensure correct CN ratios in `sptf_cdec1`, issue #26

# Changed
* all elements for A_CACO3_MI replaced with A_CACO3_IF, issue #30

# Added
* options for parameter `B_SOILCLASS_USDA` in `sptf_parameters`, issue #23
* add ptfs cec75, cec76 and cec77

## Version 0.5.0  2023-04-29

## Added
* add vignette `how to contribute`
* add vignette `introduction`

## Fixed
* add missing id in wrapper functions `ptf_xxx_all'
* add mineralogy check in `sptf_textureclass` and replace missing input when 2 of the 3 inputs are known.

## Version 0.4.1 2023-04-29

### Fixed
* function `sptf_fc_zinc` was renamed to `sptf_fc_zn` to be consistent with other metal ptfs

## Version 0.4.0 2023-04-29

### Added
* predict functions for all ptfs: `ptf_cec`,`ptf_phbc`,`ptf_mwd`,`ptf_wsa`,`ptf_hwc`,`ptf_sss`,and `ptf_cdec` 
* package table `sptf_soilproperties` updated for metals, cec, hwc, cdec, whc and paw

### Changed
* script names for functions are updated, making them consistent with function names

## Version 0.3.0  2023-04-27

### Added
* 12 pedotransfer functions for HWC
* 10 pedotransfer functions for PMN
* 1 function for carbon decomposition
* 4 partition functions for freundlich coefficients for metals
* 7 predict functions: `ptf_cec_all`,`ptf_phbc_all`,`ptf_mwd_all`,`ptf_wsa_all`,`ptf_hwc_all`,`ptf_sss_all`,and `ptf_cdec_all` 
* all original HWC ptfs are renamed to PAW because they were wrongly named. 
* HWC ptfs were updated
* optimizer function and examples for EGU2023 in script in dev directory `derive_optimum_soc`

### Fixed
* calculation moisture content at given pF were fixed for all ptf_paw functions
* all added checkmate functions were updated

## Version 0.2.1 2023-04-24
### Fixed
* summing and rounding error in when checking that `A_SILT_MI`, `A_CLAY_MI`, and 
`A_SAND_MI` never exceed 100 resolving #18

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