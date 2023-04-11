# Changelog carboncastr

## Version 0.2.0  2023-04-11

### Added
* 6 extra pedotransfer functions for bulk density
* 67 pedotransfer functions for CEC
* 15 pedotransfer functions of Mean Weight Diameter of soil aggregates
* 8 pedotransfer functions for pH buffer capacity
* 9 pedotransfer functions for Water Stable Aggregates
* 3 pedotransfer functions for threshold velocity for wind erosion
* 3 pedotransfer functions for soil shear strength
* 2 pedotransfer functions for soil erodibility

## Version 0.1.0  2022-08-15
First start of pedotransferfunction for bulk density

### Added
* 181 pedotransfer functions from scientific literature
* an internal package table `sptf_bulkdensity` showing background information of all the ptfs collected
* a prediction function `sptf_predict` to estimate bulk density for a soil sample
* a function `sptf_textureclass` to estimate USDA soil texture classification from soil mineralogy
* an internal package table `sptf_countries` showing allowed country codes