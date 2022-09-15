# Changelog carboncastr

## Version 0.1.0  2022-08-15
First start of pedotransferfunction for bulk density

### Added
* 181 pedotransfer functions from scientific literature
* an internal package table `sptf_bulkdensity` showing background information of all the ptfs collected
* a prediction function `sptf_predict` to estimate bulk density for a soil sample
* a function `sptf_textureclass` to estimate USDA soil texture classification from soil mineralogy
* an internal package table `sptf_countries` showing allowed country codes