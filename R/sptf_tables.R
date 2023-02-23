# tables with supportive information for pedotransferunctions


#' Table with background information for bulkdensity pedotransferfunctions
#'
#' This table contains background information on the applicability of pedotransferfunctions collected from literature
#'
#' \describe{
#'   \item{ptf_id}{an unique number for each pedotransfer function}
#'   \item{country_code}{The country or region where the pedotransfer function can be applied,country codes from https://datahub.io/core/country-codes}
#'   \item{continent_code}{The continent where the pedotransfer function can be applied,country codes from https://datahub.io/core/continent-codes}
#'   \item{soiltype}{Applicability of the ptf given USDA soil classifications}
#'   \item{landuse}{Applicability of the ptf given landuse}
#'   \item{depth}{Applicability of the ptf for topsoil (0-30cm) or subsoil (>30cm), given by the maximum depth observed in the datset}
#'   \item{nsample}{the number of samples used to calibrate pedotransfunction}
#'   \item{r2}{}
#'   \item{soilproperties}{The soil properties required to estimate the bulk density}
#'   \item{reference}{the paper describing the pedotransferfunction}
#'   \item{url}{weblink to reference}

#'   }
"sptf_bulkdensity"

#' Table with country codes used in the package
#'
#' This table contains country names as used for the spatial extent where sptfs have been derived from.
#'
#' \describe{
#'   \item{country_name}{the name of the country}
#'   \item{country_code}{the country code;from https://datahub.io/core/country-codes}
#'   \item{continent_code}{The continent where the pedotransfer function can be applied,country codes from https://datahub.io/core/continent-codes}
#'}
"sptf_countries"


#' Table with water retention properties of 'bouwstenen'
#' 
#' This table contains water retention curve parameters and typical mineral composition of 18 'bouwstenen'
#' ref: Wösten, J. H. M., Veerman, G. ., de Groot, W. J., & Stolte, J. (2001). Waterretentie en doorlatendheidskarakteristieken van boven- en ondergronden in Nederland: de Staringreeks. Alterra Rapport, 153, 86. https://edepot.wur.nl/43272
#' 
#' @format A data.frame with 36 rows and 14 columns:
#' \describe{
#'   \item{bouwsteen}{soil type bouwsteen}
#'   \item{omschrijving}{description of 'bouwsteen'}
#'   \item{thres}{residual water content (cm3/cm3). Table 3 of Wosten 2001}
#'   \item{thsat}{water content at saturation (cm3/cm3). Table 3 of Wosten 2001}
#'   \item{Ks}{saturated hydraulic conductivity (cm/d). Table 3 of Wosten 2001}
#'   \item{alpha}{parameter alpha of pF curve (1/cm) Table 3 of Wosten 2001}
#'   \item{l}{parameter l of pF curve (-). Table 3 of Wosten 2001}
#'   \item{n}{parameter n of pF curve (-). Table 3 of Wosten 2001}
#'   \item{sand\%}{sand content (\%) within soil mineral parts. Middle value of Table 1 of Wosten 2001}
#'   \item{silt\%}{silt content (\%) within soil mineral parts. Middle value of Table 1 of Wosten 2001}
#'   \item{clay\%}{clay content (\%) within soil mineral parts. Middle value of Table 1 of Wosten 2001}
#'   \item{OM\%}{organic matter content (\%). Middle value of Table 1 of Wosten 2001}
#'   \item{bulkdensity}{soil bulk density (g/cm3). Middle value of Table 2 of Wosten 2001}
#'   \item{M50}{size of sand particles (um). Middle value of Table 2 of Wosten 2001}
#' }
"sptf_bouwsteen"
