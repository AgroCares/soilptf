# tables with supportive information for pedotransferunctions


#' Table with background information for bulkdensity pedotransferfunctions
#'
#' This table contains background information on the applicability of pedotransferfunctions collected from literature
#'
#' @format A data.frame with x rows and x columns:
#' \describe{
#'   \item{id}{an unique number for each pedotransfer function}
#'   \item{country}{The country or region where the pedotransfer function can be applied,country codes from https://datahub.io/core/country-codes}
#'   \item{continent}{The continent where the pedotransfer function can be applied,country codes from https://datahub.io/core/continent-codes}
#'   \item{soiltype}{Applicability of the ptf given USDA soil classifications}
#'   \item{landuse}{Applicability of the ptf given landuse}
#'   \item{depth}{Applicability of the ptf for topsoil (0-30cm) or subsoil (>30cm), given by the maximum depth observed in the datset}
#'   \item{soilproperties}{The soil properties required to estimate the bulk density}
#'   \item{reference}{the paper describing the pedotransferfunction}
#'   \item{url}{weblink to reference}
#'   \item{nsample}{the number of samples used to calibrate pedotransfunction}
#'   }
"sptf_bulkdensity"

#' Table with country codes used in the package
#'
#' This table contains country names as used for the spatial extent where sptfs have been derived from.
#'
#' @format A data.frame with x rows and x columns:
#' \describe{
#'   \item{country_name}{the name of the country}
#'   \item{country_code}{the country code;from https://datahub.io/core/country-codes}
#'}
"sptf_countries"

#' Table with variables that functions may use
#' 
#' This table contains the codes for variables used by soilptf functions as well
#' as a description, unit, and minimum and maximum values.
#' 
#' \describe{
#'  \item{code}{The parameter code}
#'  \item{parameter}{Brief description of the parameter}
#'  \item{unit}{The unit of the parameter if applicable}
#'  \item{product}{Data classifier, A = Soil measurements, B = Environmental characteristics,
#'   D = Soil or feed characteristics derived from soil/feed measurements,
#'   M = Soil management measures, S = Scores, RM = Recommendations (measures/gifts),
#'   I = Indicators, F = Feed measurements, P = soil amendment product}
#'  \item{element}{Indicates the chemical element or parameter name}
#'  \item{method1}{Method used to determine value}
#'  \item{method2}{Additional details on method}
#'  \item{data_type}{Type of data the parameter pertains to: numeric, integer, char, bool, geom, enum}
#'  \item{value_min}{Lowest possible value the parameter may have if numeric or integer}
#'  \item{value_max}{Highest possible value the parameter may have if numeric or integer}
#'  \item{explanation}{Some additional explanation}
#'  \item{enum}{boolean whether parameter values are drawn from a limited set}
#'  \item{options}{Allowed values for a parameter of type enum seperated by "||"}
#' }
"sptf_parameters"

#' Table with background information for pedotransferfunctions in relation to other soil properties and functions
#'
#' This table contains background information on the applicability of pedotransferfunctions collected from literature
#'
#' @format A data.frame with x rows and x columns:
#' \describe{
#'   \item{ptf_id}{an unique number for each pedotransfer function}
#'   \item{ptf_tid}{an unique number for each pedotransfer function per ptf_type}
#'   \item{ptf_type}{The soil function category used in package}
#'   \item{country_code}{The country or region where the pedotransfer function can be applied,country codes from https://datahub.io/core/country-codes}
#'   \item{continent_code}{The continent where the pedotransfer function can be applied,country codes from https://datahub.io/core/continent-codes}
#'   \item{soiltype}{Applicability of the ptf given USDA soil classifications}
#'   \item{landuse}{Applicability of the ptf given landuse}
#'   \item{depth}{Applicability of the ptf for topsoil (0-30cm) or subsoil (>30cm), given by the maximum depth observed in the datset}
#'   \item{nsample}{The number of samples used to calibrate pedotransfunction}
#'   \item{r2}{The explained variance of the ptf}
#'   \item{soilproperties}{The soil properties required to estimate the bulk density}
#'   \item{reference}{the paper describing the pedotransferfunction}
#'   \item{url}{weblink to reference}
#'   }
"sptf_soilproperties"

#' Table with supprting infromation for class ptfs pF curve
#'
#' This table contains parameters for Dutch categorial ptfs for pF curve
#'
#' @format A data.frame with x rows and x columns:
#' \describe{
#'   \item{bouwsteen}{}
#'   \item{omschrijving}{}
#'   \item{thres}{}
#'   \item{thsat}{}
#'   \item{Ks}{}
#'   \item{alpha}{}
#'   \item{l}{}
#'   \item{n}{}
#'   \item{sand}{}
#'   \item{silt}{}
#'   \item{clay}{}
#'   \item{OM}{}
#'   \item{bulkdensity}{}
#'   \item{M50}{}
#'   }
"sptf_bouwsteen"