# tables with supportive information for pedotransferunctions


#' Table with background information for bulkdensity pedotransferfunctions
#'
#' This table contains background information on the applicability of pedotransferfunctions collected from literature
#'
#' @format A data.frame with x rows and x columns:
#' \describe{
#'   \item{id}{an unique number for each pedotransfer function}
#'   \item{country}{The country or region where the pedotransfer function can be applied}
#'   \item{soiltype}{Applicability of the ptf given USDA soil classifications}
#'   \item{landuse}{Applicability of the ptf given landuse}
#'   \item{soilproperties}{The soil properties required to estimate the bulk density}
#'   \item{reference}{the paper describing the pedotransferfunction}
#'   \item{url}{weblink to reference}
#'   \item{nsample}{the number of samples used to calibrate pedotransfunction}
#'   }
"sptf_bulkdensity"