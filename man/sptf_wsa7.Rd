% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wsa.R
\name{sptf_wsa7}
\alias{sptf_wsa7}
\title{Predicting the percentage water stable aggregates}
\usage{
sptf_wsa7(A_SOM_LOI, A_CLAY_MI, A_SILT_MI, A_PH_WA, A_CACO3_IF)
}
\arguments{
\item{A_SOM_LOI}{(numeric) The percentage organic matter in the soil determined by loss on ignition (\%)}

\item{A_CLAY_MI}{(numeric) The clay content of the mineral soil fraction (\%).}

\item{A_SILT_MI}{(numeric) Silt content (\%)}

\item{A_PH_WA}{(numeric) Acidity in water (-)}

\item{A_CACO3_IF}{(numeric) Carbonated Lime inorganic soil fraction (\%)}
}
\description{
Calculate the water stable aggragates for agricultural topsoils in Spain given the pedotransferfunction of Canasveras et al. (2010)
}
\references{
Canasveras et al. (2010) Estimation of aggregate stability indices in Mediterranean soils by diffuse reflectance spectroscopy
}
