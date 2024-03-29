% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hydraulic_properties.R
\name{sptf_vg2}
\alias{sptf_vg2}
\title{Estimate water retention curve parameters based on Wosten 2001}
\usage{
sptf_vg2(A_CLAY_MI, A_SILT_MI, A_SOM_LOI, A_SAND_M50, A_DEPTH)
}
\arguments{
\item{A_CLAY_MI}{(numeric) The clay content of the mineral soil fraction (\%).}

\item{A_SILT_MI}{(numeric) Silt content (\%)}

\item{A_SOM_LOI}{(numeric) The percentage organic matter in the soil determined by loss on ignition (\%)}

\item{A_SAND_M50}{(numeric) description (µm)}

\item{A_DEPTH}{(numeric) The depth of the sampled soil layer (m)}
}
\description{
This function estimates water retention curve parameters using Pedo transfer function of Wosten (2001)
}
\references{
Wösten, J. H. M., Veerman, G. ., de Groot, W. J., & Stolte, J. (2001). Waterretentie en doorlatendheidskarakteristieken van boven- en ondergronden in Nederland: de Staringreeks. Alterra Rapport, 153, 86. https://doi.org/153
}
