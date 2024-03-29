% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paw.R
\name{sptf_paw4}
\alias{sptf_paw4}
\title{Calculate the Plant Available Water (PAW) given the pedotransferfunction of Oosterveld and Chang (1980)}
\usage{
sptf_paw4(A_C_OF, A_CLAY_MI, A_SAND_MI)
}
\arguments{
\item{A_C_OF}{(numeric) Organic Carbon Content (g / kg)}

\item{A_CLAY_MI}{(numeric) The clay content of the mineral soil fraction (\%).}

\item{A_SAND_MI}{(numeric) Sand content (\%)}
}
\description{
Calculate the Plant Available Water (PAW) given the pedotransferfunction of Oosterveld and Chang (1980)
}
\details{
For this function A_DEPTH should be between 0.08 and 0.18
}
\references{
Oosterveld and Chang (1980) Empirical relations between laboratory determinations of soil texture and moisture retention
}
