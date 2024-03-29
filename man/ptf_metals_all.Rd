% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sptf_predict.R
\name{ptf_metals_all}
\alias{ptf_metals_all}
\title{Predict the impact of soil organic carbon on metal adsorption.}
\usage{
ptf_metals_all(dt)
}
\arguments{
\item{dt}{(data.table) Data table which includes
A_SOM_LOI (numeric) The percentage of organic matter in the soil (\%).
A_C_OF (numeric) The fraction organic carbon in the soil (g / kg).
A_CLAY_MI (numeric) The clay content of the soil (\%).
A_SAND_MI (numeric) The sand content of the soil (\%).
A_SILT_MI (numeric) The silt content of the soil (\%).}
}
\description{
Predict the impact of soil organic carbon on metal adsorption.
}
\details{
This function returns a melted form of data table, containing values of freundlich coefficient with different PTFs
}
