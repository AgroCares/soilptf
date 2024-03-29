% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkmate_tools.R
\name{get_parval}
\alias{get_parval}
\title{Get value of a parameter}
\usage{
get_parval(this.parameter, this.column, check.num = FALSE)
}
\arguments{
\item{this.parameter}{(character) Quoted name of the parameter code}

\item{this.column}{(character) Quoted name of the column in sptf_parameters}

\item{check.num}{(boolean) If TRUE, check whether the parameter is of data_type 'num' or 'int' in sptf_parameters}
}
\description{
This function retrieves the value of a parameter for a given column name in sptf_parameters
}
\details{
Vectors of parameter codes are also possible
}
