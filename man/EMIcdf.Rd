% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportingFunctions.R
\name{EMIcdf}
\alias{EMIcdf}
\title{EMI Modified Cumulative Distribution Fuction (CDF) Graph}
\usage{
EMIcdf(vector, lineColor = EMIcolors(1))
}
\arguments{
\item{vector}{a numeric vector}

\item{lineColor}{color of the line to plot. Defaults to EMI blue}
}
\description{
Creates a CDF graph from the cumulative sum of a vector.
Takes a numeric vector, computes the cumulative sum of the vector,
then plots a modified CDF using EMIplot.
Plots the percent cumulative values on the y-axis
and the actual data values (in ascending order) on the x-axis.
}
\examples{
plt = EMIcdf(1:1000)
}
\seealso{
ecdf
}

