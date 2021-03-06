% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generalFunctions.R
\name{almostEqual}
\alias{almostEqual}
\title{Almost Equal}
\usage{
almostEqual(v1, v2, digits, compError = 15)
}
\arguments{
\item{v1}{first vector}

\item{v2}{second vector}

\item{digits}{primary rounding digits}

\item{compError}{digits for computer error (default is 15)}
}
\description{
Determines if two numbers are almost equal, based on rounding by significant
digits. Does 2 rounds of rounding.
}

