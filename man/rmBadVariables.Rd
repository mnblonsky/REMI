% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importFunctions.R
\name{rmBadVariables}
\alias{rmBadVariables}
\title{******* Cleaning Functions *******
Remove Bad Variables}
\usage{
rmBadVariables(df, badItems = c(NA))
}
\arguments{
\item{df}{data frame}

\item{badItems}{vector of bad values. By default, NA is the only bad value.}
}
\description{
Removes variables in a data frame if ALL values are "bad". Bad values can
be specified.
}

