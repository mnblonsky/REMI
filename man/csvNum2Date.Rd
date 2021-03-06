% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generalFunctions.R
\name{csvNum2Date}
\alias{csvNum2Date}
\title{Convert a .csv Number to a Date}
\usage{
csvNum2Date(num)
}
\arguments{
\item{num}{number from csv file}
}
\value{
date object (POSIXct)
}
\description{
Converts number from a csv file to a date. Based on Excel's date format (number
of days since 1900). For example, the number 42014 corresponds to Jan. 12, 2015.
}

