% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generalFunctions.R
\name{assert}
\alias{assert}
\title{assert}
\usage{
assert(expression, message = NULL, error = FALSE)
}
\arguments{
\item{expression}{expression that should be True}

\item{message}{warning or error message (default is "Assert Error: Value is False")}

\item{error}{turns the warning into an error (stops the program execution)}
}
\value{
value of the expression
}
\description{
Asserts if a value is True. If False, throws a warning (default) or error
}

