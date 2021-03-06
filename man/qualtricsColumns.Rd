% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysisFunctions.R
\name{qualtricsColumns}
\alias{qualtricsColumns}
\title{qualtricsColumns}
\usage{
qualtricsColumns(df, q, id, dfQuestions = NULL, category = "Category",
  func = NULL, remove = c(NA, NaN, NULL, "", "-99"), questionChar = "-",
  charInQ = 0, includeRespondent = FALSE, ...)
}
\arguments{
\item{df}{main data frame}

\item{q}{string representing question name (ex. "Q1_")}

\item{id}{vector specifying columns (1,2,3...)}

\item{dfQuestions}{data frame with same columns as df - contains question
strings}

\item{category}{name of category to print in final data}

\item{func}{function to run on cleaned data. An example could be fixing text
(removing a ":") or converting to a different class. Default is no function.}

\item{remove}{vector of strings/objects to remove from each column (ex. NA, NaN)}

\item{questionChar}{character to help specify data in the question. Default is
"-", which Qualtrics uses to show the question options.}

\item{charInQ}{number of times that questionChar shows up in the question.}

\item{includeRespondent}{If true, adds a column to the output specifying the row
from the original data frame.}
}
\value{
a data frame containing one row for each response that was not removed.
The data frame can be quickly summarized or tabled, or graphed using EMI functions.
}
\description{
Runs a desired function on multiple columns of a data frame with similar
column names (ex. Q1_1, Q1_2, ...). Defaults are useful for Qualtrics data
structures.
}

