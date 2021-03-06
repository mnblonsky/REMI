% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportingFunctions.R
\name{EMIlikertPlot}
\alias{EMIlikertPlot}
\title{EMI Likert Plot}
\usage{
EMIlikertPlot(df, likertcols, group = NULL, ...)
}
\arguments{
\item{df}{is a dataframe with likert responses as columns; can contain other data: caseids and groups (like type)}

\item{likertcols}{is a vector of columns of likert responses to plot}

\item{group}{is a column in the data referring to groups}

\item{...}{variables passed to col.strip.background}
}
\value{
a dataframe object with the ID, code, name, start date, end dte, and health of each project
}
\description{
EMIlikert(): function to make an EMI color formatted likert plot
}
\examples{
mydf = data.frame(group = rep(1:3, each = 10),
                  Q1 = factor(sample(1:5, 30, replace = TRUE)),
                  Q2 = factor(sample(1:5, 30, replace = TRUE)))
my_likert_plot <- EMIlikertPlot(mydf, 2:3)
my_likert_plot
}

