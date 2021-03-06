% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importFunctions.R
\name{loadData}
\alias{loadData}
\title{Load Data}
\usage{
loadData(dataFile, dputFile = NULL, labelFile = NULL, emptyStr = "#NULL!",
  saveCsv = FALSE, overrideDput = FALSE, twoHeader = FALSE, ...)
}
\arguments{
\item{dataFile}{file path to data file (csv, R, xlsx, xls, sav supported)}

\item{dputFile}{file path to .R data file (loading or saving). Default is to
not save any file; 'same' indicates the same name and location as dataFile}

\item{labelFile}{file path to .csv label file (From SPSS output)}

\item{emptyStr}{character vector of strings to remove. By default, it removes
the terrible "#NULL!" string that Excel puts in crazy places...}

\item{saveCsv}{saves a .csv file if True (default is False)}

\item{overrideDput}{if True, read .csv instead of .R file if both exist (default is False)}

\item{twoHeader}{if False (default) there is only one header row in the dataFile. If True,
it reads two header rows and returns the second row as a separate data frame. This affects
the return value (see below). Set this to True if you are getting data from Qualtrics!}

\item{...}{extra variables passed to read.xls, read.spss, or read.csv}
}
\value{
By default, the main data frame is returned. If twoHeader is set to True,
a list of two data frames is returned: the first element is the main data frame and
the second is the 2nd header (Qualtrics questions) data frame. It is best to assign
each to a separate variable (see examples)
}
\description{
Reads data from an excel, R, or csv file and stores as a data frame. It can read
labels from a separate csv file (labelFile), which can be generated from SPSS output.
It can read double header csv files from Qualtrics output. Options to save data
as a .csv or .R file.
}
\details{
Note that converting Excel files requires python and an external python script
(exceltocsv). It is best to manually convert the Excel file to .csv and then load
it into R.
}
\examples{
tf = paste0(tempfile(), '.csv')
write.csv(mtcars, tf)
dataFrame = loadData(tf)

# to read file from secure server:
dataFile = file.path(secureServer(), 'project/path/datafile.csv')

# To load data from Qualtrics:
qualtricsData = data.frame(Q1 = c('Question 1', 'A', 'B', 'C'),
                           Q2 = c('Question 2', '1', '2', '3'))
write.csv(qualtricsData, tf)
dataList = loadData(tf)
mainDataFrame = dataList[[1]]
questionsDataFrame = dataList[[2]]
}

