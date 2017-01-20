# ---
# title: "Importing Functions"
# author: "EMI Consulting"
# date: "January 27, 2015"
# ---
#
# Contains functions for importing and cleaning data for projects

# ******* Importing Functions *******
. = NULL

#' Load Data
#'
#' Reads data from an excel, R, or csv file and stores as a data frame. It can read
#' labels from a separate csv file (labelFile), which can be generated from SPSS output.
#' It can read double header csv files from Qualtrics output. Options to save data
#' as a .csv or .R file.
#'
#' Note that converting Excel files requires python and an external python script
#' (exceltocsv). It is best to manually convert the Excel file to .csv and then load
#' it into R.
#' @param dataFile file path to data file (csv, R, xlsx, xls, sav supported)
#' @param dputFile file path to .R data file (loading or saving). Default is to
#' not save any file; 'same' indicates the same name and location as dataFile
#' @param labelFile file path to .csv label file (From SPSS output)
#' @param emptyStr character vector of strings to remove. By default, it removes
#' the terrible "#NULL!" string that Excel puts in crazy places...
#' @param saveCsv saves a .csv file if True (default is False)
#' @param overrideDput if True, read .csv instead of .R file if both exist (default is False)
#' @param twoHeader if False (default) there is only one header row in the dataFile. If True,
#' it reads two header rows and returns the second row as a separate data frame. This affects
#' the return value (see below). Set this to True if you are getting data from Qualtrics!
#' @param ... extra variables passed to read.xls, read.spss, or read.csv
#' @return By default, the main data frame is returned. If twoHeader is set to True,
#' a list of two data frames is returned: the first element is the main data frame and
#' the second is the 2nd header (Qualtrics questions) data frame. It is best to assign
#' each to a separate variable (see examples)
#' @examples
#' tf = paste0(tempfile(), '.csv')
#' write.csv(mtcars, tf)
#' dataFrame = loadData(tf)
#'
#' # to read file from secure server:
#' dataFile = file.path(secureServer(), 'project/path/datafile.csv')
#'
#' # To load data from Qualtrics:
#' qualtricsData = data.frame(Q1 = c('Question 1', 'A', 'B', 'C'),
#'                            Q2 = c('Question 2', '1', '2', '3'))
#' write.csv(qualtricsData, tf)
#' dataList = loadData(tf)
#' mainDataFrame = dataList[[1]]
#' questionsDataFrame = dataList[[2]]
loadData = function(dataFile, dputFile = NULL, labelFile = NULL,
                    emptyStr = '#NULL!',
                    saveCsv = FALSE,
                    overrideDput = FALSE,
                    twoHeader = FALSE,
                    ...) {
    if (!is.null(dputFile) && dputFile == 'same') dputFile = paste0(myBasename(dataFile, T),'.R')
    if (!is.null(dputFile) && file.exists(dputFile) && ! overrideDput) {
        data = dget(dputFile)
        if (saveCsv) write.csv(data, file = paste0(myBasename(dataFile, T),'.csv'))
        return(data)
    }

    # initialize data frame
    if (fileExtn(dataFile) %in% c('xls','xlsx')) {
        #         Removed python dependency
        #         addMyShellPath() # requires python!
        #         tmpCsv = file.path(dirname(dataFile),'temp.csv')
        #         cmd = paste('exceltocsv -s', shQuote(as.character(sheet)),
        #                     '--outFile', shQuote(tmpCsv),
        #                     shQuote(dataFile))
        #         system(cmd)
        #         survey = read.csv(tmpCsv, na.strings = c('NA', emptyStr),
        #                           quote = "\"", stringsAsFactors = FALSE)
        #         file.remove(tmpCsv)
        survey = gdata::read.xls(dataFile, stringsAsFactors = FALSE, ...)
    } else if (fileExtn(dataFile) %in% c('csv')) {
        survey = read.csv(dataFile, na.strings = c('NA', emptyStr),
                          quote = "\"", stringsAsFactors = FALSE, ...)
    } else if (fileExtn(dataFile) %in% c('sav')) {
        survey = foreign::read.spss(dataFile, to.data.frame = TRUE, ...)
    } else if (fileExtn(dataFile) %in% c('R')) {
        survey = dget(dataFile)
        if (saveCsv) write.csv(data, file = paste0(myBasename(dataFile, T),'.csv'))
    } else {
        stop(paste('Improper file extension:', fileExtn(dataFile)))
    }

    # remove second header if there
    if (twoHeader) {
        h2 = survey[1,]
        survey = survey[2:nrow(survey),]
    }

    # get labels
    if (!is.null(labelFile)) {
        labelExtn = fileExtn(labelFile)
        if (labelExtn == 'csv') {
            lblInfo = .getCSVLabels(labelFile)
        } else {
            lblInfo = NULL
        }
        for (var in names(lblInfo)) {
            lblList = lblInfo[[var]]
            if (var %in% names(survey)) {
                survey[[var]] = factor(survey[[var]],lblList[['levels']],lblList[['labels']])
            }
        }
    }

    # save R and csv files and return data
    if (!is.null(dputFile)) {dput(survey,file=dputFile)}
    if (saveCsv && (fileExtn(dataFile) != 'csv'))
        write.csv(data, file = paste0(myBasename(dataFile, T),'.csv'))
    if (twoHeader) {
        list(survey, h2)
    } else {
        survey
    }

}

#' Load MySQL Table
#'
#' Loads table from EMI's MySQL database as a data frame. Must define
#' the database name, table name, and your username and password. Selects
#' all records of the table by default, there is an option to run a
#' specific query to select data.
#' @param user MySQL username
#' @param password MySQL password
#' @param database MySQL database name
#' @param table MySQL table name
#' @param query query to run to select specific data. By default, selects
#' all records from the table
#' @param ... not used
loadSQL = function(user, password, database, table, query = NULL, ...) {
    emiHost = '10.100.10.250'
    mydb = RMySQL::dbConnect(RMySQL::MySQL(), user=user, password=password,
                     dbname = database, host = emiHost)

    tables = RMySQL::dbListTables(mydb)

    if (table %in% tables) {
        if (is.null(query)) {
            rs = RMySQL::dbSendQuery(mydb, paste('select * from', table))
        } else {
            rs = RMySQL::dbSendQuery(mydb, query)
        }
        data = RMySQL::fetch(rs, n=-1)
    } else {
        stop(paste('Table does not exist in database:', table))
    }
}

#' getCSVLabels
#'
#' Gets variable and value labels from a .csv file. Default is from spss output
#' table.
#' @param labelFile file path to label file.
#' @return list of variables where each variable is a list that contains a
#' 'labels' vector and a 'values' vector
.getCSVLabels = function(labelFile) {
    # output is list of varLabels and valueLabels
    # valueLabels is list of each variable - contains levels and labels
    vData = read.csv(labelFile)
    start = if (vData[[1,1]] == 'Value') {2} else {1}
    vData = vData[start:dim(vData)[1],]
    vars = vData[[1]][!vData[[1]]=='']
    vIdx = sapply(vars, match, vData[[1]])
    vIdx[length(vIdx)+1] = dim(vData)[1]+1
    valueLabels = list()
    vars = as.character(vars)
    for (idx in 1:length(vars)) {
        valueLabels[[vars[idx]]] = list(levels=vData[vIdx[idx]:(vIdx[idx+1]-1),2],
                                        labels=vData[vIdx[idx]:(vIdx[idx+1]-1),3])
    }
    valueLabels
}

#' Convert Excel File to .csv
#'
#' Saves an excel file (.xls, .xlsx, .xlsm) to csv files. Options specify whether to
#' save multiple sheets to separate csv files and what the output file names
#' should be.
#' @param infile file path to input excel file (.xls, .xlsx, or .xlsm)
#' @param sheets number vector or character vector describing the sheets to save.
#' Default is all sheets are saved.
#' @param outfileNames output file string, or the string 'number' or 'name' to
#' specify the file names. Default is 'number', and output files are in
#' the format '<infile>_<sheetNumber>.csv'
excel2csv = function(infile, sheets = NULL, outfileNames = 'number', ...) {
    # may need read.xlsx or read.xlsx2 for .xlsm files (works for now)
    if( !'XLSX' %in% gdata::xlsFormats() ) gdata::installXLSXsupport()
    numSheets = gdata::sheetCount(infile)
    sheetNames = gdata::sheetNames(infile)
    if (length(sheets)==0) sheets = 1:numSheets
    for (i in 1:numSheets) {
        if (i %in% sheets || sheetNames[i] %in% sheets) {
            data = gdata::read.xls(infile, sheet = i, header = FALSE, colClasses = 'character',
                            blank.lines.skip = FALSE, ...)
            outfile = if (outfileNames == 'number') {
                paste0(myBasename(infile, T), '_', i, '.csv')
            } else if (outfileNames == 'name') {
                paste0(myBasename(infile, T), '_', i, '.csv')
            } else {
                outfileNames
            }
            print(dim(data))
            write.table(data, file = outfile, sep = ',', col.names = FALSE, row.names = FALSE)
        }
    }

}

#' Fuzzy Matching for Merging Data Frames
#'
#' Merges two data frames using one shared column. Left merges only!
#' Direct matches are checked first, followed by multiple sets of
#' fuzzy matches. A random match is chosen if multiple values match.
#' @param dfX first data frame to match. The returned data frame will have the
#' same number of rows as this data frame.
#' @param dfY second data frame to match. Note: there should be no duplicates in
#' the matching column in this data frame!
#' @param by column name (or number) in data frames to use for matching. Can only
#' be one column! By default, it is the first matching column name in dfX and dfY
#' @param byX column name in dfX if column names are different
#' @param byY column name in dfY if column names are different
#' @param costs The costs associated with string changes. See agrep for details
#' @param distance vector of maximum distances for fuzzy matching. See agrep for
#' details. Length corresponds to the number of matching iterations.
#' @param keepOriginal if True, adds the column "Original" in the final data frame
#' which contains vector.
#' @param ... parameters sent to agrep for fuzzy matching
#' @return a data frame with the same length as vector and the same columns as df.
#' The matched column will have the same name as col.
#' @seealso agrep
fuzzyMerge = function(dfX, dfY, by = intersect(names(dfX), names(dfY))[1],
                      byX = by, byY = by,
                      costs = list(ins=2, del=1, sub=3),
                      distance = c(0,1,2,3,5,7,10,15,20), keepOriginal = FALSE, ...) {
    if (! byX %in% names(dfX)) stop(print(paste(by, 'column is not in data frame X')))
    if (! byY %in% names(dfY)) stop(print(paste(by, 'column is not in data frame Y')))
    x = as.character(dfX[[byX]])
    y = as.character(dfY[[byY]])
    assert(all(! duplicated(y)), 'Column must not have duplicates', error = T)
    rows = rep(NA, length(x))
    for (d in distance) {
        unmatched = x[is.na(rows)]
        if (length(unmatched) == 0) break
        fuzzy = lapply(unmatched, agrep, x=y, max.distance=d, costs=costs)
        isMatched = sapply(fuzzy, function(x) length(x) > 0)
        rows[is.na(rows)][isMatched] = unlist(lapply(fuzzy[isMatched], function(x) if (length(x)==1) x else sample(x,1)))
        print(paste('At distance', d, 'there are', sum(is.na(rows)), 'unmatched cases.'))
    }
    newY = dfY[rows,]
    if (keepOriginal) newY$Original = newY[[byY]]
    newY[[byY]] = NULL

    cbind(dfX, newY)
}


#' ******* Cleaning Functions *******

#' Remove Bad Variables
#'
#' Removes variables in a data frame if ALL values are "bad". Bad values can
#' be specified.
#' @param df data frame
#' @param badItems vector of bad values. By default, NA is the only bad value.
rmBadVariables = function(df, badItems = c(NA)) {
    # removes columns in data.frame if all values are in badItems
    isGood = sapply(df, function(x) !all(x %in% badItems))
    df[isGood]
}
