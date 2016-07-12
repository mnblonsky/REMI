# ---
# title: "EMI General R Functions"
# author: "EMI Consulting"
# date: "January 27, 2015"
# ---

# ******* General Functions *******
. = NULL

#' Only Numbers
#'
#' Removes all non-numeric characters
#' @param x input string
onlynumbers <- function (x)  as.numeric(gsub("[^0-9]","",x))

#' Remove Commas
#'
#' Removes commas from string
#' @param x input string
rmcommas <- function (x) as.numeric(as.character(gsub(",", "", x)))

#' mySplit
#'
#' Splits a string by a separator. Similar to strsplit, but returns a vector
#' instead of a list. Works well for a single string, or for a vector if
#' you include the index that you want returned.
#' @param string input character vector to split
#' @param split the separator used to split string
#' @param index optional, indices of split string to return.
#' Default returns the full split string
#' @param reverseIdx if TRUE, reference indices from the end of the string.
#' Default is FALSE
#' @param collapse if TRUE and there are multiple indices,
#' collapses all indices into 1 string using split. Default is FALSE
#' @param ... extra arguments passed to strsplit()
#' @examples
#' mySplit('This_That', '_')
#' mySplit('This_That', '_', 2)
#' mySplit('This_That_Another', '_', -1, reverseIdx = TRUE)
#' @return character vector of split strings.
mySplit = function(string, split, idx=NA,
                   reverseIdx = FALSE, collapse = FALSE, ...) {
    splitList = function(x) {
        x = unlist(strsplit(x, split, ...))
        if (! reverseIdx) x[idx] else rev(rev(x)[idx])
    }
    if (is.na(idx)) return(unlist(strsplit(string, split, ...)))
    splits = lapply(string, splitList)
    if (! collapse) unlist(splits) else
        sapply(splits, paste0, collapse = split)
}

#' isnothing
#'
#' True if v is NA, NULL, or NaN. Returns a boolean vector of length v.
isnothing = function(v) {
    is.null(v) | is.na(v) | is.nan(v)
}

#' assert
#'
#' Asserts if a value is True. If False, throws a warning (default) or error
#' @param expression expression that should be True
#' @param message warning or error message (default is "Assert Error: Value is False")
#' @param error turns the warning into an error (stops the program execution)
#' @return value of the expression
assert = function(expression, message=NULL, error = FALSE) {
    message = if (is.null(message)) "Assert Error: Value is not True" else message
    if (is.na(expression) | ! expression) {
        if(error) stop(message) else warning(message)
    }
    expression
}

#' All Equal
#'
#' Checks if all values in a vector are the same. Uses assert function.
#' @param v vector
#' @param error if True, stops the program execution
#' @return value of each item in the vector if True, else NA
allEqual = function(v, error = FALSE) {
    if (all(isnothing(v))) return(v[1])
    msg = "Assert Error: All values are not equal"
    if (assert(all(v[1]==v), msg, error)) as.vector(v[1]) else NA
}

#' Almost Equal
#'
#' Determines if two numbers are almost equal, based on rounding by significant
#' digits. Does 2 rounds of rounding.
#' @param v1 first vector
#' @param v2 second vector
#' @param digits primary rounding digits
#' @param compError digits for computer error (default is 15)
almostEqual = function(v1, v2, digits, compError = 15) {
    v1 = signif(signif(v1, compError), digits)
    v2 = signif(signif(v2, compError), digits)
    v1 == v2
}

#' Convert a .csv Number to a Date
#'
#' Converts number from a csv file to a date. Based on Excel's date format (number
#' of days since 1900). For example, the number 42014 corresponds to Jan. 12, 2015.
#' @param num number from csv file
#' @return date object (POSIXct)
csvNum2Date = function(num) {
    # Excel uses origin of 1900 and uses days instead of seconds
    secPerDay = 60*60*24
    as.POSIXct(as.numeric(num*secPerDay),origin=as.POSIXct('1900-01-01'))
}

#' ******* File System Functions *******

#' File Extension
#'
#' Returns file extenstion (ex. "csv" or "xls")
#' @param fName file name
fileExtn = function(fName) tail(unlist(strsplit(fName,'.',T)),1)

#' myBasename
#'
#' By default, returns the file name without the full path or the file extension.
#' It can include folders and include the file extension.
#' @param fName full file path
#' @param includeFolders default is FALSE
#' @param includeExtn default is FALSE
myBasename = function(fName, includeFolders = FALSE, includeExtn = FALSE) {
    fName = if (includeFolders) fName else basename(fName)
    if (includeExtn) {
        fName
    } else {
        mySplit(fName, "\\.", -1, reverseIdx = TRUE, collapse = TRUE)
    }
}

#' Make Default Analysis Folders
#'
#' Creates EMI analysis folder template, including subfolders for:
#'
#' - Input Data
#' - Figures
#' - Working
#' @param mainFolder main analysis folder
#' @return data frame of subfolders
createAnalysisFolders = function(mainFolder, addCSS = FALSE) {
    if (! file.exists(mainFolder)) dir.create(mainFolder)
    df = data.frame(main = mainFolder,
                    data = file.path(mainFolder, 'Input Data'),
                    figures = file.path(mainFolder, 'Figures'),
                    working = file.path(mainFolder, 'Working'),
                    stringsAsFactors = FALSE)
    apply(df, 2, dir.create, showWarnings = FALSE)
    if (addCSS && ! file.exists(file.path(mainFolder, 'custom.css'))) getCSS(mainFolder)
    df
}

#' Secure Server
#'
#' Returns the secure server main directory. May only work for macs.
secureServer = function() {
    fNames = list.files('/Volumes/')
    fNames = fNames[grepl('Secure', fNames)]
    for (fName in fNames) {
        full = file.path('/Volumes', fName)
        if (length(list.files(full)) > 0) return(full)
    }
    stop('Could not find secure server.')
}

REMI_Extras = function() {
    file.path(secureServer(), 'Code Library', 'R', 'REMI_Extras')
}

#' addMyShellPath
#'
#' Adds extra folder to R path if necessary. Used to implement python, which can
#' convert Excel files to .csv files (which R can read). If this function does not work,
#' the best solution is to open the Excel file and save it as a .csv file. Then tell
#' R to load the .csv file instead.
addMyShellPath = function(path = NULL) {
    myPath = if (is.null(path)) REMI_Extras() else path
    curPath = Sys.getenv('PATH')
    if (! myPath %in% mySplit(curPath, ':')) {
        print('Updating path for python capability')
        Sys.setenv(PATH=paste(curPath, myPath, sep=':'))
    }
}

#' Load Proxima Nova Font
#'
#' Imports the Proxima Nova font (found in the REMI directory on the secure
#' server) and loads it for pdf and postscript graphics. Returns TRUE on
#' success.
loadProximaNova = function() {
    extrafont::loadfonts(quiet = TRUE)
    extrafont::loadfonts(device = 'postscript', quiet = TRUE)
    if (! 'Proxima Nova Regular' %in% extrafont::fonttable()$FullName) {
        font_import(REMI_Extras())
        extrafont::loadfonts(quiet = TRUE)
        extrafont::loadfonts(device = 'postscript', quiet = TRUE)
    }
    'Proxima Nova Regular' %in% extrafont::fonttable()$FullName
}

#' Get EMI CSS File
#'
#' Adds the default EMI CSS file to the chosen directory. Necessary for use in
#' generating "pretty" HTML files, for example using R Markdown (Rmd).
#' @param dir directory to copy the CSS file to. It should be the same as the
#' directory of the analysis file.
getCSS = function(dir) {
    cssFile = file.path(REMI_Extras(), 'custom.css')
    file.copy(cssFile, file.path(dir, 'custom.css'))
}
