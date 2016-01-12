#' ---
#' title: "Analysis Functions for Projects"
#' author: "EMI Consulting"
#' date: "January 27, 2015"
#' ---
#
#' Contains functions for analyzing, visualizing, and reporting data

#' ### Analysis Functions
. = NULL

#' Weighted Variance
#'
#' Returns the weighted variance of a vector. A numeric vector and its weights
#' must be provided.
#'
#' For 'tapply' or 'by' compatibility, the function can take a matrix or data frame
#' as long as the vector and weight column locations are specified.
#' @param x a numeric vector. Alternatively, x can be a matrix-like object that
#' includes data for the vector and the weights.
#' @param w a numeric vector of weights.
#' @param xLoc if x is matrix-like, this specifies the column containing vector
#' information. Can be the column number or name.
#' @param wLoc if x is matrix-like, this specifies the column containing weight
#' information. Can be the column number or name.
weighted.var <- function(x, w = NULL, xLoc = 2, wLoc = 3, na.rm = FALSE) {
    if (! is.vector(x)) {
        w = if (is.null(w)) x[[wLoc]] else w
        x = x[[xLoc]]
    }
    if (na.rm) {
        w <- w[i <- !is.na(x)]
        x <- x[i]
    }
    sum.w <- sum(w)
    (sum(w*x^2) * sum.w - sum(w*x)^2) / (sum.w^2 - sum(w^2))
}

#' Weighted Mean
#'
#' Returns a weighted mean given a vector and weights.
#'
#' For 'tapply' or 'by' compatibility, the function can take a matrix or data frame
#' as long as the vector and weight column locations are specified.
#' @param x a numeric vector. Alternatively, x can be a matrix-like object that
#' includes data for the vector and the weights.
#' @param w a numeric vector of weights.
#' @param xLoc if x is matrix-like, this specifies the column containing vector
#' information. Can be the column number or name.
#' @param wLoc if x is matrix-like, this specifies the column containing weight
#' information. Can be the column number or name.
weighted.mean <- function(x, w = NULL, xLoc = 2, wLoc = 3, na.rm = FALSE) {
    if (! is.vector(x)) {
        w = if (is.null(w)) x[[wLoc]] else w
        x = x[[xLoc]]
    }
    if (na.rm) {
        nas = is.na(w) | is.na(x)
        w = w[! nas]
        x = x[! nas]
    }
    sum(x*w) / sum(w)
}

#' Weighted Variance - Standard Error
#'
#' Returns the weighted standard error of a vector. A numeric vector and its weights
#' must be provided.
#'
#' For 'tapply' or 'by' compatibility, the function can take a matrix or data frame
#' as long as the vector and weight column locations are specified.
#' @param x a numeric vector. Alternatively, x can be a matrix-like object that
#' includes data for the vector and the weights.
#' @param w a numeric vector of weights.
#' @param xLoc if x is matrix-like, this specifies the column containing vector
#' information. Can be the column number or name.
#' @param wLoc if x is matrix-like, this specifies the column containing weight
#' information. Can be the column number or name.
weighted.var.se = function(x, w = NULL, xLoc = 2, wLoc = 3, na.rm = FALSE) {
    #  Computes the variance of a weighted mean following Cochran 1977 definition
    if (! is.vector(x)) {
        w = if (is.null(w)) x[[wLoc]] else w
        x = x[[xLoc]]
    }
    if (na.rm) {
        w <- w[i <- !is.na(x)]
        x <- x[i]
    }
    n = length(w)
    xWbar = weighted.mean(x,w,na.rm=na.rm)
    wbar = mean(w)
    out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
    return(out)
}


#' Sample Size Calculator
#'
#' Determines the required sample size based on the population size, levels of
#' confidence and precision, and coefficient of variation (CV) of the variable of
#' interest.
#' @param N Population Size
#' @param P Level of Confidence (~90 \%)
#' @param A Level of Precision (~10 \%)
#' @param cv Coefficient of Variation (~0.5)
sample.size = function(N, P, A, cv) {
    z = qnorm((P+1)/2)
    n = (z * cv / A)^2
    if (N < 200) {
        n / (1 + n/N)
    } else {
        n
    }
}

#' Find Outliers
#'
#' Determines if there are any outliers in a set of data. By default, it assumes
#' a normal distribution and removes data beyond 5 std. deviations.
#' @param v input vector
#' @param sd number of standard deviations to determine if a data point is an
#' outlier. Default is 5.
#' @param method method used to determine outliers. Default is to assume a
#' normal distribution
outliers = function(v, deviations = 5, method='normal') {
    if (length(v)==1) return(TRUE)
    if (all(isnothing(v))) return(v)
    m = mean(v)
    sd = sd(v)
    if (method=='normal') {
        v >= m - sd*deviations & v <= m + sd*deviations
    }

}

#' Most Common Item
#'
#' Returns the most common item(s) (the mode) in a vector.
#' @param v vector or factor
mostCommon = function(v) {
    t = table(v)
    x = names(t)[t == max(t)]
    as.vector(x, mode = mode(v))
}

#' Duplicate Information
#'
#' Determines the number of unique, duplicate, and missing values in a vector
#' @param x vector of interest
#' @param missingVals values within x to consider as missing. Defaults to
#' considering empty strings and NAs as missing
duplicateInfo = function(x, missingVals = c('', NA))
    c(length(unique(x[! x %in% missingVals])),
      sum(duplicated(x[! x %in% missingVals])),
      sum(x %in% missingVals))

# TODO: option for dfQuestions to be character vector of questions, or just the useful text!

#' qualtricsColumns
#'
#' Runs a desired function on multiple columns of a data frame with similar
#' column names (ex. Q1_1, Q1_2, ...). Defaults are useful for Qualtrics data
#' structures.
#' @param df main data frame
#' @param q string representing question name (ex. "Q1_")
#' @param id vector specifying columns (1,2,3...)
#' @param dfQuestions data frame with same columns as df - contains question
#' strings
#' @param func function to run on cleaned data. An example could be fixing text
#' (removing a ":") or converting to a different class. Default is no function.
#' @param remove vector of strings/objects to remove from each column (ex. NA, NaN)
#' @param questionChar character to help specify data in the question. Default is
#' "-", which Qualtrics uses to show the question options.
#' @param charInQ number of times that questionChar shows up in the question.
#' @param includeRespondent If true, adds a column to the output specifying the row
#' from the original data frame.
#' @return a data frame containing one row for each response that was not removed.
#' The data frame can be quickly summarized or tabled, or graphed using EMI functions.
qualtricsColumns = function(df, q, id, dfQuestions = NULL, category = 'Category',
                            func = NULL, remove = c(NA, NaN, NULL, '', '-99'),
                            questionChar = '-', charInQ = 0,
                            includeRespondent = FALSE, ...) {
    # TODO: option to include row numbers (by-respondent analysis); keep all data and use remove at end
    cols = paste0(q, id)
    names(cols) = cols
    collectData = function(col, rm = remove, f = func) {
        isClean = ! col %in% rm
        data = if (is.null(f)) col[isClean] else f(col[isClean])
    }
    data = lapply(df[, cols], collectData)

    collectQuestions = function(col, qChar = questionChar, nChar = charInQ, ...) {
        split = mySplit(as.character(col), qChar, ...)
        question = paste(tail(split,-1 - nChar), collapse=qChar)
    }
    questions = if (is.null(dfQuestions)) cols else sapply(dfQuestions[, cols], collectQuestions)

    out = do.call(rbind, lapply(names(data), function(x) data.frame(Cat = questions[[x]],
                                                                    Data= data[[x]])))
    out = as.data.frame(out, stringsAsFactors = FALSE)
    names(out) = c(category, 'Values')
    out
}

#' qualtricsMatrix
#'
#' Runs a desired function on an array of columns of a data frame with similar
#' column names (ex. Q_1_1, Q_1_2, ..., Q_2_1, Q_2_2, ...).
#' Defaults are useful for Qualtrics data structures.
#' @param df main data frame
#' @param q string representing question name (ex. "Q")
#' @param idRow vector specifying the row numbers (1,2,3...)
#' @param idCol vector specifying the column numbers (1,2,3...)
#' @param dfQuestions data frame with same columns as df - contains question
#' strings
#' @param func function to run on cleaned data. An example could be fixing text
#' (removing a ":") or converting to a different class. Default is no function.
#' @param remove vector of strings/objects to remove from each column (ex. NA, NaN)
#' @param questionChar character to help specify data in the question. Default is
#' "-", which Qualtrics uses to show the question options.
#' @param charInQ number of times that questionChar shows up in the unused part of the question.
#' @param charInCol number of times that questionChar shows up in the column names
#' part of the question (the last part).
#' @return a data frame containing one row for each response that was not removed.
#' The data frame can be quickly summarized or tabled, or graphed using EMI functions.
qualtricsMatrix = function(df, q, id1, id2, dfQuestions,
                           func = NULL,
                           remove = c(NA, NaN, NULL, '', '-99'),
                           questionChar = '-',
                           charInQ = 0,
                           charInCol = 0,
                           ...) {
    # TODO: option for dfQuestions to be character vector of questions, or just the useful text!
    # also copy new code from qualColumns

    x = outer(id1, id2, paste, sep='_')
    colNames = paste0(q, x)
    v1 = character()
    v2 = character()
    v3 = character()
    for (col in colNames) {

        if (cat=='') cat = col
        v1 = c(v1, rep(cat, length(data)))
        v2 = c(v2, data)

        clean = ! df[[col]] %in% remove
        data = if (is.null(func)) df[[col]][clean] else func(df[[col]][clean])

        qString = as.character(dfQuestions[[col]])
        cat = tail(mySplit(qString, questionChar, ...),-1 - charInQ)
        rData = paste0(head(cat, -1 - charInCol), collapse=questionChar)
        cData = paste0(tail(cat,  1 + charInCol), collapse=questionChar)

        v1 = c(v1, rep(rData, sum(clean)))
        v2 = c(v2, rep(cData, sum(clean)))
        v3 = c(v3, data)
    }
    if (v2==v3) data.frame(v1=v1, v2=v2, stringsAsFactors = F) else data.frame(v1=v1, v2=v2, v3=v3, stringsAsFactors = F)
}

#' ### Statistical Tests

#' pTest
#'
#' Performs a t-test (using t.test()) to compare two numeric vectors. Can also
#' test ordinal factors (ex. for satisfaction) using type='satis5'
#' @param v1 numeric vector
#' @param v2 numeric vector
#' @param type Specifies type of vector if v1 and v2 are not numeric.
#' @param out determines return value. Default ('p') returns the p statistic.
#' @param ... Other parameters to t.test
pTest = function(v1, v2, type = '', out='p', ...) {
    # Computes p statistic from two numeric vectors
    if (type == 'satis5') {
        v1 = rep(5:1, v1)
        v2 = rep(5:1, v2)
    }
    tt = t.test(v1, v2, ...)
    if (out == 'p') tt[['p.value']] else tt
}

#' Standard Deviations from Mean
#'
#' Determines the number of standards deviations from the mean, given a certain
#' confidence. For example, 0.95 (95% confidence) will return 1.96.
#' @param Confidence percentage (as a decimal)
sdFromMean = function(confidence) {
    qnorm((1 + confidence) / 2) - qnorm(0.5)
}

#' corstarsl
#'
#' Creates a correlation table with stars for significance!!!
#'
#' *** = 0.1%   ** = 1%   * = 5%
#' @param x matrix-like R object
#' @return data frame of correlations with significance ratings
corstarsl <- function(x){
    require(Hmisc)
    x <- as.matrix(x)
    R <- rcorr(x)$r
    p <- rcorr(x)$P

    ## define notions for significance levels; spacing is important.
    mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))

    ## trunctuate the matrix that holds the correlations to three decimal
    R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]

    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")

    ## remove upper triangle
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)

    ## remove last column and return the matrix (which is now a data frame)
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
    return(Rnew)
}

#' By: For Data Frames and Vectors
#'
#' Wrapper functions for by().
#'
#' bydf converts the output to a data frame. It should only be used if the
#' output of FUN is a length-1 vector. Creates a data frame with 1 column per
#' index, plus a 'Freq' column that is the output of FUN.
#'
#' byvector converts the output to a vector. This is likely useful when calling
#' `by` with only 1 index and when the output of FUN is always the same length.
bydf = function(...) as.data.frame(as.table(by(...)))
byvector = function(...) unlist(by(...))

#'
#' ---
#'
