# ---
# title: "Reporting Functions for Projects"
# author: "EMI Consulting"
# date: "January 27, 2015"
# ---
#
# Contains functions for visualizing and reporting data as tables and figures

# ******* Figure Functions *******
. = NULL

#' EMI Plot
#'
#' Sets up a ggplot with the EMI formatting defaults - excluding colors
#'
#' Typical EMI conventions for figures include:
#'
#' - No title
#' - Size 9 font, Proxima Nova Regular
#' - Major gridlines only
#'
#' Note: attempts to load and use Proxima Nova font. Defaults to Arial if
#' Proxima Nova cannot be loaded.
#' @param ... parameters for ggplot()
EMIplot = function(...) {
    EMIplot = ggplot(...)

    font = ifelse('font' %in% names(list(...)), list(...)[['font']],
        ifelse(loadProximaNova(), 'Proxima Nova Regular', 'Arial'))

    EMIplot +
        theme_minimal()+
        theme(text=element_text(size=9, family = font),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y=element_text(vjust=1),
              #panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position="bottom")
}

#' EMI Save Figure
#'
#' Saves the specified figure using ggsaev using EMI defaults. Defauts include
#' 6in x 4in size and other ggsave defaults.
#' @param filename name of the file to save the plot
#' @param ... other parameters passed to ggsave
EMIsave = function(filename, ...) {
    width = if ('width' %in% names(list(...))) list(...)$width else 6
    height = if ('height' %in% names(list(...))) list(...)$height else 4
    ggsave(filename, width = width, height = height, ...)
}

#' EMI Color Scheme
#'
#' Returns a set of colors from the EMI template. Can return a list of colors,
#' the EMI accent color (orange), or a faded range of colors.
#' @param colors vector of numbers corresponding to EMI colors
#' @param accentLoc numbers specifying index to replace with accent color
#' @param ramp number of colors to interpolate - only works if length of colors
#' is 1 (fade to black) or 2 (interpolate). Default is for no ramping.
#' @param accent accent color. EMI Orange by default
#' @param order ordered vector of colors. Default is the EMI color set.
#' @examples
#' barplot(rep(1,4), col=EMIcolors(rep(1,4), accentLoc = 3))
#' barplot(rep(1,10), col=EMIcolors(1:10)) # all EMI colors
#' barplot(rep(1,5), col=EMIcolors(1:2, ramp=5)) # for satisfaction
EMIcolors = function(colors = 1, accentLoc = NA, ramp = 0,
                     accent = NULL, order = NULL) {
    # needs fixing...
    # EMI Colors for reference
    #191919 Very dark grey mostly black
    #FFFFFF White
    #344D6D Very dark desaturated blue (3)
    #ECECEF Light grayish blue (7)
    #ff6d2e Vivid orange (2, accent)
    #4372b9 Moderate blue (1)
    #808080 Trolley grey; dark grey (6)
    #5ba0d7 Lighter moderate blue (5)
    #f19f2a Bright orange (faded) (4) OR #df893e
    #6fccdc Soft cyan (light aqua) (8)
    #f4ce25 Bright yellow (9)
    #71903d Dark moderate green (10)
    accent = if (is.null(accent)) "#ff6d2e" else accent
    shade = "#191919"
    neutral="#BBBBBB"
    order = if (is.null(order)) c("#4372b9", "#ff6d2e", "#808080", "#344D6D", "#df893e", "#5ba0d7",
                                  "#71903d", "#6fccdc", "#f4ce25", "#ECECEF", "#191919") else order
    if (ramp==0) {
        colors = order[colors]
    } else if (length(colors)==1) { # shade to dark
        colors = colorRampPalette(c(order[colors], shade))(ramp+2)[1:ramp]
    } else if (length(colors)==2) { # used for satisfaction - neutral color in the middle
        colors = colorRampPalette(c(order[colors[1]], neutral, order[colors[2]]))(ramp)
    } else {
        colors = colorRampPalette(order[colors])(ramp)
    }
    if (! is.na(accentLoc)) colors[accentLoc] = accent
    colors
}

#' Show EMI Colors
#'
#' Displays a barplot of EMI's color scheme.
#' @param colors list of colors, by number, to show. Defaults to all 10 colors
EMIcolorsShow = function(colors = 1:10, ...) {
    df = data.frame(X = factor(as.character(colors),
                               levels = as.character(colors)),
                    Count = rep(1, length(colors)))
    EMIbar(df, colors = EMIcolors(colors), ...)
}

#' @rdname EMIcolors
Navigantcolors = function(...) {
    order = c("#A74B22", "#EEAF2E", "#167D79", "#0B5499", "#576B14", "#801670")
    EMIcolors(..., order = order)
}

#' @rdname EMIcolors
Navigantcolors2 = function(...) {
    order = c("#A15F15", "#546A21", "#ECB02E", "#0A549B", "#6D6855", "#80176B", "#A62621")
    EMIcolors(..., order = order)
}


#' EMI Bar Graph 1-D
#'
#' Creates generic EMI bar graph. Takes a data frame (or data from myTable) to
#' create a 1-D plot. Colors are customizable.
#' @param data a data frame with columns X (bar names), and Count (bar size).
#' Works best with data from the myTable function.
#' @param colors vector of colors with length of 1 or the same length as data.
#' @param dataCols length 2 vector specifying the location of data within the
#' data frame. The first column should have category info, the second should have
#' counts.
#' Default colors are all EMI Blue (EMIcolors(1))
EMIbar = function(data, colors = EMIcolors(1), dataCols = 1:2, ...) {
    names(data)[dataCols] = c('X', 'Count')
    EMIplot(data, aes(x=X, y=Count), ...) +
        geom_bar(fill=colors, stat='identity') +
        labs(title=NULL, x=NULL) +
        coord_flip()
}

#' EMI Bar Graph 2-D
#'
#' Creates generic EMI bar graph. Takes a data frame (or data from myTableMulti) to
#' create a 2-D plot. Colors are customizable.
#' @param data a data frame with columns X (bar names), Fill (bar color),
#' and Count (bar size). Works best with data from the myTableMulti function.
#' @param colors vector of colors with length of 1 or the same length as the
#' number of Fill options. Default colors are all EMI Blue (EMIcolors(1))
#' @param ... parameters passed to geom_bar
EMIbarMulti = function(data, colors = NULL) {
    # needs updating - color defaults?
    fillLen = length(levels(data$Fill))
    xLen = length(levels(data$X))
    colors = if (is.null(colors)) EMIcolors(1:fillLen) else colors
    EMIplot() + geom_bar(aes(x=X, y=Count, fill=Fill),
                         data=data,
                         stat='identity') +
        labs(title=NULL, x=NULL) + coord_flip() +
        scale_fill_manual(values=rev(colors))
}

#' EMI Modified Cumulative Distribution Fuction (CDF) Graph
#'
#' Creates a CDF graph from the cumulative sum of a vector.
#' Takes a numeric vector, computes the cumulative sum of the vector,
#' then plots a modified CDF using EMIplot.
#' Plots the percent cumulative values on the y-axis
#' and the actual data values (in ascending order) on the x-axis.
#' @param vector a numeric vector
#' @param lineColor color of the line to plot. Defaults to EMI blue
#' @examples
#' plt = EMIcdf(1:1000)
#' @seealso ecdf
EMIcdf <- function(vector, lineColor = EMIcolors(1)) {
    options(scipen=999) # Avoid sci notation

    vector <- sort(vector) # Sort the vector
    vectorcum <- cumsum(as.numeric(vector)) # Compute the cumulative sum
    vectorcum.perc <- vectorcum / sum(as.numeric(vector)) # Compute the percent of the cumulative sum.

    df <- data.frame(vector,vectorcum,vectorcum.perc) # Create temporary dataframe containing the three vectors to graph from.

    # Plot the result.
    EMIplot(data=df, aes(x=vector, y=vectorcum.perc)) +
        geom_line(color=lineColor) +
        xlab(paste0("Values")) +
        ylab("Cumulative %") +
        scale_y_continuous(labels = scales::percent)
}

#' EMI Likert Plot
#'
#' EMIlikert(): function to make an EMI color formatted likert plot
#' @param df is a dataframe with likert responses as columns; can contain other data: caseids and groups (like type)
#' @param likertcols is a vector of columns of likert responses to plot
#' @param group is a column in the data referring to groups
#' @param ... variables passed to col.strip.background
#' @return a dataframe object with the ID, code, name, start date, end dte, and health of each project
#' @examples
#' mydf = data.frame(group = rep(1:3, each = 10),
#'                   Q1 = factor(sample(1:5, 30, replace = TRUE)),
#'                   Q2 = factor(sample(1:5, 30, replace = TRUE)))
#' my_likert_plot <- EMIlikertPlot(mydf, 2:3)
#' my_likert_plot
EMIlikertPlot <- function(df, likertcols, group=NULL, ...){
  # df is df
  # likertcols is vector of logical representing columns in df that are likert Qs to plot
  # group is varname in df that want to group on
  qlikerts <- df[,likertcols]
  if (!is.null(group)) {
    if (length(group)==1) {
      mygroup <- df[[group]]
    } else { # doesn't work yet dunno why. max group = 1 now
      stop("Can only have one group in this function, use regular likert")
      #mygroup <- paste0(df[group[1]],df[group[2]])
    }
     #mygroup <- df[[group]]
  } else {
    mygroup = NULL
  }
  ldf <- likert::likert(qlikerts, grouping=mygroup)
  # ! make palette
  colnoplot <- length(unique(qlikerts[[1]])) #number of items in likert scale
  is.odd <- function(x) x %% 2 != 0
  if(is.odd(colnoplot)) {
			EMIlikertcol <- c('#4372b9','#5ba0d7','#ECECEF','#FDA785','#ff6d2e')
		} else {
			EMIlikertcol <-c('#4372b9','#5ba0d7','#FDA785','#ff6d2e')
		}
  thisPalette <- colorRampPalette(EMIlikertcol)(colnoplot)
  plot(ldf,
       col=thisPalette,
       col.strip.background='#ECECEF',...)
}


# ****** Table Functions *******

#' myTable
#'
#' Creates a 1-D table (i.e. from 1 factor) in a nice format. Useful for converting
#' to a printable table or for converting to a figure (using EMIbar).
#' @param table any 1-D table, or an object that can be converted to a two
#' column data frame
#' @param name string for first column header
#' @param sort sort table from highest to lowest (default is TRUE)
#' @param percent add column indicating percentages (default is FALSE)
#' @param total add row indicating total (default is FALSE)
#' @param sum total number to base percentages (default is the sum of all values)
#' @param remove vector of row names to remove (ex. NA)
#' @param bottom vector of row names to keep at the bottom of the table (ex. Other)
myTable = function(table, name = 'X', sort = T, percent = F, total = percent,
                   sum = NULL, remove = c(), bottom = c()) {
    df = as.data.frame(table, stringsAsFactors = FALSE)
    df = if (ncol(df)==3) {
        df[,c(1,3)]
    } else if (ncol(df)==1) {
        data.frame(rownames(df), df[,1])
    } else {
        df
    }
    if (!nrow(df)) return(NULL)

    names(df) = c(name, 'Count')
    df = df[! df[[1]] %in% remove,]
    row.names(df) = NULL

    if (sort) {
        sortedIdxAll = order(df$Count, decreasing = T)
        isInBottom = (df[[1]] %in% bottom)[sortedIdxAll]
        bottomIdx = unlist(sapply(bottom, function(x) which(df[[1]] %in% x)))
        df = rbind(df[sortedIdxAll[! isInBottom],], df[bottomIdx,])
        row.names(df) = NULL
    }
    df[[1]] = as.character(factor(df[[1]], levels = rev(df[[1]])))

    # add percent column, total row, or (n=XX) in header
    sumN = if (is.null(sum)) sum(df[['Count']]) else sum
    if (total && !is.null(sum)) names(df)[2] = paste0('Count (n=', sumN, ')')
    if (total &&  is.null(sum)) df = rbind(df, list('Total', sumN))

    if (percent) df$Percent = scales::percent(df$Count / sumN)
    df
}

#' myTableMulti
#'
#' Creates a 2-D table (i.e. from 2 factors) in a nice format. Useful for converting
#' to a printable table or determining percentages or totals.
#' @param table any 2-D table, or data frame that resembles a table
#' @param name Left-most column name (describing rows). Defaults to ""
#' @param rowNames names for table rows (pre-sorted) - defaults to table row names
#' @param colNames names for table columns (pre-sorted), EXCLUDING leftmost row
#' @param sortRow vector of column names/numbers to sort by (highest to lowest)
#' @param sortCol vector of column names/numbers to sort by (left to right)
#' @param percent convert all values to percentages.
#' Can evaluate percentages by 'row', 'col', or 'all'
#' @param totalRow add row indicating total (default is FALSE)
#' @param totalCol add column indicating total (default is FALSE)
#' @param totalN add (n=#) to row/col names instead of a new row/column (default is FALSE)
#' @param revRow reverse order of rows
#' @param revCol reverse order of columns
#' @param rmCol vector of column names to remove
#' @param botCol vector of column names to keep at the right of the table
#' @param rmRow vector of row names to remove
#' @param botRow vector of row names to keep at the bottom of the table
myTableMulti = function(table, name = "", rowNames = NULL, colNames = NULL,
                        sortRow = NULL, sortCol = NULL, percent = '',
                        totalRow = F, totalCol = F, totalN = F,
                        revRow = F, revCol = F,
                        rmCol = c(), botCol = c(),
                        rmRow = c(), botRow = c(),
                        ...) {
    df = as.data.frame.matrix(table)
    if (!nrow(df)) return(NULL)
    if(is.null(rowNames)) rowNames = row.names(df)
    if(is.null(colNames)) colNames = names(df)

    # remove and sort - rows and then columns
    df = df[! rowNames %in% rmRow , ! colNames %in% rmCol]
    if (! is.null(sortRow)) {
        bottom = which(rowNames %in% botRow)
        sortR = order(sortRow, decreasing = T)
        sortR = c(sortR[! sortR %in% bottom], bottom)
        df = df[sortR, ]
        rowNames = rowNames[sortR]
    }
    if (revRow) df = df[rev(1:nrow(df)), ]
    if (! is.null(sortCol)) {
        right = which(colNames %in% botCol)
        sortC = order(sortCol, decreasing = T)
        sortC = c(sortC[! sortC %in% right], right)
        df = df[, sortC]
        colNames = colNames[sortC]
    }
    if (revCol) df = df[, rev(1:length(df))]

    # add totals and percent
    if (totalRow) {
        rSums = rowSums(df)
        if (! totalN) {
            df$Total = rSums
            colNames = c(colNames, 'Total')
        } else rowNames = paste0(rowNames, ' (n = ', rSums, ')')
    }
    if (totalCol) {
        cSums = colSums(df)
        if (! totalN) {
            df = rbind(df, cSums)
            rowNames = c(rowNames, 'Total')
        } else colNames = paste0(colNames, ' (n = ', cSums, ')')
    }
    if (percent == 'row') dfp = scales::percent(apply(df, 1, function(x) x/sum(x[colNames != 'Total'])))
    if (percent == 'col') dfp = scales::percent(apply(df, 2, function(x) x/sum(x[rowNames != 'Total'])))
    if (percent == 'all') dfp = scales::percent(as.matrix(df / sum(df[rowNames != 'Total', colNames != 'Total'])))
    if (percent != '') df = as.data.frame(matrix(dfp, nrow = nrow(df)))
    # update names, and return df
    df = cbind(rows = rowNames, as.data.frame.matrix(df))
    names(df) = c(name, colNames)
    row.names(df) = NULL
    df
}


#' Print Table
#'
#' Prints and saves table or data frame with specified formats.
#' @param table table-like R object (best results are with data frames)
#' @param file file path to save table. Default does not save the table
#' @param type format of output (default is HTML)
#' @param include.rownames Should row names be printed? (default is FALSE)
#' @param addCommas converts numbers to strings (1234 -> "1,234")
#' @param ... extra parameters sent to xtable(table, ...)
printTable = function(table, file = NULL, type = 'HTML',
                      include.rownames = FALSE, addCommas = FALSE,
                      ...) {
    if (is.null(table)) stop('No data in table')
    format.args = if (addCommas) list(big.mark = ',') else list()

    print(xtable::xtable(table, ...), type = type, include.rownames = include.rownames,
          format.args = format.args)

    if (!is.null(file)) write.csv(table, file = file)
}

# ******* KnitR Functions *********

#' EMI KnitR Setup
#'
#' Sets up formatting to knit (or compile) an R notebook. Optimized for HTML
#' outputs, although it should work for PDF or Word outputs as well.
#' @param tempPath folder path to temporarily save files during compilation.
#' Currently only used for figures.
EMIknitSetup = function(tempPath = NULL) {
    if (is.null(tempPath)) tempPath = ''
    knitr::opts_chunk$set(message = FALSE, echo=FALSE, warning=FALSE,
                          fig.path = file.path(tempPath, 'Figs/'))

    ztn = local({
        i = 0
        function(x) {
            i <<- i + 1
            paste('<b>\n\nTable ', i, ': ', x, '</b>',sep = '')
            #  wraps your caption in <caption></caption>
        }
    })
    knitr::knit_hooks$set(tab.cap = function(before, options, envir) {
        if(before)
            ztn(options$tab.cap)
    })
    zfn = local({
        i = 0
        function(x) {
            i <<- i + 1
            paste('<b>\n\nFigure ', i, ': ', x,  '</b>',sep = '')
            #  wraps your caption in <caption></caption>
        }
    })
    knitr::knit_hooks$set(fig.cap = function(before, options, envir) {
        if(before)
            zfn(options$fig.cap)
    })

}

#' Print Knitted List
#' Prints a vector of strings as a list in KnitR format. Works for
#' markdown and latex output formats.
#' @param x vector of strings
#' @param out.format either 'markdown' or 'latex'.
#' @param environment used for latex formatting
#' @param marker used for latex formatting
printList <- function(x, out.format = knitr::opts_knit$get("out.format"),
                      environment = "itemize",
                      marker = NULL) {
    if (out.format == "markdown") {
        if (environment != 'itemize' || is.null(marker)) {
            warning("Ignoring arguments that are not supported for markdown output.")
        }
        out <- sprintf("\n\n%s\n \n", paste("*", x, collapse = "\n"))
    } else {
        if (out.format == "latex") {
            itemCommand <- if (missing(marker)) {
                "\\item"
            } else {
                sprintf("\\item[%s]", marker)
            }
            listEnv <- c(
                sprintf("\\begin{%s}\n", environment),
                sprintf("\n\\end{%s}\n", environment))
            out <- paste(itemCommand, x, collapse = "\n")
            out <- sprintf("%s%s%s", listEnv[1], out, listEnv[2])
        } else {
            stop("Output format not supported.")
        }
    }
    return(knitr::asis_output(out))
}

#' Histogram Frequencies
#'
#' Turns a vector (or factor) into a data frame used for a table.
#' Input must be turned into numeric once 'others' are removed.
#' @param v vector to analyze
#' @param increment number to increment histogram bins (ex. 1)
#' @param others non-numeric levels to remove
histFreqs = function(v, increment, others = c(), ...) {
    # turns a vector (factor) into a data frame used for a table
    # input must be turned into numeric once 'others' are removed
    # others are non-numeric levels
    otherCounts = sapply(others, function(x) sum(v==x))
    vNum = as.numeric(v[sapply(v, function(x) !x %in% others)])

    # Make histogram
    hist = hist(vNum, plot=FALSE, ...)
    nBins = hist[['breaks']]
    nCounts = hist[['counts']]

    # make bin strings
    binStrings = paste(nBins[1], '-', nBins[2])
    for (x in 3:length(nBins)) {
        binStrings = c(binStrings, paste(nBins[x-1] + increment, '-', nBins[x]))
    }

    # return data frame of levels and counts
    data.frame(levels = c(binStrings, others),
               counts = c(nCounts, otherCounts))
}
