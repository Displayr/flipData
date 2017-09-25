#' Tidy Data For Use With Displayr Charting Functions
#'
#' For use before calling a Displayr charting function, ensures the
#' data to be used is in a convenient format for plotting
#' @param formChartType character; chart type to be plotted
#' @param subset subset An optional vector specifying a subset of
#'     observations to be used in the fitting process, or, the name of
#'     a variable in \code{data}. It may not be an expression.
#' @param weights An optional vector of sampling weights, or, the name
#'     of a variable in \code{data}. It may not be an expression.
#' @param formTable array; assumed to be a Qtable and will be
#'     processed using \code{\link[flipTables]{AsBasicTable}}
#' @param formTables list of array; each component is assumed to be a
#'     Qtable and will be processed using
#'     \code{\link[flipTables]{AsBasicTable}}
#' @param formBinary a PickAny Multi Q variable
#' @param pasted list of length six; the first component of which is
#' assumed to be from a user-entered/pasted table; will be processed
#' by \code{\link{ParseUserEnteredTable}}
#' @param raw.data data.frame, containing Variables from a Q/Displayr
#'     Data Set
#' @param formTranspose logical; should the supplied data be transposed?
#' @param number.format list containing user-supplied parameters relating
#' to number formatting in the chart
#' @param missing character; One of \code{"Error if missing data"},
#'     \code{"Exclude cases with missing data"} (the default, which is
#'     equivalent to 'complete.cases'), and \code{"Use partial data"},
#'     which removes no data; ignored if raw data is not supplied
#' @param colors list containing user-supplied values for colour
#'     parameters in charts
#' @details It is assumed that only one of \code{pasted},
#'     \code{formTable}, \code{formTables}, \code{formBinary},
#'     \code{raw.data} is non-NULL.  They are checked for nullity in
#'     that order
#' @return If possible, a named vector or matrix, or if that is not
#'     posible or a data.frame is requested, a data.frame
#' @export
PrepareData <- function(formChartType, subset, weights,
                        formTable, formTables, formBinary,
                        pasted = list(NULL),
                        raw.data = NULL,
                        formTranspose = FALSE,
                        number.format = list(NULL),
                        missing, colors = list(NULL))
{

    is.pasted <- !is.null(pasted[[1L]])
    data <- processDataArgs(pasted = pasted, formTable = get0("formTable"), formTables = get0("formTables"),
                            formBinary = get0("formBinary"), raw.data = get0("raw.data"))
    is.raw.data <- attr(data, "raw.data")

    # Processing pasted or manually inputted data.
    if (is.pasted)
    {
        data <- ParseUserEnteredTable(data[[1]],
                                      want.data.frame = data[[2]],
                                      want.factors = data[[3]],
                                      want.col.names = data[[4]],
                                      want.row.names = data[[5]],
                                      us.format = data[[6]])
        if (is.data.frame(data)) # Raw data aka data.frame
            is.raw.data <- TRUE
    }else if (is.raw.data)
    {  # formBinary is non-NULL or raw.data is non-NULL
        data <- flipData::TidyRawData(data, subset = subset, weights = weights,
                                          missing = missing)
        weights <- attr(data, "weights")
    }

    ## Aggregate if raw data or convert to tidy table(s)
    data <- if (is.raw.data)
            {
                aggregateDataForCharting(raw.data, weights, formChartType)
            }
            else if(inherits(data, "list"))
            {  # user provided multiple existing tables
                lapply(data, AsBasicTable)
            }
            else
                AsBasicTable(data)

    ## Switching rows and columns
    if (!is.null(formTranspose) && formTranspose)
        data <- t(data)

    ## Processing number formats
    x.number.format <- getNumberFormat(number.format)

    ## Processing color data
    series.colors <- getColourPars(data, colors, formChartType)

    list(data = data,  # if (!is.null(table)) table else raw.data,
         weights = weights,
         series.colors = series.colors,
         x.number.format = x.number.format,
         chart.type = switch(formChartType,
                                   "Venn Diagram" = "Venn",
                                   "Stream Graph" = "Streamgraph",
                                   "Column Chart" = "ColumnChart",
                                   "Stacked Column Chart" = "StackedColumnChart",
                                   "Pie Chart" = "PieChart",
                                   "Scatter Plot" = "LabeledScatterChart"

                             ))
}

#' Get Data for charting
#'
#' Processes form variables to get first non-NULL
#' @param ... named components of data components
#' @details currently recognized components are formTable,
#' formTables, formBinary, pasted, raw.data
#' @return the first non-NULL component \code{...} with an added attribute
#' indicating if the data is raw data
#' @noRd
processDataArgs <- function(...)
{  #
    args <- list(...)
    non.null.idx <- which(!vapply(args, function(x) is.null(unlist(x)), FALSE))[1]
    if (is.na(non.null.idx))
        stop("no data supplied")
    structure(args[[non.null.idx]],
              raw.data = names(args)[non.null.idx] %in% c("raw.data", "formBinary", "pasted"))
}

#' Aggregrate Raw Data For Charting
#' @param data \code{data.frame} containing raw data
#' @param weights numeric vector of weights
#' @param chart.type character; type of chart to be plotted
#' @return aggregated data
#' @noRd
#' @importFrom flipStatistics Table WeightedTable
aggregateDataForCharting <- function(data, weights, chart.type)
{
    if (!chart.type %in% c("Scatter Plot", "Bubble Chart"))
    {
        if (ncol(data) == 1)
        {
            out <- flipStatistics::WeightedTable(data[[1]]) #, weights)
            out <- prop.table(out) * 100
            out <- as.matrix(out)
        }
        else if (ncol(data) == 2)
        {
            names(data) <- c("x", "y")
            data$w <- if (is.null(weights)) rep(1, nrow(data)) else weights
            out <- flipStatistics::Table(w  ~ x + y, data = data, FUN = sum)
            if (chart.type == "Pie") # Total %
                out = prop.table(out, 1:2) * 100
            else # Column %
                out <- prop.table(out, 2) * 100
         }
    }
    out
}

#' Process user-specified number format for charting
#'
#' @return character string number format or \code{NULL}
#' if none specified
#' @noRd
getNumberFormat <- function(num.format){
    if (is.null(num.format[[1L]]))
        NULL
    else
    {
        if (!is.null(num.format[[2L]])) num.format[[2L]] else
            switch(num.format[[1L]],
                   "Number" = "Number",
                    "YY (Year, 2 digit)" = "%y",
                    "DD Mon YY" = "%d %b %y",
                    "DD Month YY" = "%d %B %y",
                    "DD MM YY" = "%d %m %y",
                    "YYYY (Year, 4 digit)" = "%Y",
                    "DD Mon YYYY" = "%d %b %Y",
                    "DD Month YYYY" = "%d %B %Y",
                    "DD MM YYYY" = "%d %m %Y",
                    "Mon DD YY" = "%%b d %y",
                    "Month DD YY" = "%B %d %y",
                    "MM DD YY" = "%m %d %y",
                    "Mon DD YYYY" = "%b %d %Y",
                    "Month DD YYYY" = "%B %d %Y",
                    "MM DD YYYY" = "%m %d %Y",
                    "YY Mon DD" = "%y %b %d",
                    "YY Month DD" = "%y %B %d",
                    "YY MM DD" = "%y %m %d",
                    "YYYY Mon DD" = "%Y %b %d",
                    "YYYY Month DD" = "%Y %B %d",
                    "YYYY MM DD" = "%Y %m %d")
    }
}

#' Setup Colour Parameters For Charting Functions
#'
#' Processes user-specified colour parameters to a format
#' suitable for Displayr charting functions
#' @importFrom flipChartBasics ChartColors
#' @param data data to be used for creating the chart
#' @param colours list containing user inputs from the GUI controls
#' @param chart.type character type of chart to be plotted
#' @return see \code{\link[flipChartBasics]{ChartColors}}
## @return \code{list} containing two components
## \itemize
## {
## \item \code{n.series.colors} - number of colours to be plotted
## \item \code{series.colors} - \code{NULL} if \code{colours} is \code{NULL},
## or the output from calling \code{\link[flipChartBasics]{ChartColors}}
## }
#' @seealso \code{\link[flipChartBasics]{ChartColors}}
#' @noRd
getColourPars <- function(data, colours, chart.type){
    n.series.colors <- switch(chart.type,
                             "Venn Diagram" = ncol(data),
                             "Stream Graph" = nrow(data),
                             ## "Column Chart" = 12, #ncol(table),
                             12)

    series.colors <- if (is.null(colors[[1]])) NULL else
         flipChartBasics::ChartColors(n.series.colors,
                             given.colors = colors[[1]],
                             custom.color = colors[[2]],
                             custom.gradient.start = colors[[3]],
                             custom.gradient.end = colors[[4]],
                             custom.palette = colors[[5]])
    series.colors
}

