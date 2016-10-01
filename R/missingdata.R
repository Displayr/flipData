#' \code{RemoveCasesWithAnyNA}
#'
#' @description Remove rows which contain NA.
#' @param x The input dataframe.
#' @export
RemoveCasesWithAnyNA <- function(x)
{
    x[apply(is.na(x), 1, sum) == 0, , drop = FALSE]
}

#' \code{RemoveCasesWithAllNA}
#'
#' @description Remove rows which are all NA.
#' @param x The input dataframe.
#' @export
RemoveCasesWithAllNA <- function(x)
{
    x[apply(is.na(x), 1, sum) < ncol(x), , drop = FALSE]
}

#' \code{ExcludeCasesWithAnyMissingData}
#' @description This is a wrapper for \code{\link{na.omit}}
#' @param data A \code{\link{data.frame}}.
#' @examples
#' df <- data.frame(x = c(NA, 1), y = 1:2)
#' ExcludeCasesWithAnyMissingData(df)
#' @importFrom stats na.omit
#' @export
ExcludeCasesWithAnyMissingData <- function(data)
{
    result <- na.omit(data)
    if(nrow(result) == 0)
        noData()
    result
}

#' \code{ExcludeCasesWithCompletelyMissingData}
#' @description Create a copy of a data frame where any cases which have only missing values
#' are removed.
#' @param data A \code{\link{data.frame}}.
#' @examples
#' my.df <- data.frame("A" = c(1, 2, 3, 4, NA), "B" = c(NA, 1, 2, 3, NA), "C" = c(NA, NA, 1, 2, NA))
#' ExcludeCasesWithCompletelyMissingData(my.df)
#' @export
ExcludeCasesWithCompletelyMissingData <- function(data)
{
    result <- data[rowSums(is.na(data)) < ncol(data), ]
    if (nrow(result) == 0)
    {
        noData()
    }
    return(result)
}

#' \code{ErrorIfMissingDataFound}
#' @description This is a wrapper for \code{\link{na.fail}}
#' @param data A \code{\link{data.frame}}.
#'# @examples
#'# df = data.frame(x = c(NA, 1), y = 1:2)
#'# ErrorIfMissingDataFound(df)
#' @export
ErrorIfMissingDataFound <- function(data)
{
    if (any(is.na(data)))
        missingDataFail()
    data
}


notAvailable <- function(unavailable.function.name)
{
    stop(paste(unavailable.function.name,"is not available for this analysis. Please contact support if you believe this option should be available."))
}

noData <- function()
{
    stop("All observations contains some missing data, so an analysis is not possible. Check to see if there are any 'missing' options that can be used.")
}

missingDataFail <- function()
{
    stop("The data contains missing values. Change the 'missing' option to run the analysis.")
}


#' \code{MissingValuesByVariable}
#' @description Computes the number of missing values by variable.
#' @param data A \code{\link{data.frame}}.
#' @importFrom flipFormat FormatAsPercent
#' @export
MissingValuesByVariable <- function(data)
{
    n <- nrow(data)
    missing <- apply(is.na(data), 2, sum)
    result <- data.frame("Missing" = missing,
        proportion = FormatAsPercent(missing / n))
    names(result)[2] <- paste0("Percent (of ", n, ")")
    result}


#' \code{CleanSubset}
#' Takes a QSubset variable and turns it into a logical vector with no missing values
#' @param subset A QSubset variable from Displayr or Q.
#' @param n.total The total number of observations.
#' @export

CleanSubset <- function(subset, n.total)
{
    subset.label <- attr(subset, "label")
    if (is.null(subset))
    {
        n.subset <- n.total
        subset <- rep(TRUE, n.total)
    }
    else
    {
        subset.length <- length(subset)
        if(subset.length == 1)
        {
            n.subset <- n.total
            subset <- rep(subset, n.total)
        }
        else
        {
            if (subset.length != n.total)
                stop("subset.length != n.total")
            subset[is.na(subset)] <- FALSE
            subset[is.na(subset)] <- FALSE
            n.subset <- sum(subset)

        }
    }
    attr(subset, "n.subset") = n.subset
    attr(subset, "label") = subset.label
    subset
}

#' \code{CleanWeights}
#' Takes a QSubset variable and turns it into a logical vector with no missing values
#' @param weights A weights variable from Displayr or Q (i.e., \code{QcalibratedWeight}
#' or \code{QPopulationWeight}).
#' @export

CleanWeights <- function(weights)
{
    if (is.null(weights))
        return(weights)
    weights[is.na(weights) | weights < 0] <- 0
    weights
}

