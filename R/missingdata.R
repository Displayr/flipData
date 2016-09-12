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

#' \code{SampleDescription}
#'
#' @description Describes the sample, for use as footers in multivariate
#'   analyses.
#' @param n.total  Total number of observations in the database.
#' @param n.subset Total number of observations in the subset (less than or
#'   equal to \code{n.total}).
#' @param n.estimation The total number of observations used in estimation (less
#'   than or equal to \code{subset}).
#' @param subset.label Total number of observations in the database.
#' @param weighted Total number of observations in the database.
#' @param weight.label Total number of observations in the database.
#' @param missing How missing data is to be treated in the analysis. Options
#'   are: \code{"Error if missing data"}, \code{"Exclude cases with missing
#'   data"}, ,and \code{"Imputation (replace missing values with estimates)"}.
#' @param imputation.label Method used to impute the data.
#' @param m Number of imputaton samples.
#'
#' @export
SampleDescription <- function(n.total, n.subset, n.estimation, subset.label, weighted = TRUE, weight.label = "", missing, imputation.label = NULL, m)
{
    # Warning if there is less than 50% data.
    missing.data.proportion <- 1 - n.estimation / n.subset
    if (missing.data.proportion > 0.50)
        warning(paste(FormatAsPercent(missing.data.proportion), "of the data is missing and has been excluded from the analysis.",
                      "Consider either filters to ensure that the data that is missing is in-line with your expectations,",
                      "or, set 'Missing Data' to another option."))
    # Creating description.
    missing.data <- n.estimation < n.subset
    imputation <-  missing == "Imputation (replace missing values with estimates)" | missing == "Multiple imputation"
    description <- BaseDescription(paste0("n = ", n.estimation," cases used in estimation"),
        n.total, n.subset, n.estimation, subset.label, weighted, weight.label)
    description <- paste(description, if(missing.data | imputation)
        switch(missing, "Error if missing data" = "",
                   "Exclude cases with missing data" = "cases containing missing values have been excluded;",
                   "Imputation (replace missing values with estimates)" =
                        paste0("missing values of predictor variables have been imputed using ", imputation.label, ";"),
                   "Multiple imputation" =
                        paste0("multiple imputation (m = ", m, ", ", imputation.label, ") has been used to impute missing values of predictor variables;"))
        else "")
    description
}



#' \code{BaseDescription}
#'
#' @description Describes the sample, for use as footers in multivariate
#'   analyses.
#' @param description.of.n A description of the sample (e.g., "People").
#' @param n.total  Total number of observations in the database.
#' @param n.subset Total number of observations in the subset (less than or
#'   equal to \code{n.total}).
#' @param n.estimation The total number of observations used in estimation (less
#'   than or equal to \code{subset}).
#' @param subset.label E.g., "Males living in New York".
#' @param weighted Total number of observations in the database.
#' @param weight.label Total number of observations in the database.
#'
#' @export
BaseDescription <- function(description.of.n,
                            n.total, n.subset, n.estimation, subset.label, weighted = TRUE, weight.label = "")
{
    base <- if(n.estimation < n.subset) paste0(" of a total sample size of ", n.subset, ";") else ""
    if (n.subset < n.total)
        base <- paste0(base, " (", as.character(subset.label), ")")
    paste0(description.of.n,
           base,
        ifelse(weighted,
               paste0(" Data has been weighted (", weight.label, ");"),
               ""))
}





#' \code{AnyNA}
#'
#' Checks to see if the are any missing valus
#' @param data A \code{\link{data.frame}}.
#' @param formula A no optional \code{\link{formula}}. Variables not listed in a formula are excluded from the evaluation.
#'

#' @importFrom flipU AllVariablesNames
#' @export
AnyNA <- function(data, formula = NULL)
{
    if (!is.null(formula))
    {
        data <- data[, AllVariablesNames(formula)]
    }
    any(is.na(data))
}

# k <- 1:ncol(dat)
# characters <- k[unlist(lapply(dat, is.character))]
# ordered <- unlist(lapply(dat, is.ordered))
# factors <- k[unlist(lapply(dat, is.factor)) & !ordered]
# ordered <- k[ordered]
# amelia(dat, noms = factors, ords = ordered, idvars = characters)
#
# mdf <- missing_data.frame(dat)
# mi(mdf)
#     a     b     c     d     e
# FALSE FALSE FALSE FALSE  TRUE
# > unlist(lapply(dat, is.ordered))
#     a     b     c     d     e
# FALSE FALSE FALSE  TRUE FALSE
# > amelia(dat)
# Amelia Error Code:  37
#  The following variable(s) are 'factors':
# c, d
# You may have wanted to set this as a ID variable to remove it
# from the imputation model or as an ordinal or nominal
# variable to be imputed.  Please set it as either and
# try again.



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

