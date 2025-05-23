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
        NoData()
    result
}

#' \code{ExcludeCasesWithCompletelyMissingData}
#' @description Create a copy of a data frame where any cases which have only missing values
#' are removed.
#' @param data A \code{\link{data.frame}}.
#' @examples
#' my.df <- data.frame("A" = c(1, 2, 3, 4, NA), "B" = c(NA, 1, 2, 3, NA), "C" = c(NA, NA, 1, 2, NA))
#' ExcludeCasesWithCompletelyMissingData(my.df)
#' @importFrom verbs SumEachRow
#' @export
ExcludeCasesWithCompletelyMissingData <- function(data)
{
    result <- data[SumEachRow(is.na(data), remove.missing = FALSE) < ncol(data), ]
    if (nrow(result) == 0)
    {
        NoData()
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
        MissingDataFail()
    data
}

#' @importFrom flipU StopForUserError
notAvailable <- function(unavailable.function.name)
{
    StopForUserError(paste(unavailable.function.name,"is not available for this analysis. Please contact support if you believe this option should be available."))
}

#' \code{NoData}
#' @description Error thrown when all cases contain missing data and no data is
#'     available for use as the "Exclude cases with missing data" option was
#'     selected.
#' @export
#' @importFrom flipU StopForUserError
NoData <- function()
{
    StopForUserError("All observations contain some missing data, so an analysis is not ",
         "possible. Check to see if there are any 'missing' options that can ",
         "be used.")
}

#' \code{MissingDataFail}
#' @description Error thrown when missing values are present and the
#'     "Error if missing data" option was selected.
#' @export
#' @importFrom flipU StopForUserError
MissingDataFail <- function()
{
    StopForUserError("The data contains missing values. Change the 'missing' option to run the analysis.")
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
#' @importFrom verbs Sum
#' @export
#' @importFrom flipU StopForUserError
CleanSubset <- function(subset, n.total)
{
    new.subset <- NULL
    if (is.null(subset))
    {
        n.subset <- n.total
        new.subset <- rep(TRUE, n.total)
    }
    else
    {
        subset.length <- length(subset)
        if(subset.length == 1)
        {
            n.subset <- n.total
            new.subset <- rep(subset, n.total)
        }
        else
        {
            if (subset.length != n.total)
                StopForUserError("subset.length != n.total")
            subset[is.na(subset)] <- FALSE
            subset[is.na(subset)] <- FALSE

        }
    }
    if (!is.null(new.subset))
        subset <- CopyAttributes(new.subset, subset)
    n.subset <- Sum(subset, remove.missing = FALSE)
    attr(subset, "n.subset") = n.subset
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


#' Returns an error if any column(s) contain +/- ininity.
#'
#' @param data A \code{data.frame}.
#' @export
#' @importFrom flipU StopForUserError
ErrorIfInfinity <- function(data)
{
    infinite.cols <- sapply(data, function(x) any(is.infinite(x)))
    if (any(infinite.cols))
    {
        infinite.vars <- paste(names(infinite.cols)[infinite.cols], collapse = ", ")
        StopForUserError("Variable(s) ", infinite.vars, " contain infinite values.",
             " Either recode the infinities to finite values or set them as missing data.")
    }
}

#' @name AddDummyVariablesForNAs
#'
#' @title Add dummy variables to a \code{data.frame} suitable for regression models.
#'
#' @description Appends a matrix to a \code{data.frame} containing columns of dummy variables. It assumes
#'   the input data is suitable for regression and contains a single column for an outcome variable.
#'   Other columns would be for individual predictors used in the regression. The appended columns of dummy
#'   variables indicate the missing status of each predictor. i.e. 1 if the predictor is missing in that case,
#'   zero otherwise. A column is added for each predictor that has at least one case of missing data.
#'
#' @param data A \code{data.frame} of the data to be used for a Regression model assuming a single outcome.
#' @param outcome.name A \code{characater} of the name of the outcome variable in the data.
#' @param checks A \code{logical} to determine if further checks are done to remove cases from the data.
#'   If \code{TRUE}, cases are removed if all predictors are missing or if the response is missing.
#' @export
AddDummyVariablesForNAs <- function(data, outcome.name, checks = TRUE)
{
    outcome <- data[which(names(data) == outcome.name)]
    predictor.df <- data[-which(names(data) == outcome.name)]
    # Create dummy variable matrix, only create column if necessary
    dummy.variable.df <- lapply(predictor.df, function(x) {
        z <- is.na(x)
        if (any(z)) as.integer(z)
    })
    # Remove the NULL elements (no missing)
    dummy.variable.df <- Filter(length, dummy.variable.df)
    missing.outcomes <- is.na(outcome[[1]])
    # If no missing data in predictors, return original data, trimming missing outcomes if req
    if (length(dummy.variable.df) == 0)
    {
        if (any(missing.outcomes) && checks)
            data <- data[!missing.outcomes, ]
        return(data)
    }
    dummy.variable.df <- checkAndMapDummyVariables(dummy.variable.df)
    if (ncol(data) == 2)
        cases.all.predictors.missing <- rep(FALSE, nrow(predictor.df))
    else
        cases.all.predictors.missing <- apply(predictor.df, 1, function(x) all(is.na(x)))

    dummy.variable.df <- as.data.frame(dummy.variable.df, check.names = FALSE)
    names(dummy.variable.df) <- paste0(names(dummy.variable.df), ".dummy.var_GQ9KqD7YOf")
    # replace NAs in predictor df with means or reference level
    predictor.df <- remapDataFrame(predictor.df)
    # Create new data.frame
    new.data <- cbind.data.frame(outcome, predictor.df, dummy.variable.df)
    if (checks)
    {
        # Check if all predictors missing, if so, subset, attributes to be preserved in EstimationData
        if (any(cases.all.predictors.missing) || any(missing.outcomes))
            new.data <- new.data[!cases.all.predictors.missing & !missing.outcomes, ]
        # Inspect dummy matrix and remove dummy variables no longer required
        dummy.parts <- names(new.data) %in% names(dummy.variable.df)
        if (any(empty.dummy.vars <- lapply(new.data[dummy.parts], sum) == 0))
            new.data[names(which(empty.dummy.vars))] <- NULL
        # Copy attributes from the dummy variable (required for aliased dummy mapping)
        new.data <- CopyAttributes(new.data, dummy.variable.df)
    }
    new.data
}

# original.data should be data.frame input with numeric and factor columns
#' @importFrom flipU StopForUserError
remapDataFrame <- function(dataframe)
{
    remapped.list <- lapply(dataframe, function(x) {
        if (is.numeric(x))
            x[is.na(x)] <- mean(x, na.rm = TRUE)
        else if (is.factor(x))
            x[is.na(x)] <- levels(x)[1]
        else
            StopForUserError("Unexpected class when using dummy variable adjustment. ",
                 "Supplied variable should be 'numeric' or 'factor', ",
                 "however suppled variable is ", sQuote(class(x)))
        return(x)
    })
    as.data.frame(remapped.list, check.names = FALSE)
}

# Helper function for the dummy variable adjustment.
# This function adds an attribute which contains a character vector mapping the dummy variable
# to each variable that are linked to its binary 0/1 values.
# This is usually redundant since it typically only maps to a single variable and that information
# is evident in the dummy variable name. The exception is in the case of aliased
# Dummy variables. That is, predictors have precisely the same missing cases, causing the
# dummy variables for those predictors will be identical (aliased). In this case, a single
# dummy variable is created for the group of predictors with identical missing value structure
# and the mapping contains a character vector of all variables in that group that correspond to it.
checkAndMapDummyVariables <- function(data)
{
    x <- 1L
    while(x < length(data))
    {
        current.duplicates <- vapply(names(data)[-(1L:x)],
                                     function(y) all(data[[y]] == data[[x]]), logical(1))
        names.to.map <- names(data)[x]
        if (any(current.duplicates))
        {
            names.to.map <- c(names.to.map, names(which(current.duplicates)))
            data[names(which(current.duplicates))] <- NULL
        }
        attr(data[[x]], "predictors.matching.dummy") <- names.to.map
        x <- x + 1
    }
    # Add the attribute for the last list element if it wasn't added in the prior steps
    if (is.null(attr(data[[length(data)]], "predictors.matching.dummy")))
        attr(data[[length(data)]], "predictors.matching.dummy") <- names(data)[length(data)]
    data
}
