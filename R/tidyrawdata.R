#' Tidy Data From Displayr
#'
#' Gets the raw data, tidies subset and weights, and applies the
#' subset, dealing with any missing values along the way.
#'
#' @param data A \code{data.frame}.
#' @param as.numeric If TRUE, converts factors into numeric variables.
#' @param as.binary If \code{TRUE}, unordered factors are represented
#'     as dummy variables.  Otherwise, they are represented as
#'     sequential integers. Only applied if \code{as.numeric == TRUE}.
#' @param subset An optional vector specifying a subset of
#'     observations to be used in the fitting process, or, the name of
#'     a variable in \code{data}. It may not be an expression.
#' @param weights An optional vector of sampling weights, or, the name
#'     of a variable in \code{data}. It may not be an expression.
#' @param missing character; One of \code{"Error if missing data"},
#'     \code{"Exclude cases with missing data"} (the default, which is
#'     equivalent to 'complete.cases'), and \code{"Use partial data"},
#'     which removes no data.
#' @param error.if.insufficient.obs Throw an error if there are more
#'     variables than observations.
#' @param extract.common.lab.prefix logical; if true,
#'     \code{\link[flipFormat]{ExtractCommonPrefix}} will be used to
#'     attempt to extract the common prefix from the data labels, and
#'     if one exists, the shortened variable names without the prefix
#'     will be used for names in the returned data.frame.
#' @return A \code{data.frame} containing the filtered raw data, which
#'     has an attribute called \code{"weights"}, containing the
#'     (filtered) vector of weights.  If
#'     \code{extract.common.lab.prefix} is \code{TRUE} and a common
#'     label prefix is found, it will be return in an attribute called
#'     \code{"label.prefix"}.
#' @seealso \code{\link[flipFormat]{ExtractCommonPrefix}
#' @importFrom flipFormat ExtractCommonPrefix
#' @importFrom flipTransformations ProcessQVariables AsNumeric
#' @importFrom stats as.formula
#' @export
TidyRawData <- function(data,
                        as.numeric = FALSE,
                        as.binary = FALSE,
                        subset = NULL,
                        weights = NULL,
                        missing = "Exclude cases with missing data",
                        error.if.insufficient.obs = TRUE)
{
    data <- ProcessQVariables(data.frame(data))
    partial <- missing == "Use partial data"
    if (as.numeric)
        data <- AsNumeric(data, binary = as.binary, remove.first = FALSE)
    n.total <- nrow(data)
    if (has.subset  <- !is.null(subset) && length(subset) != 1)
    {
        subset <- eval(substitute(subset), data, parent.frame())
        subset.description <- if (is.null(substitute(subset))) NULL else deparse(substitute(subset))
        if (!is.null(subset.description))
            attr(subset, "description") <- subset.description
        if (length(subset) > 1 & length(subset) != nrow(data))
            stop("'subset' and 'data' are required to have the same number of observations. They do not.")
        if (partial)
        {
            subset <- CleanSubset(subset, n.total)
            n.subset <- attr(subset, "n.subset")
            original.subset <- subset
        }
    }
    weighted <- !is.null(weights)
    if(weighted)
    {
        if (is.null(attr(weights, "name")))
            attr(weights, "name") <- deparse(substitute(weights))
        weights <- eval(substitute(weights), data, parent.frame())
        if (length(weights) != nrow(data))
            stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    }
    # Filtering the data (if required)
    input.formula <- as.formula(paste0("~", paste(names(data), collapse = "+")))
    processed.data <- EstimationData(input.formula,
                                     data = data,
                                     subset = subset,
                                     missing = missing,
                                     weights = weights,
                                     error.if.insufficient.obs = error.if.insufficient.obs)
    data <- processed.data$estimation.data
    attr(data, "weights") <- processed.data$weights
    data
}

