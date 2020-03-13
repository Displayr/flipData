#' Tidy Data From Displayr
#'
#' Tidies a data frame, by applying subets, weights, removing duplicate
#' variables, and dealing with missing values.
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
#' @param remove.missing.levels Logical; whether levels are removed
#'     if they do not occur in the observed data.
#' @param extract.common.lab.prefix logical; if true,
#'     \code{\link[flipFormat]{ExtractCommonPrefix}} will be used to
#'     attempt to extract the common prefix from the data labels, and
#'     if one exists, the shortened variable names without the prefix
#'     will be used for names in the returned data.frame.
#' @param auto.correct.class If \code{TRUE}, the class of each variable
#'     is automatically inferred and corrected if required.
#' @return A \code{data.frame} containing the filtered raw data, which
#'     has an attribute called \code{"weights"}, containing the
#'     (filtered) vector of weights.  If
#'     \code{extract.common.lab.prefix} is \code{TRUE} and a common
#'     label prefix is found, it will be return in an attribute called
#'     \code{"label.prefix"}.
#' @seealso \code{\link[flipFormat]{ExtractCommonPrefixFromLabels}}
#' @importFrom flipFormat ExtractCommonPrefixFromLabels "Labels<-"
#' @importFrom flipTransformations ProcessQVariables AsNumeric
#' @importFrom stats as.formula
#' @export
TidyRawData <- function(data,
                        as.numeric = FALSE,
                        as.binary = FALSE,
                        subset = NULL,
                        weights = NULL,
                        missing = "Exclude cases with missing data",
                        error.if.insufficient.obs = TRUE,
                        remove.missing.levels = TRUE,
                        extract.common.lab.prefix = FALSE,
                        auto.correct.class = FALSE)
{

    if (missing(data) || !length(data))
        stop("No data supplied")
    if (auto.correct.class)
        for (i in seq_along(data))
            data[[i]] <- AutoCoerceClass(data[[i]])

    ## Removing duplicate variables (put back in at the end)
    nms <- names(data)
    duplicates <- duplicated(nms)
    if (any(duplicates))
    {
        data <- data[, !duplicates, drop = FALSE]
        names(data) <- nms[!duplicates]
        warning("Variables containing duplicated variable names have been removed (give the variables unique names if you do not want this to happen): ", paste(sort(unique(nms[duplicates])), collapse = ", "), ".")
    }

    ## handle variables of QDate class
    ## data.frame ensures ProcessQVariables also returns a data.frame
    data <- flipTransformations::ProcessQVariables(data.frame(data, check.names = FALSE))  # can't have check.names = FALSE

    partial <- missing == "Use partial data"

    ## convert factor columns to matrix of indicators if as.binary is TRUE
    ##  else unclass factors and remove levels attribute
    if (as.numeric)
        data <- flipTransformations::AsNumeric(data, binary = as.binary,
                                               remove.first = FALSE)
    n.total <- nrow(data)

    ## Deal with subset
    if (has.subset  <- !is.null(subset) && length(subset) != 1)
    {
        subset <- eval(substitute(subset), data, parent.frame())
        attr(subset, "description") <- if (!is.null(substitute(subset)))
                                           deparse(substitute(subset))  # else NULL
        if (length(subset) > 1 && length(subset) != nrow(data))
            stop("'subset' and 'data' are required to have the same number of observations. They do not")
    }

    ## Deal with weights
    weighted <- !is.null(weights)
    if(weighted)
    {
        if (is.null(attr(weights, "name")))
            attr(weights, "name") <- deparse(substitute(weights))
        weights <- eval(substitute(weights), data, parent.frame())
        if (length(weights) != nrow(data))
            stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    }
    ## Filter and impute missing values in the data (if required)
    names.without.backticks <- names(data)
    input.formula <- as.formula(paste0("~`", paste(names.without.backticks, collapse = "`+`"), "`"))
    # EstimationData requires colnames of data match formaula names including backticks
    names(data) <- AllVariablesNames(input.formula)
    processed.data <- EstimationData(input.formula,
                                     data = data,
                                     subset = subset,
                                     missing = missing,
                                     weights = weights,
                                     remove.missing.levels = remove.missing.levels,
                                     error.if.insufficient.obs = error.if.insufficient.obs)
    data <- processed.data$estimation.data
    names(data) <- names.without.backticks

    ## Search for common prefix for labels
    if (extract.common.lab.prefix)
    {
        labs <- ExtractCommonPrefixFromLabels(data, tidy = FALSE)
        if (!is.na(labs$common.prefix))  ## update labels and add common prefix attribute
        {
            attr(data, "label.prefix") <- labs$common.prefix
            Labels(data) <- labs$shortened.labels
        }
    }

    attr(data, "weights") <- processed.data$weights
    data
}

