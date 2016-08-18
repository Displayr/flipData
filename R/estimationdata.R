
#' \code{EstimationData} Selects the data from a data frame for estimation.
#' Conducts imputation if necessary.
#' @param formula An object of class \code{\link{formula}} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   The details of type specification are given under \sQuote{Details}.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Options
#'   are: \code{"Error if missing data"}, \code{"Exclude cases with missing
#'   data"}, \code{"Use partial data"}, \code{"Use partial data (pairwise
#'   correlations)"}, \code{"Imputation (replace missing values with
#'   estimates)"}, and  \code{"Multiple imputation"}.
#' @param m Number of imputation samples.
#' @param seed The random number seed used in the imputation.
#' @importFrom flipImputation Imputation
#' @export
EstimationData <- function(formula = NULL,
                           data = NULL,
                           subset = NULL,
                           weights = NULL,
                           missing = "Exclude cases with missing data",
                           m = 10,
                           seed = 12321)
{
    # Cleaning weights and subsets.
    n.total <- nrow(data)
    subset <- CleanSubset(subset, n.total)
    n.subset <- attr(subset, "n.subset")
    if (weighted <- !is.null(weights))
    {
        weights <- CleanWeights(weights)
        weight.label <- attr(weights, "label")
    }
    unfiltered.weights <- weights
    # Filtering the data
    filter.ewerrfdfdsrew045 <- if (weighted) subset & weights > 0 else subset #Name to avoid bug in subset.data.frame
    data.subset <- subset(data, filter.ewerrfdfdsrew045)
    # Selecting the relevant variables from the data frame (unless imputation is being used).
    variable.names <- AllVariablesNames(formula)
    single.imputation <- missing == "Imputation (replace missing values with estimates)"
    if (single.imputation | missing ==  "Multiple imputation")
    {
        if (single.imputation)
            m = 1
        # Imputation is performed only using the subset, as otherwise probelms can occurif the subset
        # is based on a range of values of a variable, and the imputation causes values outside this
        # range to be imputed.
        data.for.estimation = Imputation(data.subset, formula, m = m, seed = seed)
        imputation.label <- attr(data.for.estimation[[1]], "imputation.method")
        #Filtering for the whole data set (as if using only the non-filter,the sample may be too small)
        data$filter.ewerrfdfdsrew045 <- as.integer(filter.ewerrfdfdsrew045) # Adding the filter as a variable to assist the imputation (name is to avoid duplicates).
        # Removing the variables not in the model.
        for (i in 1:m)
            data.for.estimation[[i]] <- data.for.estimation[[i]][, variable.names, drop = FALSE]
        # Imputing for the entire data set for prediction purposes.
        data = Imputation(data, m = m)[[1]][, variable.names, drop = FALSE]
        estimation.sample <- row.names(data) %in% rownames(data.for.estimation[[1]])
        data[estimation.sample, ] = data.for.estimation[[1]]
        if (single.imputation)
            data.for.estimation = data.for.estimation[[1]]
    }
    else
    {
        data.subset <- data.subset[ ,variable.names, drop = FALSE]
        data.for.estimation <- switch(missing, "Error if missing data" = ErrorIfMissingDataFound(data.subset),
                   "Exclude cases with missing data" = removeCasesWithAnyNA(data.subset),
                   "Use partial data" = removeCasesWithAllNA(data.subset),
                   "Use partial data (pairwise correlations)" = removeCasesWithAllNA(data.subset))
        estimation.sample <- row.names(data) %in% rownames(data.for.estimation)
    }
    if (weighted)
        weights <- weights[estimation.sample]
    # Reporting.
    n.estimation <- sum(estimation.sample)
    if (n.estimation < length(variable.names))
        stop("Sample size is too small ")
    description <- SampleDescription(n.total, n.subset, n.estimation,
        attr(subset, "label"), weighted, weight.label, missing, imputation.label, m)
    list(estimation.data = data.for.estimation,
         weights = weights,
         unfiltered.weights = unfiltered.weights,
         post.missing.data.estimation.sample = estimation.sample,
         #estimation.subset = estimation.subset,
         data = data,
         #subset = filter,
         description = description)
}

#' \code{EstimationDataFormula}
#' @description The formula to be used with estimation data. Same as the original formula except when
#' a dataframe variable is referred to, in which case the dataframe variable is converted to a normal
#' variable by adding backticks around it.
#' @param formula An object of class \code{\link{formula}}.
#' @export
EstimationDataFormula <- function(formula)
{
    formula.str <- paste(deparse(formula), collapse = "")
    var.names <- AllVariablesNames(formula)
    for (i in 1:length(var.names))
    {
        name <- var.names[i]
        if (indexOfUnescapedCharacter(name, "$") > -1)
            formula.str <- gsub(name, paste("`", name, "`", sep = ""), formula.str, fixed = TRUE)
    }
    formula(formula.str)
}
