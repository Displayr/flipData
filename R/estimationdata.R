#' Select the data from a data frame for estimation.
#'
#' Conducts imputation if necessary.
#' @param formula An object of class \code{\link{formula}} (or one
#'     that can be coerced to that class): a symbolic description of
#'     the model to be fitted.  The details of type specification are
#'     given under \sQuote{Details}.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of
#'     observations to be used in the fitting process, or, the name of
#'     a variable in \code{data}. It may not be an
#'     expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name
#'     of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the
#'     regression. Options are: \code{"Error if missing data"},
#'     \code{"Exclude cases with missing data"},
#'     \code{"Use partial data"},
#'     \code{"Use partial data (pairwise correlations)"},
#'     \code{"Dummy variable adjustment"},
#'     \code{"Imputation (replace missing values with estimates)"},
#'     and \code{"Multiple imputation"}.
#' @param m Number of imputation samples.
#' @param seed The random number seed used in the imputation.
#' @param error.if.insufficient.obs Throw an error if there are more
#'     variables than observations.
#' @param remove.missing.levels Logical; whether levels are removed
#'     if they do not occur in the observed data.
#' @param impute.full.data logical; if \code{TRUE} and \code{missing}
#'     is either
#'     \code{"Imputation (replace missing values with estimates)"} or
#'     \code{"Multiple imputation"}, imputation is performed on both
#'     the full \code{data} and on the requested subset of
#'     \code{data}; otherwise, imputation is only performed on the
#'     subset.  Ignored for other options of \code{missing}.
#' @details Removes any empty levels from factors.
#' @importFrom flipTransformations RemoveMissingLevelsFromFactors
#' @importFrom flipU AllVariablesNames CopyAttributes HasOutcome OutcomeName
#' @importFrom flipFormat Labels SampleDescription
#' @importFrom flipImputation Imputation
#' @seealso \code{\link[flipImputation]{Imputation}},
#'     \code{\link[flipFormat]{SampleDescription}}
#' @return A list with components \itemize{ \item
#'     \code{estimation.data} - tidied (filtered/subsetted and
#'     NA-free) \code{data.frame} \item \code{weights} - the cleaned
#'     weights with any filters applied(i.e. the weights with NA and
#'     negative weights set to 0), \item \code{unfiltered.weights} -
#'     the cleaned weights from the complete data (i.e. with no filter
#'     applied) \item \code{post.missing.data.estimation.sample} -
#'     logical vector with length equal to the number of rows of
#'     \code{data} with a \code{TRUE} value in position \code{i}
#'     indicating that the \code{i}th row of \code{data} appears in
#'     the tidied data \code{estimation.data} \item \code{data} -
#'     original \code{data} (without subset applied), but with
#'     imputation performed (if requested) \item \code{description} -
#'     character; description of the data; see
#'     \code{\link[flipFormat]{SampleDescription}} }
#' @export
EstimationData <- function(formula = NULL,
                           data = NULL,
                           subset = NULL,
                           weights = NULL,
                           missing = "Exclude cases with missing data",
                           m = 10,
                           seed = 12321,
                           error.if.insufficient.obs = TRUE,
                           remove.missing.levels = TRUE,
                           impute.full.data = TRUE)
{
    # Cleaning weights and subsets.
    n.total <- nrow(data)
    subset <- CleanSubset(subset, n.total)
    # Removing cases with completely missing data
    n.subset <- attr(subset, "n.subset")
    if (weighted <- !is.null(weights))
    {
        weights <- CleanWeights(weights)  # convert NA and negative weights to 0
        weight.label <- Labels(weights)
    }
    unfiltered.weights <- weights
    ## Selecting the relevant variables from the data frame (unless imputation is being used).
    variable.names <- AllVariablesNames(formula, data)
    labels <- Labels(data[, variable.names], show.name = TRUE)
    # Removing cases with entirely missing data
    some.data <- !apply(is.na(data[, variable.names, drop = FALSE]), 1, all)
    # Filtering the data
    .filter <- if (weighted) subset & weights > 0 & some.data else subset & some.data  # Name to avoid bug in subset.data.frame
    data.subset <- subset(data, .filter)
    data.subset <- CopyAttributes(data.subset, data)
    dummy.adjusted <- FALSE
    ##############
    ## Imputation
    single.imputation <- missing == "Imputation (replace missing values with estimates)"
    if (single.imputation | missing ==  "Multiple imputation")
    {
        if (single.imputation)
            m = 1
        # Imputation is performed only using the subset, as otherwise problems can occur if the subset
        # is based on a range of values of a variable, and the imputation causes values outside this
        # range to be imputed.
        data.for.estimation <- Imputation(data.subset, formula, m = m, seed = seed)
        imputation.label <- attr(data.for.estimation[[1]], "imputation.method")
        # Filtering for the whole data set (as if using only the non-filter, the sample may be too small)
        data$.filter <- as.integer(.filter) # Adding the filter as a variable to assist the imputation (name is to avoid duplicates).
        # Removing the variables not in the model.
        for (i in seq_len(m))
        {
            est.data <- data.for.estimation[[i]]
            est.data <- est.data[, variable.names, drop = FALSE]
            est.data <- RemoveMissingLevelsFromFactors(est.data)
            data.for.estimation[[i]] <- est.data  # CopyAttributes(est.data, data.for.estimation[[i]])

        }
        ## Imputing for the entire data set for prediction purposes.
        if (impute.full.data)
            data <- Imputation(data, m = 1L)[[1]][, variable.names, drop = FALSE]

        ## for portion of data in subset, use values from imputing on the subset only
        estimation.sample <- rownames(data) %in% rownames(data.for.estimation[[1]])
        data[estimation.sample, ] = data.for.estimation[[1]]

        if (single.imputation)
            data.for.estimation = data.for.estimation[[1]]
        ## data <- CopyAttributes(data, data.for.estimation[[1]])
    }
    else
    {  # handle missing values without imputation
        data.subset <- data.subset[, variable.names, drop = FALSE]
        data.for.estimation <- switch(missing, "Error if missing data" = ErrorIfMissingDataFound(data.subset),
                   "Exclude cases with missing data" = RemoveCasesWithAnyNA(data.subset),
                   "Dummy variable adjustment" = AddDummyVariablesForNAs(data.subset, OutcomeName(formula)),
                   "Assign partial data to clusters" = RemoveCasesWithAnyNA(data.subset),
                   "Use partial data" = RemoveCasesWithAllNA(data.subset),
                   "Use partial data (pairwise correlations)" = RemoveCasesWithAllNA(data.subset),
                   stop(paste("Unknown 'missing' method:", missing)))
        data.for.estimation <- CopyAttributes(data.for.estimation, data.subset)
        if (missing == "Dummy variable adjustment")
        { # Dummy variable adjustment can be selected but sometimes not used
            dummy.adjusted <- any(grepl(".dummy.var_GQ9KqD7YOf$", colnames(data.for.estimation)))
            data.cols <- names(data.for.estimation) %in% names(data.subset)
        }
        else
            data.cols <- rep(TRUE, ncol(data.for.estimation))

        if (remove.missing.levels)
        {
            levels.pre <- paste0(rep(labels, vapply(data.for.estimation[data.cols], nlevels, 0L)), ": ",
                                unlist(lapply(data.for.estimation, levels)))
            data.for.estimation <- RemoveMissingLevelsFromFactors(data.for.estimation)
            levels.post <- paste0(rep(labels, vapply(data.for.estimation[data.cols], nlevels, 0L)), ": ",
                                 unlist(lapply(data.for.estimation, levels)))
            
            levels.diff <- setdiff(levels.pre, levels.post)
            if (length(levels.diff) > 0)
            {
                labls <- paste(levels.diff, collapse = "', '")
                warning("Some categories do not appear in the data: '", labls,
                        "'. This may be because they are empty in the raw data, or ",
                        "because they are empty after any weights, filters/subsets, ",
                        "or missing data settings are applied. This may cause an error. ",
                        "It is recommended that you merge categories prior to estimating",
                        " the model, use an alternative missing data method, filter the ",
                        "data, or make the data numeric.")
            }
        }
        estimation.sample <- rownames(data) %in% rownames(data.for.estimation)  # row.names is S3 generic
    }

    if (weighted)
        weights <- weights[estimation.sample]

    # Reporting.
    n.estimation <- sum(estimation.sample)
    if (error.if.insufficient.obs && n.estimation < length(variable.names))
        stop(gettextf("There are fewer observations (%d)%s(%d)", n.estimation,
                      " than there are variables ", length(variable.names)))
    description <- SampleDescription(n.total, n.subset, n.estimation,
                                     Labels(subset), weighted, weight.label, missing, imputation.label, m,
                                     if(HasOutcome(formula)) "predictor" else "",
                                     dummy.adjusted = dummy.adjusted)
    # Add statements about removing cases with missing outcomes and/or all predictors missing
    if (missing == "Dummy variable adjustment")
    {
        outcome.name <- OutcomeName(formula)
        missing.outcomes <- is.na(data[[outcome.name]])
        outcome.index <- which(names(data) == outcome.name)
        if (ncol(data) == 2)
            all.predictors.missing <- rep(FALSE, nrow(data))
        else
            all.predictors.missing <- apply(data[-outcome.index], 1, function(x) all(is.na(x)))
        if (any(missing.outcomes | all.predictors.missing))
        {
            missing.outcomes <- if (any(missing.outcomes)) "an outcome variable" else NULL
            all.predictors.missing <- if (any(all.predictors.missing)) "all predictor variables" else NULL
            description <- paste0(description, paste0(" cases missing ",
                                                      paste0(c(missing.outcomes, all.predictors.missing),
                                                             collapse = " or missing "),
                                                      " have been excluded;"))
        }
    }
    list(estimation.data = data.for.estimation,
         weights = weights,
         unfiltered.weights = unfiltered.weights,
         post.missing.data.estimation.sample = estimation.sample,
         data = data,
         description = description)
}



