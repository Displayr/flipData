#' CheckForPositiveVariance
#'
#' @description Throws an error if any variables cotnain a standard deviation of 0.
#' @param x A \code{\link{matrix}} or \code{\link{data.frame}}.
#' @param variable.names Variable names.
#' @importFrom stats sd
#' @export
CheckForPositiveVariance <- function(x, variable.names = names(x))
{
    if (any(no.variation <- apply(x, 2, sd, na.rm = TRUE) == 0))
    {
        vars <- paste(variable.names[no.variation], collapse = ", ")
        stop(paste0("Some variables have no variation: ", vars))
    }
}


#' CheckForLinearDependence
#'
#' @description Throws an error if there is linear dependence amongst the variables.
#' @param x A \code{\link{matrix}} or \code{\link{data.frame}}.
#' @param variable.names Variable names.
#' @param correlations If TRUE, the data is assumed to be a correlation matrix.
#' @importFrom stats cor
#' @export
CheckForLinearDependence <- function(x, variable.names = colnames(x), correlations = FALSE)
{
    if (correlations)
        CheckForLinearDependenceInCorrelations(x, variable.names)
    if (any(is.na(x)))
        CheckCorrelationMatrix(cor(x, use = "pairwise.complete.obs"), variable.names)
    if (ncol(x) > qr(as.matrix(x))$rank)
    {
        # Creating a nice error message if the linear dependence is a function of having two perfectly correlated variables.
        CheckCorrelationMatrix(cor(x, use = "pairwise.complete.obs"), variable.names)
        stop("There is perfect multicollinearity in the data (i.e., the variables are linearly dependent).")
    }
}

CheckForLinearDependenceInCorrelations <- function(correlations, variable.names)
{
    cors <- correlations
    k <- nrow(cors)
    if (k != ncol(cors))
        stop("Matrix is not a correlation matrix.")
    if (any(ONEs <- cors > .9999999999999))
    {
        rs <- matrix(variable.names, k, k)[ONEs]
        cs <- matrix(, k, k, byrow = TRUE)[ONEs]
        vars <- paste(paste0(rs, ":", cs), collapse = ", ")
        stop(paste0("Some variables are perfectly correlated: ",
                    vars))
    }
    if (min(eigen(correlations)$values) < -1e-15)
        stop("There is perfect multicollinearity in the data (i.e., the variables are linearly dependent).")
}

#' CheckCorrelationMatrix
#'
#' @description Throws an error if there is linear dependence, or NAs, or 1s in the correlation matrix.
#' @param correlations A correlation matrix.
#' @param variable.names Variable names.
#' @export
CheckCorrelationMatrix <- function(correlations, variable.names = colnames(correlations))
{
    cors <- correlations
    k <- nrow(cors)
    if (k != ncol(cors))
        stop("Matrix is not a correlation matrix.")
    cors[lower.tri(cors)] <- 0 # Avoiding duplicates.
    diag(cors) <- 0
    if (any(NAs <- is.na(cors)))
    {
        rs <- matrix(variable.names, k, k)[NAs]
        cs <- matrix(variable.names, k, k, byrow = TRUE)[NAs]
        vars <- paste(paste0(rs, ":", cs), collapse = ", ")
        stop(paste0("Correlations cannot be computed. Perhaps this is due to variables having no variation, or, no overlapping data (i.e., there are some variables in the data where no respondents saw both variables.) This makes it impossible to compute correlations.: ",
                    vars))
    }
}




#' CheckForUniqueVariableNames
#'
#' @description Checks that the same variable name is not used twice in a formula.
#' @param formula A \code{\link{formula}}.
#' @importFrom flipFormat TrimWhitespace
#' @export
CheckForUniqueVariableNames <- function(formula)
{
    formula.as.character <- as.character(formula)
    formula.as.character <- gsub('\\$', '', formula.as.character)
    dep <- formula.as.character[2]
    ind <- TrimWhitespace(strsplit(formula.as.character[3], "\\+")[[1]])
    names <- c(dep, ind)
    n.times <- table(names)
    max.times <- max(n.times)
    if (max.times > 1)
    {
        nm <- (names(n.times)[max.times == n.times])[1]
        stop(paste0("A variable may only appear once in a formula, but " , nm, " appears ", max.times, " times."))
    }
}


#' \code{CheckPredictionVariables}
#'
#' Verifies that newdata is consistent with data used to used to fit a model.  newdata must contain a
#' superset of the variables used to fit the model or an error results. If a factor variable of
#' newdata contains fewer levels than the factor used for fitting, the levels are expanded.  If a factor
#' variable contains more levels than the factor used for fitting, a warning is given.  Returns newdata
#' with potentially expanded factor levels and 'new.level.flags' indicating which instances have new levels.
#' @param object A \code{SupportVectorMachine} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the actual data used to fit the model is used (before any filtering).
#' @export
CheckPredictionVariables <- function(object, newdata)
{
    if (is.null(newdata))
    {
        newdata <- object$model
        predicting.training <- TRUE
    }
    else
        predicting.training <- FALSE

    # EstimationData removes unused levels from training data (after filter and removing NA). svm.predict() binary
    # encodes factor variables according to the number of their levels and will fail if newdata contains
    # a different number of levels from training data (even if no instances have the unused levels).
    # Hence we filter out instances of newdata with new levels (predicting NA), and add back to newdata any
    # levels not present but were in training data.  Thus droplevels(newdata) is aligned with fitted levels.

    training <- object$model[object$subset, names(object$model) != object$outcome.name, drop = FALSE]
    train.levels <- sapply(droplevels(training), levels)

    if (!identical(setdiff(names(training), names(newdata)), character(0)))
        stop("Attempting to predict based on fewer variables than those used to train the model.")
    newdata <- newdata[, names(training)]
    prediction.levels <- sapply(newdata, levels)

    new.level.flags <- rep(FALSE, nrow(newdata))
    nb.flags <- 0

    for (i in 1:length(train.levels))
    {
        if (!is.null(train.levels[[i]]))    # factor variables only
        {
            # if there are any prediction levels that have not been used to train
            new.levels <- setdiff(prediction.levels[[i]], train.levels[[i]])
            if (!identical(new.levels, character(0)))
            {
                # set flags to TRUE for any newdata row with a new factor level
                new.level.flags[newdata[, i] %in% new.levels] <- TRUE
                updated.nb.flags <- sum(new.level.flags[new.level.flags == TRUE])
                if ((updated.nb.flags - nb.flags) > 0 & !predicting.training)
                    warning(sprintf("Prediction variable %s contains categories (%s) that were not used for training. %d instances are affected.",
                                    names(training[i]), new.levels, updated.nb.flags - nb.flags))
                nb.flags <- updated.nb.flags
            }
            # if train has any levels not in prediction, then add those levels to prediction
            if (!identical(setdiff(train.levels[[i]], prediction.levels[[i]]), character(0)))
                levels(newdata[, i]) <- train.levels[[i]]
        }
    }
    return(list(newdata = newdata, new.level.flags = new.level.flags))
}
