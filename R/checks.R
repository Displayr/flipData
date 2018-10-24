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
#' @importFrom flipU TrimWhitespace
#' @export
CheckForUniqueVariableNames <- function(formula)
{
    #formula.as.character <- strsplit(toString(formula), "\\,")[[1]]
    formula.as.character <- as.character(formula)
    formula.as.character <- gsub('\\$', '', formula.as.character)
    #formula.as.character <- TrimWhitespace(strsplit(formula.as.character, "\\+"))
    #formula.as.character <- TrimWhitespace(strsplit(formula.as.character, "\\~"))
    dep <- TrimWhitespace(formula.as.character[2])
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
#' Verifies that \code{newdata} is consistent with data used to used to fit a model.
#' The \code{newdata} must contain a superset of the variables used to fit the model or an error results.
#' Variables that are factors in the training data are coerced to factors in \code{newdata}.
#' If a factor variable in \code{newdata} contains additional levels than the factor used for fitting,
#' a warning is given and instances with such additional levels are set to NA in the returned data.
#' The levels of the returned data frame are set to those of the fitted model.
#'
#' @param object A model object for which prediction is desired.
#' @param newdata A \code{data.frame} including the variables used to fit the model.
#' @importFrom flipU CopyAttributes
#' @export
CheckPredictionVariables <- function(object, newdata)
{
    # Check that newdata contains all training variables
    training <- object$model[object$subset, names(object$model) != object$outcome.name, drop = FALSE]
    if (ncol(training) == 0)
        return(newdata)
    if (!identical(setdiff(names(training), names(newdata)), character(0)))
        stop("Attempting to predict based on fewer variables than those used to train the model.")

    # Identify training factors
    train.levels <- lapply(droplevels(training), levels)
    train.levels <- train.levels[!sapply(train.levels, is.null)]

    # Use xlevels attribute with training levels if available
    att.levels <- attr(object, "xlevels")
    if (!is.null(att.levels) && length(att.levels) != 0)
        train.levels <- att.levels

    # Remove unused new variables and ensure training factors are factors
    newdata <- newdata[, names(training), drop = FALSE]
    newdata[, names(train.levels)] <- lapply(newdata[, names(train.levels), drop = FALSE], as.factor)
    prediction.levels <- lapply(newdata, levels)
    prediction.levels <- prediction.levels[!sapply(prediction.levels, is.null)]

    # For each training factor, identify whether any prediction levels have not been used for training.
    if (length(train.levels) > 0)
    {
        for (train.factor in names(train.levels))
        {
            # Find newdata levels that have not been used to train
            new.levels <- setdiff(prediction.levels[[train.factor]], train.levels[[train.factor]])

            if (!identical(new.levels, character(0)))
            {
                level.counts <- table(newdata[, train.factor])
                level.counts <- level.counts[new.levels]
                level.counts <- level.counts[level.counts != 0]

                if (length(level.counts) != 0) # some newdata instances have new levels
                {
                    # Set all new factor levels to NA
                    newdata[newdata[, train.factor] %in% new.levels, train.factor] <- NA
                    warning(sprintf("Prediction variable %s contains categories (%s) that were not used for training. %d instances are affected. ",
                                    train.factor, names(level.counts), level.counts))
                }
            }
            # Set prediction levels to those used for training
            saved.atrributes <- newdata[, train.factor]
            newdata[, train.factor] <- droplevels(newdata[, train.factor])
            newdata[, train.factor] <- factor(as.character(newdata[, train.factor]), levels = train.levels[[train.factor]])
            newdata[, train.factor] <- CopyAttributes(newdata[, train.factor], saved.atrributes)
        }
    }
    return(newdata)
}
