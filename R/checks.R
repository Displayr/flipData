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
#' @importFrom flipFormat Labels
#' @export
CheckPredictionVariables <- function(object, newdata)
{
    regression.model <- inherits(object, "Regression")
    relevant.cols <- names(object$model)[names(object$model) != object$outcome.name]
    # Check if a regression object is being processed and the outlier removal has been implemented.
    outliers.removed <- (regression.model && !all(non.outliers <- object$non.outlier.data))
    if (outliers.removed)
        training <- object$estimation.data[non.outliers, relevant.cols, drop = FALSE]
    else # otherwise use the possibly subsetted data
        training <- object$model[object$subset, relevant.cols, drop = FALSE]

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
                    # Use label if available
                    factor.label <- Labels(object$model, names.to.lookup = train.factor)
                    if (outliers.removed) # Check if all occurences of new.levels were removed as outliers
                    {
                        in.training <- new.levels %in% training[[train.factor]]
                        in.estimation <- new.levels %in% object$estimation.data[[train.factor]]
                        warning.due.to.outlier.removal <- all(in.estimation & !in.training)
                        warning.msg <- checkPredictionWarningMessage(factor.label, level.counts, warning.due.to.outlier.removal)
                    } else
                        warning.msg <- checkPredictionWarningMessage(factor.label, level.counts)
                    # Set all new factor levels to NA
                    newdata[newdata[, train.factor] %in% new.levels, train.factor] <- NA
                    warning(warning.msg)
                }
            }
            # Set prediction levels to those used for training
            saved.atrributes <- newdata[, train.factor]
            newdata[, train.factor] <- droplevels(newdata[, train.factor])
            newdata[, train.factor] <- factor(as.character(newdata[, train.factor]),
                                              levels = train.levels[[train.factor]],
                                              ordered = is.ordered(training[, train.factor]))
            newdata[, train.factor] <- CopyAttributes(newdata[, train.factor], saved.atrributes)
        }
    }
    return(newdata)
}

#' Creates an informative warning message about cases where categorical data has had a level entirely remvoed from the training
#' data and has been input in the prediction data.
#' @param label The label of the variable in question
#' @param level.counts A named integer vector which has the counts of each level that applies in this scenario,
#'    the names of the vector are the level labels.
#' @param warning.due.to.outlier.removal Logical to flag if this warning is being thrown only because of the automated outlier removal.
#' @noRd
checkPredictionWarningMessage <- function(label, level.counts, warning.due.to.outlier.removal = FALSE)
{
    levels <- names(level.counts)
    variable.label <- sQuote(label)
    categories <- ngettext(length(levels), "the category", "categories")
    # Describe how many cases are affected
    n.levels <- length(level.counts)
    if (n.levels == 1)
        instances <- ngettext(level.counts, "instance was", "instances were")
    else
        instances <- "instances respectively were"
    # Give the reason for the warning being thrown
    context.msg <- paste0("that ", ngettext(length(levels), "was", "were"), " not used in the training data")
    if (warning.due.to.outlier.removal)
        context.msg <- paste0(context.msg, paste0(" since the automated outlier removal identified those ",
                                                  "observations as outliers and removed them"))
    # Give grammatically correct description of counts.
    if (n.levels == 2)
    {
        levels <- paste0(sQuote(levels), collapse = " and ")
        level.counts <- paste0(level.counts, collapse = " and ")
    }
    else if (n.levels > 2)
    {
        levels <- paste0(c(paste0(sQuote(levels[1:(n.levels - 1)]), collapse = ", "),
                           sQuote(levels[n.levels])), collapse = " and ")
        level.counts <- paste0(c(paste0(level.counts[1:(n.levels - 1)], collapse = ", "),
                                 level.counts[n.levels]), collapse = " and ")
    } else
        levels <- sQuote(levels)

    # Collate and return the message
    sprintf(paste0("The prediction variable %s contained ", categories, " (%s) ", context.msg, ". ",
                   "It is not possible to predict outcomes in these cases and they are coded as missing as a result. ",
                   "%s ", instances, " affected. ",
                   "If non-missing predictions are required, consider merging categories if merging categories ",
                   "is applicable for this variable."),
            variable.label, levels, level.counts)
}
