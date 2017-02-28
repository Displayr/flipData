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
#' superset of the variables used to fit the model or an error results. If a factor variable contains
#' more levels than the factor used for fitting, a warning is given and instances with such new factor
#' levels are set to NA in the returned data frame.  The levels of the returned data frame are set to those
#' of the fitted model.
#' @param object A model object for which prediction is desired.
#' @param newdata A data frame including the variables used to fit the model.
#' @export
CheckPredictionVariables <- function(object, newdata)
{
    training <- object$model[object$subset, names(object$model) != object$outcome.name, drop = FALSE]
    if (ncol(training) == 0)
        return(newdata)
    train.levels <- lapply(droplevels(training), levels)

    if (!identical(setdiff(names(training), names(newdata)), character(0)))
        stop("Attempting to predict based on fewer variables than those used to train the model.")
    newdata <- newdata[, names(training), drop = FALSE]
    prediction.levels <- lapply(newdata, levels)

    #train.list <- paste(train.levels, collapse = " ")
    #prediction.list <- paste(prediction.levels, collapse = " ")
    #warning(sprintf("Trained factors are %s, Prediction factors are %s", train.list, prediction.list))

    for (i in 1:length(train.levels))
    {
        if (!is.null(train.levels[[i]]))    # factor variables only
        {
            # if there are any prediction levels that have not been used to train
            new.levels <- setdiff(prediction.levels[[i]], train.levels[[i]])
            if (!identical(new.levels, character(0)))
            {
                # set all new factor levels to NA
                new.level.rows <- nrow(newdata[newdata[, i] %in% new.levels, ])
                newdata[newdata[, i] %in% new.levels, ] <- NA
                if (new.level.rows > 0)
                    warning(sprintf("Prediction variable %s contains categories (%s) that were not used for training. %d instances are affected.",
                                    names(training[i]), new.levels, new.level.rows))
            }
            # Set prediction levels to those used for training
            newdata[, i] <- droplevels(newdata[, i])
            #warning(sprintf("%d : Trained levels are %s, Prediction levels are %s",
            #                i, paste(train.levels[[i]], collapse = " "), paste(levels(newdata[, i]), collapse = " ")))
            levels(newdata[, i]) <- train.levels[[i]]
        }
    }
    return(newdata)
}
