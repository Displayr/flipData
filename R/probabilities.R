#' Probabilities
#'
#' Estimates the probability of group membership for the data passed into \code{newdata} or
#' the data used to fit the model if \code{newdata} is not specified. Intended to be used
#' for the classifiers in the packages \code{flipRegression}, \code{flipMultivariates} and
#' \code{flipTrees}.
#'
#' @param object A \code{CART}, \code{MachineLearning}, \code{MachineLearningEnsemble}
#'        or \code{Regression} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#'        If not provided, the object$model is used instead.
#' @param ... Optional arguments to pass to \code{predict} or other functions.
#' @return A matrix of predicted probabilities for the observation to belong to each
#'         class label.
#' @export
Probabilities <- function(object, newdata = NULL, ...)
{
    validateProbabilityArguments(object, newdata)
    UseMethod("Probabilities")
}

#' @export
Probabilities.default <- function(object, newdata = NULL, ...)
{
    stop("object not supported")
}

throwErrorUnsupportedPredictionClass <- function(valid.classes)
{
    n.valid <- length(valid.classes)
    valid.classes <- sQuote(valid.classes)
    valid.classes <- paste0(paste0(valid.classes[seq_len(n.valid - 1)], collapse = ", "),
                            " or ", valid.classes[n.valid])
    stop(sQuote("object"), " must be a ", valid.classes, " object. ")
}

validateProbabilityArguments <- function(object, newdata)
{
    valid.classes <- c("CART", "MachineLearning", "MachineLearningEnsemble", "Regression")
    not.valid.object <- !inherits(object, valid.classes)
    if (not.valid.object)
        throwErrorUnsupportedPredictionClass(valid.classes)
    # Machine Learning Ensembles don't have a model slot
    if (inherits(object, "MachineLearningEnsemble")) return()
    # If newdata is not provided, use the model data and check it is valid
    if (is.null(newdata))
        newdata <- object[["model"]]
    valid.newdata <- is.data.frame(newdata) && NROW(newdata) > 0
    if (!valid.newdata)
        stop(sQuote("newdata"), " must be a data.frame with at least one observation")
}
