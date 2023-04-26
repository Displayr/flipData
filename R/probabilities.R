#' Probabilities
#'
#' Estimates the probability of group membership for the data passed into \code{newdata} or
#' the data used to fit the model if \code{newdata} is not specified. Intended to be used
#' for the classifiers in the packages \code{flipRegression} and \code{flipMultivariates}.
#'
#' @param object A \code{MachineLearning} or \code{Regression} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#'        If not provided, the object$model is used instead.
#' @param ... Optional arguments to pass to \code{predict} or other functions.
#' @return A matrix of predicted probabilities for the observation to belong to each
#'         class label.
#' @export
Probabilities <- function(object, newdata = NULL, ...)
{
    newdata <- validateNewData(object, newdata)
    UseMethod("Probabilities")
}

validateNewData <- function(object, newdata)
{
    # CheckPredictionVariables is still required without newdata because empty training levels are removed
    if (is.null(newdata))
        return(suppressWarnings(CheckPredictionVariables(object, object$model)))
    stopifnot("newdata must be a data.frame" = is.data.frame(newdata),
              "Need at least one observation in the newdata argument" = NROW(newdata) > 0)
    CheckPredictionVariables(object, newdata)
}

Probabilities.default <- function(object, newdata = NULL, ...)
{
    stop("Probabilities is not implemented for this object type")
}
