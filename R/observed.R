
#' \code{Observed}
#'
#' Returns the variable used as the outcome when fitting a model.
#' @param x A model.
#' @details Extracted for the entire data set (i.e., not just the specific values used in the fitting).
#' @export
Observed <- function(x) UseMethod("Observed", x)

#' @param x A model.
#' @describeIn Observed Extracts the dependant variable from an object from its formula.
#' @importFrom stats formula
#' @export
Observed.default <- function(x)
{
    if (!is.null(x$outcome.name))
        return(x$model[, x$outcome.name])
    x$model[, all.vars(formula(x))[1]]
}

#' @param x A model.
#' @describeIn Observed Extracts the dependant variable directly from an object.
#' @export
Observed.MachineLearningEnsemble <- function(x)
{
    x$outcome
}
