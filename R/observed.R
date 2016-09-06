
#' \code{observed} The variable used as the outcome when fitting a model. .
#' @param x A model.
#' @details Extracted for the entire data set (i.e., not just the specific values used in the fitting).
#' @export
Observed <- function(x) UseMethod("Observed", x)

#' @inheritParams Observed
#' @describeIn Observed Exctracts the dependant variable from an object from its formula.
#' @importFrom stats formula
#' @export
Observed.default <- function(x)
{
    x$model[, all.vars(formula(x))[1]]
}

