#' Probabilities
#' \code{Probabilities} A generic function used to extract one or more
#' variables containing probabilities relating to cases (e.g., segment membership).
#' @param object An object for which probabilities are desired.
#' @param ... Additional argument
#' @export
Probabilities <- function(object, ...) {
    UseMethod("GoodnessOfFitPlot")
}

#' @inheritParams Probabilities
#' @describeIn Probabilities Error occurs as no method has been specified.
#' @export
Probabilities.default = function(object, ...)
{
    stop("No 'Probabilities' method exists for this class of objects.")
}
