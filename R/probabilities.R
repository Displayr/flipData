#' Goodness-of-Fit Plot
#' \code{Probabilities} A generic function used to extract one or more
#' variables containing probabilities relating to cases (e.g., segment membership).
#' @param object An object for which probabilities are desired.
#' @param ... Additional argument
#' @export
Probabilities <- function(object, ...) {
    UseMethod("GoodnessOfFitPlot")
}

#' @describeIn GoodnessOfFitPlot  Default goodness-of-fit plot
#' @importFrom stats lm
#' @export
Probabilities.default = function(object, ...)
{
    stop("No 'Probabilities' method exists for this class of objects.")
}
