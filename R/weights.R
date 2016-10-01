#' weightedSurveyDesign
#' @param data The data frame.
#' @param weights Weights The sampling weights.
#' @importFrom survey svydesign
#' @export
WeightedSurveyDesign <- function(data, weights)
{
    svydesign(ids = ~ 1, weights = weights, data = data)
}


#' \code{EffectiveSampleSize}
#' @description Uses Kish's formula for computing an effective sample size.
#' @param weights A vector of weights.
#' @references Kish, Leslie (1965). Survey Sampling. New York: Wiley.
#' @return numeric
#' @export
EffectiveSampleSize <- function(weights)
{
    if (any(is.na(weights) | weights < 0 | !is.finite(weights)))
        stop("'weights' must be positive numbers.")
    sum(weights)^2 / sum(weights^2)
}


#' \code{CalibrateWeight}
#' @description Calibrates a weight to sum to the Effective Sample Size.
#' @param weights A vector of weights.
#' @return vector
#' @export
CalibrateWeight <- function(weights)
{
    weights / sum(weights) * EffectiveSampleSize(weights)
}

