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
#' @importFrom verbs Sum
#' @export
EffectiveSampleSize <- function(weights)
{
    if (any(is.na(weights) | weights < 0 | !is.finite(weights)))
        stop("'weights' must be positive numbers.")
    Sum(weights, remove.missing = FALSE)^2 / Sum(weights^2, remove.missing = FALSE)
}


#' \code{CalibrateWeight}
#' @description Calibrates a weight to sum to the Effective Sample Size.
#' @param weights A vector of weights.
#' @param strata Strata, to perform the weight-calibration within.
#' @return vector
#' @importFrom verbs Sum
#' @export
CalibrateWeight <- function(weights, strata = NULL)
{
    .calibrate <- function(weights) weights / Sum(weights, remove.missing = FALSE) * EffectiveSampleSize(weights)
    if (is.null(strata))
        return(.calibrate(weights))
    strata <- factor(strata)
    result <- rep(NA, length(weights))
    for (st in levels(strata))
    {
        i <- st == strata
        result[i] <- CalibrateWeight(weights[i])
    }
    result
}

