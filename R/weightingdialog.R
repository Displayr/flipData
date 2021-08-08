#' \code{WeightsDialog}
#' @description Code for the Displayr Weighting dialog box.
#' @param categorical.variables An optional list or data frame of categorical adjustment variables.
#' @param categorical.targets The target probabilities for each category listed in \code{categorical.variables}
#' @param numeric.variables An optional list or data frame of categorical adjustment variables.
#' @param numeric.targets the target mean for each numeric variable in numeric.variables
#' @param lower A lower bound weight value (not guaranteed to be achieved).
#' @param upper An upper bound weight value (not guaranteed to be achieved).
#' @param calfun The calibration function: Raking (Default), Linear, Quadratic.
#' @param input.weight An optional weight variable; if supplied, the created weight is created to be as close
#' to this input.weight as possible
#' @return numeric A vector of weights
#' @importFrom icarus calibration
#' @export


WeightingDialog <- function(categorical.variables = NULL,
                      categorical.targets = NULL,
                      numeric.variables = NULL,
                      numeric.targets = NULL,
                      lower = "",
                      upper = "",
                      calfun = c("Raking", "Linear", "Logit")[1],
                      input.weight = NULL)
{

    # Preparing inputs
    adjustment.variables = NULL
    calfun = tolower(calfun)
    targets = list()

    if ((is.null(categorical.variables) || length(categorical.variables) == 0) && (is.null(numeric.variables) || length(numeric.variables) == 0)) {
        stop("Nothing to do! At least one categorical OR numeric variable required.")
    }

    # Categorical inputs
    if (!is.null(categorical.variables))
    {
        adjustment.variables = convertToDataFrame(categorical.variables)
        categorical.targets = if (is.null(categorical.targets) || is.list(categorical.targets)) categorical.targets else list(categorical.targets)
        targets = categoricalTargets(adjustment.variables, categorical.targets)
    }
    n.categorical = length(targets)

    # Numeric inputs
    has.numerics <- !is.null(numeric.variables)
    if (has.numerics)
    {
        num.adjustment.variables = convertToDataFrame(numeric.variables)
        adjustment.variables = if (is.null(categorical.variables)) num.adjustment.variables else cbind(adjustment.variables, num.adjustment.variables)
        targets = numericTargets(targets, adjustment.variables, numeric.targets)
    }

    # Adding input.weight or a proxy (and normalizing to a mean of 1)
    n = NROW(adjustment.variables)
    weight = if (is.null(input.weight)) rep(1, n) else input.weight / mean(input.weight)

    # Removing empty factor levels
    if (n.categorical > 0)
        for (i in 1:n.categorical) # ordered = FALSE needed for weirdness in survey package
            adjustment.variables[[i]] = factor(adjustment.variables[[i]], ordered = FALSE)

    # Creating the table of margins/targets in the desired format
    marg = createMargins(targets, adjustment.variables, n.categorical, FALSE, "survey")
    # Calculating/updating the weight
    wgt = computeWeightsDialog(adjustment.variables, has.numerics, marg, weight, lower, upper, calfun)
    wgt / mean(wgt)
    class(wgt) <- "Calibrate"
    wgt
}



# Calibration function
#' @importFrom survey calibrate rake
#' @importFrom stats model.matrix weights terms.formula
#' @importFrom CVXR Variable Minimize Problem entr solve
#' @importFrom verbs Sum
computeWeightsDialog <- function(adjustment.variables, has.numerics, margins, input.weight, lower, upper, calfun)
{
    if (lower == "" & upper == "" & !has.numerics)
        stop("This should be processed via the existing Q algorithm and this code should not have been called.")
    # Bounds
    if (lower == "")
        lower = 0
    if (upper == "")
        upper = Inf
    if (calfun == "logit")
    {
        upper = min(1000, upper)# Avoiding having a denominator if Inf in Phi_R
        # We are using CVXR to do logit just in case survey package doesn't scale well, so there is a fallback
        # That is, we could have done it all in survey, but
            formula = createFormula(adjustment.variables)
            # wrapping formula with terms.formula fixes the
            # "argument "frml" is missing, with no default"
            # bug on the R server
            X <- model.matrix(object = terms.formula(formula),
                              data = adjustment.variables)
            A <- input.weight * X
            n <- NROW(X)
            g <- Variable(n)
            constraints = list(t(A) %*% g == margins)
            Phi_R = Minimize(sum(input.weight * (-entr((g - lower) / (upper - lower)) - (entr((upper - g) / (upper - lower))))))
            p = Problem(Phi_R, constraints)
            res = solve(p)
            as.numeric(input.weight * res$getValue(g))
    } else {
        weights(calibrate(svydesign(ids = ~1, weights = ~input.weight, data = adjustment.variables),
                          createFormula(adjustment.variables),
                          maxit = 1000,
                          epsilon = 1e-8,
                          bounds = c(lower, upper),
                          population = margins,
                          calfun = calfun))
    }
}

