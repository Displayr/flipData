#' \code{WeightsDialog}
#' @description Code for the Displayr Weighting dialog box.
#' @param categorical.variables An optional list or data frame of categorical adjustment variables.
#' @param categorical.targets The target probabilities for each category listed in \code{categorical.variables}
#' @param numeric.variables An optional list or data frame of categorical adjustment variables.
#' @param numeric.targets the target mean for each numeric variable in numeric.variables
#' @param lower A lower bound weight value (not guaranteed to be achieved).
#' @param upper An upper bound weight value (not guaranteed to be achieved).
#' @param calfun The calibration function: \code{"Raking"} (Default), \code{"Linear"}, or \code{"Logit"}.
#' @param input.weight An optional weight variable; if supplied, the created weight is created to be as close
#' to this input.weight as possible
#' @param force.to.n Force the sum of weights to equal the sample size.
#' @return numeric A vector of weights
#' @export
WeightingDialog <- function(categorical.variables = NULL,
                      categorical.targets = NULL,
                      numeric.variables = NULL,
                      numeric.targets = NULL,
                      lower = "",
                      upper = "",
                      calfun = c("Raking", "Linear", "Logit")[1],
                      force.to.n = TRUE,
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
    gross <- NULL
    n.categorical = if(is.null(categorical.variables)) 0 else NCOL(categorical.variables)
    if (n.categorical > 0 )
    {
        adjustment.variables = convertToDataFrame(categorical.variables)
        categorical.targets = if (is.null(categorical.targets) || is.list(categorical.targets)) categorical.targets else list(categorical.targets)
        gross <- rep(NA, n.categorical)
        for (i in 1:n.categorical)
        {
            nm <- as.numeric(categorical.targets[[i]][, 2])
            gross[i] <- sum(nm)
            categorical.targets[[i]][,2] <- prop.table(nm)
        }
        targets = categoricalTargets(adjustment.variables, categorical.targets)
    }

    # Numeric inputs
    has.numerics <- !is.null(numeric.variables)
    if (has.numerics)
    {
        num.adjustment.variables = convertToDataFrame(numeric.variables)
        adjustment.variables = if (is.null(categorical.variables)) num.adjustment.variables else cbind(adjustment.variables, num.adjustment.variables)
        targets = numericTargets(targets, adjustment.variables, numeric.targets)
    }

    # Adding input.weight or a proxy (and normalizing to a mean of Total / n)
    n = NROW(adjustment.variables)
    weight = if (is.null(input.weight)) NULL else input.weight / mean(input.weight)

    # Removing empty factor levels
    if (n.categorical > 0)
        for (i in 1:n.categorical) # ordered = FALSE needed for weirdness in survey package
            adjustment.variables[[i]] = factor(adjustment.variables[[i]], ordered = FALSE)

    # Creating the table of margins/targets in the desired format
    marg = createMargins(targets, adjustment.variables, n.categorical, FALSE, "survey")
    # Calculating/updating the weight
    wgt = computeWeightsDialog(adjustment.variables, has.numerics, marg, weight, lower, upper, calfun)
    wgt = if (force.to.n) wgt / mean(wgt) else
    {
        if (!is.null(gross))
            wgt * (gross[[1]] / n / mean(wgt))
        else # We don't have enough information to gross, so just return the weight "as is"
            wgt
    }
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
    if (lower == "" & upper == "" & !has.numerics & is.null(input.weight))
        stop("This should be processed via the existing Q algorithm and this code should not have been called.")
    if (is.null(input.weight))
        input.weight <- rep(1, NROW(adjustment.variables))
    # Bounds
    lower = if (lower == "") lower = 0 else as.numeric(lower)
    upper = if (upper == "") upper = Inf else as.numeric(upper)
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
            checkSolverStatus(res)
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
