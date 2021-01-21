#' \code{Calibrate}
#' @description Creates a sampling weight based on raking/calibration. Raking is used if it can be used (i.e.,
#' no numeric adjustment variables and \code{always.calibrate} not \code{FALSE})
#' @param categorical.variables An optional list or data frame of categorical adjustment variables.
#' @param categorical.targets The target probabilities for each category listed in \code{categorical.variables}
#' @param numeric.variables An optional list or data frame of categorical adjustment variables.
#' @param numeric.targets the target mean for each numeric variable in numeric.variables
#' @param lower A lower bound weight value (not guaranteed to be achieved).
#' @param upper An upper bound weight value (not guaranteed to be achieved).
#' @param trim.iterations The number of times to run the trim loop over the final weightings
#' @param always.calibrate If \code{FALSE}, whcih is the default,
#' problems with only categorical adjustment variables are solved via
#' iterative-proprtional fitting (raking). Otherwise, they are solved via calibration.
#' @param package The R package used to calibrate the model when raking is not conducted.
#' Defaults to \code{CVXR} (see https://cvxr.rbind.io/cvxr_examples/cvxr_survey_calibration/). Other options
#' are \code{icarus} and \code{survey}. .
#' Use icarus with care, as sometimes target categories can be switched around
#' @param subset A logical vector indicating which subset of cases should be used to create the weight
#' @param input.weight An optional weight variable; if supplied, the created weight is created to be as close
#' to this input.weight as possible
#' @return numeric A vector of weights
#' @importFrom icarus calibration
#' @export
Calibrate <- function(categorical.variables = NULL,
                      categorical.targets = NULL,
                      numeric.variables = NULL,
                      numeric.targets = NULL,
                      lower = NA,
                      upper = NA,
                      trim.iterations = 20,
                      package = c("CVXR", "survey", "icarus")[1],
                      always.calibrate = FALSE,
                      subset = NULL,
                      input.weight = NULL)
{

    # Preparing inputs
    adjustment.variables = NULL
    targets = list()

    if ((is.null(categorical.variables) || length(categorical.variables) == 0) && (is.null(numeric.variables) || length(numeric.variables) == 0)) {
        stop("Nothing to do! At least one categorical OR numeric variable required.")
    }

    # Categorical inputs
    if (!is.null(categorical.variables))
    {
        adjustment.variables = convertToDataFrame(categorical.variables)
        categorical.targets = if (is.null(categorical.targets) || is.list(categorical.targets)) categorical.targets else list(categorical.targets)
        targets = categoricalTargets(adjustment.variables, categorical.targets, subset)
    }
    n.categorical = length(targets)

    # Numeric inputs
    if (!is.null(numeric.variables))
    {
        num.adjustment.variables = convertToDataFrame(numeric.variables)
        adjustment.variables = if (is.null(categorical.variables)) num.adjustment.variables else cbind(adjustment.variables, num.adjustment.variables)
        targets = numericTargets(targets, adjustment.variables, numeric.targets, subset)
    }

    # Bounds
    if (is.na(lower))
        lower = 0
    if (is.na(upper))
        upper = Inf

    # Adding the weight variable or a proxy (and normalizing to a mean of 1)
    n = NROW(adjustment.variables)
    weight = if (is.null(input.weight)) rep(1, n) else input.weight / mean(input.weight)

    # Filtering data if required
    if (!is.null(subset) & length(subset) > 1)
    {
        filter.lookup = which(subset)
        adjustment.variables = adjustment.variables[filter.lookup, , drop = FALSE]
        weight = weight[filter.lookup]
    }

    # Removing empty factor levels
    if (n.categorical > 0)
        for (i in 1:n.categorical) # ordered = FALSE needed for weirdness in survey package
            adjustment.variables[[i]] = factor(adjustment.variables[[i]], ordered = FALSE)

    # Working out if to rake
    raking = n.categorical == length(adjustment.variables) & !always.calibrate
    if (raking & min(sapply(targets, min)) == 0)
        stop("One of your targets is set to 0. To achieve this, remove this category from the target and instead apply a filter to the weight to remove the category (or, in the R CODE set always.calibrate = TRUE)")

    # Creating the table of margins/targets in the desired format
    marg = createMargins(targets, adjustment.variables, n.categorical, raking, package)

    # Calculating/updating the weight
    result <- trimmedCalibrate(adjustment.variables, marg, weight, lower, upper, trim.iterations, raking, package)

    if (length(result) < n)
    {
        out = rep(NA, length(subset))
        out[filter.lookup] = result
        result = out
    }
    class(result) <- "Calibrate"
    result
}

# Converts lists to data fames, checking the data for errors along the way.
#' @importFrom verbs SumColumns
convertToDataFrame <- function(x)
{
    x = as.data.frame(x, stringsAsFactors = TRUE)
    # Check that data frame is complete
    var.missing = SumColumns(is.na(x), remove.missing = FALSE) > 0
    if (any(var.missing))
    {
        var.missing = paste(names(var.missing)[var.missing], collapse = ", ")
        stop("One or more of the adjustment variables contains missing values; you cannot have missing values in adjustment variables: ", var.missing)
    }
    x
}

## Checks and tidies categorical targets
#' @importFrom stringr str_trim
#' @importFrom verbs Sum
categoricalTargets <- function(adjustment.variables, categorical.targets, subset)
{
    targets = list()
    n.categorical = length(adjustment.variables)
    if (n.categorical != length(categorical.targets)) {
        stop("The number of categorical adjustment variables needs to be the same as the number of sets of targets (it isn't)")
    }
    for (i in 1 : n.categorical)
    {
        tgt = categorical.targets[[i]]
        targets[[i]] = suppressWarnings(as.numeric(str_trim(tgt[, 2])))
        names(targets[[i]]) = tgt[, 1]
        adj.variable = if(is.null(subset)) adjustment.variables[[i]] else droplevels(adjustment.variables[[i]][subset])
        adj.unique = levels(adj.variable)
        missing.targets = ! adj.unique %in% tgt[, 1]
        varname = names(adjustment.variables)[i]
        if(any(idx <- is.na(targets[[i]])))
        {
            bad.inputs <- paste(paste(tgt[idx, 1], tgt[idx, 2], sep = " - "), collapse = ", ")
            stop("Invalid target values for ", varname, ": ", bad.inputs, ".")
        }

        if (any(missing.targets))
        {
            missing.cats = paste0(adj.unique[missing.targets], collapse = ", ")
            if (nchar(missing.cats) > 0)
                stop("No targets have been provided for ", varname, ": ", missing.cats)
        }
        excess.targets = ! tgt[, 1] %in% adj.unique
        if (any(excess.targets))
        {
            excess.cats = paste0(tgt[excess.targets, 1], collapse = ", ")
            if (nchar(excess.cats) > 0)
                stop("Target category that does not appear in variable ", varname, ": ",
                     excess.cats, (if(is.null(subset)) "" else " (after applying filter/subset)"))
        }
        if ((sm = Sum(targets[[i]], remove.missing = FALSE)) != 1)
        {
            if (round(sm, 6) == 1)# Forcing to add up to 1 if difference likely due to rounding errors.
                targets[[i]] = prop.table(targets[[i]])
            else
                stop("Targets for ", varname, " add up to ", sm, "; they need to add up to exactly 1.")
        }
        # Reordering targets to match order of levels
        targets[[i]] = targets[[i]][adj.unique]
    }
    targets
}

# Extract and checks the targets for numeric  adjustment variables
numericTargets <- function(targets, adjustment.variables, numeric.targets, subset)
{
    n.categorical = length(targets)
    n = NROW(adjustment.variables)
    if (length(adjustment.variables) - n.categorical != length(numeric.targets))
    {
        stop("The number of numeric adjustment variables needs to be the same as the number of sets of targets (it isn't)")
    }
    n.numeric = length(adjustment.variables) - n.categorical
    for (i in 1:n.numeric)
    {
        i.adjustment.variables = i + n.categorical
        numvar = adjustment.variables[[i.adjustment.variables]]
        if(!is.null(subset))
            numvar = numvar[subset]
        varname = names(adjustment.variables)[i.adjustment.variables]
        targets[[i.adjustment.variables]] = tgt = numeric.targets[i]
        if (is.na(tgt))
            stop("The target for ", varname, " is invalid.")
        if (!is.numeric(numvar))
            stop(varname, " is not a numeric variable; numeric adjustment variables must be numeric.")
        if (max(numvar) < tgt)
            stop("The target for ", varname, " of ", tgt, " is higher than the maximum value of the variable", (if(is.null(subset)) "" else " (after applying filter/subset)"))
        if (min(numvar) > tgt)
            stop("The target for ", varname, " of ", tgt, " is lower than the maximum value of the variable", (if(is.null(subset)) "" else " (after applying filter/subset)"))
    }
    targets
}

# Formats table of margins, as required by the various functions and packages
#' @importFrom stats terms.formula
createMargins <- function(targets, adjustment.variables, n.categorical, raking, package)
{
    # Creating the targets in the required format
    if (raking) # Iterative post-stratification
    {
        margins = list()
        for (i in seq_along(adjustment.variables))
        {
            tgt = targets[[i]]
            out = data.frame(x = names(tgt), Freq = unname(tgt))
            names(out)[1] = names(adjustment.variables)[i]
            margins[[i]] = out
        }
    } else {
        n = NROW(adjustment.variables)
        if (package == "icarus")
        {
            n.cats = sapply(targets, length)
            nms = names(adjustment.variables)
            max.cats = max(n.cats)
            n.vars = length(adjustment.variables)
            tgts = matrix("0", n.vars, max.cats)
            for (i in 1 : n.vars)
            {
                if (i <= n.categorical) {
                    tgts[i, 1 : n.cats[i]] = targets[[i]]
                } else {
                    tgts[i, 1] = targets[[i]] * n # Sums rather than averages in icarus
                    n.cats[i] = 0 # icarus assumes 0 in this field for numeric variables
                }
            }
            margins = cbind(nms, n.cats, tgts)
        }
        else { # survey
            margins = c(1)
            for (i in seq_along(adjustment.variables))
            {
                new.margin = if (i <= n.categorical) targets[[i]][-1] else targets[[i]]
                margins = c(margins, new.margin)
            }
            margins = margins * n
            formula = createFormula(adjustment.variables)
            names(margins) = colnames(model.matrix(terms.formula(formula), data = adjustment.variables))
        }
    }
    margins
}
# Calibration function
#' @importFrom icarus calibration
#' @importFrom survey calibrate rake
#' @importFrom stats model.matrix weights terms.formula
#' @importFrom CVXR Variable Minimize Problem entr solve
#' @importFrom verbs Sum
computeCalibrate <- function(adjustment.variables, margins, input.weight, raking, package)
{
    if (package == "survey" | raking)
        design = svydesign(ids = ~1, weights = ~input.weight, data = adjustment.variables)
    if (raking)
    {
        sample.margins = lapply(names(adjustment.variables), function(x) as.formula(paste("~", x)))
        weights(rake(design,
                      sample.margins,
                      margins,
                      control = list(maxit = 1000, epsilon = 1e-10)))
    } else switch(package,
                  icarus = calibration(data = data.frame(adjustment.variables,
                                                         q_pop_wgt = input.weight),
                                       marginMatrix = margins,
                                       colWeights = "q_pop_wgt",
                                       pct = TRUE,
                                       popTotal = NROW(adjustment.variables),
                                       method = "raking",
                                       description = FALSE),
                  survey = weights(calibrate(design,
                                             createFormula(adjustment.variables),
                                             maxit = 1000,
                                             epsilon = 1e-8,
                                             population = margins,
                                             calfun = "raking")),
                  CVXR = {
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
                      Phi_R = Minimize(Sum(input.weight * (-entr(g) - g + 1), remove.missing = FALSE))
                      p = Problem(Phi_R, constraints)
                      res = solve(p)
                      as.numeric(input.weight * res$getValue(g))
                      }
                  )
}

# Creating formula as required by survey package
createFormula = function(adjustment.variables)
{
    as.formula(paste("~", paste(names(adjustment.variables), collapse = "+")))
}

# Trims the weights (not really trimming, but this is the language in the literature
trimWeight = function(weight, lower, upper)
{
    weight[weight < lower] = lower
    weight[weight > upper] = upper
    weight / mean(weight)
}

# Trimming of weight (not really trimming, but this is the language in the literature
trimmedCalibrate <- function(adjustment.variables, margins, input.weight, lower, upper, trim.iterations, raking, package)
{
    weight = computeCalibrate(adjustment.variables, margins, input.weight, raking, package)
    trims = 0
    prev_diff = Inf
    dif = diffCalculation(weight, lower, upper)
    while (trims < trim.iterations &
           dif > 0.000001 &
           dif < prev_diff)
    {
        #cat("Difference between bounds and weight: ", dif, "\n")
        trims = trims + 1
        weight = trimWeight(weight, lower, upper)
        weight = computeCalibrate(adjustment.variables, margins, weight, raking, package)
        prev_diff = dif
        dif = diffCalculation(weight, lower, upper)
    }
    trimWeight(weight, lower, upper)
}

# Sum of maximum differences between bounds and weight
diffCalculation = function(weight, lower, upper)
{
    rng = range(weight)
    max(rng[2] - upper, 0) - max(lower - rng[1], 0)
}

#' \code{print.Calibrate}
#' @description Print method for calibrate weights
#' @param x A n object of class 'Calibrate'.
#' @param ... Other ignored parameters
#' @return character string
#' @method print Calibrate
#' @importFrom flipFormat FormatAsReal FormatAsPercent
#' @export
print.Calibrate <- function (x, ...)
{
    x = x[!is.na(x)]
    instruction.for.getting.variable = ""
    product = get0("productName")
    if (!is.null(product))
        instruction.for.getting.variable <- "\n\nTo save the variable, click ACTION > Save weight variable"
    ess = EffectiveSampleSize(x)
    ess.percent = round(ess / length(x) * 100)
    n = length(x)
    rng = range(x)


    cat(paste0("The weight has been computed for ", FormatAsReal(n, decimals = 0), " observations.\n",
              "The weight has an effective sample size of ",
              FormatAsReal(ess, decimals = 0),
              " (",  ess.percent, "%)\n",
              "Smallest weight is ", FormatAsReal(rng[1], decimals = 3), "\n",
              "Largest weight is ", FormatAsReal(rng[2], decimals = 3), " (", FormatAsReal(rng[2] / rng[1], decimals = 1), " times the smallest weight)",
              instruction.for.getting.variable))
}
