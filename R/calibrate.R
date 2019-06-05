


#' \code{Calibrate}
#' @description Performs calibration and reweighting for a sample set according to one or more categorical/numeric variables.
#' @param formCategorical Zero or more categorical sample vectors that are to be used to weight the data
#' @param categorical.targets The target probabilities for each category listed in formCategorical
#' @param formNumeric the numeric variables associated with each sample that are to be used to create the output weights
#' @param numeric.targets the target probabilities for each numeric variable in formNumeric
#' @param lower The lower bound for weighting of an individual sample
#' @param upper The upper bound for weighting of an individual sample
#' @param input.weight The sample size
#' @param trim.iterations The number of times to run the trim loop over the final weightings
#' @return numeric A vector of weights
#' @importFrom icarus calibration
#' @export
Calibrate <- function(formCategorical = NULL, categorical.targets = NULL, formNumeric = NULL, numeric.targets = NULL, lower = NA, upper = NA, input.weight = NULL, trim.iterations = 20)
{
    # The functions defined inside here should be moreved to separate functions when included in the R package
    # Converts a list into a data frame and throws an error if it contains any missing values
    convertToDataFrame <- function(x)
    {
        x = as.data.frame(x)
        # Check that data frame is complete
        var.missing = colSums(is.na(x)) > 0
        if (any(var.missing))
        {
            var.missing = paste(names(var.missing)[var.missing], collapse = ", ")
            stop("One or more of the adjustment variables contains missing values; you cannot have missing values in adjustment variables: ", var.missing)
        }
        x
    }

    categoricalTargets <- function(adjustment.variables, categorical.targets)
    {
        targets = list()
        n.categorical = length(adjustment.variables)
        for (i in 1 : n.categorical)
        {
            tgt = categorical.targets[[i]]
            targets[[i]] = as.numeric(tgt[, 2])
            names(targets[[i]]) = tgt[, 1]
            adj.unique = unique(adjustment.variables[[i]])
            missing.targets = ! adj.unique %in% tgt[, 1]
            varname = names(adjustment.variables)[i]
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
                stop("Target category that does not appear in variable ", varname, ": ", excess.cats)
            }
            if ((sm = sum(targets[[i]])) != 1)
            {
                if (round(sm, 6) == 1)# Forcing to add up to 1 if difference likely due to rounding errors.
                targets[[i]] = prop.table(targets[[i]])
                else
                stop("Targets for ", varname, " add up to ", sm, "; they need to add up to exactly 1.")
            }
        }
        targets
    }
    # Extract and checks the targets for numeric  adjustment variables
    numericTargets <- function(targets, adjustment.variables, numeric.targets)
    {
        n.categorical = length(targets)
        n = NROW(adjustment.variables)
        for (i in 1 : (length(formNumeric)))
        {
            numvar = formNumeric[[i]]
            varname = names(formNumeric)[i]
            targets[[i + n.categorical]] = tgt = numeric.targets[i]
            if (is.na(tgt))
                stop("The target for ", varname, " is invalid.")
            if (! is.numeric(numvar))
                stop(varname, " is not a numeric variable; numeric adjustment variables must be numeric.")
            if (max(numvar) < tgt)
                stop("The target for ", varname, " of ", tgt, " is higher than the maximum value of the variable.")
            if (min(numvar) > tgt)
                stop("The target for ", varname, " of ", tgt, " is lower than the maximum value of the variable.")
        }
        targets
    }

    # Table of margins for input into icarus function
    margins <- function(targets, adjustment.variables, n.categorical)
    {
        n = NROW(adjustment.variables)
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
    # Calibration function
    calibrate <- function(adjustment.variables, margins, input.weight)
    {
        adjustment.variables$q_pop_wgt = input.weight
        icarus::calibration(data = adjustment.variables,
        margin = margins,
        colWeights = "q_pop_wgt",
        pct = TRUE,
        popTotal = NROW(adjustment.variables),
        method = "raking",
        description = FALSE)
    }
    # Trimming (not really trimming, but this is the language in the literature
    trim = function(weight, lower, upper)
    {
        weight[weight < lower] = lower
        weight[weight > upper] = upper
        weight / mean(weight)
    }

    # Trimming (not really trimming, but this is the language in the literature
    trimmedCalibrate <- function(adjustment.variables, margins, input.weight, lower, upper, trim.iterations)
    {
        weight = calibrate(adjustment.variables, margins, input.weight)
        trims = 0
        prev_diff = Inf
        dif = diff(weight, lower, upper)
        while (trims < trim.iterations &
            dif > 0.000001 &
            dif < prev_diff)
        {
            cat("Difference between bounds and weight: ", dif, "\n")
            trims = trims + 1
            weight = trim(weight, lower, upper)
            weight = calibrate(adjustment.variables, margins, weight)
            prev_diff = dif
            dif = diff(weight, lower, upper)
        }
        trim(weight, lower, upper)
    }

    # Sum of maximum differences between bounds and weight
    diff = function(weight, lower, upper)
    {
        rng = range(weight)
        max(rng[2] - upper, 0) - min(lower - rng[1], 0)
    }
    # Extract and checks the targets for categorical adjustment variables

    # Preparing inputs
    adjustment.variables = NULL
    targets = list()

    if ( (is.null(formCategorical) || length(formCategorical) == 0) && (is.null(formNumeric) || length(formNumeric) == 0)) {
        stop("Nothing to do! At least one categorical OR numeric variable required.")
    }

    # Categorical inputs
    if (! is.null(formCategorical))
    {
        adjustment.variables = convertToDataFrame(formCategorical)
        targets = categoricalTargets(adjustment.variables, categorical.targets)
    }
    n.categorical = length(targets)

    # Numeric inputs
    if (! is.null(formNumeric))
    {
        num.adjustment.variables = convertToDataFrame(formNumeric)
        adjustment.variables = if (is.null(formCategorical)) num.adjustment.variables else cbind(adjustment.variables, num.adjustment.variables)
        targets = numericTargets(targets, adjustment.variables, numeric.targets)
    }

    # Creating the table of margins
    marg = margins(targets, adjustment.variables, n.categorical)

    # Bounds
    if (is.na(lower))
        lower = 0
    if (is.na(upper))
        upper = Inf

    # Adding the weight variable or a proxy (and normalizing to a mean of 1)
    weight = if (is.null(input.weight)) 1 else input.weight / mean(input.weight)

    # Calculating/updating the weight
    result <- trimmedCalibrate(adjustment.variables, marg, weight, lower, upper, trim.iterations)

    class(result) <- "Calibrate"
    result
}

#' \code{print.Calibrate}
#' @description Print method for calibrate weights
#' @param x A vector of weights.
#' @param ... Other ignored parameters
#' @return character string
#' @method print Calibrate
#' @export
print.Calibrate <- function (x, ...) {

    ess = flipData::EffectiveSampleSize(x)
    ess.percent = round(ess / length(x) * 100)
    paste0("Effective sample size: ", flipFormat::FormatAsReal(ess, decimals = 0), " (", ess.percent, "%)")
}
