#' \code{Calibrate}
#' @description Creates a sampling weight based on raking/calibration.
#' @param categorical.variables An optional list or data frame of categorical adjustment variables.
#' @param categorical.targets The target probabilities for each category listed in \code{categorical.variables}
#' @param numeric.variables An optional list or data frame of categorical adjustment variables.
#' @param numeric.targets the target mean for each numeric variable in numeric.variables
#' @param lower A lower bound weight value (not guaranteed to be achieved).
#' @param upper An upper bound weight value (not guaranteed to be achieved).
#' @param trim.iterations The number of times to run the trim loop over the final weightings
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
                      subset = NULL,
                      input.weight = NULL)
{
    # Extract and checks the targets for categorical adjustment variables

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
        targets = categoricalTargets(adjustment.variables, categorical.targets)
    }
    n.categorical = length(targets)

    # Numeric inputs
    if (!is.null(numeric.variables))
    {
        num.adjustment.variables = convertToDataFrame(numeric.variables)
        adjustment.variables = if (is.null(categorical.variables)) num.adjustment.variables else cbind(adjustment.variables, num.adjustment.variables)
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

# Converts lists to data fames, checking the data for errors along the way.
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

# Checks and tidies categorical targets
categoricalTargets <- function(adjustment.variables, categorical.targets)
{
    targets = list()
    n.categorical = length(adjustment.variables)
    if (n.categorical != length(categorical.targets)) {
        stop("The number of categorical adjustment variables needs to be the same as the number of sets of targets (it isn't)")
    }
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
    if (length(adjustment.variables) - n.categorical != length(numeric.targets)) {
        stop("The number of numeric adjustment variables needs to be the same as the number of sets of targets (it isn't)")
    }
    n.numeric = length(adjustment.variables) - n.categorical
    for (i in 1:n.numeric)
    {
        i.adjustment.variables = i + n.categorical
        numvar = adjustment.variables[[i.adjustment.variables]]
        varname = names(adjustment.variables)[i.adjustment.variables]
        targets[[i.adjustment.variables]] = tgt = numeric.targets[i]
        if (is.na(tgt))
            stop("The target for ", varname, " is invalid.")
        if (!is.numeric(numvar))
            stop(varname, " is not a numeric variable; numeric adjustment variables must be numeric.")
        if (max(numvar) < tgt)
            stop("The target for ", varname, " of ", tgt, " is higher than the maximum value of the variable.")
        if (min(numvar) > tgt)
            stop("The target for ", varname, " of ", tgt, " is lower than the maximum value of the variable.")
    }
    targets
}

# Formats table of margins for input into icarus function
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

# Trims the weights (not really trimming, but this is the language in the literature
trimWeight = function(weight, lower, upper)
{
    weight[weight < lower] = lower
    weight[weight > upper] = upper
    weight / mean(weight)
}

# Trimming of weight (not really trimming, but this is the language in the literature
trimmedCalibrate <- function(adjustment.variables, margins, input.weight, lower, upper, trim.iterations)
{
    weight = calibrate(adjustment.variables, margins, input.weight)
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
        weight = calibrate(adjustment.variables, margins, weight)
        prev_diff = dif
        dif = diffCalculation(weight, lower, upper)
    }
    trimWeight(weight, lower, upper)
}

# Sum of maximum differences between bounds and weight
diffCalculation = function(weight, lower, upper)
{
    rng = range(weight)
    max(rng[2] - upper, 0) - min(lower - rng[1], 0)
}

#' \code{print.Calibrate}
#' @description Print method for calibrate weights
#' @param x A n object of class 'Calibrate'.
#' @param ... Other ignored parameters
#' @return character string
#' @method print Calibrate
#' @export
print.Calibrate <- function (x, ...)
{
    instruction.for.getting.variable = ""
    product = get0("productName")
    if (!is.null(product))
    {
        instruction.for.getting.variable = if(product == "Q")
            "\n\nTo save the variable, click Automate > Browse Online Library > Weighting > Save Variable"
        else "\n\nTo save the variable, click Insert > More > Weighting > Save Variable"
    }
    ess = EffectiveSampleSize(x)
    ess.percent = round(ess / length(x) * 100)
    cat(paste0("Effective sample size: ",
               flipFormat::FormatAsReal(ess, decimals = 0),
               " (",
               ess.percent, "%)",
               instruction.for.getting.variable))
}
