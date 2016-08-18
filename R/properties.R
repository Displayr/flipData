#' \code{AllIntegers}
#' @description  that all items in a variable are integers.
#' @param x A vector.
#' @return logical.
#' @export
AllIntegers <- function(x)
{
    all(x %% 1 == 0)
}

#' \code{AllVariablesNames}
#' @description Find the names of the variables (including those in dataframes) in a formula.
#' @param formula A \code{\link{formula}}.
#' @export
AllVariablesNames <- function(formula)
{
    rand.str <- "wPpJPcPZGeUTPe2j"
    var.names <- all.vars(formula(gsub("$", rand.str, deparse(formula), fixed = TRUE)))
    sapply(var.names, function(x) gsub(rand.str, "$", x, fixed = TRUE), USE.NAMES = FALSE)
}


#' \code{OutcomeName}
#' @description Find the name of the outcome variable.
#' @param formula A \code{\link{formula}}.
#' @return character.
#' @export
OutcomeName <- function(formula)
{
    if (HasOutcome(formula))
        return(AllVariablesNames(formula)[1])
    return(NULL)
}


#' \code{HasOutcome}
#' @description Checking if the formula contains an outcome (varib)i.e., dependent variable).
#' @param formula A \code{\link{formula}}.
#' @return logical
#' @importFrom stats terms
#' @export
HasOutcome <- function(formula)
{
    attr(terms(formula), "response") != 0
}

#' \code{PrintDetails}
#' @description A print function for error checking.
#' Prints its name and a \code{\link{summary}}.
#' @param x Something to be printed.
#' @export
PrintDetails <- function(x)
{
    cat(paste0(deparse(substitute(x)), " n:", length(x), " valid:", sum(!is.na(x)), " missing:",sum(is.na(x)), "\n"))
    print(summary(x))
    cat("\n")
}

#' \code{AnyNegative}
#' @description The values contain a negative value.
#' @param x A vector.
#' @return logical.
#' @export
AnyNegative <- function(x)
{
    min(c(x, NA), na.rm = TRUE) < 0
}

#' \code{IsCount}
#' @description Checks of data, or, a model description, counts or represents counts.
#' @param x A variable or text string describing a family (e.g., "Poisson").
#' @return logical.
#' @export
IsCount <- function(x) {
    if(is.factor(x))
        return(FALSE)
    if(!is.numeric(x)) {
        if (!is.character(x))
            x <- x$type
        return(x == "Poisson" | x == "Quasi-Poisson" | x == "NBD")
    }
    x <- x[!is.na(x)]
    if (length(x) == 0)
        stop("No data.")
    u = unique(x)
    if (min(u, na.rm = TRUE) < 0)
        return(FALSE)
    sum(as.integer(u) != u, na.rm = TRUE) == 0}


#' \code{OutcomeVariable}
#' @description Returns the outcome variable from a model.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @return A vector of data.
#' @export
OutcomeVariable <- function(formula, data)
{
    data[[OutcomeName(formula)]]
}

#' \code{HasSubset}
#' @description Checks that the subset contains data.
#' @param subset The filter used to filter data in a model.
#' @return true if the subset contains information
#' @export
HasSubset <- function(subset)
{
    !is.null(subset) & length(subset) != 1
}


