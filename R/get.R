#' Extracts data from the environment of the formula, if it has not
#' been provided as an argument.
#'
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @param auxiliary.data A \code{\link{data.frame}} containing additional variables to be used in imputation (if required).
#' @return character.
#' @importFrom flipU AllVariablesNames Stop
#' @importFrom flipTransformations ProcessQVariables
#' @importFrom flipU Stop
#' @export
GetData <- function(formula, data, auxiliary.data)
{
    data.provided <- !is.null(data)
    CheckForUniqueVariableNames(formula)
    variable.names <- AllVariablesNames(formula, data)
    if (!data.provided) # Extracting the data from the environment
    {
        data <- environment(formula)
        data <- as.data.frame(lapply(variable.names, function(x)
        {
            i <- indexOfUnescapedCharacter(x, "$")
            if (i == -1)
                get(RemoveBackticks(x), data) # variable names in environment do not have surrounding backticks
            else
            {
                v <- get(gsub("`", "", substr(x, 1, i - 1), fixed = TRUE), data)
                eval(parse(text = paste("v", substr(x, i, nchar(x)))))
            }
        }
        ))
        names(data) <- variable.names
    }
    else if (!is.data.frame(data))
        Stop("'data' must be a 'data.frame'.")
    else if (!all(RemoveBackticks(variable.names) %in% colnames(data)))
        Stop(paste("Variable(s)", variable.names[!(RemoveBackticks(variable.names) %in% colnames(data))],
                   "were specified in the formula but are not in the data."))
    else # Extracting the variables from the data.frame.
        data <- data[, RemoveBackticks(variable.names), drop = FALSE]

    if (!is.null(auxiliary.data) & length(auxiliary.data) > 0)
    {
        if (!is.data.frame(auxiliary.data))
            auxiliary.data <- data.frame(auxiliary.data)
        if(nrow(data) != nrow(auxiliary.data))
            Stop(paste("'data' has", nrow(data), "rows, whereas 'auxiliary.data' has", nrow(auxiliary.data), "rows. They need to have the same number of rows."))
        matches <- match(names(auxiliary.data), names(data))
        matched <- !is.na(matches)
        if(!all(matched))
        {
            if (any(matched)) #De-duping
                auxiliary.data <- auxiliary.data[, is.na(matches)]
            data <- cbind(data, auxiliary.data)
        }
    }
    ProcessQVariables(data)
}

#' \code{DataFormula}
#' @description Modifies formula so that any variables that refer to dataframes
#' with a dollar sign are surrounded by backticks. Any variables that already
#' contain backticks will have those backticks escaped.
#' @param formula An object of class \code{\link{formula}}.
#' @param data An object of class \code{\link{data.frame}}
#' @importFrom flipU AllVariablesNames
#' @export
DataFormula <- function(formula, data = NULL)
{
    formula.str <- paste(deparse(formula), collapse = "")
    var.names <- AllVariablesNames(formula, data)
    # Check for dummy variables that include dataset specification
    in.dataset <- vapply(var.names, indexOfUnescapedCharacter, char = "$", numeric(1)) > -1
    uses.backticks <- grepl("`", var.names)
    names(uses.backticks) <- var.names
    dummy.vars.found <- grepl("\\.dummy\\.var_GQ9KqD7YOf$", var.names)
    dummy.vars.dataset.referral <- dummy.vars.found & any(in.dataset | uses.backticks)
    if (dummy.vars.exist <- any(dummy.vars.dataset.referral))
    { # Extract the dummy variables from var.names and formula.str, handle each separately
        dummy.vars <- var.names[dummy.vars.dataset.referral]
        var.names <- var.names[!dummy.vars.dataset.referral]
        # Temporarily remove dummy variables from formula.str
        dummy.vars.patt <- sub(pattern = ".dummy.var_GQ9KqD7YOf",
                               replacement = "\\.dummy\\.var_GQ9KqD7YOf",
                               dummy.vars, fixed = TRUE)
        patt <- paste0("?\\s\\+\\s*", gsub("$", "\\$", dummy.vars.patt, fixed = TRUE), collapse = "|")
        formula.str <- gsub(patt, "", formula.str)
    }
    # We sort names from longest to shortest since we will be substituting by name
    sorted.names <- var.names[order(nchar(var.names), decreasing = TRUE)]

    for (name in sorted.names)
    {
        if (in.dataset[name] || uses.backticks[name])
        {
            if (in.dataset[name])
            { # Split by the $ character being careful not to split if there is a $ inside the data or
              # or variable/question names
                split.names <- strsplit(name, r"(\$(?=([^`]*`[^`]*`)*[^`]*$))", perl = TRUE)[[1]]
                # Escape the $ for the final sub matching pattern below
                name <- paste0(split.names, collapse = "\\$")
                # Escape the backticks for the replacements
                split.names[-2] <- gsub("`", "\\\\`", split.names[-2], fixed = TRUE)
                new.name <- paste0("`", paste0(split.names, collapse = "$"), "`")
            } else
                new.name <- paste0("`", gsub("`", "\\\\`", name, fixed = TRUE), "`")

            formula.str <- sub(paste0(name, r"((?=[^`]*(?:`[^`]*`[^`]*)*$))"),
                               new.name, formula.str, perl = TRUE)
        }
    }
    # Add dummy variables at the end, applying the same conditions, wrapping in backticks and
    # escaping existing backticks if necessary
    if (dummy.vars.exist)
    {
        dummy.vars <- paste0("`", gsub("`", "\\`", dummy.vars, fixed = TRUE), "`")
        formula.str <- paste0(formula.str, " + ", paste0(dummy.vars, collapse = " + "))
    }
    formula(formula.str)
}


#' Clean Backticks in variable names
#'
#' Removes extra backticks and unescapes original backticks in
#' variable names due to usage of DataFormula. Handles cases where
#' variable names have had to be disambiguated by appending a data
#' file name as well as dummy variables from a categorical predictor; see the
#' examples.
#' @param nms Vector of variable names.
#' @importFrom stringr str_match
#' @examples
#' CleanBackticks("`data.sav$Variables$q1`")
#' CleanBackticks("`\\`my data.sav\\`$Variables$catvar`2")
#' @export
CleanBackticks <- function(nms)
{
    # temporarily remove integer suffix (added to distinguish factor levels)
    suffix <- str_match(nms, "([0-9.]+$)")[, 2]
    nms <- sub("([0-9.]+$)", "", nms)

    nms <- sapply(nms, function(nm) {
        if (any(grepl("$", nms, fixed = TRUE)))
            gsub("\\`" , "`", gsub("(^`|`$)", "", nm), fixed = TRUE)
        else
            nm
    } , USE.NAMES = FALSE)

    # add back suffix
    suffix[is.na(suffix)] <- ""
    nms <- paste0(nms, suffix)
    return(nms)
}

#' \code{RemoveBackticks}
#' @description Removes backticks surrounding variable names (first and last character).
#' @param nms Vector of variable names.
#' @export
RemoveBackticks <- function(nms)
{
    ## DS-1769 causes the nasty setting of the perl argument depending on platform.
    sub("^[`]([[:print:]]*)[`]$", "\\1", nms, perl = (Sys.info()["sysname"] == "Windows"))
}

indexOfUnescapedCharacter <- function(s, char)
{
    idx <- -1
    inside.backticks <- FALSE
    for (i in 1:nchar(s))
    {
        c <- substr(s, i, i)
        if (c == char && !inside.backticks)
        {
            idx <- i
            break
        } else if (c == "`" && (i == 1 || substr(s, i - 1, i - 1) != "\\"))
            inside.backticks <- !inside.backticks
    }
    idx
}

#' \code{GetTidyTwoDimensionalArray}
#' @description Checks that an array is two dimensional and tidies if appropriate (assuming it is a Q table).
#' @param x The data that is being analyzed
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#' @importFrom flipU RemoveAt
#' @importFrom flipU Stop
#' @export
GetTidyTwoDimensionalArray <- function(x, row.names.to.remove = NULL, column.names.to.remove = NULL)
{
    dim.x <- dim(x)
    dim.names <- dimnames(x)
    if (length(dim.x) != 2)
    {
        if (length(dim.x) == 3 & !is.null(dim.names))
        {
            x <- x[ , ,1]
            warning(paste0("The analysis has been performed on the first statistic in the table (",
                           dim.names[[3]][1], ")."))
            if (is.character(x[1,1]))
                x <- matrix(as.numeric(x), nrow(x), dimnames = dimnames(x))
        }
        else
        {
            Stop("This analysis requires a two-dimensional table (i.e., a table with one set of row headings, one set of columns headings, and one statistic in each cell.")
        }
    }
    if (is.null(dim.names))
    {
        dimnames(x) <- list(Rows = 1:nrow(x), Columns = 1:ncol(x))
    }
    else
    {
        x <- RemoveAt(x, at = list(row.names.to.remove, column.names.to.remove), split = ",")
    }
    x
}
