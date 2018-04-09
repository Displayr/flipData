#' \code{GetData}
#' @description Extracts data from the environment of the formula, if it has not
#' been provided as an argument.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @param auxiliary.data A \code{\link{data.frame}} containing additional variables to be used in imputation (if required).
#' @return character.
#' @importFrom flipU AllVariablesNames
#' @importFrom flipTransformations ProcessQVariables
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
                    get(RemoveBackticks(x), data)
                else
                {
                    v <- get(gsub("`", "", substr(x, 1, i - 1), fixed = TRUE), data)
                    eval(parse(text = paste("v", substr(x, i, nchar(x)))))
                }
            }
        ))
        names(data) <- RemoveBackticks(variable.names)
    }
    else if (!is.data.frame(data))
        stop("'data' must be a 'data.frame'.")
    else  # Extracting the variables from the data.frame.
        data <- data[, variable.names, drop = FALSE]
    if (!is.null(auxiliary.data) & length(auxiliary.data) > 0)
    {
        if (!is.data.frame(auxiliary.data))
            auxiliary.data <- data.frame(auxiliary.data)
        if(nrow(data) != nrow(auxiliary.data))
            stop(paste("'data' has", nrow(data), "rows, whereas 'auxiliary.data' has", nrow(auxiliary.data), "rows. They need to have the same number of rows."))
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
#' with a dollar sign are surrounded by backticks. Any such variables that already
#' contain backticks will have those backticks escaped.
#' @param formula An object of class \code{\link{formula}}.
#' @param data An object of class \code{\link{data.frame}}
#' @importFrom flipU AllVariablesNames
#' @export
DataFormula <- function(formula, data = NULL)
{
    formula.str <- paste(deparse(formula), collapse = "")
    var.names <- AllVariablesNames(formula, data)

    # We sort names from longest to shortest since we will be substituting by name
    sorted.indices <- sort(sapply(var.names, nchar), decreasing = TRUE, index.return = TRUE)$ix
    sorted.names <- var.names[sorted.indices]

    for (name in sorted.names)
    {
        if (indexOfUnescapedCharacter(name, "$") > -1)
        {
            new.name <- paste0("`", gsub("`", "\\`", name, fixed = TRUE), "`")
            formula.str <- gsub(name, new.name, formula.str, fixed = TRUE)
        }
    }
    formula(formula.str)
}


#' \code{CleanBackticks}
#' @description Removes extra backticks and unescapes original backticks
#' in variable names due to usage of DataFormula.
#' @param nms Vector of variable names.
#' @export
CleanBackticks <- function(nms)
{
    sapply(nms, function(nm) {
        if (length(grep("$", nms, fixed = TRUE)) > 0)
            gsub("\\`" , "`", gsub("(^`|`$)", "", nm), fixed = TRUE)
        else
            nm
    } , USE.NAMES = FALSE)
}


#' \code{RemoveBackticks}
#' @description Removes backticks surrounding variable names.
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
            stop("This analysis requires a two-dimensional table (i.e., a table with one set of row headings, one set of columns headings, and one statistic in each cell.")
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
