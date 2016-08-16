

#' \code{GetData}
#' @description Extracts data from the environment of the formula, if it has not
#' been provided as an argument.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @param auxiliary.data A \code{\link{data.frame}} containing additional variables to be used in imputation (if required).
#' @return character.
#' @export
GetData <- function(formula, data, auxiliary.data)
{
    data.provided <- !is.null(data)
    variable.names <- all.vars(formula)
    if (!data.provided) # Extracting the data from the environment
        data <- model.frame(formula)
    else if (!is.data.frame(data))
        stop("'data' must be a 'data.frame'.")
    else  # Extracting the variables from the data.frame.
        data <- data[, variable.names, drop = FALSE]
    if (!is.null(auxiliary.data))
    {
        matches <- match(names(auxiliary.data), names(data))
        matched <- !is.na(matches)
        if(!all(matched))
        {
            if (any(matched)) #De-duping
                auxiliary.data <- auxiliary.data[, is.na(matches)]
            data <- cbind(data, auxiliary.data)
        }
    }
    data
}

#' \code{GetTidyTwoDimensionalArray}
#' @description Checks that an array is two dimensional and tidies if appropriate (assuming it is a Q table).
#' @param x The data that is being analyzed
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#' @importFrom flipTransformations RemoveRowsAndOrColumns
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
            warning(paste0("Correspondence analysis has been performed on the first statistic in the table (",
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
        x <- RemoveRowsAndOrColumns(x, row.names.to.remove, column.names.to.remove)
    }
    x
}
