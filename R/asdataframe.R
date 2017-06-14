#' \code{AsDataFrame}
#'
#' @description Converts input data to a numeric \code{\link{data.frame}}.
#' @param input.data Either a \code{\link{data.frame}}, a \code{\link{list}} of
#' \code{\link{data.frame}}s and/or \code{\link{vector}}s, or a \code{\link{matrix}}.
#' @param use.names Whether to use names in place of labels, used only for a
#' \code{\link{list}} of \code{\link{vector}}s.
#' @param ignore.columns A list of names of columns to ignore. When \code{input.data}
#' is a \code{\link{matrix}}, rows are also ignored.
#' @importFrom flipFormat ExtractCommonPrefix Labels
#' @importFrom flipTransformations AsNumeric QuestionListToDataFrame
#' @export
AsDataFrame <- function(input.data, use.names = FALSE, ignore.columns = "")
{
    dat <- if (is.data.frame(input.data) || !any(sapply(input.data, is.data.frame))) {  # coerce list of variables to data.frame
        input.data <- data.frame(input.data)
        input.data <- input.data[, !(tolower(names(input.data)) %in% tolower(ignore.columns))]
        var.dat <- AsNumeric(ProcessQVariables(input.data), binary = FALSE)
        # Changing names to labels.
        if (!use.names)
            names(var.dat) <- ExtractCommonPrefix(Labels(var.dat))$shortened.labels
        var.dat

    } else if (is.list(input.data)) {       # list of questions and potentially variables
        names.to.remove <- trimws(unlist(strsplit(ignore.columns, split = ",")))
        qns.dat <- AsNumeric(QuestionListToDataFrame(input.data, names.to.remove = names.to.remove), binary = FALSE)

    } else if (is.matrix(input.data)) {
        mat <- GetTidyTwoDimensionalArray(input.data, ignore.columns, ignore.columns)
        AsNumeric(data.frame(mat, check.names = FALSE))
    } else
        stop(paste("input.data must be a data.frame, list of data.frames/vectors or a matrix."))

    return(dat)
}
