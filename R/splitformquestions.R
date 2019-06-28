#' \code{SplitFormQuestions}
#'
#' @description Converts a list input from Q/Displayr dropbox controls which
#'   could contain variables and questions/variable sets and converts it to a
#'   dataframe.
#' @param form.data The list a from Q/Displayr dropbox controls containing
#'   variables and questions/variable sets.
#' @param use.names Whether to name variables in the data frame using the
#'   variable names in Q/Displayr. Otherwise the variable label is used.
#' @export
SplitFormQuestions <- function(form.data, use.names = FALSE)
{
    dat <- list()
    for (nm in names(formVariables))
    {
        elem <- formVariables[[nm]]
        if (is.data.frame(elem))
        {
            for (nm2 in setdiff(names(elem), c("NET", "SUM")))
                dat[[nm2]] <- elem[[nm2]]
        }
        else if (use.names)
            dat[[attr(elem, "name")]] <- elem
        else
            dat[[nm]] <- elem
    }

    if (use.names && any(sapply(formVariables, is.data.frame)))
        warning("Variable names cannot be shown when questions are selected. ",
                "To show variable names, please select the variables from the question.")

    data.frame(dat, check.names = FALSE)
}
