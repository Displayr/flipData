#' \code{SplitFormQuestions}
#'
#' @description Converts a list input from Q/Displayr dropbox controls which
#'   could contain variables and questions/variable sets and converts it to a
#'   dataframe.
#' @param form.data The list a from Q/Displayr dropbox controls containing
#'   variables and questions/variable sets.
#' @param show.labels Whether to name variables in the data frame using the
#'   variable labels in Q/Displayr, otherwise the variable name is used
#'   (the variable name cannot be extracted from a question/variable set,
#'   and a warning is shown).
#' @export
SplitFormQuestions <- function(form.data, show.labels = TRUE)
{
    dat <- list()
    for (nm in names(form.data))
    {
        elem <- form.data[[nm]]
        if (is.data.frame(elem))
        {
            for (nm2 in setdiff(names(elem), c("NET", "SUM")))
                dat[[nm2]] <- elem[[nm2]]
        }
        else if (show.labels)
            dat[[nm]] <- elem
        else
            dat[[attr(elem, "name")]] <- elem
    }

    if (!show.labels && any(sapply(form.data, is.data.frame)))
        warning("Variable names cannot be shown when questions are selected. ",
                "To show variable names, please select the variables from the question.")

    data.frame(dat, check.names = FALSE)
}
