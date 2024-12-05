#' \code{SplitFormQuestions}
#'
#' @description Converts a list input from Q/Displayr dropbox controls which
#'   could contain variables and questions/variable sets and converts it to a
#'   dataframe.
#' @param form.data The list from Q/Displayr dropbox controls containing
#'   variables and questions/variable sets.
#' @param show.labels Whether to name variables in the data frame using the
#'   variable labels in Q/Displayr, otherwise the variable name is used
#'   (the variable name cannot be extracted from a question/variable set,
#'   and a warning is shown).
#' @param include.grid.flag Whether to return a flag indicating which variables
#'   come from Pick Any - Grid or Number - Grid questions.
#' @export
#' @importFrom flipU Stop
SplitFormQuestions <- function(form.data, show.labels = TRUE,
                               include.grid.flag = FALSE)
{
    .checkForDuplicateNames <- function(dat, new.name)
    {
        if (new.name %in% names(dat))
            Stop("The data cannot have two columns with same name: '",
                 new.name, "'. Modify the inputs to avoid this.")
    }

    dat <- list()
    is.grid <- logical(0)
    for (i in seq_along(form.data))
    {
        elem <- form.data[[i]]
        if (is.data.frame(elem))
        {
            nms <- setdiff(names(elem), c("NET", "SUM"))

            # Grid questions also have column and row NETs/SUMs which need to
            # be removed
            is.grid.question <- attr(elem, "questiontype") %in%
                                c("PickAnyGrid", "NumberGrid")
            if (is.grid.question)
                nms <- nms[!grepl("(^NET, |^SUM, |, NET$|, SUM$)", nms)]

            for (nm2 in nms)
            {
                .checkForDuplicateNames(dat, nm2)
                dat[[nm2]] <- elem[[nm2]]
                is.grid <- c(is.grid, is.grid.question)
            }
        }
        else if (show.labels)
        {
            .checkForDuplicateNames(dat, attr(elem, "label"))
            dat[[attr(elem, "label")]] <- elem
            is.grid <- c(is.grid, FALSE)
        }
        else
        {
            .checkForDuplicateNames(dat, attr(elem, "name"))
            dat[[attr(elem, "name")]] <- elem
            is.grid <- c(is.grid, FALSE)
        }
    }

    if (!show.labels && any(sapply(form.data, is.data.frame)))
        warning("Variable names cannot be shown when questions are selected. ",
                "To show variable names, please select the variables from ",
                "the question.")

    # check for duplicate names

    if (include.grid.flag)
        list(dat = data.frame(dat, check.names = FALSE), is.grid = is.grid)
    else
        data.frame(dat, check.names = FALSE)
}

#' Perform Variable Label Matching
#'
#' Match variable labels to labels that
#' come from a mixture of questions and variables. The labels from
#' questions may have been modified because the question is a grid
#' question and this requires special treatment.
#' @param labels.from.mixed.input Variable labels obtained from
#'     SplitFormQuestions.
#' @param variable.labels Variable labels to be matched.
#' @param is.grid Logical flag indicating which
#'     labels.from.mixed.input are from a Pick Any - Grid or Number -
#'     Grid questions.
#' @param variable.labels.source The source of the variable labels, to
#'     be used to show the error message.
#' @importFrom verbs Sum
#' @export
#' @importFrom flipU Stop
MatchVariableLabelsToQuestion <- function(labels.from.mixed.input,
                                          variable.labels,
                                          is.grid, variable.labels.source)
{
    not.found <- setdiff(variable.labels, labels.from.mixed.input)

    # The categories of Pick Any - Grid and Number - Grid questions do not
    # correspond to their variable's labels. However, we do know that the
    # category will be comma separated, with the two parts (we ignore the case
    # where there are more than two) present in the variable label.
    if (any(is.grid) && length(not.found) > 0)
    {
        new.variable.labels <- variable.labels
        grid.labels <- labels.from.mixed.input[is.grid]
        grid.labels.split <- strsplit(grid.labels, ", ")
        # Discard if there is more than one split
        ind <- sapply(grid.labels.split, length) == 2
        grid.labels <- grid.labels[ind]
        grid.labels.split <- grid.labels.split[ind]
        n.grid.labels <- Sum(ind, remove.missing = FALSE)
        for (lbl in not.found)
        {
            min.nchar.diff <- Inf
            for (i in seq_len(n.grid.labels))
            {
                lower.lbl <- tolower(lbl)
                lower.split.1 <- tolower(grid.labels.split[[i]][1]) # part 1
                lower.split.2 <- tolower(grid.labels.split[[i]][2]) # part 2
                if (grepl(lower.split.1, lower.lbl, fixed = TRUE) &&
                    grepl(lower.split.2, lower.lbl, fixed = TRUE))
                {
                    # Determine the size of the leftover part that can't be
                    # matched. We chose the category that minimises this as the
                    # match.
                    nchar.diff <- nchar(gsub(lower.split.2, "",
                                             gsub(lower.split.1, "",
                                                  tolower(lbl))))
                    if (nchar.diff < min.nchar.diff)
                    {
                        new.variable.labels[variable.labels == lbl] <- grid.labels[i]
                        min.nchar.diff <- nchar.diff
                    }
                }
            }
        }
        variable.labels <- new.variable.labels
        not.found <- setdiff(variable.labels, labels.from.mixed.input)
    }

    if (length(not.found) > 0)
    {
        question.name <- ifelse(get0("productName", ifnotfound = "Q") == "Q",
                                     "question", "variable set")
        if (length(not.found) == 1)
            Stop("The following variable was specified in '",
                 variable.labels.source, "' but could not be matched to ",
                "those in the list of alternatives: ",
                paste0("'", not.found, "'"),
                ". Either this variable is not present in the set of ",
                "alternatives, or the variable label does not match any ",
                "category from the input ", question.name, "s in which case ",
                "the variables from the ", question.name,
                " should be selected as alternatives instead.")
        else
            Stop("The following variables were specified in '",
                 variable.labels.source, "' but could not be matched to ",
                 "those in the list of alternatives: ",
                 paste0(paste0("'", not.found, "'"), collapse = ", "),
                 ". Either these variables are not present in the set of ",
                 "alternatives, or the variables' labels do not match any ",
                 "categories from input ", question.name, "s, in which case ",
                 "the variables from the ", question.name,
                 " should be selected as alternatives instead.")
    }

    match(variable.labels, labels.from.mixed.input)
}
