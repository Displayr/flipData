#' @title Stack data set
#' @description Stacks variables in a data set.
#' @param input.data.set.name Name of data file to stack, either as a path to a
#'   local file or file in the Displayr Cloud Drive
#' @param stacked.data.set.name Name of the stacked data file to be saved in
#'   the Displayr Cloud Drive (if run from Displayr) or saved locally.
#' @param common.labels A character vector of common labels to be used to
#'   identify variables to stack. To be identified, a set of variables to be
#'   stacked must contain these labels, have the same prefix and suffix before
#'   and after these labels and be contiguous.
#' @param specify.by "Variable" or "Case"
#' @param manual.stacking To be filled
#' @param variables.to.omit String of comma-separated variable names to omit.
#'   Variable ranges can be specified by supplying the start and end variables
#'   separated by a dash '-'. If the start or end variables are left out, then
#'   the range is assumed to start from the first variable or end at the last
#'   variable respectively. Wildcards in variable names can be specified
#'   with an asterisk '*'.
#' @export
StackData <- function(input.data.set.name,
                      stacked.data.set.name = NULL,
                      common.labels = NULL,
                      specify.by = NULL,
                      manual.stacking = NULL,
                      variables.to.omit = NULL)
{
    input.data.set <- readDataSets(input.data.set.name, 1)[[1]]
    input.data.set.metadata <- metadataFromDataSet(input.data.set,
                                                   input.data.set.name)
    n.vars <- input.data.set.metadata$n.variables

    # auto common.labels

    stacking.groups <- stackWithCommonLabels(common.labels,
                                             input.data.set.metadata)
    # stacking.groups <- stackManually(stacking.groups, manual.stacking,
    #                                  specify.by, input.data.set.metadata)

    stacked.data.set <- stackedDataSet(input.data.set, input.data.set.metadata,
                                       stacking.groups, variables.to.omit)

    variables.to.omit <- parseVariablesToOmit(variables.to.omit,
                                              names(stacked.data.set))
    omitted.stacked.variables <-  omittedStackedVariables(variables.to.omit,
                                                          stacked.data.set)
    stacked.data.set <- omitVariablesFromDataSet(stacked.data.set,
                                                 variables.to.omit)

    stacked.data.set.name <- cleanStackedDataSetName(stacked.data.set.name,
                                                     input.data.set.name)
    writeDataSet(stacked.data.set, stacked.data.set.name)

    stacked.data.set.metadata <- metadataFromStackedDataSet(stacked.data.set,
                                                            stacked.data.set.name)


    result <- list()
    result$stacked.data.set.metadata <- stacked.data.set.metadata
    result$unstackable.names <- attr(stacking.groups, "unstackable.names")
    result$omitted.variables <- variables.to.omit
    result$omitted.stacked.variables <- omitted.stacked.variables
    class(result) <- "StackedData"
    result
}

# Manual stacking specified by variable
# Manual stacking specified by case
# Automatic common labels

stackWithCommonLabels <- function(common.labels, input.data.set.metadata)
{
    if (is.null(common.labels) || length(common.labels) == 0)
        return(NULL)

    v.names <- input.data.set.metadata$variable.names
    v.labels <- input.data.set.metadata$variable.labels
    v.types <- input.data.set.metadata$variable.types
    v.categories <- input.data.set.metadata$variable.categories
    nchar.labels <- nchar(common.labels)

    stacking.groups <- matrix(nrow = 0, ncol = length(common.labels))

    current.group <- rep(NA_integer_, length(common.labels))
    current.prefix <- NA_character_
    current.suffix <- NA_character_
    for (i in seq_along(v.labels))
    {
        lbl <- v.labels[i]
        ind <- which(vapply(common.labels, grepl, logical(1), lbl, fixed = TRUE))
        ind <- ind[which.max(nchar.labels[ind])]

        if (length(ind) == 0)
        {
            if (any(!is.na(current.group)))
            {
                stacking.groups <- rbind(stacking.groups, current.group)
                current.group <- rep(NA_integer_, length(common.labels))
                current.prefix <- NA_character_
                current.suffix <- NA_character_
            }
            next
        }

        if (!is.na(current.group[ind]))
        {
            stacking.groups <- rbind(stacking.groups, current.group)
            current.group <- rep(NA_integer_, length(common.labels))
            current.prefix <- NA_character_
            current.suffix <- NA_character_
        }

        matched.common.label <- common.labels[ind]
        start.ind <- gregexpr(matched.common.label, lbl, fixed = TRUE)[[1]]
        prefix <- substr(lbl, 1, start.ind - 1)
        suffix <- substr(lbl, start.ind + nchar(matched.common.label), nchar(lbl))

        if (all(is.na(current.group)))
        {
            current.group[ind] <- i
            current.prefix <- prefix
            current.suffix <- suffix
        }

        if (prefix == current.prefix && suffix == current.suffix)
            current.group[ind] <- i
    }

    # Remove groups with only one element
    stacking.groups <- stacking.groups[rowSums(!is.na(stacking.groups)) > 1, ]

    # Remove groups with mismatching variable types and categories
    unstackable.ind <- which(apply(stacking.groups, 1, function(ind) {
        ind <- removeNA(ind)
        !allIdentical(v.types[ind]) || !allIdentical(v.categories[ind])
    }))
    unstackable.names <- lapply(unstackable.ind, function(ind) {
        v.names[removeNA(stacking.groups[ind, ])]
    })
    if (length(unstackable.names) > 0)
        warning("Variables could not be stacked due ",
                "to mismatching variable types or categories. ",
                "See Notes section in output for more details.")

    if (length(unstackable.ind) > 0)
        stacking.groups <- stacking.groups[-unstackable.ind, ]
    attr(stacking.groups, "unstackable.names") <- unstackable.names
    stacking.groups
}

stackManually <- function(stacking.groups, manual.stacking,
                          specify.by, input.data.set.metadata)
{
    if (is.null(manual.stacking))
        return(stacking.groups)

    variable.names <- input.data.set.metadata$variable.names

    stacking.groups <- if (specify.by == "Variable")
        stackingSpecifiedByVariable(stacking.groups, manual.stacking,
                                    variable.names)
    else
        stackingSpecifiedByCase(stacking.groups, manual.stacking,
                                variable.names)
}

stackingSpecifiedByVariable <- function(stacking.groups, manual.stacking,
                                        variable.names)
{
    for (input.text in manual.stacking)
    {

    }
}

stackingSpecifiedByCase <- function(stacking.groups, manual.stacking,
                                    input.data.set.metadata)
{

}

stackedDataSet <- function(input.data.set, input.data.set.metadata,
                           stacking.groups, variables.to.omit)
{
    input.var.names <- input.data.set.metadata$variable.names
    input.var.labels <- input.data.set.metadata$variable.labels
    retained.indices <- retainedIndices(stacking.groups,
                                        input.data.set.metadata$n.variables)
    stacked.indices <- stackedIndices(stacking.groups, retained.indices)

    # refactor this, don't like the name "stackedVariableText"
    stacked.data.set.var.names <- stackedVariableText(stacking.groups,
                                                      input.var.names,
                                                      retained.indices,
                                                      stacked.indices, TRUE)
    stacked.data.set.var.labels <- stackedVariableText(stacking.groups,
                                                       input.var.labels,
                                                       retained.indices,
                                                       stacked.indices)

    n.stacked <- ncol(stacking.groups)
    stacked.data.set <- data.frame(lapply(seq_along(stacked.data.set.var.names), function(i) {
        ind <- match(i, stacked.indices)
        if (!is.na(ind)) # Stacked variable
        {
            group.ind <- stacking.groups[ind, ]
            v <- unlist(lapply(group.ind, function(j) {
                if (!is.na(j))
                    input.data.set[[j]]
                else
                    rep(NA, nrow(input.data.set))
            }))
            attr(v, "is.stacked") <- TRUE
            attr(v, "stacking.input.variable.names") <- input.var.names[group.ind]
            attr(v, "stacking.input.variable.labels") <- input.var.labels[group.ind]
            categories <- attr(input.data.set[[removeNA(group.ind)[1]]],
                               "labels", exact = TRUE)
        }
        else # Not stacked variable
        {
            input.var <- input.data.set[[stacked.data.set.var.names[i]]]
            v <- rep(input.var, n.stacked)
            attr(v, "is.stacked") <- FALSE
            categories <- attr(input.var, "labels", exact = TRUE)
        }
        attr(v, "label") <- stacked.data.set.var.labels[i]
        if (!is.null(categories))
        {
            attr(v, "labels") <- categories
            class(v) <- c(class(v), "haven_labelled")
        }
        v
    }))
    names(stacked.data.set) <- stacked.data.set.var.names
    stacked.data.set
}

# Indices of input variables retained after stacking
retainedIndices <- function(stacking.groups, n.vars)
{
    ind.to.remove <- unlist(lapply(seq_len(nrow(stacking.groups)), function(i) {
        ind <- removeNA(stacking.groups[i, ])
        ind <- ind[-which.min(ind)]
    }))
    seq_len(n.vars)[-ind.to.remove]
}

# Indices of stacked variables in stacked data set
stackedIndices <- function(stacking.groups, retained.indices)
{
    stacking.ind <- vapply(seq_len(nrow(stacking.groups)), function(i) {
        ind <- removeNA(stacking.groups[i, ])
        ind <- ind[which.min(ind)]
    }, integer(1))

    vapply(stacking.ind, match, integer(1), retained.indices)
}

stackedVariableText <- function(stacking.groups, variable.text,
                                retained.indices, stacked.indices,
                                prevent.duplicates = FALSE)
{
    stacked.text <- vapply(seq_len(nrow(stacking.groups)), function(i) {
        ind <- removeNA(stacking.groups[i, ])
        text <- variable.text[ind]
        common.prefix <- trimws(getCommonPrefix(text))
        if (common.prefix != "")
            common.prefix
        else
            "stacked_var"
    }, character(1))

    result <- variable.text[retained.indices]
    result[stacked.indices] <- stacked.text

    if (!prevent.duplicates)
        return(result)

    dup <- which(duplicated(result))
    for (i in dup)
    {
        j <- 2
        repeat
        {
            candidate.name <- paste0(result[i], "_", j)
            if (!(candidate.name %in% result))
            {
                result[i] <- candidate.name
                break
            }
        }
    }

    result
}

getCommonPrefix <- function(nms)
{
    common_prefix <- ""
    for (i in 1:min(nchar(nms)))
    {
        if (allIdentical(vapply(tolower(nms), substr, character(1), 1, i)))
            common_prefix <- substr(nms[1], 1, i)
        else
            break
    }
    common_prefix
}

parseVariablesToOmit <- function(variables.to.omit, variable.names)
{
    if (is.null(variables.to.omit) || length(variables.to.omit) == 0 ||
        variables.to.omit == "")
        return(character(0))

    result <- character(0)
    for (input.text in variables.to.omit)
    {
        split.text <- trimws(strsplit(input.text, ",")[[1]])
        split.text <- split.text[split.text != ""]

        for (t in split.text)
        {
            result <- if (grepl("-", t, fixed = TRUE)) # contains range
                c(result, parseRange(t, variable.names))
            else if (grepl("*", t, fixed = TRUE)) # contains wildcard
                c(result, parseWildcard(t, variable.names))
            else
                c(result, parseVariableName(t, variable.names))
        }
    }

    # Order omitted variables according to variable.names
    result[order(vapply(result, match, integer(1), variable.names))]
}

parseRange <- function(range.text, variable.names)
{
    dash.ind <- match("-", strsplit(range.text, "")[[1]])
    start.var.text <- trimws(substr(range.text, 1, dash.ind - 1))
    end.var.text <- trimws(substr(range.text, dash.ind + 1, nchar(range.text)))
    if (grepl("*", start.var.text, fixed = TRUE))
    {
        warning("The start variable from the input range '", range.text, "' ",
                "contains the wildcard character '*' which is not permitted ",
                "in a range. The input range has been ignored.")
        return(character(0))
    }
    if (grepl("*", end.var.text, fixed = TRUE))
    {
        warning("The end variable from the input range '", range.text, "' ",
                "contains the wildcard character '*' which is not permitted ",
                "in a range. The input range has been ignored.")
        return(character(0))
    }

    start.ind <- if (start.var.text != "")
        match(start.var.text, variable.names)
    else
        1

    end.ind <- if (end.var.text != "")
        match(end.var.text, variable.names)
    else
        length(variable.names)

    if (is.na(start.ind))
    {
        warning("The start variable from the input range '", range.text, "' ",
                "could not be identified. The input range has been ignored. ",
                "Ensure that the variable name is correctly specified.")
        return(character(0))
    }
    if (is.na(end.ind))
    {
        warning("The end variable from the input range '", range.text, "' ",
                "could not be identified. The input range has been ignored. ",
                "Ensure that the variable name is correctly specified.")
        return(character(0))
    }
    if (start.ind > end.ind)
    {
        warning("The start variable from the input range '", range.text, "' ",
                "appears after the end variable in the data set. ",
                "The input range has been ignored. Ensure that the range has ",
                "been correctly specified.")
        return(character(0))
    }
    variable.names[start.ind:end.ind]
}

#' @importFrom flipU EscapeRegexSymbols
parseWildcard <- function(wildcard.text, variable.names)
{
    ind.asterisk <- match("*", strsplit(wildcard.text, "")[[1]])
    start.var.text <- trimws(substr(wildcard.text, 1, ind.asterisk - 1))
    end.var.text <- trimws(substr(wildcard.text, ind.asterisk + 1,
                                  nchar(wildcard.text)))
    pattern <- paste0("^", EscapeRegexSymbols(start.var.text), ".*",
                      EscapeRegexSymbols(end.var.text), "$")
    is.match <- grepl(pattern, variable.names)
    if (!any(is.match))
    {
        warning("No matches were found for the wildcard variable name '",
                wildcard.text, "'. Ensure that the wildcard variable name ",
                "has been correctly specified.")
        return(character(0))
    }
    variable.names[is.match]
}

parseVariableName <- function(variable.name.text, variable.names)
{
    if (variable.name.text %in% variable.names)
        variable.name.text
    else
    {
        warning("The input varible name '", variable.name.text,
                "' could not be identified and has been ignored.")
        return(character(0))
    }
}

omittedStackedVariables <- function(variables.to.omit, stacked.data.set)
{
    is.stacked <- vapply(stacked.data.set, attr, logical(1), "is.stacked")
    names(which(is.stacked[variables.to.omit]))
}

omitVariablesFromDataSet <- function(data.set, variables.to.omit)
{
    data.set[!(names(data.set) %in% variables.to.omit)]
}

cleanStackedDataSetName <- function(stacked.data.set.name, input.data.set.name)
{
    if (is.null(stacked.data.set.name) || stacked.data.set.name == "")
    {
        if (grepl("\\.sav$", input.data.set.name))
        {
            n <- nchar(input.data.set.name)
            name.without.sav <- substr(input.data.set.name, 1, n - 4)
            return(paste0(name.without.sav, " stacked.sav"))
        }
        else
            return(paste0(input.data.set.name, " stacked.sav"))
    }
    else if (!grepl("\\.sav$", stacked.data.set.name))
        return(paste0(stacked.data.set.name, ".sav"))
    else
        return(stacked.data.set.name)
}

metadataFromStackedDataSet <- function(stacked.data.set, stacked.data.set.name)
{
    result <- metadataFromDataSet(stacked.data.set, stacked.data.set.name)
    result$is.stacked.variable <- vapply(stacked.data.set, attr, logical(1),
                                         "is.stacked")
    result$stacking.input.variable.names <- lapply(stacked.data.set, attr,
                                                   "stacking.input.variable.names")
    result$stacking.input.variable.labels <- lapply(stacked.data.set, attr,
                                                    "stacking.input.variable.labels")
    result
}

allIdentical <- function(x)
{
    length(unique(x)) < 2
}

removeNA <- function(x)
{
    x[!is.na(x)]
}

#' @importFrom flipFormat StackingWidget
#' @export
print.StackedData <- function(x, ...)
{
    StackingWidget(x$stacked.data.set.metadata,
                   x$unstackable.names,
                   x$omitted.variables,
                   x$omitted.stacked.variables)
}
