#' @title Merge Data Sets by Case
#' @description Merges multiple data sets by case where the data sets contain
#'  similar variables but different cases, e.g., data sets from different time
#'  periods.
#' @param data.set.names A character vector of names of data sets from the
#'  Displayr cloud drive to merge (if run from Displayr) or file paths of local
#'  data sets.
#' @param merged.data.set.name A string of the name of the merged data set in
#'  the Displayr cloud drive (if run from Displayr) or the local file path of
#'  the merged data set. If NULL, the data set is not written.
#' @param match.by One of Automatic, All, Variable names, Variable labels,
#'  Variable and value labels
#' @param min.match.percentage To be decided, possibly a percentage.
#' @param variables.to.combine A character vector of pairs of comma-separated
#'  variable names indicating which variables are to be combined together.
#'  Ranges of variables can be specified by separating variable names by '-'.
#'  Variables can be specified from specific data sets by appending '(x)' to
#'  the variable name where x is the data set index.
#' @param variables.to.not.combine A character vector of comma-separated variable
#'  names specifying variables that should never be combined together.
#'  To specify variables from a specific data set, suffix variable names
#'  with the data set index in parentheses, e.g., 'Q2(3)'.
#' @param variables.to.omit Character vector of variable names to omit from
#'  the merged data set. To specify variables from a specific data set,
#'  suffix the name with the data set index in parentheses, e.g., 'Q2(3)'.
#' @param include.merged.data.set.in.output Whether to include the merged data
#'  set in the output.
#' @param write.data.set Whether to save the merged data set file.
#' @param prioritize.early.data.sets Whether earlier data sets should be
#'  given priority instead of later data sets when determining which
#'  names and labels to use.
#' # need to describe outputs!
#' @param category.value.with.multiple.labels How to deal with a single category value
#'  with multiple labels. Either "Use first label" or "Create new values" for
#'  each label.
#' @param required.data.sets A logical vector indicating the data sets that
#'  each variable in the merged data set is required to contain values from.
#' @importFrom verbs Sum
#' @export
MergeDataSetsByCase <- function(data.set.names,
                                merged.data.set.name = NULL,
                                match.by = "Automatic",
                                min.match.percentage = 100,
                                variables.to.combine = NULL,
                                variables.to.not.combine = NULL,
                                variables.to.omit = NULL,
                                include.merged.data.set.in.output = FALSE,
                                write.data.set = TRUE,
                                prioritize.early.data.sets = TRUE,
                                category.value.with.multiple.labels = "Use first label",
                                required.data.sets = NULL)
{
    data.sets <- readDataSets(data.set.names, 2)
    input.data.sets.metadata <- metadataFromDataSets(data.sets)

    matched.names <- matchVariables(input.data.sets.metadata, match.by,
                                    min.match.percentage, variables.to.combine,
                                    variables.to.not.combine,
                                    variables.to.omit, data.sets,
                                    required.data.sets)
    merge.map <- mergeMap(matched.names, input.data.sets.metadata,
                          prioritize.early.data.sets)
    merged.data.set <- mergeDataSetsWithMergeMap(data.sets, merge.map,
                                                 prioritize.early.data.sets,
                                                 input.data.sets.metadata$data.set.names,
                                                 category.value.with.multiple.labels)
    merged.data.set.name <- cleanMergedDataSetName(merged.data.set.name,
                                                   data.set.names)

    if (write.data.set)
        writeDataSet(merged.data.set, merged.data.set.name)

    outputForMergeDataSetsByCase(merged.data.set, input.data.sets.metadata, merge.map,
                                 include.merged.data.set.in.output,
                                 merged.data.set.name)
}

# Fuzzy matching:
# Start with last data set first perform exact match against second last data set.
# For the remaining labels, compute relative distances (0-1) for all possible pairs
# and match if threshold is met, starting from best matches
# Repeat for third last data set etc.

# TODO

# Need to ensure any new variable names we generate are valid for sav files, e.g. not too long

# Checkbox to choose datasets where variables are excluded when they don't have matches

# Option to merge mutually exclusive variables from same data set, e.g. Internet_provider
# variables for US and Canada in Bain data sets?

# Smarter merging of categories where labels are 'close'

# Subtitle in output showing summary status?

# Remove data set names in output and show row lines, improve spacing between notes

metadataFromDataSets <- function(data.sets)
{
    list(variable.names = lapply(data.sets, names),
         variable.labels = lapply(data.sets, function(data.set) {
             vapply(data.set, function(v) {
                 lbl <- attr(v, "label", exact = TRUE)
                 if (!is.null(lbl))
                     lbl
                 else
                     ""
             }, character(1))
         }),
         variable.categories = lapply(data.sets, function(data.set) {
             lapply(data.set, attr, "labels", exact = TRUE)
         }),
         variable.types = lapply(data.sets, function(data.set) {
             vapply(data.set, variableType, character(1))
         }),
         data.set.names = names(data.sets),
         n.data.sets = length(data.sets))
}

matchVariables <- function(input.data.set.metadata, match.by,
                           min.match.percentage, variables.to.combine,
                           variables.to.not.combine, variables.to.omit,
                           data.sets, required.data.sets)
{
    variables.to.combine <- parseVariablesToCombine(variables.to.combine,
                                                    input.data.set.metadata)
    variables.to.not.combine <- parseVariablesToNotCombine(variables.to.not.combine,
                                                           input.data.set.metadata)
    variables.to.omit <- parseVariablesToOmitForMerging(variables.to.omit,
                                                        input.data.set.metadata)
    checkInputVariables(variables.to.combine, variables.to.not.combine,
                        variables.to.omit)

    if (match.by == "Variable names")
        matched.names <- matchVariableNames(input.data.set.metadata,
                                            variables.to.combine,
                                            variables.to.not.combine,
                                            variables.to.omit)
    else if (match.by == "Variable labels")
    {
        matched.names <- matchVariableLabels(matched.names,
                                             input.data.set.metadata,
                                             variables.to.omit)
    }
    else if (match.by == "Variable and value labels")
    {

    }
    else if (match.by == "Automatic")
    {

    }
    else if (match.by == "All")
    {

    }

    matched.names <- unmatchVariablesOfDifferentTypes(matched.names, data.sets)

    matched.names
}

# Parse the character vector variables.to.combine and return a matrix where
# each row contains a set of variables to be combined, with the columns
# corresponding to the input data sets.
parseVariablesToCombine <- function(variables.to.combine, input.data.set.metadata)
{
    result <- do.call("rbind",
                      lapply(variables.to.combine, parseInputVariableText,
                             input.data.set.metadata))

    # Check that variables to combine have not been specified multiple times
    for (i in seq_len(input.data.set.metadata$n.data.sets))
    {
        date.set.vars.names <- result[, i]
        names.table <- table(date.set.vars.names)
        if (any(names.table > 1))
        {
            duplicate.name <- names(names.table)[names.table > 1][1]
            duplicate.match.source <- variables.to.combine[date.set.vars.names == duplicate.name]
            stop("The variable '", duplicate.name, "' has been specified to ",
                 "be combined in multiple inputs: ",
                 paste0(paste0("'", duplicate.match.source, "'"), collapse = ", "),
                 ". Ensure that any of the variables to be combined are ",
                 "specified in at most one input.")
        }
    }
    result
}

# Parse the character vector variables.to.not.combine and return a matrix where
# each row contains a set of variables that should not be combined, with the
# columns corresponding to the input data sets.
parseVariablesToNotCombine <- function(variables.to.not.combine,
                                       input.data.set.metadata)
{
    do.call("rbind", lapply(variables.to.not.combine, parseInputVariableText,
                            input.data.set.metadata))
}

# Parse the character vector variables.to.omit and return a matrix where
# each row contains a set of variables that should be omitted, with the
# columns corresponding to the input data sets.
parseVariablesToOmitForMerging <- function(variables.to.omit,
                                           input.data.set.metadata)
{
    split.text <- unlist(lapply(variables.to.omit, splitByComma),
                         use.names = FALSE)
    do.call("rbind", lapply(split.text, parseVariablesToOmitText,
                            input.data.set.metadata))
}

parseVariablesToOmitText <- function(input.text, input.data.set.metadata)
{
    n.data.sets <- input.data.set.metadata$n.data.sets
    var.names <- input.data.set.metadata$variable.names
    data.set.names <- input.data.set.metadata$data.set.names

    dash.ind <- match("-", strsplit(input.text, "")[[1]])
    if (!is.na(dash.ind)) # range of variables
    {
        range.start <- trimws(substr(input.text, 1, dash.ind - 1))
        range.end <- trimws(substr(input.text, dash.ind + 1, nchar(input.text)))

        data.set.ind <- parseDataSetIndicesForRange(range.start,
                                                    range.end,
                                                    n.data.sets)
        if (!is.na(data.set.ind)) # data set index supplied for range
        {
            range.start.without.index <- removeDataSetIndex(range.start)
            range.end.without.index <- removeDataSetIndex(range.end)
            range.vars <- variablesFromRange(var.names[[data.set.ind]],
                                             range.start.without.index,
                                             range.end.without.index,
                                             data.set.names[data.set.ind],
                                             input.text)

            result <- matrix(NA_character_, nrow = end.ind - start.ind + 1,
                             ncol = n.data.sets)
            result[, data.set.ind] <- range.vars
            return(result)
        }
        else # data set index not supplied for range
        {
            parsed.names <- lapply(seq_len(n.data.sets), function(i) {
                variablesFromRange(var.names[[i]], range.start, range.end,
                                   data.set.names[i], input.text, FALSE)
            })
            range.lengths <- vapply(parsed.names, length, integer(1))

            if (all(range.lengths == 0))
                stop("The input range '", input.text, "' was not found ",
                     "in any of the input data sets. Ensure that the ",
                     "range has been correctly specified.")

            if (!allIdentical(range.lengths))
                stop("The input '", input.text, "' contains ",
                     "variable ranges with differing numbers of variables. ",
                     "Ensure that the ranges have been correctly specified ",
                     "so that they all contain the same number of variables.")

            return(do.call("cbind", lapply(parsed.names, function(nms) {
                if (is.null(nms))
                    rep(NA_character_, max(range.lengths))
                else
                    nms
            })))
        }
    }
    else # single variable (not range)
    {
        data.set.ind <- parseDataSetIndex(input.text, n.data.sets)
        if (!is.na(data.set.ind)) # data set index supplied
        {
            input.text.without.index <- removeDataSetIndex(input.text)
            if (!(input.text.without.index %in% var.names[[data.set.ind]]))
                variableNotFoundError(input.text.without.index,
                                      data.set.names[data.set.ind])

            result <- matrix(nrow = 1, ncol = n.data.sets)
            result[data.set.ind] <- input.text.without.index
            return(result)
        }
        else # data set index not supplied
        {
            ind.with.match <- which(vapply(var.names, function(nms) {
                input.text %in% nms
            }, logical(1)))

            if (length(ind.with.match) == 0)
                variableNotFoundError(input.text)

            result <- matrix(nrow = 1, ncol = n.data.sets)
            result[ind.with.match] <- input.text
            return(result)
        }
    }
}

checkInputVariables <- function(variables.to.combine, variables.to.not.combine,
                                variables.to.omit)
{
    # Check variables.to.combine against variables.to.not.combine
    if (!is.null(variables.to.combine) && !is.null(variables.to.not.combine))
    {
        for (i in seqRow(variables.to.combine))
        {
            row.to.combine <- variables.to.combine[i, ]
            for (j in seqRow(variables.to.not.combine))
            {
                row.to.not.combine <- variables.to.not.combine[j, ]
                ind <- which(row.to.combine == row.to.not.combine)
                if (length(ind) > 1)
                    stop("The variables ",
                         paste0(paste0("'", row.to.combine[ind], "'"),
                                collapse = ", "),
                         " have been specified to be both combined and not ",
                         "combined. Ensure that they are specified to be ",
                         "either combined or not combined.")
            }
        }
    }

    # Check variables.to.combine against variables.to.omit
    if (!is.null(variables.to.combine) && !is.null(variables.to.omit))
    {
        for (i in seq_len(ncol(variables.to.combine)))
        {
            ind <- which(variables.to.combine[, i] %in%
                         removeNA(variables.to.omit[, i]))
            v <- unique(variables.to.combine[ind, i])

            if (length(v) == 1)
                stop("The variable ",
                     paste0("'", v, "'"),
                     " has been specified to be both combined and omitted. ",
                     "Ensure that it is specified to be either combined or ",
                     "or omitted.")
            else if (length(v) > 1)
                stop("The variable(s) ",
                     paste0(paste0("'", v, "'"), collapse = ", "),
                     " have been specified to be both combined and omitted. ",
                     "Ensure that they are specified to be either combined ",
                     "or omitted.")
        }
    }
}

# Parses a string of comma-separated names of variables and returns a matrix
# of names where columns correspond to input data. Ranges of variables can be
# specified with a dash. Variables are specified to be from a data set when
# their names have the suffix consisting of the data set index in parentheses.
parseInputVariableText <- function(input.text, input.data.set.metadata)
{
    n.data.sets <- input.data.set.metadata$n.data.sets
    var.names <- input.data.set.metadata$variable.names
    data.set.names <- input.data.set.metadata$data.set.names
    split.text <- splitByComma(input.text)

    parsed.names <- vector(mode = "list", length = n.data.sets)
    source.text <- rep(NA_character_, n.data.sets)
    for (i in seq_along(split.text))
    {
        t <- split.text[i]
        dash.ind <- match("-", strsplit(t, "")[[1]])

        if (!is.na(dash.ind)) # range of variables
        {
            range.start <- trimws(substr(t, 1, dash.ind - 1))
            range.end <- trimws(substr(t, dash.ind + 1, nchar(t)))

            data.set.ind <- parseDataSetIndicesForRange(range.start,
                                                        range.end,
                                                        n.data.sets)

            if (!is.na(data.set.ind)) # data set index supplied for range
            {
                range.start.without.index <- removeDataSetIndex(range.start)
                range.end.without.index <- removeDataSetIndex(range.end)
                range.vars <- variablesFromRange(var.names[[data.set.ind]],
                                                 range.start.without.index,
                                                 range.end.without.index,
                                                 data.set.names[data.set.ind],
                                                 t)
                parsed.names <- addToParsedNames(parsed.names, range.vars,
                                                 data.set.ind, data.set.names,
                                                 source.text, t)
            }
            else # data set index not supplied for range
            {
                is.range.found <- FALSE
                for (j in seq_len(n.data.sets))
                {
                    range.vars <- variablesFromRange(var.names[[j]], range.start, range.end,
                                       data.set.names[j], t, FALSE)
                    if (is.null(range.vars))
                        next

                    parsed.names <- addToParsedNames(parsed.names,
                                                     range.vars, j, data.set.names,
                                                     source.text, t)
                    is.range.found <- TRUE
                }
                if (!is.range.found)
                    stop("The input range '", t, "' was not found ",
                         "in any of the input data sets. Ensure that the ",
                         "range has been correctly specified.")
            }
        }
        else # single variable (not range)
        {
            data.set.ind <- parseDataSetIndex(t, n.data.sets)
            if (!is.na(data.set.ind)) # data set index supplied
            {
                t.without.index <- removeDataSetIndex(t)
                if (!(t.without.index %in% var.names[[data.set.ind]]))
                    variableNotFoundError(t.without.index,
                                          data.set.names[data.set.ind])

                parsed.names <- addToParsedNames(parsed.names,
                                                 t.without.index,
                                                 data.set.ind,
                                                 data.set.names,
                                                 source.text, t)
                source.text[data.set.ind] <- t
            }
            else # data set index not supplied
            {
                ind.with.match <- which(vapply(var.names, function(nms) {
                    t %in% nms
                }, logical(1)))

                if (length(ind.with.match) == 0)
                    variableNotFoundError(t)

                for (j in ind.with.match)
                {
                    parsed.names <- addToParsedNames(parsed.names, t,
                                                     j, data.set.names,
                                                     source.text, t)
                    source.text[j] <- t
                    is.var.found <- TRUE
                }
            }
        }
    }

    if (sum(!vapply(parsed.names, is.null, logical(1))) == 0)
        stop("The input '", input.text, "' does not specify any variables. ",
             "This input needs to specify variables from two or more ",
             "data sets.")

    if (sum(!vapply(parsed.names, is.null, logical(1))) == 1)
      stop("The input '", input.text, "' only specifies variables from one ",
           "data set. This input needs to specify variables from two or more ",
           "data sets.")

    n.vars <- vapply(parsed.names, length, integer(1))
    if (!allIdentical(n.vars[n.vars > 0]))
        stop("The input '", input.text, "' contains ",
             "variable ranges with differing numbers of variables. ",
             "Ensure that the ranges have been correctly specified ",
             "so that they all contain the same number of variables.")

    n.var <- max(vapply(parsed.names, length, integer(1)))
    do.call("cbind", lapply(parsed.names, function(nms) {
        if (is.null(nms))
            rep(NA_character_, n.var)
        else
            nms
    }))
}

# Parse variable names with '(x)' appended where 'x' is a data set index
parseDataSetIndex <- function(input.text, n.data.sets)
{
    if (grepl("\\([[:digit:]]+\\)$", input.text))
    {
        split.into.char <- strsplit(input.text, "")[[1]]
        start.ind <- match("(", split.into.char) + 1
        end.ind <- match("(", split.into.char) + 1
        data.set.ind <- as.integer(substr(input.text, start.ind, end.ind))
        if (data.set.ind < 1 || data.set.ind > n.data.sets)
            stop("The data set index in the input '", input.text,
                 "' is out of range.")
        data.set.ind
    }
    else
        NA
}

removeDataSetIndex <- function(input.text)
{
    if (grepl("\\([[:digit:]]+\\)$", input.text))
    {
        split.into.char <- strsplit(input.text, "")[[1]]
        end.ind <- match("(", split.into.char) - 1
        trimws(substr(input.text, 1, end.ind))
    }
    else
        input.text
}

# Data set index for a range can either be specified for start or end or both
# as long as they are consistent
parseDataSetIndicesForRange <- function(input.text.start, input.text.end, n.data.sets)
{
    data.set.ind.start <- parseDataSetIndex(input.text.start, n.data.sets)
    data.set.ind.end <- parseDataSetIndex(input.text.end, n.data.sets)
    if (!is.na(data.set.ind.start))
    {
        if (!is.na(data.set.ind.end) && data.set.ind.start != data.set.ind.end)
            stop("The specified variable range contains two ",
                 "different data set indices: '", t, "'. The ",
                 "indices need refer to the same data set.")
        else
            data.set.ind.start
    }
    else if (!is.na(data.set.ind.end))
        data.set.ind.end
    else
        NA
}

variablesFromRange <- function(variable.names, range.start, range.end,
                               data.set.name, input.text,
                               error.if.not.found = TRUE)
{
    start.ind <- ifelse(range.start != "", match(range.start, variable.names), 1)
    end.ind <- ifelse(range.end != "", match(range.end, variable.names),
                      length(variable.names))

    if (error.if.not.found)
    {
        if (is.na(start.ind))
            variableNotFoundError(range.start, data.set.name)
        if (is.na(end.ind))
            variableNotFoundError(range.end, data.set.name)
    }
    else
    {
        if (is.na(start.ind) || is.na(end.ind))
            return(NULL)
    }

    if (start.ind > end.ind)
        rangeVariablesOrderError(range.start, range.end, data.set.name, input.text)
    variable.names[start.ind:end.ind]
}

variableNotFoundError <- function(var.name, data.set.name = NULL)
{
    data.set.text <- if(is.null(data.set.name))
        "any of the input data sets."
    else
        paste0("the input data set '", data.set.name, "'.")

    stop("The input variable '", var.name, "' could not be found in ",
         data.set.text, " Ensure that the variable ",
         "has been correctly specified.")
}

rangeVariablesOrderError <- function(start.var, end.var, data.set.name,
                                     range.text)
{
    stop("The start variable '", start.var,
         "' appears after the end variable '", end.var,
         "' in the input data set '", data.set.name,
         "' for the input range '", range.text, "'.")
}

addToParsedNames <- function(parsed.names, input.text.without.index,
                             data.set.ind, data.set.names, source.text,
                             input.text)
{
    if (is.na(source.text[data.set.ind]))
    {
        parsed.names[[data.set.ind]] <- input.text.without.index
        parsed.names
    }
    else
        stop("The manually specified names to match '",
             source.text[data.set.ind], "' and '", input.text,
             "' are both present in data set '",
             data.set.names[data.set.ind], "' and cannot be matched. ",
             "To specify a variable from a specific data set, append '(x)' ",
             "to the variable name when specifying it, ",
             "where 'x' is replaced with the data set index, e.g., use 2 for ",
             "the 2nd input data set.")
}

matchVariableNames <- function(input.data.set.metadata, variables.to.combine,
                               variables.to.not.combine, variables.to.omit)
{
    remaining.vars.to.match <- remainingVarsToMatch(input.data.set.metadata,
                                                    variables.to.combine,
                                                    variables.to.omit)

    matched.names <- matchNamesExactly(remaining.vars.to.match,
                                       variables.to.combine,
                                       variables.to.not.combine)
}

matchNamesExactly <- function(names.list, variables.to.combine,
                              variables.to.not.combine)
{
    n.data.sets <- length(names.list)
    unlisted.names <- unlist(names.list)
    unique.names <- unique(unlisted.names)

    # Data set indices of variables in unlisted.vars
    data.set.ind <- rep(seq_along(names.list),
                        vapply(names.list, length, integer(1)))

    matched.names <- if (!is.null(variables.to.combine))
        variables.to.combine
    else
        matrix(nrow = 0, ncol = n.data.sets)

    for (nm in unique.names)
    {
        if (is.null(variables.to.not.combine))
        {
            rw <- rep(NA, n.data.sets)
            rw[data.set.ind[unlisted.names == nm]] <- nm
            matched.names <- rbind(matched.names, rw)
            next
        }

        # Combine variables provided they can be combined
        remaining.ind <- data.set.ind[unlisted.names == nm]
        repeat
        {
            rw.ind <- remaining.ind[1]
            remaining.ind <- remaining.ind[-1]
            ind.to.consider.adding <- remaining.ind

            for (ind in ind.to.consider.adding)
            {
                isnt.combinable <- apply(variables.to.not.combine, 1, function(nms) {
                    sum(nms[c(rw.ind, ind)] == nm, na.rm = TRUE) > 1
                })
                # Add data set index to row if it is combinable with current
                # indices in the row
                if (!any(isnt.combinable))
                {
                    rw.ind <- c(rw.ind, ind)
                    remaining.ind <- remaining.ind[remaining.ind != ind]
                }
            }

            rw <- rep(NA, n.data.sets)
            rw[rw.ind] <- nm
            matched.names <- rbind(matched.names, rw)

            if (length(remaining.ind) == 0)
                break
        }
    }

    matched.names
}

# Returns list of variables in each data set that remain to be matched,
# i.e., not already combined or omitted.
remainingVarsToMatch <- function(input.data.set.metadata, variables.to.combine,
                                 variables.to.omit)
{
    lapply(seq_len(input.data.set.metadata$n.data.sets), function(i) {
        result <- input.data.set.metadata$variable.names[[i]]
        if (!is.null(variables.to.combine))
            result <- setdiff(result, variables.to.combine[, i])
        if (!is.null(variables.to.omit))
            result <- setdiff(result, variables.to.omit[, i])
        result
    })
}

matchVariableLabels <- function(matched.names, input.data.set.metadata,
                                variables.to.omit)
{

}

matchVariableAndValueLabels <- function(input.data.set.metadata)
{

}

# Split variable match if variables have different types
# and type conversion is not possible
unmatchVariablesOfDifferentTypes <- function(matched.names, data.sets)
{
    n.data.sets <- length(data.sets)
    result <- matrix(nrow = 0, ncol = n.data.sets)
    non.combinable.variables <- matrix(nrow = 0, ncol = n.data.sets)

    for (i in seqRow(matched.names))
    {
        ind <- which(!is.na(matched.names[i, ]))
        var.list <- vector(mode = "list", length = n.data.sets)
        var.list[ind] <- lapply(ind, function(j) data.sets[[j]][[matched.names[i, j]]])

        # Only one variable in the row
        if (length(ind) == 1)
        {
            result <- rbind(result, matched.names[i, ])
            next
        }

        v.types <- vapply(var.list[ind], variableType, character(1))

        # All variables have the same type
        if (allIdentical(removeNA(v.types)))
        {
            result <- rbind(result, matched.names[i, ])
            next
        }

        cat.types <- c("Categorical", "Categorical with string values")

        k <- 1
        repeat
        {
            merge.ind <- integer(0)

            # Text to Date/Time if possible
            if (any(v.types %in% "Date/Time") && any(v.types %in% "Text"))
            {
                text.ind <- ind[v.types == "Text"]
                parsable.ind <- text.ind[vapply(text.ind, function(j) {
                    isParsableAsDateTime(var.list[[j]])
                }, logical(1))]
                merge.ind <- c(ind[v.types == "Date/Time"], parsable.ind)
            }
            # Text to Duration if possible
            else if (any(v.types %in% "Duration") && any(v.types %in% "Text"))
            {
                text.ind <- ind[v.types == "Text"]
                parsable.ind <- text.ind[vapply(text.ind, function(j) {
                    isParsableAsDiffTime(var.list[[j]])
                }, logical(1))]
                merge.ind <- c(ind[v.types == "Duration"], parsable.ind)
            }
            # Numeric or text to categorical as long as there aren't too many
            # unique values: less than max of 20 or 150% of the number of
            # categorical values.
            else if (any(v.types %in% cat.types))
            {
                cat.ind <- ind[v.types %in% cat.types]

                n.category.values <- length(unique(unlist(lapply(cat.ind, function (j) {
                    as.character(attr(var.list[[j]], "labels", exact = TRUE))
                }))))

                num.or.text.ind <- ind[v.types %in% c("Numeric", "Text")]
                num.or.text.merge.ind <- num.or.text.ind[vapply(num.or.text.ind, function(j) {
                    length(unique(var.list[[j]])) <= max(20, n.category.values * 1.5)
                }, logical(1))]

                merge.ind <- c(cat.ind, num.or.text.merge.ind)
            }
            # Text to numeric or numeric to text
            else if (any(v.types %in% "Numeric") && any(v.types %in% "Text"))
                merge.ind <- c(ind[v.types %in% c("Numeric", "Text")])
            else
                merge.ind <- ind[v.types == v.types[1]]

            new.row <- rep(NA_character_, n.data.sets)
            new.row[merge.ind] <- matched.names[i, merge.ind]
            result <- rbind(result, new.row, deparse.level = 0)

            # Remove the merged indices from consideration and break if none
            # are left, otherwise repeat.
            new.ind <- setdiff(ind, merge.ind)
            if (length(new.ind) == 0)
                break

            v.types <- v.types[ind %in% new.ind]
            ind <- new.ind
            k <- k + 1
        }
        if (k > 1)
            non.combinable.variables <- rbind(non.combinable.variables,
                                              matched.names[i, ])
    }
    attr(result, "non.combinable.variables") <- non.combinable.variables
    result
}

isMissingValue <- function(text)
{
    tolower(trimws(text)) %in% c(NA, "", "na", "n/a", "-")
}

isParsableAsNumeric <- function(text)
{
    missing.ind <- isMissingValue(text)
    all(!is.na(suppressWarnings(as.numeric(text[!missing.ind]))))
}

isParsableAsDateTime <- function(text)
{
    missing.ind <- isMissingValue(text)
    all(!is.na(AsDateTime(text[!missing.ind])))
}

isParsableAsDiffTime <- function(text)
{
    missing.ind <- isMissingValue(text)
    all(!is.na(as.difftime(text[!missing.ind])))
}

# Creates a list containing the character matrix input.names and the character
# vector merged.names. merged.names contains the variable names in the merged
# data set and the rows of input.names contain the names of variables in the
# input data sets that were used create the corresponding variable in
# merged.names. This is essentially a map from the input variables to the new
# variable.
mergeMap <- function(matched.names, input.data.set.metadata,
                     prioritize.early.data.sets)
{
    ordered.matched.names <- orderMatchedNames(matched.names,
                                               input.data.set.metadata,
                                               prioritize.early.data.sets)

    merged.names <- apply(ordered.matched.names, 1, function(nms) {
        if (prioritize.early.data.sets)
            removeNA(nms)[1]
        else
            removeNA(rev(nms))[1]
    })

    # Merged names may contain duplicate variable names due to the user
    # specifying variables with the same name to not be combined or variables
    # with the same name not being combined as their types are incompatible.
    # We rename variables so that the names are unique.
    renamed.variables <- list()
    dup <- which(duplicated(merged.names))
    for (i in dup)
    {
        new.name <- uniqueName(merged.names[i], merged.names, "_")
        renamed.variables <- c(renamed.variables,
                               list(list(original.name = merged.names[i],
                                         new.name = new.name)))
        merged.names[i] <- new.name
    }

    list(input.names = ordered.matched.names,
         merged.names = merged.names,
         non.combinable.variables = attr(matched.names, "non.combinable.variables"),
         renamed.variables = renamed.variables)
}

orderMatchedNames <- function(matched.names, input.data.set.metadata,
                              prioritize.early.data.sets)
{
    n.data.sets <- input.data.set.metadata$n.data.sets
    v.names <- input.data.set.metadata$variable.names

    # Convert list of variable names to list of row indices relative to the
    # matched names matrix, removing any names that do not appear in
    # matched.names (i.e., those that have been omitted).
    v.indices <- lapply(seq_len(n.data.sets), function(i) {
        removeNA(match(v.names[[i]], matched.names[, i]))
    })

    # We require non-combinable names to appear consecutively. Therefore
    # we pass a matrix of row indices (relative to the matched names matrix)
    # where each row specifies which variables should be kept together.
    non.combinable.variables <- attr(matched.names, "non.combinable.variables")
    indices.to.keep.togther <- do.call("rbind", lapply(seqRow(non.combinable.variables), function(i) {
        vapply(seq_len(n.data.sets), function(j) {
            if (!is.na(non.combinable.variables[i, j]))
                match(non.combinable.variables[i, j], matched.names[, j])
            else
                NA_integer_
        }, integer(1))
    }))

    ordering <- mergeIndicesList(v.indices, prioritize.early.data.sets,
                                 indices.to.keep.togther)

    ordered.matched.names <- matched.names[ordering, , drop = FALSE]
}

# Takes a list of character vectors each containing indices in a certain order
# and merges them into a single integer vector, respecting the order in
# each vector as much as possible, with earlier vectors taking precedence
# in case of ties.
mergeIndicesList <- function(indices.list, prioritize.early.elements,
                             indices.to.keep.togther = NULL)
{
    # A set of indices are kept together by replacing all in a set with a
    # representative, which is then replaced with the set after merging
    if (!is.null(indices.to.keep.togther))
    {
        representative.indices <- apply(indices.to.keep.togther, 1,
                                        function(indices) removeNA(indices)[1])
        indices.list <- lapply(seq_along(indices.list), function(j) {
            indices <- indices.list[[j]]
            for (i in which(!is.na(indices.to.keep.togther[, j])))
                indices[indices == indices.to.keep.togther[i, j]] <- representative.indices[i]
            indices
        })
    }
    else
        representative.indices <- integer(0)

    if (!prioritize.early.elements)
        indices.list <- rev(indices.list)

    merged.indices <- integer()
    repeat
    {
        if (length(indices.list) == 0)
            break
        else if (length(indices.list) == 1)
        {
            merged.indices <- c(merged.indices, indices.list[[1]])
            break
        }

        # First index from each element in the list
        first.indices <- unique(vapply(indices.list, `[`, integer(1), 1))

        # Rank (index) of each first index
        ranks <- lapply(first.indices, function(candidate.index) {
            vapply(indices.list, function(indices) match(candidate.index, indices),
                   integer(1))
        })

        # Select the first of the indices in first.indices that aren't dominated
        # by another index in first.indices. An index is dominated if another index
        # is always ranked ahead of it whenever they appear together.
        undominated <- vapply(seq_along(first.indices), function(i) {
            all(vapply(seq_along(first.indices)[-i], function(j) {
                i.beats.j <- removeNA(ranks[[i]] < ranks[[j]])
                length(i.beats.j) == 0 || any(i.beats.j)
            }, logical(1)))
        }, logical(1))

        selected.index <- if (any(undominated))
            first.indices[undominated][1]
        else
            first.indices[1]

        ind <- match(selected.index, representative.indices)
        if (!is.na(ind))
            # If selected.index is a representative, replace it with the set
            merged.indices <- c(merged.indices,
                                removeNA(indices.to.keep.togther[ind, ]))
        else
            merged.indices <- c(merged.indices, selected.index)

        # Remove selected index from indices.list
        indices.list <- lapply(indices.list, setdiff, selected.index)

        # Remove empty list elements
        indices.list <- indices.list[vapply(indices.list, length, integer(1)) > 0]
    }
    merged.indices
}

mergeDataSetsWithMergeMap <- function(data.sets, merge.map,
                                      prioritize.early.data.sets,
                                      data.set.names,
                                      category.value.with.multiple.labels)
{
    n.vars <- nrow(merge.map$input.names)
    n.data.set.cases <- vapply(data.sets, nrow, integer(1))

    merged.data.set <- data.frame(lapply(seq_len(n.vars), function(i) {
        compositeVariable(merge.map$input.names[i, ], data.sets,
                          prioritize.early.data.sets,
                          category.value.with.multiple.labels)
    }))

    names(merged.data.set) <- merge.map$merged.names

    mergesrc.name <- uniqueName("mergesrc", names(merged.data.set), "_")
    merged.data.set[[mergesrc.name]] <- mergeSrc(n.data.set.cases, data.set.names)

    merged.data.set
}

# Combine variables from different data sets (end-to-end) to create a
# composite variable
compositeVariable <- function(variable.names, data.sets,
                              prioritize.early.data.sets,
                              category.value.with.multiple.labels)
{
    n.data.sets <- length(data.sets)
    var.list <- lapply(seq_len(n.data.sets), function(i) {
         if(!is.na(variable.names[i]))
             data.sets[[i]][[variable.names[i]]]
        else
            NULL
    })
    v.types <- vapply(var.list, variableType, character(1))

    result <- if (any(v.types %in% c("Categorical", "Categorical with string values")))
        combineCategoricalVariables(var.list, data.sets,
                                    prioritize.early.data.sets, v.types,
                                    category.value.with.multiple.labels)
    else
        combineNonCategoricalVariables(var.list, data.sets, v.types)

    attr(result, "label") <- variableLabelFromDataSets(variable.names,
                                                       data.sets,
                                                       prioritize.early.data.sets)

    result
}

variableType <- function(variable)
{
    if (is.null(variable))
        NA_character_
    else if (!is.null(attr(variable, "labels", exact = TRUE)))
    {
        if (is.numeric(attr(variable, "labels", exact = TRUE)))
            "Categorical"
        else
            "Categorical with string values"
    }
    else if (is.numeric(variable))
        "Numeric"
    else if (is.character(variable))
        "Text"
    else if (inherits(variable, "POSIXct") ||
             inherits(variable, "POSIXt") ||
             inherits(variable, "Date"))
        "Date/Time"
    else if (inherits(variable, "difftime"))
        "Duration"
    else
    {
        stop("Variable type not recognised")
    }
}

combineCategoricalVariables <- function(var.list, data.sets,
                                        prioritize.early.data.sets, v.types,
                                        category.value.with.multiple.labels)
{
    string.values <- "Categorical with string values" %in% v.types

    if (string.values)
    {
        ind <- !is.na(v.types) & v.types == "Categorical"
        var.list[ind] <- lapply(var.list[ind], function(v) {
            categories <- attr(v, "labels", exact = TRUE)
            result <- as.character(v)
            lbls <- names(categories)
            categories <- as.character(categories)
            names(categories) <- lbls
            attr(result, "labels") <- categories
            result
        })
    }

    categories.list <- lapply(var.list, attr, "labels")

    cat.types <- c("Categorical", "Categorical with string values")
    cat.ind <- which(v.types %in% cat.types)
    if (!prioritize.early.data.sets)
        cat.ind <- rev(cat.ind)

    merged.categories <- categories.list[[cat.ind[1]]]
    value.map <- vector("list", length = length(var.list))

    for (i in cat.ind)
    {
        # 2-column matrix representing a remapping of values where the
        # 1st column contains the original value and the 2nd column contains
        # the new value
        map <- matrix(nrow = 0, ncol = 2)

        categories <- categories.list[[i]]
        for (category.label in names(categories))
        {
            category.value <- categoryValue(categories, category.label)
            output <- mergeCategory(category.value, category.label,
                                    merged.categories, map,
                                    category.value.with.multiple.labels)
            merged.categories <- output$merged.categories
            map <- output$map
        }
        if (nrow(map) > 0)
            value.map[[i]] <- map
    }

    n.data.sets <- length(data.sets)

    # List containing input category values where elements are category values
    # of a variable of an input data set that correspond to merged categories.
    # This is used to show the category values table in the output. This list
    # is attached as an attribute to the returned variable.
    input.category.values <- vector(mode = "list", length = n.data.sets)

    # Create composite categorical variable
    result <- NULL
    for (i in seq_len(n.data.sets))
    {
        v <- var.list[[i]]
        if (string.values || v.types[i] %in% "Text") # use %in% instead of == to work with NA
            input.category.values[[i]] <- rep(NA_character_, length(merged.categories))
        else
            input.category.values[[i]] <- rep(NA_real_, length(merged.categories))

        if (is.null(v))
            result <- c(result, rep(NA, nrow(data.sets[[i]])))
        else if (v.types[i] == "Text")
        {
            is.missing <- isMissingValue(v)
            unique.v <- unique(v[!is.missing])

            # text becomes category (numeric) values
            if (!string.values && isParsableAsNumeric(unique.v))
            {
                var.values <- suppressWarnings(as.numeric(v))
                var.values[is.missing] <- NA
                result <- c(result, var.values)

                for (val in as.numeric(unique.v))
                {
                    if (val %in% merged.categories)
                        input.category.values[[i]][merged.categories == val] <- val
                    else
                    {
                        ind <- length(merged.categories) + 1
                        merged.categories[ind] <- val
                        names(merged.categories)[ind] <- as.character(val)
                        input.category.values[[i]][ind] <- val
                    }
                }
            }
            # text becomes category (string) values
            else if (string.values && any(unique.v %in% merged.categories))
            {
                result <- c(result, v)
                for (val in unique.v)
                {
                    if (val %in% merged.categories)
                        input.category.values[[i]][merged.categories == val] <- val
                    else
                    {
                        ind <- length(merged.categories) + 1
                        merged.categories[ind] <- val
                        names(merged.categories)[ind] <- val
                        input.category.values[[i]][ind] <- val
                    }
                }
            }
            else # text becomes category labels
            {
                for (lbl in unique.v)
                {
                    if (lbl %in% names(merged.categories))
                    {
                        ind <- lbl == names(merged.categories)
                        input.category.values[[i]][ind] <- unname(merged.categories[ind])
                    }
                    else if (string.values)
                    {
                        merged.categories[lbl] <- lbl
                        input.category.values[[i]][length(merged.categories)] <- lbl
                    }
                    else
                    {
                        ind <- length(merged.categories) + 1
                        val <- ceiling(max(merged.categories)) + 1
                        merged.categories[ind] <- val
                        names(merged.categories)[ind] <- lbl
                        input.category.values[[i]][ind] <- lbl
                    }
                }

                var.values <- if (string.values)
                    character(length(v))
                else
                    numeric(length(v))

                for (lbl in unique.v)
                    var.values[lbl == v] <- merged.categories[lbl == names(merged.categories)]
                result <- c(result, var.values)
            }
        }
        else if (v.types[i] == "Numeric")
        {
            unique.v <- unique(removeNA(v))

            if (string.values)
            {
                result <- c(result, as.character(v))
                unique.v <- as.character(unique.v)
            }
            else
                result <- c(result, v)

            for (val in unique.v)
            {
                if (val %in% merged.categories)
                    input.category.values[[i]][merged.categories == val] <- val
                else
                {
                    new.ind <- length(merged.categories) + 1
                    merged.categories[new.ind] <- val
                    names(merged.categories)[new.ind] <- val
                    input.category.values[[i]][new.ind] <- val
                }
            }
        }
        else # Categorical
        {
            map <- value.map[[i]]
            result <- c(result, remapValuesInVariable(v, map))

            input.category.values[[i]] <- vapply(merged.categories, function(val) {
                if (!is.null(map))
                {
                    ind <- match(val, map[, 2])
                    if (!is.na(ind))
                        val <- map[ind, 1]
                }
                if (val %in% categories.list[[i]])
                    val
                else
                    ifelse(string.values, NA_character_, NA_real_)
            }, ifelse(string.values, character(1), numeric(1)), USE.NAMES = FALSE)
        }
    }

    attr(result, "labels") <- merged.categories
    attr(result, "input.category.values") <- input.category.values
    class(result) <- c(class(result), "haven_labelled")

    result
}

categoryValue <- function(categories, label)
{
    if (label != "")
        unname(categories[label])
    else
        # need to do this since categories[""] will return NA
        unname(categories[names(categories) == ""])
}

# Merge category into merged.categories
mergeCategory <- function(category.value, category.label, merged.categories, map,
                          category.value.with.multiple.labels)
{
    if (category.label %in% names(merged.categories))
    {
        merged.category.value <- categoryValue(merged.categories, category.label)
        if (category.value != merged.category.value) # same label with different values
        {
            map <- rbind(map, c(category.value, merged.category.value)) # use the value in merged.categories
        }
        # else: same label, same value, no action required as it is already in merged.categories
    }
    else
    {
        if (category.value %in% merged.categories) # different labels with same value
        {
            if (category.value.with.multiple.labels == "Create new values")
            {
                new.value <- if (is.numeric(merged.categories)) # create new numeric value for label
                    ceiling(max(merged.categories)) + 1
                else # is character, create new character value
                {
                    j <- 2
                    repeat
                    {
                        if (!(paste0(category.value, j) %in% merged.categories))
                            break
                        else
                            j <- j + 1
                    }
                    paste0(category.value, j)
                }

                merged.categories[category.label] <- new.value
                map <- rbind(map, c(category.value, new.value))
            }
            # else "Use first value", no action required as it is already in merged.categories
        }
        else # value and label not in merged.categories
            merged.categories[category.label] <- category.value # create new category in merged.categories
    }
    list(merged.categories = merged.categories, map = map)
}

combineNonCategoricalVariables <- function(var.list, data.sets, v.types)
{
    n.data.sets <- length(data.sets)

    .convertTextVar <- function(parser)
    {
        result <- NULL
        for (i in seq_len(n.data.sets))
        {
            v <- var.list[[i]]
            new.vals <- if (is.null(v))
                rep(NA, nrow(data.sets[[i]]))
            else if (v.types[i] == "Text")
            {
                missing.ind <- isMissingValue(v)
                parsed <- rep(NA, length(v))
                parsed[!missing.ind] <- parser(v[!missing.ind])
                parsed
            }
            else
                v
            if (is.null(result))
                result <- new.vals
            else
                result <- c(result, new.vals)
        }
        result
    }

    unique.v.types <- unique(removeNA(v.types))

    if (setequal(unique.v.types, c("Date/Time", "Text")))
        .convertTextVar(AsDateTime)
    else if (setequal(unique.v.types, c("Duration", "Text")))
        .convertTextVar(as.difftime)
    else if (setequal(unique.v.types, c("Numeric", "Text")))
    {
        text.ind <- which(v.types == "Text")
        is.parsable <- all(vapply(text.ind, function(j) {
            isParsableAsNumeric(var.list[[j]])
        }, logical(1)))
        if (is.parsable)
            .convertTextVar(as.numeric)
        else
        {
            unlist(lapply(seq_len(n.data.sets), function(i) {
                v <- var.list[[i]]
                if (is.null(v))
                    rep(NA, nrow(data.sets[[i]]))
                else if (v.types[i] == "Numeric")
                    as.character(v)
                else
                    v
            }))
        }
    }
    else # all same type
    {
        result <- NULL
        for (i in seq_len(n.data.sets))
        {
            v <- var.list[[i]]
            new.vals <- if (!is.null(v))
                v
            else
                rep(NA, nrow(data.sets[[i]]))
            if (is.null(result))
                result <- new.vals
            else
                result <- c(result, new.vals)
        }
        result
    }
}

remapValuesInVariable <- function(variable, map)
{
    result <- variable
    if (!is.null(map))
        for (i in seqRow(map))
            result[variable == map[i, 1]] <- map[i, 2]
    result
}

variableLabelFromDataSets <- function(variable.names, data.sets,
                                      prioritize.early.data.sets)
{
    ind <- if (prioritize.early.data.sets)
        seq_along(data.sets)
    else
        rev(seq_along(data.sets))

    for (i in ind)
    {
        data.set <- data.sets[[i]]

        if (!is.na(variable.names[i]))
        {
            v <- data.set[[variable.names[i]]]
            lbl <- attr(v, "label", exact = TRUE)
            if (!is.null(lbl))
                return(lbl)
        }
    }
    return("")
}

mergeSrc <- function(n.data.set.cases, data.set.names)
{
    n.data.sets <- length(n.data.set.cases)
    result <- rep(seq_len(n.data.sets), n.data.set.cases)
    attr(result, "label") <- "Source of cases"
    attr(result, "labels") <- structure(seq_len(n.data.sets),
                                        .Names = data.set.names)
    class(result) <- c(class(result), "haven_labelled")
    result
}

outputForMergeDataSetsByCase <- function(merged.data.set, input.data.set.metadata,
                                         merge.map,
                                         include.merged.data.set.in.output,
                                         merged.data.set.name)
{
    result <- list()
    if (include.merged.data.set.in.output)
        result$merged.data.set <- merged.data.set

    result$input.data.set.metadata <- input.data.set.metadata
    result$merged.data.set.metadata <- metadataFromDataSet(merged.data.set,
                                                           merged.data.set.name)
    result$merge.map <- merge.map
    result$omitted.variables <- omittedVariables(input.data.set.metadata, merge.map)
    result$input.category.values <- lapply(merged.data.set, attr, "input.category.values")
    class(result) <- "MergeDataSetByCase"
    result
}

omittedVariables <- function(input.data.set.metadata, merge.map)
{
    lapply(seq_len(input.data.set.metadata$n.data.sets), function(i) {
        nms <- input.data.set.metadata$variable.names[[i]]
        ind <- !(nms %in% merge.map$input.names[, i])
        nms[ind]
    })
}

# Convenience function: seq_len of nrow of matrix m
seqRow <- function(m)
{
    seq_len(nrow(m))
}

#' @importFrom flipFormat DataSetMergingWidget
#' @export
print.MergeDataSetByCase <- function(x, ...)
{
    DataSetMergingWidget(x$input.data.set.metadata,
                         x$merged.data.set.metadata,
                         x$merge.map,
                         x$omitted.variables,
                         x$input.category.values)
}
