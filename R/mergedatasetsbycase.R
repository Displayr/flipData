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
#' @param variables.to.match A character vector of pairs of comma-separated
#'  variable names indicating which variables are to be matched together.
#'  Ranges of variables can be specified by separating variable names by '-'.
#'  Variables can be specified from specific data sets by appending '(x)' to
#'  the variable name where x is the data set index.
#' @param variables.to.not.match A character vector of comma-separated variable
#'  names specifying variables that should never be matched together.
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
                                variables.to.match = NULL,
                                variables.to.not.match = NULL,
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
                                    min.match.percentage, variables.to.match,
                                    variables.to.not.match,
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

# Option to specify variables not to merge

# Subtitle in output showing summary status?

# Remove data set names in output and show row lines, improve spacing between notes

# Wildcards variable inputs

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
                           min.match.percentage, variables.to.match,
                           variables.to.not.match, variables.to.omit,
                           data.sets, required.data.sets)
{
    variables.to.not.match <- parseVariablesToNotMatch(variables.to.not.match,
                                                       input.data.set.metadata)
    variables.to.omit <- parseVariablesToOmit(variables.to.omit,
                                              input.data.set.metadata)

    checkVariablesToOmit(input.data.set.metadata, variables.to.omit)

    # matched.names is a matrix where the columns correspond to the data
    # sets and each row contains the names of variables that have been matched
    # together. NA is used if a variable is absent in a match.
    matched.names <- matrix(nrow = 0, ncol = input.data.set.metadata$n.data.sets)
    matched.names <- applyManualMatches(matched.names,
                                        input.data.set.metadata,
                                        variables.to.match,
                                        variables.to.not.match,
                                        variables.to.omit)

    # TODO: perhaps move call to remainingVarsNames outside of matchVariableNames

    if (match.by == "Variable names")
        matched.names <- matchVariableNames(matched.names,
                                            input.data.set.metadata,
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

parseVariablesToMatch

#
parseVariablesToNotMatch <- function(variables.to.not.match, input.data.set.metadata)
{
    var.names <- input.data.set.metadata$variable.names



    # var.names <- input.data.set.metadata$variable.names
    # n.data.sets <- input.data.set.metadata$n.data.sets
    # result <- matrix(nrow = 0, ncol = n.data.sets)
    # for (i in seq_along(variables.to.not.match))
    # {
    #     split.text <- splitByComma(t)
    #     if (length(split.text) != 2)
    #         stop("A pair of variables or variable ranges need to be specified ",
    #              "for the variables to not match.")
    #     dash.ind.1 <- match("-", strsplit(split.text[1], "")[[1]])
    #     dash.ind.2 <- match("-", strsplit(split.text[2], "")[[1]])
    #     if (is.na(dash.ind.1) && !is.na(dash.ind.2) ||
    #         !is.na(dash.ind.1) && is.na(dash.ind.2))
    #         stop("Ranges need to be specified for either none or both of the ",
    #              "inputs to variables to not match.")
    #     else if (is.na(dash.ind.1) && is.na(dash.ind.2))
    #     {
    #         data.set.ind.1 <- parseDataSetIndex(split.text[1])
    #         data.set.ind.2 <- parseDataSetIndex(split.text[2])
    #         if (is.na(data.set.ind.1) || is.na(data.set.ind.2))
    #             stop("Data set indices need to be specified for the variables ",
    #                  "to not match, e.g. 'Q2(2)' for variable 'Q2' from data ",
    #                  "2.")
    #         nm.1 <- removeDataSetIndex(split.text[1])
    #         nm.2 <- removeDataSetIndex(split.text[2])
    #         if (!(nm.1 %in% var.names[[data.set.ind.1]]))
    #             stop("Variable ", nm.1, " (variable to not match) could not ",
    #                  "be found in data set ", data.set.ind.1)
    #         if (!(nm.2 %in% var.names[[data.set.ind.2]]))
    #             stop("Variable ", nm.2, " (variable to not match) could not ",
    #                  "be found in data set ", data.set.ind.2)
    #         new.row <- rep(NA_character_, n.data.sets)
    #         new.row[data.set.ind.1] <-
    #         result <- rbind(result, new.row)
    #     }
    #     else # ranges
    #     {
    #
    #     }
    # }
    # result


    # lapply(variables.to.not.match, function(t) {
    #     split.text <- trimws(strsplit(t, ",")[[1]])
    #     split.text <- split.text[split.text != ""]
    #
    # })
}

#
parseVariablesToOmit <- function()
{

}

applyManualMatches <- function(matched.names, input.data.set.metadata,
                               variables.to.match, variables.to.not.match,
                               variables.to.omit)
{
    if (is.null(variables.to.match))
        return (matched.names)

    for (match.text in variables.to.match)
    {
        manual.matched.names <- parseManualMatchText(match.text,
                                                     input.data.set.metadata,
                                                     variables.to.omit)
        matched.names <- rbind(matched.names,
                               manual.matched.names)
    }
    checkMatchForDuplication(matched.names, variables.to.match)

    matched.names
}

# Parses a string of comma-separated variable names to be manually matched
# and returns matrix of matched names where columns correspond to input data
parseManualMatchText <- function(manual.match.text, input.data.set.metadata,
                                 variables.to.omit)
{
    n.data.sets <- input.data.set.metadata$n.data.sets
    var.names <- input.data.set.metadata$variable.names
    data.set.names <- input.data.set.metadata$data.set.names
    split.text <- splitByComma(manual.match.text)

    if (sum(split.text != "") < 2)
        stop("The manual match input '", manual.match.text, "' is invalid as ",
             "it needs to contain two or more variables.")

    parsed.names <- vector(mode = "list", length = n.data.sets)
    source.text <- rep(NA_character_, n.data.sets)
    for (i in seq_along(split.text))
    {
        input.text <- split.text[i]

        dash.ind <- match("-", strsplit(input.text, "")[[1]])

        if (!is.na(dash.ind)) # range of variables
        {
            input.text.start <- trimws(substr(input.text, 1, dash.ind - 1))
            input.text.end <- trimws(substr(input.text, dash.ind + 1, nchar(input.text)))

            if (input.text.start == "" || input.text.end == "")
                stop("The manual match input '", manual.match.text,
                     "' is invalid as it contains an incorrectly specified ",
                     "variable range: '", input.text, "'.")

            data.set.ind <- parseDataSetIndicesForRange(input.text.start,
                                                        input.text.end,
                                                        n.data.sets)

            if (!is.na(data.set.ind))
            {
                nms <- var.names[[data.set.ind]]
                input.text.start.without.index <- removeDataSetIndex(input.text.start)
                input.text.end.without.index <- removeDataSetIndex(input.text.end)
                start.ind <- match(input.text.start.without.index, nms)
                end.ind <- match(input.text.end.without.index, nms)
                if (is.na(start.ind))
                    variableNotFoundError(input.text.start.without.index,
                                          data.set.names[data.set.ind])
                if (is.na(end.ind))
                    variableNotFoundError(input.text.end.without.index,
                                          data.set.names[data.set.ind])
                if (start.ind > end.ind)
                    rangeVariablesOrderError(input.text.start.without.index,
                                             input.text.end.without.index,
                                             data.set.names[data.set.ind],
                                             input.text)

                parsed.names <- addToParsedNames(parsed.names,
                                                 nms[start.ind:end.ind],
                                                 data.set.ind,
                                                 data.set.names,
                                                 source.text, input.text)
            }
            else
            {
                is.range.found <- FALSE
                for (j in seq_len(n.data.sets))
                {
                    nms <- var.names[[j]]
                    start.ind <- match(input.text.start, nms)
                    end.ind <- match(input.text.end, nms)
                    if (is.na(start.ind) || is.na(end.ind))
                        next

                    if (start.ind > end.ind)
                        rangeVariablesOrderError(input.text.start,
                                                 input.text.end,
                                                 data.set.names[j],
                                                 input.text)

                    parsed.names <- addToParsedNames(parsed.names,
                                                     nms[start.ind:end.ind],
                                                     j, data.set.names,
                                                     source.text, input.text)
                    is.range.found <- TRUE
                }
                if (!is.range.found)
                    stop("The input range '", input.text, "' was not found ",
                         "in any of the input data sets. Ensure that the ",
                         "range has been correctly specified.")
            }
        }
        else # single variable (not range)
        {
            data.set.ind <- parseDataSetIndex(input.text, n.data.sets)
            if (!is.na(data.set.ind))
            {
                input.text.without.index <- removeDataSetIndex(input.text)
                if (!(input.text.without.index %in% var.names[[data.set.ind]]))
                    variableNotFoundError(input.text.without.index,
                                          data.set.names[data.set.ind])

                parsed.names <- addToParsedNames(parsed.names,
                                                 input.text.without.index,
                                                 data.set.ind,
                                                 data.set.names,
                                                 source.text, input.text)
                source.text[data.set.ind] <- input.text
            }
            else
            {
                ind.with.match <- which(vapply(var.names, function(nms) {
                    input.text %in% nms
                }, logical(1)))

                if (length(ind.with.match) == 0)
                    variableNotFoundError(input.text)

                for (j in ind.with.match)
                {
                    parsed.names <- addToParsedNames(parsed.names, input.text,
                                                     j, data.set.names,
                                                     source.text, input.text)
                    source.text[data.set.ind] <- input.text
                    is.var.found <- TRUE
                }
            }
        }
    }

    n.vars <- vapply(parsed.names, length, integer(1))
    if (!allIdentical(n.vars[n.vars > 0]))
        stop("The manual match input '", manual.match.text, "' contains ",
             "variable ranges with differing numbers of variables. ",
             "Ensure that the ranges have been correctly specified ",
             "so that they all contain the same number of variables.")

    if (!is.null(variables.to.omit))
        for (i in seq_len(n.data.sets))
        {
            is.omitted <- parsed.names[[i]] %in% variables.to.omit[[i]]
            if (sum(is.omitted) == 1)
                stop("The variable ", parsed.names[[i]][is.omitted],
                     " has been specified in both a manual match and to be ",
                     "omitted. It needs to be removed from the manual match ",
                     "or the variables to be omitted.")
            else if (sum(is.omitted) > 1)
                stop("The variable(s) ",
                     paste0(parsed.names[[i]][is.omitted], collapse = ", "),
                     " have been specified in both manual matches both and ",
                     "to be omitted. They need to be removed from the manual ",
                     "matches or the variables to be omitted.")
        }

    n.var <- max(vapply(parsed.names, length, integer(1)))
    result <- vapply(parsed.names, function(nms) {
        if (is.null(nms))
            rep(NA_character_, n.var)
        else
            nms
    }, character(n.var))

    # If there is only one variable, vapply returns a vector,
    # so we need to transpose
    if (!is.matrix(result))
        t(result)
    else
        result
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

variableNotFoundError <- function(var.name, data.set.name = NULL)
{
    data.set.text <- if(is.null(data.set.name))
        "any of the input data sets. "
    else
        paste0("the input data set '", data.set.name, "'. ")

    stop("The input variable '", var.name, "' specified for manual matching ",
         "could not be found in ", data.set.text, "Ensure that the variable ",
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
             "to the variable name when specifying it in a manual match, ",
             "where 'x' is replaced with the data set index, e.g., use 2 for ",
             "the 2nd input data set.")
}

# Check that manual matches have not been specified multiple times
checkMatchForDuplication <- function(matched.names, match.source)
{
    n.data.sets <- ncol(matched.names)
    for (i in seq_len(n.data.sets))
    {
        date.set.vars.names <- matched.names[, i]
        names.table <- table(date.set.vars.names)
        if (any(names.table > 1))
        {
            duplicate.name <- names(names.table)[names.table > 1][1]
            duplicate.match.source <- match.source[date.set.vars.names == duplicate.name]
            stop("The variable '", duplicate.name, "' has been specified in ",
                 "multiple manual match inputs: ",
                 paste0(paste0("'", duplicate.match.source, "'"), collapse = ", "),
                 ". Ensure that all variables are only specified in at most ",
                 "one manual match.")
        }
    }
}

checkVariablesToOmit <- function(input.data.set.metadata, variables.to.omit)
{
    if (is.null(variables.to.omit))
        return()

    if (length(variables.to.omit) != input.data.set.metadata$n.data.sets)
        stop("variables.to.omit must be specified as a list whose elements ",
             "are the variables to omit in the corresponding data sets.")

    for (i in seq_len(input.data.set.metadata$n.data.sets))
    {
        var.names <- input.data.set.metadata$variable.names[[i]]
        ind <- which(!(variables.to.omit[[i]] %in% var.names))
        if (length(ind) > 0)
        {
            stop("The following variable(s) were specified to be omitted but ",
                 "could not be found in the data set ",
                 input.data.set.metadata$data.set.names[i], ": ",
                 paste0(variables.to.omit[[i]][ind], collapse = ", "), ".")
        }
    }
}

matchVariableNames <- function(matched.names, input.data.set.metadata,
                               variables.to.omit)
{
    remaining.vars.names <- remainingVarsNames(matched.names,
                                               input.data.set.metadata,
                                               variables.to.omit)
    rbind(matched.names, matchNamesExactly(remaining.vars.names))
}

matchNamesExactly <- function(names.list)
{
    n.list.entries <- length(names.list)
    unlisted.names <- unlist(names.list)
    unique.names <- unique(unlisted.names)

    # Data set indices of variables in unlisted.vars
    data.set.ind <- rep(seq_along(names.list),
                        vapply(names.list, length, integer(1)))

    t(vapply(unique.names, function(nm) {
        result <- rep(NA, n.list.entries)
        result[data.set.ind[unlisted.names == nm]] <- nm
        result
    }, character(n.list.entries)))
}

remainingVarsNames <- function(matched.names, input.data.set.metadata,
                               variables.to.omit)
{
    lapply(seq_len(input.data.set.metadata$n.data.sets), function(i) {
        excluded.variables <- matched.names[, i]
        if (!is.null(variables.to.omit))
            excluded.variables <- c(excluded.variables, variables.to.omit[[i]])
        setdiff(input.data.set.metadata$variable.names[[i]], excluded.variables)
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
    unmatched.names <- matrix(nrow = 0, ncol = n.data.sets)
    for (i in seq_len(nrow(matched.names)))
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

            new.ind <- setdiff(ind, merge.ind)
            if (length(new.ind) == 0)
                break

            v.types <- v.types[ind %in% new.ind]
            ind <- new.ind
            k <- k + 1
        }
        if (k > 1)
            unmatched.names <- rbind(unmatched.names, matched.names[i, ],
                                     deparse.level = 0)
    }

    attr(result, "unmatched.names") <- unmatched.names
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
    unordered.merged.names <- mergedNamesFromMatchedNames(matched.names,
                                                          prioritize.early.data.sets)
    merged.names <- orderMergedNames(unordered.merged.names,
                                     matched.names,
                                     input.data.set.metadata,
                                     prioritize.early.data.sets)
    input.names <- orderMatchedNames(matched.names,
                                     unordered.merged.names,
                                     merged.names)
    list(input.names = input.names,
         merged.names = merged.names,
         unmatched.names = attr(matched.names, "unmatched.names"),
         unmatched.names.renamed = attr(unordered.merged.names,
                                        "unmatched.names.renamed"))
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

  # TODO: deal with case where mergesrc already exists
    merged.data.set[["mergesrc"]] <- mergeSrc(n.data.set.cases, data.set.names)

    merged.data.set
}

# Merged names are the names shown in the merged data set.
# They are obtained from the latest non-missing name in each row of
# matched.names.
mergedNamesFromMatchedNames <- function(matched.names,
                                        prioritize.early.data.sets)
{
    .select.name <- if (prioritize.early.data.sets)
        function(nms) removeNA(nms)[1]
    else
        function(nms) rev(removeNA(nms))[1]

    unmatched.names <- attr(matched.names, "unmatched.names", exact = TRUE)
    unmatched.names.renamed <- vector(mode = "list",
                                      length = nrow(unmatched.names))

    result <- apply(matched.names, 1, .select.name)

    # As a result of calling unmatchVariablesOfDifferentTypes, two variables
    # with the same name from different data sets could not be merged if they
    # have incompatible types, and therefore become two separate variables in
    # the merged data set. We need to rename one of these variables in the
    # merged data set as names need to be unique.
    dup <- duplicated(result)
    if (any(dup))
    {
        ind <- which(dup)
        for (i in ind)
        {
            base.name <- result[i]
            j <- 2
            repeat
            {
                nm <- paste0(base.name, "_", j)
                if (!(nm %in% result))
                {
                    result[i] <- nm

                    unmatched.ind <- which(vapply(seq_len(nrow(unmatched.names)), function(j) {
                        any(unmatched.names[j, ] == matched.names[i, ])
                    }, logical(1)))
                    unmatched.names.renamed[[unmatched.ind]] <- c(unmatched.names.renamed[[unmatched.ind]],
                                                                  list(nm))
                    break
                }
                else
                    j <- j + 1

            }
        }
    }

    attr(result, "unmatched.names.renamed") <- unmatched.names.renamed
    result
}

# Produce an ordering of the matched variables based on the order if the
# variables in the data set
orderMergedNames <- function(unordered.merged.names, matched.names,
                             input.data.set.metadata, prioritize.early.data.sets)
{
    n.data.sets <- input.data.set.metadata$n.data.sets
    names.list <- lapply(seq_len(n.data.sets), function(i) {
        ind <- match(input.data.set.metadata$variable.names[[i]], matched.names[, i])
        unordered.merged.names[removeNA(ind)]
    })

    merged.names <- mergeNamesListRespectingOrder(names.list, prioritize.early.data.sets)

    # Move deduplicated names to be just below the original ones
    deduplicated.names <- attr(unordered.merged.names, "deduplicated.names")
    if (!is.null(deduplicated.names))
    {
        for (i in seq_len(nrow(deduplicated.names)))
        {
            dedup.ind <- match(deduplicated.names[i, 2], merged.names)
            merged.names <- merged.names[-dedup.ind]
            original.ind <- match(deduplicated.names[i, 1], merged.names)
            merged.names <- c(merged.names[seq_len(original.ind)],
                              deduplicated.names[i, 2],
                              merged.names[-seq_len(original.ind)])
        }
    }

    merged.names
}

# Order the rows in the matched names matrix according to the (ordered) merged names
orderMatchedNames <- function(matched.names, unordered.merged.names, merged.names)
{
    n.data.sets <- ncol(matched.names)
    t(vapply(merged.names, function(nm) {
        matched.names[match(nm, unordered.merged.names), ]
    }, character(n.data.sets)))
}

# Takes a list of character vectors each containing names in a certain order
# and merges them into a single character vector, respecting the order in
# each vector as much as possible, with earlier vectors taking precedence
# in case of ties.
mergeNamesListRespectingOrder <- function(names.list, prioritize.early.elements)
{
    if (!prioritize.early.elements)
        names.list <- rev(names.list)

    merged.names <- character()
    repeat
    {
        if (length(names.list) == 0)
            break
        else if (length(names.list) == 1)
        {
            merged.names <- c(merged.names, names.list[[1]])
            break
        }

        first.names <- unique(vapply(names.list, `[`, character(1), 1))

        ranks <- lapply(first.names, function(candidate.name) {
            vapply(names.list, function(nms) match(candidate.name, nms), integer(1))
        })

        # Select the first of the names in first.names that aren't dominated
        # by another name in first.names. A name is dominated if another name
        # is always ranked ahead of it whenever they appear together.
        undominated <- vapply(seq_along(first.names), function(i) {
            all(vapply(seq_along(first.names)[-i], function(j) {
                i.beats.j <- removeNA(ranks[[i]] < ranks[[j]])
                length(i.beats.j) == 0 || any(i.beats.j)
            }, logical(1)))
        }, logical(1))

        selected.name <- if (any(undominated))
            first.names[undominated][1]
        else
            first.names[1]

        merged.names <- c(merged.names, selected.name)

        # Remove selected variable from names.list
        names.list <- lapply(names.list, setdiff, selected.name)

        # Remove empty list elements
        names.list <- names.list[vapply(names.list, length, integer(1)) > 0]
    }
    merged.names
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
        for (i in seq_len(nrow(map)))
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
