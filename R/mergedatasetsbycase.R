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
#' @param prioritize.early.data.sets Whether earlier data sets should be
#'  given priority instead of later data sets when determining which
#'  names and labels to use.
#' @param use.first.value.label Whether to use the first label for a value if
#'  there are multiple labels for the same value. Otherwise new values are
#'  created for each label. If \code{prioritize.early.data.sets} is FALSE, the
#'  last label is used instead of the first label.
#' @param ignore.case Ignore case when matching variable names.
#' @param ignore.special.characters Ignore special characters in variable names
#'  except when digit characters appear both before and after special characters,
#'  e.g., Q2__3, in which case all but the first special character is ignored,
#'  i.e., Q2__3 is equivalent to Q2_3.
#' @param data.sets.whose.variables.are.kept An integer vector of indices of data
#'  sets whose variables are to be kept. Any variable not in these data sets
#'  will not be included in the merged data set.
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{merged.data.set} If \code{include.merged.data.set.in.output},
#'   is TRUE, this is a data frame of the merged data set.
#'   \item \code{input.data.set.metadata} A list containing metadata on the
#'     the input data sets such as variable names, labels etc.
#'   \item \code{merged.data.set.metadata} A list containing metadata on the
#'     the merged data set such as variable names, labels etc.
#'   \item \code{matched.names} A matrix where each row contains the names of
#'     the variables from the input data sets that have been merged together.
#'   \item \code{merged.names} A character vector containing the names of the
#'     variables in the merged data set.
#'   \item \code{omitted.variables} A list where each element contains the
#'     names of variables from an input data set that have been omitted from
#'     the merged data set.
#'   \item \code{input.value.attributes} A list containing value attributes
#'     from the input data sets, which are shown in the value attributes tables
#'     in the output.
#'   \item \code{is.saved.to.cloud} Whether the merged data set was saved to
#'     the Displayr cloud drive.
#' }
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
                                prioritize.early.data.sets = TRUE,
                                use.first.value.label = TRUE,
                                ignore.case = TRUE,
                                ignore.special.characters = TRUE,
                                data.sets.whose.variables.are.kept = seq_along(data.set.names),
                                required.data.sets = NULL)
{
    data.sets <- readDataSets(data.set.names, 2)
    input.data.sets.metadata <- metadataFromDataSets(data.sets)

    matched.names <- matchVariables(input.data.sets.metadata, match.by,
                                    min.match.percentage, variables.to.combine,
                                    variables.to.not.combine,
                                    variables.to.omit, data.sets,
                                    ignore.case,
                                    ignore.special.characters,
                                    data.sets.whose.variables.are.kept,
                                    prioritize.early.data.sets)
    merged.names <- mergedVariableNames(matched.names,
                                        prioritize.early.data.sets)
    merged.data.set <- mergedDataSet(data.sets, matched.names, merged.names,
                                      prioritize.early.data.sets,
                                      input.data.sets.metadata$data.set.names,
                                      use.first.value.label)
    merged.data.set.name <- cleanMergedDataSetName(merged.data.set.name,
                                                   data.set.names)

    writeDataSet(merged.data.set, merged.data.set.name)

    outputForMergeDataSetsByCase(merged.data.set, input.data.sets.metadata,
                                 matched.names, merged.names,
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

# Smarter merging of categories where labels are 'close'

# Deal with case when value and label match differently, e.g. 2013 Q1 and Q2, Q1_CA with 80% tolerance
# A value cannot be mapped twice?

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
         variable.value.attributes = lapply(data.sets, function(data.set) {
             lapply(data.set, attr, "labels", exact = TRUE)
         }),
         variable.types = lapply(data.sets, function(data.set) {
             vapply(data.set, variableType2, character(1))
         }),
         data.set.names = names(data.sets),
         n.data.sets = length(data.sets))
}

matchVariables <- function(input.data.set.metadata, match.by,
                           min.match.percentage, variables.to.combine,
                           variables.to.not.combine, variables.to.omit,
                           data.sets,
                           ignore.case,
                           ignore.special.characters,
                           data.sets.whose.variables.are.kept,
                           prioritize.early.data.sets)
{
    v.names.to.combine <- parseVariablesToCombine(variables.to.combine,
                                                  input.data.set.metadata)
    v.names.to.not.combine <- parseVariablesToNotCombine(variables.to.not.combine,
                                                         input.data.set.metadata)
    v.names.to.omit <- parseVariablesToOmitForMerging(variables.to.omit,
                                                      input.data.set.metadata)
    checkInputVariables(v.names.to.combine, v.names.to.not.combine,
                        v.names.to.omit)

    if (match.by == "Variable names")
        matched.names <- matchVariableNames(input.data.set.metadata,
                                            v.names.to.combine,
                                            v.names.to.not.combine,
                                            v.names.to.omit,
                                            min.match.percentage,
                                            data.sets.whose.variables.are.kept,
                                            ignore.case,
                                            ignore.special.characters,
                                            prioritize.early.data.sets)
    else if (match.by == "Variable labels")
    {
        matched.names <- matchVariableLabels(input.data.set.metadata,
                                             v.names.to.combine,
                                             v.names.to.not.combine,
                                             v.names.to.omit,
                                             min.match.percentage,
                                             data.sets.whose.variables.are.kept,
                                             ignore.case,
                                             ignore.special.characters,
                                             prioritize.early.data.sets)
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

    matched.names <- unmatchVariablesOfDifferentTypes(matched.names, data.sets,
                                                      variables.to.combine)
    matched.names <- removeVariablesInOnlySomeDataSets(matched.names,
                                                       data.sets.whose.variables.are.kept,
                                                       input.data.set.metadata,
                                                       variables.to.combine)
    matched.names <- orderMatchedNames(matched.names,
                                       input.data.set.metadata,
                                       prioritize.early.data.sets)

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

checkInputVariables <- function(v.names.to.combine, v.names.to.not.combine,
                                v.names.to.omit)
{
    # Check v.names.to.combine against v.names.to.not.combine
    for (i in seqRow(v.names.to.combine))
    {
        row.to.combine <- v.names.to.combine[i, ]
        for (j in seqRow(v.names.to.not.combine))
        {
            row.to.not.combine <- v.names.to.not.combine[j, ]
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

    # Check v.names.to.combine against v.names.to.omit
    if (!is.null(v.names.to.combine) && !is.null(v.names.to.omit))
    {
        for (i in seq_len(ncol(v.names.to.combine)))
        {
            ind <- which(v.names.to.combine[, i] %in%
                         removeNA(v.names.to.omit[, i]))
            v <- unique(v.names.to.combine[ind, i])

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

matchVariableNames <- function(input.data.set.metadata, v.names.to.combine,
                               v.names.to.not.combine, v.names.to.omit,
                               min.match.percentage,
                               data.sets.whose.variables.are.kept,
                               ignore.case, ignore.special.characters,
                               prioritize.early.data.sets)
{
    n.data.sets <- input.data.set.metadata$n.data.sets
    remaining.names <- lapply(seq_len(n.data.sets), function(i) {
        result <- input.data.set.metadata$variable.names[[i]]
        if (!is.null(v.names.to.combine))
            result <- setdiff(result, v.names.to.combine[, i])
        if (!is.null(v.names.to.omit))
            result <- setdiff(result, v.names.to.omit[, i])
        result
    })

    matched.names <- matrix(nrow = 0, ncol = n.data.sets)
    matched.names <- rbind(matched.names, v.names.to.combine)

    # Find matches to manually combined names
    if (!is.null(matched.names))
    {
        for (i in seqRow(matched.names))
        {
            non.missing.ind <- which(!is.na(matched.names[i, ]))
            for (j in non.missing.ind)
            {
                nm <- matched.names[i, j]
                empty.ind <- is.na(matched.names[i, ])
                for (k in empty.ind)
                {
                    exact.match.ind <- match(nm, remaining.names[[k]])
                    if (!is.na(exact.match.ind))
                    {
                        candidate.name <- remaining.names[[k]][exact.match.ind]
                        is.combinable <- isVariableCombinableIntoRow(candidate.name,
                                                                     k,
                                                                     matched.names[i, ],
                                                                     v.names.to.not.combine)
                        if (is.combinable)
                        {
                            matched.names[i, k] <- candidate.name
                            remaining.names[[k]] <- setdiff(remaining.names[[k]],
                                                            candidate.name)
                            next
                        }
                    }

                    similarities <- percentageSimilarities(nm,
                                                           remaining.names[[k]],
                                                           ignore.case,
                                                           ignore.special.characters)

                    ord <- order(similarities, decreasing = TRUE)
                    for (l in seq_along(similarities))
                    {
                        if (similarities[ord[l]] < min.match.percentage)
                            break

                        candidate.name <- remaining.names[[k]][ord[l]]
                        is.combinable <- isVariableCombinableIntoRow(candidate.name,
                                                                     k,
                                                                     matched.names[i, ],
                                                                     v.names.to.not.combine)

                        if (is.combinable)
                        {
                            matched.names[i, k] <- candidate.name
                            remaining.names[[k]] <- setdiff(remaining.names[[k]],
                                                            candidate.name)
                            break
                        }
                    }
                }
            }
        }
    }

    # Find matches for remaining names
    for (i in data.sets.whose.variables.are.kept)
    {
        nms <- remaining.names[[i]]
        other.data.set.ind <- setdiff(seq_len(n.data.sets), i)
        for (j in seq_along(nms))
        {
            new.row <- rep(NA_character_, n.data.sets)
            new.row[i] <- nms[j]
            remaining.names[[i]] <- setdiff(remaining.names[[i]], nms[j])
            for (k in other.data.set.ind)
            {
                # Reverse the names if later data sets are prioritized so that
                # in the event of a tie, names from later data sets are chosen
                names.in.row <- if (prioritize.early.data.sets)
                    removeNA(new.row)
                else
                    rev(removeNA(new.row))

                exact.match.ind <- removeNA(match(names.in.row, remaining.names[[k]]))

                if (length(exact.match.ind) > 0)
                {
                    candidate.name <- remaining.names[[k]][exact.match.ind[1]]
                    is.combinable <- isVariableCombinableIntoRow(candidate.name,
                                                                 k, new.row,
                                                                 v.names.to.not.combine)
                    if (is.combinable)
                    {
                        new.row[k] <- candidate.name
                        remaining.names[[k]] <- setdiff(remaining.names[[k]],
                                                        candidate.name)
                        next
                    }
                }

                similarities <- percentageSimilarities(names.in.row,
                                                       remaining.names[[k]],
                                                       ignore.case,
                                                       ignore.special.characters)

                ord <- order(similarities, decreasing = TRUE)
                for (l in seq_along(similarities))
                {
                    if (similarities[ord[l]] < min.match.percentage)
                        break

                    candidate.ind <- (ord[l] - 1) %% length(remaining.names[[k]]) + 1
                    candidate.name <- remaining.names[[k]][candidate.ind]
                    is.combinable <- isVariableCombinableIntoRow(candidate.name,
                                                                 k, new.row,
                                                                 v.names.to.not.combine)

                    if (is.combinable)
                    {
                        new.row[k] <- candidate.name
                        remaining.names[[k]] <- setdiff(remaining.names[[k]],
                                                        candidate.name)
                        break
                    }
                }
            }
            matched.names <- rbind(matched.names, new.row)
        }
    }
    matched.names
}

#' @importFrom stringdist stringdistmatrix
percentageSimilarities <- function(strings.1, strings.2, ignore.case,
                                   ignore.special.characters)
{
    if (ignore.case)
    {
        strings.1 <- tolower(strings.1)
        strings.2 <- tolower(strings.2)
    }
    if (ignore.special.characters)
    {
        strings.1 <- removeSpecialCharactersFromNames(strings.1)
        strings.2 <- removeSpecialCharactersFromNames(strings.2)
    }

    distances <- c(t(stringdistmatrix(strings.1, strings.2)))
    nchar.1 <- nchar(strings.1)
    nchar.2 <- nchar(strings.2)
    n.strings.1 <- length(strings.1)
    n.strings.2 <- length(strings.2)
    vapply(seq_along(distances), function(i) {
        max.nchar <- max(nchar.1[floor((i - 1) / n.strings.2) + 1],
                         nchar.2[(i - 1) %% n.strings.2 + 1])
        100 * (1 - distances[i] / max.nchar)
    }, numeric(1))
}

# Remove special characters (@#_\$.) from variable names, except when the
# removal of the special characters results in numeric characters connecting,
# e.g., Q2_1 becoming Q21. In such a case we just remove all but the first
# character, e.g., Q2__1 becomes Q2_1.
removeSpecialCharactersFromNames <- function(nms)
{
    pattern <- paste0("(^(@|#|_|\\\\|\\$|\\.)+)|", # special characters at start
                      "((@|#|_|\\\\|\\$|\\.)+$)|", # special characters at end
                      "((?<=[[:alpha:]])(@|#|_|\\\\|\\$|\\.)+(?=[[:alpha:]]))|", # special characters between alphabet characters
                      "((?<=[[:alpha:]])(@|#|_|\\\\|\\$|\\.)+(?=\\d))|", # special characters between alphabet character and digit
                      "((?<=\\d)(@|#|_|\\\\|\\$|\\.)+(?=[[:alpha:]]))") # special characters between digit and alphabet character
    nms <- gsub(pattern, "", nms, perl = TRUE)

    # Remove all but the first special character when there are multiple
    # consecutive special characters
    nms <- gsub("(?<=(@|#|_|\\\\|\\$|\\.))(@|#|_|\\\\|\\$|\\.)+", "", nms, perl = TRUE)

    nms
}

# Checks that a variable doesn't violate variables.to.not.combine when it is
# combined into a row of other variables. Doesn't check that variable types
# are compatible (this is done later in unmatchVariablesOfDifferentTypes).
isVariableCombinableIntoRow <- function(name.to.combine,
                                        data.set.ind,
                                        row.of.variables,
                                        v.names.to.not.combine)
{
    if (is.null(v.names.to.not.combine))
        return(TRUE)

    row.of.variables[data.set.ind] <- name.to.combine

    all(apply(v.names.to.not.combine, 1, function(nms) {
        sum(nms == row.of.variables, na.rm = TRUE) < 2
    }))
}

matchVariableLabels <- function(input.data.set.metadata,
                                v.names.to.combine,
                                v.names.to.not.combine,
                                v.names.to.omit,
                                min.match.percentage,
                                data.sets.whose.variables.are.kept,
                                ignore.case,
                                ignore.special.characters,
                                prioritize.early.data.sets)
{
    n.data.sets <- input.data.set.metadata$n.data.sets
    remaining.names <- lapply(seq_len(n.data.sets), function(i) {
        result <- input.data.set.metadata$variable.names[[i]]
        if (!is.null(v.names.to.combine))
            result <- setdiff(result, v.names.to.combine[, i])
        if (!is.null(v.names.to.omit))
            result <- setdiff(result, v.names.to.omit[, i])
        result
    })

    matched.names <- matrix(nrow = 0, ncol = n.data.sets)
    matched.names <- rbind(matched.names, v.names.to.combine)

    # Find matches to manually combined names
    if (!is.null(matched.names))
    {
        for (i in seqRow(matched.names))
        {
            non.missing.ind <- which(!is.na(matched.names[i, ]))
            for (j in non.missing.ind)
            {
                nm <- matched.names[i, j]
                # lbl <-
                empty.ind <- is.na(matched.names[i, ])
                for (k in empty.ind)
                {
                    # remaining.labels <-

                    exact.match.ind <- match(nm, remaining.names[[k]]) # need to match labels instead, perhaps map names to labels
                    if (!is.na(exact.match.ind))
                    {
                        candidate.name <- remaining.names[[k]][exact.match.ind]
                        is.combinable <- isVariableCombinableIntoRow(candidate.name,
                                                                     k,
                                                                     matched.names[i, ],
                                                                     v.names.to.not.combine)
                        if (is.combinable)
                        {
                            matched.names[i, k] <- candidate.name
                            remaining.names[[k]] <- setdiff(remaining.names[[k]],
                                                            candidate.name)
                            next
                        }
                    }

                    similarities <- percentageSimilarities(nm,
                                                           remaining.names[[k]],
                                                           ignore.case,
                                                           ignore.special.characters)

                    ord <- order(similarities, decreasing = TRUE)
                    for (l in seq_along(similarities))
                    {
                        if (similarities[ord[l]] < min.match.percentage)
                            break

                        candidate.name <- remaining.names[[k]][ord[l]]
                        is.combinable <- isVariableCombinableIntoRow(candidate.name,
                                                                     k,
                                                                     matched.names[i, ],
                                                                     v.names.to.not.combine)

                        if (is.combinable)
                        {
                            matched.names[i, k] <- candidate.name
                            remaining.names[[k]] <- setdiff(remaining.names[[k]],
                                                            candidate.name)
                            break
                        }
                    }
                }
            }
        }
    }

    # Find matches for remaining names
    for (i in data.sets.whose.variables.are.kept)
    {
        nms <- remaining.names[[i]]
        # lbls <-
        other.data.set.ind <- setdiff(seq_len(n.data.sets), i)
        for (j in seq_along(nms))
        {
            new.row <- rep(NA_character_, n.data.sets)
            new.row[i] <- nms[j]
            remaining.names[[i]] <- setdiff(remaining.names[[i]], nms[j])
            for (k in other.data.set.ind)
            {
                # Reverse the names if later data sets are prioritized so that
                # in the event of a tie, names from later data sets are chosen
                names.in.row <- if (prioritize.early.data.sets)
                    removeNA(new.row)
                else
                    rev(removeNA(new.row))

                exact.match.ind <- removeNA(match(names.in.row, remaining.names[[k]]))

                if (length(exact.match.ind) > 0)
                {
                    candidate.name <- remaining.names[[k]][exact.match.ind[1]]
                    is.combinable <- isVariableCombinableIntoRow(candidate.name,
                                                                 k, new.row,
                                                                 v.names.to.not.combine)
                    if (is.combinable)
                    {
                        new.row[k] <- candidate.name
                        remaining.names[[k]] <- setdiff(remaining.names[[k]],
                                                        candidate.name)
                        next
                    }
                }

                similarities <- percentageSimilarities(names.in.row,
                                                       remaining.names[[k]],
                                                       ignore.case,
                                                       ignore.special.characters)

                ord <- order(similarities, decreasing = TRUE)
                for (l in seq_along(similarities))
                {
                    if (similarities[ord[l]] < min.match.percentage)
                        break

                    candidate.ind <- (ord[l] - 1) %% length(remaining.names[[k]]) + 1
                    candidate.name <- remaining.names[[k]][candidate.ind]
                    is.combinable <- isVariableCombinableIntoRow(candidate.name,
                                                                 k, new.row,
                                                                 v.names.to.not.combine)

                    if (is.combinable)
                    {
                        new.row[k] <- candidate.name
                        remaining.names[[k]] <- setdiff(remaining.names[[k]],
                                                        candidate.name)
                        break
                    }
                }
            }
            matched.names <- rbind(matched.names, new.row)
        }
    }
    matched.names
}

matchVariableAndValueLabels <- function(input.data.set.metadata)
{

}

# Split variable match if variables have different types
# and type conversion is not possible
unmatchVariablesOfDifferentTypes <- function(matched.names, data.sets,
                                             variables.to.combine)
{
    n.data.sets <- length(data.sets)
    result <- matrix(nrow = 0, ncol = n.data.sets)
    non.combinable.variables <- matrix(nrow = 0, ncol = n.data.sets)

    for (i in seqRow(matched.names))
    {
        matched.names.row <- matched.names[i, ]
        ind <- which(!is.na(matched.names.row))
        var.list <- vector(mode = "list", length = n.data.sets)
        var.list[ind] <- lapply(ind, function(j) data.sets[[j]][[matched.names.row[j]]])

        # Only one variable in the row
        if (length(ind) == 1)
        {
            result <- rbind(result, matched.names.row, deparse.level = 0)
            next
        }

        v.types <- vapply(var.list[ind], variableType2, character(1))

        # All variables have the same type
        if (allIdentical(removeNA(v.types)))
        {
            result <- rbind(result, matched.names.row, deparse.level = 0)
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
                merge.ind <- c(ind[v.types %in% c("Date", "Date/Time")],
                               parsable.ind)
            }
            # Text to Date if possible
            else if (any(v.types %in% "Date") && any(v.types %in% "Text"))
            {
                text.ind <- ind[v.types == "Text"]
                parsable.ind <- text.ind[vapply(text.ind, function(j) {
                    isParsableAsDate(var.list[[j]])
                }, logical(1))]
                merge.ind <- c(ind[v.types == "Date"], parsable.ind)
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

                n.values <- length(unique(unlist(lapply(cat.ind, function (j) {
                    as.character(attr(var.list[[j]], "labels", exact = TRUE))
                }))))

                num.or.text.ind <- ind[v.types %in% c("Numeric", "Text")]
                num.or.text.merge.ind <- num.or.text.ind[vapply(num.or.text.ind, function(j) {
                    length(unique(var.list[[j]])) <= max(20, n.values * 1.5)
                }, logical(1))]

                merge.ind <- c(cat.ind, num.or.text.merge.ind)
            }
            # Text to numeric or numeric to text
            else if (any(v.types %in% "Numeric") && any(v.types %in% "Text"))
                merge.ind <- c(ind[v.types %in% c("Numeric", "Text")])
            # Date to Date/Time
            else if (any(v.types %in% "Date") && any(v.types %in% "Date/Time"))
                merge.ind <- c(ind[v.types %in% c("Date", "Date/Time")])
            else
                merge.ind <- ind[v.types == v.types[1]]

            new.row <- rep(NA_character_, n.data.sets)
            new.row[merge.ind] <- matched.names.row[merge.ind]
            result <- rbind(result, new.row, deparse.level = 0)

            # Remove the merged indices from consideration and break if none
            # are left, otherwise repeat.
            new.ind <- setdiff(ind, merge.ind)
            if (length(new.ind) == 0)
                break

            # Check that no variables.to.combine are being split
            for (j in seqRow(variables.to.combine))
            {
                rw <- variables.to.combine[j, ]
                if (any(matched.names.row[merge.ind] %in% rw) &&
                    any(matched.names.row[new.ind] %in% rw))
                    warning("The variables named ",
                            paste0("'", unique(removeNA(rw)), "'", collapse = ", "),
                            " specified to be combined could not be ",
                            "combined due to incompatible variable types.")
            }

            v.types <- v.types[ind %in% new.ind]
            ind <- new.ind
            k <- k + 1
        }
        if (k > 1)
            non.combinable.variables <- rbind(non.combinable.variables,
                                              matched.names.row,
                                              deparse.level = 0)
    }
    attr(result, "non.combinable.variables") <- non.combinable.variables
    result
}

#' @importFrom flipU CopyAttributes
removeVariablesInOnlySomeDataSets <- function(matched.names,
                                              data.sets.whose.variables.are.kept,
                                              input.data.set.metadata,
                                              variables.to.combine)
{
    if (is.null(data.sets.whose.variables.are.kept))
        return(matched.names)

    if (length(data.sets.whose.variables.are.kept) == 0)
    {
        warning("No data sets have been specified for the data sets whose ",
                "variables are to be kept. All variables have been retained.")
        return(matched.names)
    }

    n.data.sets <- input.data.set.metadata$n.data.sets
    if (any(!(data.sets.whose.variables.are.kept %in% seq_len(n.data.sets))))
        stop("The input for 'data.sets.whose.variables.are.kept' contains ",
             "invalid data set indices. Ensure that it contains only indices ",
             "from 1 to the number of input data sets.")

    data.sets.whose.variables.are.kept <- unique(data.sets.whose.variables.are.kept)

    for (i in seqRow(variables.to.combine))
        if (all(is.na(variables.to.combine[i, data.sets.whose.variables.are.kept])))
            stop("The variables named ",
                 paste0("'", unique(removeNA(variables.to.combine[i, ])),
                        "'", collapse = ", "),
                 " specified to be combined have been removed as they are not ",
                 "in the data sets whose variables are to be kept.")

    is.row.kept <- rowSums(!is.na(matched.names[, data.sets.whose.variables.are.kept,
                                                drop = FALSE])) > 0
    result <- matched.names[is.row.kept, , drop = FALSE]
    result <- CopyAttributes(result, matched.names)
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

#' @importFrom flipTime AsDate
isParsableAsDate <- function(text)
{
    missing.ind <- isMissingValue(text)
    all(!is.na(AsDate(text[!missing.ind])))
}

#' @importFrom flipTime AsDateTime
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

mergedVariableNames <- function(matched.names, prioritize.early.data.sets)
{
    merged.names <- apply(matched.names, 1, function(nms) {
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

    attr(merged.names, "renamed.variables") <- renamed.variables
    merged.names
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
    attr(ordered.matched.names, "non.combinable.variables") <- non.combinable.variables
    ordered.matched.names
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

mergedDataSet <- function(data.sets, matched.names, merged.names,
                           prioritize.early.data.sets,
                           data.set.names,
                           use.first.value.label)
{
    n.vars <- nrow(matched.names)
    n.data.set.cases <- vapply(data.sets, nrow, integer(1))

    merged.data.set <- data.frame(lapply(seq_len(n.vars), function(i) {
        compositeVariable(matched.names[i, ], data.sets,
                          prioritize.early.data.sets,
                          use.first.value.label)
    }))

    names(merged.data.set) <- merged.names

    mergesrc.name <- uniqueName("mergesrc", names(merged.data.set), "_")
    merged.data.set[[mergesrc.name]] <- mergeSrc(n.data.set.cases, data.set.names)

    merged.data.set
}

# Combine variables from different data sets (end-to-end) to create a
# composite variable
compositeVariable <- function(variable.names, data.sets,
                              prioritize.early.data.sets,
                              use.first.value.label)
{
    n.data.sets <- length(data.sets)
    var.list <- lapply(seq_len(n.data.sets), function(i) {
         if(!is.na(variable.names[i]))
             data.sets[[i]][[variable.names[i]]]
        else
            NULL
    })
    v.types <- vapply(var.list, variableType2, character(1))

    result <- if (any(v.types %in% c("Categorical", "Categorical with string values")))
        combineCategoricalVariables(var.list, data.sets,
                                    prioritize.early.data.sets, v.types,
                                    use.first.value.label)
    else
        combineNonCategoricalVariables(var.list, data.sets, v.types)

    attr(result, "label") <- variableLabelFromDataSets(variable.names,
                                                       data.sets,
                                                       prioritize.early.data.sets)

    result
}

combineCategoricalVariables <- function(var.list, data.sets,
                                        prioritize.early.data.sets, v.types,
                                        use.first.value.label)
{
    is.string.values <- "Categorical with string values" %in% v.types

    if (is.string.values)
    {
        ind <- !is.na(v.types) & v.types == "Categorical"
        var.list[ind] <- lapply(var.list[ind], function(v) {
            val.attr <- attr(v, "labels", exact = TRUE)
            result <- as.character(v)
            lbls <- names(val.attr)
            val.attr <- as.character(val.attr)
            names(val.attr) <- lbls
            attr(result, "labels") <- val.attr
            result
        })
    }

    val.attr.list <- lapply(var.list, attr, "labels")

    cat.types <- c("Categorical", "Categorical with string values")
    cat.ind <- which(v.types %in% cat.types)
    if (!prioritize.early.data.sets)
        cat.ind <- rev(cat.ind)

    merged.val.attr <- val.attr.list[[cat.ind[1]]]
    value.map <- vector("list", length = length(var.list))

    for (i in cat.ind)
    {
        # 2-column matrix representing a remapping of values where the
        # 1st column contains the original value and the 2nd column contains
        # the new value
        map <- matrix(nrow = 0, ncol = 2)

        val.attr <- val.attr.list[[i]]
        for (lbl in names(val.attr))
        {
            val <- labelValue(val.attr, lbl)
            merged.val.attr <- mergeValueAttribute(val, lbl, merged.val.attr, map,
                                          use.first.value.label)
            map <- attr(merged.val.attr, "map")
        }
        if (nrow(map) > 0)
            value.map[[i]] <- map
    }

    n.data.sets <- length(data.sets)

    # List containing input value attributes where elements are values of a
    # variable of an input data set that correspond to values in the merged
    # data set. This is used to show the value attributes table in the output.
    # This list is attached as an attribute to the returned variable.
    input.val.attr <- vector(mode = "list", length = n.data.sets)

    # Create composite categorical variable
    result <- NULL
    for (i in seq_len(n.data.sets))
    {
        v <- var.list[[i]]
        if (is.string.values || v.types[i] %in% "Text") # use %in% instead of == to work with NA
            input.val.attr[[i]] <- rep(NA_character_, length(merged.val.attr))
        else
            input.val.attr[[i]] <- rep(NA_real_, length(merged.val.attr))

        if (is.null(v))
            result <- c(result, rep(NA, nrow(data.sets[[i]])))
        else if (v.types[i] == "Text")
        {
            is.missing <- isMissingValue(v)
            unique.v <- unique(v[!is.missing])

            # text becomes categorical (numeric) values
            if (!is.string.values && isParsableAsNumeric(unique.v))
            {
                var.values <- suppressWarnings(as.numeric(v))
                var.values[is.missing] <- NA
                result <- c(result, var.values)

                for (val in as.numeric(unique.v))
                {
                    if (val %in% merged.val.attr)
                        input.val.attr[[i]][merged.val.attr == val] <- val
                    else
                    {
                        ind <- length(merged.val.attr) + 1
                        merged.val.attr[ind] <- val
                        names(merged.val.attr)[ind] <- as.character(val)
                        input.val.attr[[i]][ind] <- val
                    }
                }
            }
            # text becomes categorical (string) values
            else if (is.string.values && any(unique.v %in% merged.val.attr))
            {
                result <- c(result, v)
                for (val in unique.v)
                {
                    if (val %in% merged.val.attr)
                        input.val.attr[[i]][merged.val.attr == val] <- val
                    else
                    {
                        ind <- length(merged.val.attr) + 1
                        merged.val.attr[ind] <- val
                        names(merged.val.attr)[ind] <- val
                        input.val.attr[[i]][ind] <- val
                    }
                }
            }
            else # text becomes categorical labels
            {
                for (lbl in unique.v)
                {
                    if (lbl %in% names(merged.val.attr))
                    {
                        ind <- lbl == names(merged.val.attr)
                        input.val.attr[[i]][ind] <- unname(merged.val.attr[ind])
                    }
                    else if (is.string.values)
                    {
                        merged.val.attr[lbl] <- lbl
                        input.val.attr[[i]][length(merged.val.attr)] <- lbl
                    }
                    else
                    {
                        ind <- length(merged.val.attr) + 1
                        val <- ceiling(max(merged.val.attr)) + 1
                        merged.val.attr[ind] <- val
                        names(merged.val.attr)[ind] <- lbl
                        input.val.attr[[i]][ind] <- lbl
                    }
                }

                var.values <- if (is.string.values)
                    character(length(v))
                else
                    numeric(length(v))

                for (lbl in unique.v)
                    var.values[lbl == v] <- merged.val.attr[lbl == names(merged.val.attr)]
                result <- c(result, var.values)
            }
        }
        else if (v.types[i] == "Numeric")
        {
            unique.v <- unique(removeNA(v))

            if (is.string.values)
            {
                result <- c(result, as.character(v))
                unique.v <- as.character(unique.v)
            }
            else
                result <- c(result, v)

            for (val in unique.v)
            {
                if (val %in% merged.val.attr)
                    input.val.attr[[i]][merged.val.attr == val] <- val
                else
                {
                    new.ind <- length(merged.val.attr) + 1
                    merged.val.attr[new.ind] <- val
                    names(merged.val.attr)[new.ind] <- val
                    input.val.attr[[i]][new.ind] <- val
                }
            }
        }
        else # Categorical
        {
            map <- value.map[[i]]
            result <- c(result, remapValuesInVariable(v, map))

            input.val.attr[[i]] <- vapply(merged.val.attr, function(val) {
                if (!is.null(map))
                {
                    ind <- match(val, map[, 2])
                    if (!is.na(ind))
                        val <- map[ind, 1]
                }
                if (val %in% val.attr.list[[i]])
                    val
                else
                    ifelse(is.string.values, NA_character_, NA_real_)
            }, ifelse(is.string.values, character(1), numeric(1)), USE.NAMES = FALSE)
        }
    }

    attr(result, "labels") <- merged.val.attr
    attr(result, "input.value.attributes") <- input.val.attr
    class(result) <- c(class(result), "haven_labelled")

    result
}

labelValue <- function(val.attr, label)
{
    if (label != "")
        unname(val.attr[label])
    else
        # need to do this since val.attr[""] will return NA
        unname(val.attr[names(val.attr) == ""])
}

# Merge value attribute (value and label) into merged.val.attr
mergeValueAttribute <- function(val, lbl, merged.val.attr, map,
                                use.first.value.label)
{
    if (lbl %in% names(merged.val.attr))
    {
        merged.val <- labelValue(merged.val.attr, lbl)
        if (val != merged.val) # same label with different values
        {
            map <- rbind(map, c(val, merged.val)) # use the value in merged.val.attr
        }
        # else: same label, same value, no action required as it is already in merged.val.attr
    }
    else
    {
        if (val %in% merged.val.attr) # different labels with same value
        {
            if (!use.first.value.label)
            {
                new.value <- if (is.numeric(merged.val.attr)) # create new numeric value for label
                    ceiling(max(merged.val.attr)) + 1
                else # is character, create new character value
                {
                    j <- 2
                    repeat
                    {
                        if (!(paste0(val, j) %in% merged.val.attr))
                            break
                        else
                            j <- j + 1
                    }
                    paste0(val, j)
                }

                merged.val.attr[lbl] <- new.value
                map <- rbind(map, c(val, new.value))
            }
            # else "Use first value", no action required as it is already in merged.val.attr
        }
        else # value and label not in merged.val.attr
            merged.val.attr[lbl] <- val # create new value in merged.val.attr
    }
    attr(merged.val.attr, "map") <- map
    merged.val.attr
}

combineNonCategoricalVariables <- function(var.list, data.sets, v.types)
{
    n.data.sets <- length(data.sets)

    .combineVar <- function(parser, as.date.time = FALSE)
    {
        result <- NULL
        for (i in seq_len(n.data.sets))
        {
            v <- var.list[[i]]
            new.vals <- if (is.null(v))
                parser(rep(NA_character_, nrow(data.sets[[i]])))
            else if (v.types[i] == "Text")
            {
                missing.ind <- isMissingValue(v)
                parsed <- parser(rep(NA_character_, length(v)))
                parsed[!missing.ind] <- parser(v[!missing.ind])
                parsed
            }
            else if (as.date.time && v.types[i] == "Date")
                parser(as.character(v))
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

    if (setequal(unique.v.types, c("Date/Time")) ||
        setequal(unique.v.types, c("Date/Time", "Text")) ||
        setequal(unique.v.types, c("Date", "Date/Time", "Text")) ||
        setequal(unique.v.types, c("Date", "Date/Time")))
        .combineVar(AsDateTime, as.date.time = TRUE)
    else if (setequal(unique.v.types, c("Date")) ||
             setequal(unique.v.types, c("Date", "Text")))
        .combineVar(AsDate)
    else if (setequal(unique.v.types, c("Duration")) ||
             setequal(unique.v.types, c("Duration", "Text")))
        .combineVar(as.difftime)
    else if (setequal(unique.v.types, c("Numeric")) ||
             setequal(unique.v.types, c("Numeric", "Text")))
    {
        text.ind <- which(v.types == "Text")
        is.parsable <- all(vapply(text.ind, function(j) {
            isParsableAsNumeric(var.list[[j]])
        }, logical(1)))

        if (is.parsable)
            .combineVar(as.numeric)
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
    else if (setequal(unique.v.types, c("Text")))
        .combineVar(function(x) x)
    else
    {
        # Don't expect this to ever occur
        stop("Unhandled variable types combination: ",
             paste0(unique.v.types, collapse = ", "))
    }
}

# Gets the variable type from a variable. The types are used internally by
# R code and not intended to be exposed to the user.
variableType2 <- function(variable)
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
    else if (inherits(variable, "Date"))
        "Date"
    else if (inherits(variable, "POSIXct"))
        "Date/Time"
    else if (inherits(variable, "difftime"))
        "Duration"
    else
        stop("Variable type not recognised")
}

remapValuesInVariable <- function(variable, map)
{
    result <- variable
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

cleanMergedDataSetName <- function(merged.data.set.name, data.set.names)
{
    if (is.null(merged.data.set.name))
        "Merged data set.sav"
    else
    {
        result <- trimws(merged.data.set.name)
        if (!grepl(".sav$", merged.data.set.name))
            result <- paste0(result, ".sav")
        checkFileNameCharacters(result)
        result
    }
}

outputForMergeDataSetsByCase <- function(merged.data.set,
                                         input.data.set.metadata,
                                         matched.names, merged.names,
                                         include.merged.data.set.in.output,
                                         merged.data.set.name)
{
    result <- list()
    if (include.merged.data.set.in.output)
        result$merged.data.set <- merged.data.set

    result$input.data.set.metadata <- input.data.set.metadata
    result$merged.data.set.metadata <- metadataFromDataSet(merged.data.set,
                                                           merged.data.set.name)
    result$matched.names <- matched.names
    result$merged.names <- merged.names
    result$omitted.variables <- omittedVariables(input.data.set.metadata, matched.names)
    result$input.value.attributes <- lapply(merged.data.set, attr, "input.value.attributes")
    result$is.saved.to.cloud <- IsDisplayrCloudDriveAvailable()
    class(result) <- "MergeDataSetByCase"
    result
}

omittedVariables <- function(input.data.set.metadata, matched.names)
{
    lapply(seq_len(input.data.set.metadata$n.data.sets), function(i) {
        nms <- input.data.set.metadata$variable.names[[i]]
        ind <- !(nms %in% matched.names[, i])
        nms[ind]
    })
}

# Convenience function: seq_len of nrow of matrix m
seqRow <- function(m)
{
    if (is.null(m))
        integer(0)
    else
        seq_len(nrow(m))
}

#' @importFrom flipFormat DataSetMergingWidget
#' @export
print.MergeDataSetByCase <- function(x, ...)
{
    DataSetMergingWidget(x$input.data.set.metadata,
                         x$merged.data.set.metadata,
                         x$matched.names,
                         x$merged.names,
                         x$omitted.variables,
                         x$input.value.attributes,
                         x$is.saved.to.cloud)
}
