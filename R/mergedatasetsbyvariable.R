#' @title Merge Data Sets by Variable
#' @description Merges multiple data sets by case where the data sets contain
#'  similar variables but different cases, e.g., data sets from different time
#'  periods.
#' @param data.set.names A character vector of names of data sets from the
#'  Displayr cloud drive to merge (if run from Displayr) or file paths of local
#'  data sets.
#' @param merged.data.set.name A string of the name of the merged data set in
#'  the Displayr cloud drive (if run from Displayr) or the local file path of
#'  the merged data set.
#' @param id.variables A string containing comma-separated names of the optional
#'  ID variables. To specify ID variables from a specific data set, suffix the
#'  name with the data set index in parentheses, e.g., 'ID_VAR(3)'.
#' @param include.or.omit.variables A character vector where each element
#'  corresponds to an input data set, and indicates whether variables from the
#'  input data set are to be specified in the merged data set by specifying
#'  the variables to include ("Only include manually specified variables") or the
#'  variables to omit ("Include all variables except those manually omitted").
#' @param variables.to.include.or.omit A list of character vectors corresponding
#'  to each data set. Each character vector contains comma-separated names of
#'  variables to include or omit (depending on the option for the data set in
#'  \code{include.or.omit.variables}). Ranges of variables can be specified by
#'  separating variable names by '-'.
#' @param preferred.data.set Either "First data set" or "Last data set".
#'  This sets the preference for either the first or last data set when
#'  choosing the order of cases when matching with ID variables and choosing
#'  which ID variable to retain.
#' @param only.keep.cases.matched.to.all.data.sets Only keep cases if they
#'  are present in all data sets, and discard the rest.
#' @param include.merged.data.set.in.output Whether to include the merged data
#'  set in the output.
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{merged.data.set} If \code{include.merged.data.set.in.output},
#'   is TRUE, this is a data frame of the merged data set.
#'   \item \code{input.data.sets.metadata} A list containing metadata on the
#'     the input data sets such as variable names, labels etc.
#'   \item \code{merged.data.set.metadata} A list containing metadata on the
#'     the merged data set such as variable names, labels etc.
#'   \item \code{source.data.set.indices} An integer vector containing the
#'     input data set indices of the merged variables.
#'   \item \code{omitted.variable.names} A list where each element contains the
#'     names of variables from an input data set that have been omitted from
#'     the merged data set.
#'   \item \code{merged.id.variable.name} The name of the merged ID variable if
#'     it exists, otherwise NULL.
#'   \item \code{id.variable.names} A character vector of the names of ID
#'     variables from each data set (NULL if ID variables not used).
#'   \item \code{example.id.values} A character vector of example values from
#'     ID variables from each data set (NULL if ID variables not used).
#'   \item \code{is.saved.to.cloud} Whether the merged data set was saved to
#'     the Displayr cloud drive.
#' }
#' @export
MergeDataSetsByVariable <- function(data.set.names,
                                    merged.data.set.name = NULL,
                                    id.variables = NULL,
                                    include.or.exclude.variables = rep("Include all variables except those manually omitted", length(data.set.names)),
                                    variables.to.include.or.omit = NULL,
                                    preferred.data.set = "First data set",
                                    only.keep.cases.matched.to.all.data.sets = FALSE,
                                    include.merged.data.set.in.output = FALSE)
{
    data.sets <- readDataSets(data.set.names, 2)
    input.data.sets.metadata <- metadataFromDataSets(data.sets)

    matched.cases <- matchCases(input.data.sets.metadata, id.variables,
                                preferred.data.set, data.sets,
                                only.keep.cases.matched.to.all.data.sets)
    merged.data.set.var.names <- mergedDataSetVariableNames(input.data.sets.metadata,
                                                            include.or.exclude.variables,
                                                            variables.to.include.or.omit,
                                                            matched.cases,
                                                            preferred.data.set)
    merged.data.set <- mergedDataSetByVariable(data.sets, matched.cases,
                                               merged.data.set.var.names,
                                               input.data.sets.metadata,
                                               preferred.data.set)
    merged.data.set.name <- cleanMergedDataSetName(merged.data.set.name,
                                                   data.set.names)
    writeDataSet(merged.data.set, merged.data.set.name)

    result <- list()
    if (include.merged.data.set.in.output)
        result$merged.data.set <- merged.data.set

    result$input.data.sets.metadata <- input.data.sets.metadata
    result$merged.data.set.metadata <- metadataFromDataSet(merged.data.set,
                                                           merged.data.set.name)
    result$source.data.set.indices <- attr(merged.data.set.var.names,
                                           "source.data.set.indices")
    result$omitted.variable.names <- attr(merged.data.set.var.names,
                                          "omitted.variable.names")
    result$merged.id.variable.name <- attr(merged.data.set.var.names,
                                           "merged.id.variable.name")
    result$id.variable.names <- attr(matched.cases, "id.variable.names")
    result$example.id.values <- exampleIDValues(result$id.variable.names,
                                                data.sets)
    result$is.saved.to.cloud <- IsDisplayrCloudDriveAvailable()
    class(result) <- "MergeDataSetByVariable"
    result
}

matchCases <- function(input.data.sets.metadata, id.variables,
                       preferred.data.set, data.sets,
                       only.keep.cases.matched.to.all.data.sets)
{
    if (!is.null(id.variables) && trimws(id.variables) != "")
        matchCasesWithIDVariables(input.data.sets.metadata, id.variables,
                                  preferred.data.set, data.sets,
                                  only.keep.cases.matched.to.all.data.sets)
    else
        matchCasesWithoutIDVariables(input.data.sets.metadata)
}

matchCasesWithIDVariables <- function(input.data.sets.metadata, id.variables,
                                      preferred.data.set, data.sets,
                                      only.keep.cases.matched.to.all.data.sets)
{
    id.var.names <- parseIDVariables(id.variables, input.data.sets.metadata)
    n.data.sets <- input.data.sets.metadata$n.data.sets
    n.cases <- input.data.sets.metadata$n.cases

    data.set.ind <- seq_len(n.data.sets)
    if (preferred.data.set == "Last data set")
        data.set.ind <- rev(data.set.ind)

    id.var.types <- vapply(seq_len(n.data.sets), function(i) {
        variableType(data.sets[[i]][[id.var.names[i]]])
    }, character(1))

    merged.id.var.type <- mergedIDVariableType(id.var.types)

    ids.list <- lapply(seq_len(n.data.sets), function(i) {
        ids <- data.sets[[i]][[id.var.names[i]]]

        if (id.var.types[i] != merged.id.var.type)
            ids <- convertIDVariableType(ids, id.var.types[i],
                                         merged.id.var.type)
        ids <- removeNA(ids)

        if (length(ids) == 0)
            stop("The id variable '", id.var.names[i], "' from data set ", i,
                 " does not contain any non-missing IDs.")

        # Check for duplicate values
        duplicated.ids <- unique(ids[duplicated(ids)])
        if (length(duplicated.ids) > 0)
            stop("The ID variable '", id.var.names[i], "' from data set ", i,
                 " is not a suitable ID variable as it contains the following duplicated values: ",
                 paste0(duplicated.ids, collapse = ", "), ".")

        ids
    })

    merged.ids <- NULL
    for (i in data.set.ind)
    {
        ids <- ids.list[[i]]

        has.overlap <- vapply(ids.list[-i], function(other.ids) {
            any(ids %in% other.ids)
        }, logical(1))
        if (!any(has.overlap))
            warning("The IDs in data set ", i, " from variable '",
                    id.var.names[i],
                    "' are not present in the other data sets. ",
                    "Ensure that the ID variable names have been correctly specified.")

        match.ind <- match(ids, merged.ids)
        merged.ids <- c(merged.ids, ids[is.na(match.ind)])
    }

    result <- matrix(NA_integer_, nrow = length(merged.ids), ncol = n.data.sets)
    for (i in seq_len(n.data.sets))
    {
        ids <- data.sets[[i]][[id.var.names[i]]]
        non.missing.ind <- which(!is.na(ids))
        result[match(ids[non.missing.ind], merged.ids), i] <- non.missing.ind
    }

    if (only.keep.cases.matched.to.all.data.sets)
    {
        is.incomplete.case <- rowSums(is.na(result)) > 0
        if (all(is.incomplete.case))
            stop("The merged data set has no cases as there are no IDs that appear in all data sets. ",
                 "Ensure that the ID variable names have been correctly specified.")
        result <- result[!is.incomplete.case, , drop = FALSE]
    }

    attr(result, "id.variable.names") <- id.var.names
    attr(result, "merged.ids") <- merged.ids
    result
}

mergedIDVariableType <- function(id.variable.types)
{
    if (allIdentical(id.variable.types))
        id.variable.types[1]
    else if (all(isDateType(id.variable.types)))
        "Date/Time"
    else # Combination of 2 or more variable types (except "Date" + "Date/Time")
        "Text"
}

convertIDVariableType <- function(ids, id.variable.type,
                                  merged.id.variable.type)
{
    if (merged.id.variable.type == "Date/Time" &&
        id.variable.type == "Date")
        AsDateTime(as.character(ids))
    else # merged.id.variable.type == "Text"
    {
        if (id.variable.type == "Categorical")
        {
            converted.ids <- rep(NA_character_, length(ids))
            val.attrs <- attr(ids, "labels")
            val.labels <- names(val.attrs)
            for (i in seq_along(val.attrs))
                converted.ids[ids == val.attrs[i]] <- val.labels[i]
            converted.ids
        }
        else
            as.character(ids)
    }
}

matchCasesWithoutIDVariables <- function(input.data.sets.metadata)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    n.cases <- input.data.sets.metadata$n.cases

    if (!allIdentical(n.cases))
        stop("The data sets could not be combined without ID variables (side-by-side, no matching) as they have differing numbers of cases. ",
             "To combine them, ID variables need to be specified.")

    result <- matrix(NA_integer_, nrow = max(n.cases), ncol = n.data.sets)
    for (i in seq_len(n.data.sets))
        result[seq_len(n.cases[i]), i] <- seq_len(n.cases[i])
    result
}

# Returns a vector containing ID variable names corresponding to each data set.
# If an ID variable is not found for a data set, the element will be NA.
parseIDVariables <- function(id.variables, input.data.sets.metadata)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    v.names <- input.data.sets.metadata$variable.names

    split.text <- splitByComma(id.variables)
    id.var.names <- vapply(seq_len(n.data.sets), function(i) {
        match.ind <- which(!is.na(match(split.text, v.names[[i]])))
        if (length(match.ind) == 0)
            stop("No ID variable was identified in data set ", i,
                 ". Ensure that the ID variables have been correctly specified.")
        else if (length(match.ind) == 1)
            split.text[match.ind]
        else # length(match.ind) > 1
            stop("The ID variables ",
                 paste0(paste0("'", split.text[match.ind], "'"), collapse = ", "),
                 " are both present in data set ", i,
                 ". Specify the ID variable to use for this data set by appending the data set index to the variable name, e.g., '",
                 split.text[match.ind[1]], "(", i, ")'.")
    }, character(1))

    if (sum(!is.na(id.var.names)) == 1)
        warning("ID variables were only found for one data set (",
                which(!is.na(id.var.names)), "). ",
                "Ensure that the ID variables have been correctly specified so that multiple data sets can be matched.")

    not.found.ind <- which(!(split.text %in% id.var.names))
    if (length(not.found.ind) == 1)
        warning("The following input ID variable name was not found in any data set: '",
                split.text[not.found.ind], "'.")
    else if (length(not.found.ind) > 1)
        warning("The following input ID variable names were not found in any data set: ",
                paste0(paste0("'", split.text[not.found.ind], "'"), collapse = ", "), ".")

    id.var.names
}

mergedDataSetVariableNames <- function(input.data.sets.metadata,
                                       include.or.exclude.variables,
                                       variables.to.include.or.omit,
                                       matched.cases, preferred.data.set)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    v.names <- input.data.sets.metadata$variable.names
    id.var.names <- attr(matched.cases, "id.variable.names")

    v.names.to.include.or.omit <- parseInputVariableTextForDataSet(variables.to.include.or.omit,
                                                                   input.data.sets.metadata)

    input.var.names <- vector(mode = "list", length = n.data.sets)
    omitted.var.names <- vector(mode = "list", length = n.data.sets)
    merged.id.var.name <- NA_character_

    for (i in seq_len(n.data.sets))
    {
        if (include.or.exclude.variables[i] == "Only include those manually specified")
        {
            input.var.names[[i]] <- v.names.to.include.or.omit[[i]]
            omitted.var.names[[i]] <- setdiff(v.names[[i]],
                                              v.names.to.include.or.omit[[i]])
        }
        else # include.or.exclude.variables[i] == "Include all variables except those manually omitted"
        {
            input.var.names[[i]] <- setdiff(v.names[[i]],
                                            v.names.to.include.or.omit[[i]])
            omitted.var.names[[i]] <- v.names.to.include.or.omit[[i]]
        }

        # Ensure the appropriate ID variable names are present/omitted
        if (!is.null(id.var.names))
        {
            if (preferred.data.set == "First data set" && i == 1 ||
                preferred.data.set == "Last data set" && i == n.data.sets)
            {
                input.var.names[[i]] <- union(input.var.names[[i]],
                                              id.var.names[i])
                merged.id.var.name <- id.var.names[i]
            }
            else if (preferred.data.set == "First data set" && i > 1 ||
                     preferred.data.set == "Last data set" && i < n.data.sets)
                input.var.names[[i]] <- setdiff(input.var.names[[i]],
                                                id.var.names[i])
        }

        input.var.names[[i]] <- orderVariablesUsingInputDataSet(input.var.names[[i]],
                                                                v.names[[i]])
        omitted.var.names[[i]] <- orderVariablesUsingInputDataSet(omitted.var.names[[i]],
                                                                  v.names[[i]])
    }

    merged.data.set.var.names <- character(0)
    for (i in seq_len(n.data.sets))
        for (nm in input.var.names[[i]])
            merged.data.set.var.names <- c(merged.data.set.var.names,
                                           uniqueName(nm, merged.data.set.var.names, delimiter = "_"))

    attr(merged.data.set.var.names, "input.variable.names") <- input.var.names
    attr(merged.data.set.var.names, "omitted.variable.names") <- omitted.var.names

    if (!is.null(id.var.names))
    {
        merged.id.var.name <- merged.data.set.var.names[match(merged.id.var.name,
                                                              unlist(input.var.names))]
        attr(merged.data.set.var.names, "merged.id.variable.name") <- merged.id.var.name
    }

    source.data.set.indices <- unlist(lapply(seq_along(input.var.names), function(i) {
        rep(i, length(input.var.names[[i]]))
    }))
    attr(merged.data.set.var.names, "source.data.set.indices") <- source.data.set.indices

    merged.data.set.var.names
}

orderVariablesUsingInputDataSet <- function(var.names.to.order,
                                            data.set.var.names)
{
    var.names.to.order[order(match(var.names.to.order, data.set.var.names))]
}

parseInputVariableTextForDataSet <- function(input.text.list,
                                             input.data.sets.metadata)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    v.names <- input.data.sets.metadata$variable.names

    lapply(seq_len(n.data.sets), function(i) {
        split.text <- unlist(lapply(input.text.list[[i]], splitByComma),
                             use.names = FALSE)

        parsed.names <- character(0)
        for (j in seq_along(split.text))
        {
            t <- split.text[j]
            dash.ind <- match("-", strsplit(t, "")[[1]])
            if (is.na(dash.ind)) # single variable (not range)
            {
                if (!(t %in% v.names[[i]]))
                    variableNotFoundError(t, i)
                parsed.names <- union(parsed.names, t)
            }
            else # range of variables
            {
                range.start <- trimws(substr(t, 1, dash.ind - 1))
                range.end <- trimws(substr(t, dash.ind + 1, nchar(t)))
                range.var.names <- variablesFromRange(v.names[[i]],
                                                      range.start,
                                                      range.end,
                                                      i, t)
                parsed.names <- union(parsed.names, range.var.names)
            }
        }
        parsed.names
    })
}

# Parses a string of comma-separated names of variables and returns a matrix
# of names where columns correspond to input data. Ranges of variables can be
# specified with a dash. Variables are specified to be from a data set when
# their names have the suffix consisting of the data set index in parentheses.
# See unit tests in test-mergedatasetsbycase.R
parseInputVariableText <- function(input.text, input.data.sets.metadata,
                                   require.variables.in.multiple.data.sets)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    var.names <- input.data.sets.metadata$variable.names
    split.text <- splitByComma(input.text, ignore.commas.in.parentheses = TRUE)

    parsed.names <- vector(mode = "list", length = n.data.sets)
    source.text <- rep(NA_character_, n.data.sets)
    is.data.set.specified <- rep(FALSE, n.data.sets)
    for (i in seq_along(split.text))
    {
        t <- split.text[i]
        dash.ind <- match("-", strsplit(t, "")[[1]])

        if (is.na(dash.ind)) # single variable (not range)
        {
            data.set.ind <- parseDataSetIndices(t, n.data.sets)
            if (length(data.set.ind) > 0) # data set indices supplied
            {
                t.without.index <- removeDataSetIndices(t)
                for (j in data.set.ind)
                {
                    if (!(t.without.index %in% var.names[[j]]))
                        variableNotFoundError(t.without.index, j)

                    parsed.names <- addToParsedNames(parsed.names,
                                                     t.without.index,
                                                     j, source.text, t)
                    source.text[j] <- t
                }
                is.data.set.specified[data.set.ind] <- TRUE
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
                    # Existing variable was specified with data set index which
                    # takes precedence over this variable which does not have an
                    # index
                    if (!is.na(source.text[j]) &&
                        grepl("\\(.+\\)", source.text[j]))
                        next

                    parsed.names <- addToParsedNames(parsed.names, t,
                                                     j, source.text, t)
                    source.text[j] <- t
                }
            }
        }
        else # range of variables
        {
            range.start <- trimws(substr(t, 1, dash.ind - 1))
            range.end <- trimws(substr(t, dash.ind + 1, nchar(t)))

            data.set.ind <- parseDataSetIndicesForRange(range.start,
                                                        range.end,
                                                        n.data.sets)

            if (length(data.set.ind) > 0) # data set indices supplied for range
            {
                range.start.without.index <- removeDataSetIndices(range.start)
                range.end.without.index <- removeDataSetIndices(range.end)
                for (j in data.set.ind)
                {
                    range.var.names <- variablesFromRange(var.names[[j]],
                                                          range.start.without.index,
                                                          range.end.without.index,
                                                          j, t)
                    parsed.names <- addToParsedNames(parsed.names, range.var.names,
                                                     j, source.text, t)
                    source.text[j] <- t
                }
                is.data.set.specified[data.set.ind] <- TRUE
            }
            else # data set index not supplied for range
            {
                is.range.found <- FALSE
                for (j in seq_len(n.data.sets))
                {
                    range.var.names <- variablesFromRange(var.names[[j]],
                                                          range.start,
                                                          range.end,
                                                          j, t, FALSE)
                    if (is.null(range.var.names))
                        next

                    is.range.found <- TRUE

                    # Existing variable was specified with data set index which
                    # takes precedence over this variable which does not have an
                    # index
                    if (!is.na(source.text[j]) &&
                        grepl("\\(.+\\)", source.text[j]))
                        next

                    parsed.names <- addToParsedNames(parsed.names,
                                                     range.var.names, j,
                                                     source.text, t)
                    source.text[j] <- t
                }
                if (!is.range.found)
                    stop("The input range '", t, "' was not found in any of the input data sets. ",
                         "Ensure that the range has been correctly specified.")
            }
        }
    }

    if (require.variables.in.multiple.data.sets)
    {
        if (sum(!vapply(parsed.names, is.null, logical(1))) == 0)
            stop("The input '", input.text, "' does not specify any variables. ",
                 "It needs to specify variables from two or more data sets.")

        if (sum(!vapply(parsed.names, is.null, logical(1))) == 1)
            stop("The input '", input.text, "' only specifies variables from one data set. ",
                 "It needs to specify variables from two or more data sets.")
    }
    else
    {
        if (sum(!vapply(parsed.names, is.null, logical(1))) == 0)
            stop("The input '", input.text, "' does not specify any variables.")
    }

    n.vars <- vapply(parsed.names, length, integer(1))
    if (!allIdentical(n.vars[n.vars > 0]))
        stop("The input '", input.text, "' contains variable ranges with differing numbers of variables. ",
             "Ensure that the ranges have been correctly specified so that they all contain the same number of variables.")

    n.var <- max(vapply(parsed.names, length, integer(1)))
    result <- do.call("cbind", lapply(parsed.names, function(nms) {
        if (is.null(nms))
            rep(NA_character_, n.var)
        else
            nms
    }))
    n.row <- nrow(result)
    attr(result, "is.data.set.specified") <- matrix(rep(is.data.set.specified, n.row),
                                                    nrow = n.row, byrow = TRUE)
    result
}

# Parse data set indices from a variable name with data set indices appended,
# e.g. "Q2(3,4)" becomes c(3,4).
parseDataSetIndices <- function(input.text, n.data.sets)
{
    if (grepl("\\(.+\\)$", input.text))
    {
        split.char <- strsplit(input.text, "")[[1]]
        start.ind <- match("(", split.char) + 1
        end.ind <- match(")", split.char) - 1
        data.set.ind.text <- trimws(strsplit(substr(input.text, start.ind,
                                                    end.ind), ",")[[1]])
        data.set.ind <- suppressWarnings(as.integer(data.set.ind.text))
        if (any(is.na(data.set.ind)))
            stop("The data set indices in the input '", input.text,
                 "' could not be parsed. ",
                 "They need to be numbers corresponding to the data sets, e.g., 'Q2(3)'.")

        if (any(data.set.ind < 1) || any(data.set.ind > n.data.sets))
        {
            if (length(data.set.ind) == 1)
                stop("The data set index in the input '", input.text,
                     "' is out of range. ",
                     "Data set indices must be between 1 and the number of input data sets (",
                     n.data.sets, ").")
            else
                stop("One or more of the data set indices in the input '",
                     input.text, "' are out of range. ",
                     "Data set indices must be between 1 and the number of input data sets (",
                     n.data.sets, ").")
        }
        data.set.ind
    }
    else
        integer(0)
}

# Remove appended data set indices from a variable name,
# e.g., "Q2(3)" becomes "Q2"
removeDataSetIndices <- function(input.text)
{
    if (grepl("\\(.+\\)$", input.text))
    {
        split.into.char <- strsplit(input.text, "")[[1]]
        end.ind <- match("(", split.into.char) - 1
        trimws(substr(input.text, 1, end.ind))
    }
    else
        input.text
}

# Data set indices for a range can either be specified for start or end or both
# as long as they are consistent
parseDataSetIndicesForRange <- function(input.text.start, input.text.end, n.data.sets)
{
    data.set.ind.start <- parseDataSetIndices(input.text.start, n.data.sets)
    data.set.ind.end <- parseDataSetIndices(input.text.end, n.data.sets)

    if (length(data.set.ind.start) > 0)
    {
        if (length(data.set.ind.end) > 0)
        {
            if (!setequal(data.set.ind.start, data.set.ind.end))
                stop("The following specified variable range contains two different data set indices: '",
                     input.text.start, "-", input.text.end,
                     "'. The indices need refer to the same data sets.")
            return(data.set.ind.start)
        }
        else
            return(data.set.ind.start)
    }
    else
    {
        if (length(data.set.ind.end) > 0)
            return(data.set.ind.end)
        else
            return(integer(0))
    }
}

# Returns all variables within the specified start and end variables
variablesFromRange <- function(variable.names, range.start, range.end,
                               data.set.index, input.text,
                               error.if.not.found = TRUE)
{
    start.ind <- ifelse(range.start != "", match(range.start, variable.names), 1)
    end.ind <- ifelse(range.end != "", match(range.end, variable.names),
                      length(variable.names))

    if (error.if.not.found)
    {
        if (is.na(start.ind))
            variableNotFoundError(range.start, data.set.index)
        if (is.na(end.ind))
            variableNotFoundError(range.end, data.set.index)
    }
    else
    {
        if (is.na(start.ind) || is.na(end.ind))
            return(NULL)
    }

    if (start.ind > end.ind)
        stop("The start variable '", range.start,
             "' appears after the end variable '", range.end,
             "' in the input data set ", data.set.index,
             " for the input range '", input.text, "'.")
    variable.names[start.ind:end.ind]
}

variableNotFoundError <- function(var.name, data.set.index = NULL)
{
    data.set.text <- if (is.null(data.set.index))
        "any of the input data sets. "
    else
        paste0("the input data set ", data.set.index, ". ")

    stop("The input variable '", var.name,
         "' could not be found in ", data.set.text,
         "Ensure that the variable has been correctly specified.")
}

addToParsedNames <- function(parsed.names, input.text.without.index,
                             data.set.ind, source.text, input.text)
{
    if (is.na(source.text[data.set.ind]) ||
        (grepl("\\(.+\\)$", input.text) && !grepl("\\(.+\\)$", source.text[data.set.ind])))
    {
        parsed.names[[data.set.ind]] <- input.text.without.index
        parsed.names
    }
    else
        stop("The manually specified names to match '",
             source.text[data.set.ind], "' and '", input.text,
             "' are both present in data set ", data.set.ind,
             " and cannot be matched. ",
             "To specify a variable from a specific data set, append '(x)' to the variable name when specifying it, ",
             "where 'x' is replaced with the data set index, e.g., use 2 for the 2nd input data set.")
}

mergedDataSetByVariable <- function(data.sets, matched.cases,
                                    merged.data.set.variable.names,
                                    input.data.sets.metadata,
                                    preferred.data.set)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    n.merged.cases <- nrow(matched.cases)
    input.variable.names <- attr(merged.data.set.variable.names,
                                 "input.variable.names")
    id.variable.names <- attr(matched.cases, "id.variable.names")
    merged.id.data.set.ind <- ifelse(preferred.data.set == "First data set",
                                     1, n.data.sets)
    merged.id.var.name <- id.variable.names[merged.id.data.set.ind]
    merged.ids <- attr(matched.cases, "merged.ids")

    merged.data.set.variables <- vector(mode = "list", length = 0)
    merged.data.set.size <- 0
    j <- 1
    for (i in seq_len(n.data.sets))
    {
        for (nm in input.variable.names[[i]])
        {
            if (i == merged.id.data.set.ind && nm == merged.id.var.name)
            {
                merged.var <- merged.ids
                attr(merged.var, "label") <- attr(data.sets[[i]][[nm]], "label")
            }
            else
            {
                input.var <- data.sets[[i]][[nm]]
                non.missing.ind <- which(!is.na(matched.cases[, i]))
                merged.var <- rep(NA, n.merged.cases)
                merged.var[non.missing.ind] <- input.var[matched.cases[non.missing.ind, i]]

                if (isIntegerValued(merged.var))
                    merged.var <- as.integer(merged.var)
                v.type <- variableType(input.var)
                if (v.type == "Categorical")
                {
                    val.attr <- attr(input.var, "labels")
                    if (is.integer(merged.var))
                    {
                        val.lbls <- names(val.attr)
                        val.attr <- as.integer(val.attr)
                        names(val.attr) <- val.lbls
                    }
                    attr(merged.var, "labels") <- val.attr
                    class(merged.var) <- c(class(merged.var), "haven_labelled")
                }
                attr(merged.var, "label") <- attr(input.var, "label")
            }

            merged.data.set.size <- merged.data.set.size + object.size(merged.var)
            if (merged.data.set.size > DATA.SET.SIZE.LIMIT)
                stop("The merged data set is too large to create. ",
                     "Consider omitting variables or only keeping merged variables that contain input variables from a few data sets.")

            merged.data.set.variables[[j]] <- merged.var
            j <- j + 1
        }
    }
    names(merged.data.set.variables) <- merged.data.set.variable.names
    data.frame(merged.data.set.variables)
}

exampleIDValues <- function(id.variable.names, data.sets)
{
    if (is.null(id.variable.names))
        return(NULL)

    vapply(seq_along(data.sets), function(i) {
        v <- data.sets[[i]][[id.variable.names[i]]]
        val.attr <- attr(v, "labels")
        # ID variables will have non-missing values as we checked for this
        if (!is.null(val.attr))
            names(val.attr)[val.attr == removeNA(v)[1]]
        else
            removeNA(v)[1]
    }, character(1))
}

#' @importFrom flipFormat DataSetMergingByVariableWidget
#' @export
print.MergeDataSetByVariable <- function(x, ...)
{
    DataSetMergingByVariableWidget(x$input.data.sets.metadata,
                                   x$merged.data.set.metadata,
                                   x$source.data.set.indices,
                                   x$omitted.variable.names,
                                   x$merged.id.variable.name,
                                   x$id.variable.names,
                                   x$example.id.values,
                                   x$is.saved.to.cloud)
}
