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
#' @param manual.matches A character vector of comma-separated variable names
#'  indicating which variables are to be matched together, e.g.,
#'  "var_name_from_data_set_1, var_name_from_data_set_2, ..."
#' @param variables.to.omit A list of character vectors containing names of
#'  variables to omit from the merged data set. The vectors in the list
#'  correspond to the input data sets.
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
#' @importFrom verbs Sum
#' @export
MergeDataSetsByCase <- function(data.set.names,
                                merged.data.set.name = NULL,
                                match.by = "Automatic",
                                min.match.percentage = 100,
                                manual.matches = NULL,
                                variables.to.omit = NULL,
                                include.merged.data.set.in.output = FALSE,
                                write.data.set = TRUE,
                                prioritize.early.data.sets = TRUE,
                                category.value.with.multiple.labels = "Use first label")
{
    data.sets <- readDataSets(data.set.names)
    variable.metadata <- extractVariableMetadata(data.sets)
    matched.names <- matchVariables(variable.metadata, match.by,
                                    min.match.percentage, manual.matches,
                                    variables.to.omit, data.sets)
    merge.map <- mergeMap(matched.names, variable.metadata,
                          prioritize.early.data.sets)
    merged.data.set <- mergeDataSetsWithMergeMap(data.sets, merge.map,
                                                 prioritize.early.data.sets,
                                                 variable.metadata$data.set.names,
                                                 category.value.with.multiple.labels)
    merged.data.set.name <- cleanMergedDataSetName(merged.data.set.name,
                                                   data.set.names)

    if (write.data.set)
        writeDataSet(merged.data.set, merged.data.set.name)

    outputForMergeDataSetsByCase(merged.data.set, variable.metadata, merge.map,
                                 include.merged.data.set.in.output,
                                 merged.data.set.name)
}

# GUI
# textboxes for data set names
# combobox for match.by
# numeric updown for min.match.percentage
# textboxes for manual.match
# textboxes for variables.to.omit

# Output to contain:
# - Table of variable labels with columns: Merged Data Set, Data Set 1, Data Set 2, Data Set 3, ...
#   perhaps indicating the ones with a weak match and indicating the manual matches
#   and tooltips with variable names
# - Omitted variables (listed by data set), indicating the manual omits

# Fuzzy matching:
# Start with last data set first perform exact match against second last data set.
# For the remaining labels, compute relative distances (0-1) for all possible pairs
# and match if threshold is met, starting from best matches
# Repeat for third last data set etc.

# TODO

# Need to ensure any new names we generate are valid for sav files, e.g. not too long

# Option to specify whether to preserve category values or labels

# Option to include or exclude variables without full matches, and option
# to specify variables to include, or specify minimum number of variables
# to be included in a match

# Option to specify variants of variable name or label (include range) (this
# replaces manual match)

# Option to merge mutually exclusive variables

# Convert text to categorical if there are both text and categorical variables
# and the categories match closely? Treat empty/blank strings as missing

# Smarter merging of categories where labels are 'close'

readDataSets <- function(data.set.names)
{
    if (length(data.set.names) < 2)
        stop("Merging requires at least two data sets.")

    if (canAccessDisplayrCloudDrive())
        readDataSetsFromDisplayrCloudDrive(data.set.names)
    else
        readLocalDataSets(data.set.names)
}

canAccessDisplayrCloudDrive <- function()
{
    company.secret <- get0("companySecret")
    !is.null(company.secret) && company.secret != "UNKNOWN"
}

#' @importFrom haven read_sav
readLocalDataSets <- function(data.set.paths)
{
    result <- lapply(data.set.paths, function(path) {
        read_sav(path)
    })
    names(result) <- basename(data.set.paths)
    result
}

#' @importFrom flipAPI QFileExists QLoadData
readDataSetsFromDisplayrCloudDrive <- function(data.set.names)
{
    result <- lapply(data.set.names, function(nm) {
        if (!QFileExists(nm))
            stop("The data file '", nm, "' does not exist in the Display ",
                 "cloud drive. Ensure that the data file is in the Display ",
                 "cloud drive and its name has been correctly specified.",
                 call. = FALSE)
        QLoadData(nm)
    })
    names(result) <- data.set.names
    result
}

#' @importFrom haven write_sav
#' @importFrom flipAPI QSaveData
writeDataSet <- function(data.set, data.set.name)
{
    if (canAccessDisplayrCloudDrive())
        QSaveData(data.set, data.set.name)
    else
        write_sav(data.set, data.set.name)
}

generateMergedDataSetName <- function(data.set.names)
{
    common_prefix <- ""
    for (i in 1:min(nchar(data.set.names)))
    {
        if (length(unique(vapply(data.set.names, substr, character(1), 1, i))) == 1)
            common_prefix <- substr(data.set.names[1], 1, i)
        else
            break
    }
    if (common_prefix == "")
        "Merged data set.sav"
    else
        paste0(common_prefix, " merged.sav")
}

extractDataSetName <- function(data.set.name.or.path)
{
    if (is.null(data.set.name.or.path))
        "Merged data set"
    else if (canAccessDisplayrCloudDrive())
        data.set.name.or.path
    else
        basename(data.set.name.or.path)
}

cleanMergedDataSetName <- function(merged.data.set.name, data.set.names)
{
    if (is.null(merged.data.set.name) ||
                                trimws(merged.data.set.name) == "")
        generateMergedDataSetName(data.set.names)
    else
    {
        result <- trimws(merged.data.set.name)
        if (!grepl("\\.sav$", merged.data.set.name))
            result <- paste0(result, ".sav")
        checkFileNameCharacters(result)
        result
    }
}

checkFileNameCharacters <- function(file.name)
{
    if (grepl("[<>:\"/\\\\\\|\\?\\*]", file.name))
        stop("The file name '", file.name, "' is invalid as file names ",
             "cannot contain the characters ",
             "'<', '>', ':', '\"', '/', '\\', '|', '?', '*'.")
}

extractVariableMetadata <- function(data.sets)
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
             lapply(data.set, attr, "labels")
         }),
         variable.types = lapply(data.sets, function(data.set) {
             vapply(data.set, variableType, character(1))
         }),
         data.set.names = names(data.sets),
         n.data.sets = length(data.sets))
}

extractMergedVariableMetadata <- function(merged.data.set)
{
    list(variable.names = names(merged.data.set),
         variable.labels = vapply(merged.data.set, attr, character(1), "label"),
         variable.categories = lapply(merged.data.set, attr, "labels"),
         variable.types = vapply(merged.data.set, variableType, character(1)),
         n.variables = length(merged.data.set))
}

matchVariables <- function(variable.metadata, match.by, min.match.percentage,
                           manual.matches, variables.to.omit, data.sets)
{
    # matched.names is a matrix where the columns correspond to the data
    # sets and each row contains the names of variables that have been matched
    # together. NA is used if a variable is absent in a match.
    matched.names <- matrix(nrow = 0, ncol = variable.metadata$n.data.sets)
    matched.names <- applyManualMatches(matched.names,
                                             variable.metadata,
                                             manual.matches,
                                             variables.to.omit)
    checkVariablesToOmit(variable.metadata, variables.to.omit)

    if (match.by == "Variable names")
        matched.names <- matchVariableNames(matched.names,
                                            variable.metadata,
                                            variables.to.omit)
    else if (match.by == "Variable labels")
    {
        matched.names <- matchVariableLabels(matched.names,
                                                  variable.metadata,
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

applyManualMatches <- function(matched.names, variable.metadata,
                               manual.matches, variables.to.omit)
{
    if (is.null(manual.matches))
        return (matched.names)

    length(variable.metadata$variable.names)
    for (match.text in manual.matches)
    {
        manual.matched.names <- parseManualMatchText(match.text,
                                                       variable.metadata,
                                                       variables.to.omit)
        matched.names <- rbind(matched.names,
                                    manual.matched.names)
    }
    checkMatchForDuplication(matched.names, manual.matches)

    matched.names
}

# Parses a string of comma-separated variable names to be manually matched
# and returns a list of character vectors containing matched variable names
parseManualMatchText <- function(manual.match.text, variable.metadata,
                                 variables.to.omit)
{
    n.data.sets <- variable.metadata$n.data.sets

    split.text <- trimws(strsplit(manual.match.text, ",")[[1]])
    if (length(split.text) < n.data.sets)
        split.text <- c(split.text, rep("", n.data.sets - length(split.text)))

    if (sum(split.text != "") < 2)
        stop("The manual match input '", manual.match.text, "' is invalid as ",
             "it needs to contain two or more variables.")

    parsed.names <- lapply(seq_len(n.data.sets), function(i) {
        t <- split.text[i]
        if (t != "")
            parseNameRangeText(t, variable.metadata$variable.names[[i]],
                               variable.metadata$data.set.names[i])
        else
            NULL
    })

    n.vars <- vapply(parsed.names, length, integer(1))
    if (length(unique(n.vars[n.vars > 0])) > 1)
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

    matrix(unlist(parsed.names), ncol = n.data.sets)
}

parseNameRangeText <- function(input.text, variable.names, data.set.name)
{
    msg <- paste0("The input range '", input.text,
                  "' could not be recognized. It needs to contain the ",
                  "start and end variable names separated by a dash (-).")

    dash.ind <- which(strsplit(input.text, "")[[1]] == "-")
    if (length(dash.ind) == 0)
    {
        if (!(input.text %in% variable.names))
            stop("The variable '", input.text,
                 "' could not be found in the data set '", data.set.name, "'.")
        return(input.text)
    }
    else if (length(dash.ind) == 1)
    {
        start.variable <- trimws(substr(input.text, 1, dash.ind - 1))
        end.variable <- trimws(substr(input.text, dash.ind + 1, nchar(input.text)))

        if (start.variable == "" || end.variable == "")
            stop(msg)

        start.ind <- which(variable.names == start.variable)
        end.ind <- which(variable.names == end.variable)
        if (length(start.ind) == 0)
            stop("The variable '", start.variable, "' from the input range '",
                 input.text, "' could not be found in the data set '",
                 data.set.name, "'.")
        if (length(end.ind) == 0)
            stop("The variable '", end.variable, "' from the input range '",
                 input.text, "' could not be found in the data set '",
                 data.set.name, "'.")

        return(variable.names[start.ind:end.ind])
    }
    else
        stop(msg)
}

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

checkVariablesToOmit <- function(variable.metadata, variables.to.omit)
{
    if (is.null(variables.to.omit))
        return()

    if (length(variables.to.omit) != variable.metadata$n.data.sets)
        stop("variables.to.omit must be specified as a list whose elements ",
             "are the variables to omit in the corresponding data sets.")

    for (i in seq_len(variable.metadata$n.data.sets))
    {
        var.names <- variable.metadata$variable.names[[i]]
        ind <- which(!(variables.to.omit[[i]] %in% var.names))
        if (length(ind) > 0)
        {
            stop("The following variable(s) were specified to be omitted but ",
                 "could not be found in the data set ",
                 variable.metadata$data.set.names[i], ": ",
                 paste0(variables.to.omit[[i]][ind], collapse = ", "), ".")
        }
    }
}

matchVariableNames <- function(matched.names, variable.metadata,
                               variables.to.omit)
{
    remaining.vars.names <- remainingVarsNames(matched.names, variable.metadata,
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

remainingVarsNames <- function(matched.names, variable.metadata,
                               variables.to.omit)
{
    lapply(seq_len(variable.metadata$n.data.sets), function(i) {
        excluded.variables <- matched.names[, i]
        if (!is.null(variables.to.omit))
            excluded.variables <- c(excluded.variables, variables.to.omit[[i]])
        setdiff(variable.metadata$variable.names[[i]], excluded.variables)
    })
}

matchVariableLabels <- function(matched.names, variable.metadata,
                                variables.to.omit)
{

}

matchVariableAndValueLabels <- function(variable.metadata)
{

}

# Split variable match if variables have different types
# and type conversion is not possible
unmatchVariablesOfDifferentTypes <- function(matched.names, data.sets)
{
    n.data.sets <- length(data.sets)
    result <- matrix(nrow = 0, ncol = n.data.sets)
    for (i in seq_len(nrow(matched.names)))
    {
        ind <- which(!is.na(matched.names[i, ]))
        v.list <- vector(mode = "list", length = n.data.sets)
        v.list[ind] <- lapply(ind, function(j) data.sets[[j]][[matched.names[i, j]]])

        # Only one variable in the row
        if (length(ind) == 1)
        {
            result <- rbind(result, matched.names[i, ])
            next
        }

        v.types <- vapply(v.list[ind], variableType, character(1))

        # All variables have the same type
        if (length(unique(v.types)) == 1)
        {
            result <- rbind(result, matched.names[i, ])
            next
        }

        cat.types <- c("Categorical", "Categorical with string values")

        repeat
        {
            merge.ind <- integer(0)

            # Text to Date/Time if possible
            if (any(v.types %in% "Date/Time") && any(v.types %in% "Text"))
            {
                text.ind <- ind[v.types == "Text"]
                parsable.ind <- text.ind[vapply(text.ind, function(j) {
                    isParsableAsDateTime(v.list[[j]])
                }, logical(1))]
                merge.ind <- c(ind[v.types == "Date/Time"], parsable.ind)
            }
            # Text to Duration if possible
            else if (any(v.types %in% "Duration") && any(v.types %in% "Text"))
            {
                text.ind <- ind[v.types == "Text"]
                parsable.ind <- text.ind[vapply(text.ind, function(j) {
                    isParsableAsDiffTime(v.list[[j]])
                }, logical(1))]
                merge.ind <- c(ind[v.types == "Duration"], parsable.ind)
            }
            # Numeric or text to categorical if their values are present in
            # category values or labels
            else if (any(v.types %in% cat.types))
            {
                cat.ind <- ind[v.types %in% cat.types]

                all.category.values <- unlist(lapply(cat.ind, function (j) {
                    as.character(attr(v.list[[j]], "labels", exact = TRUE))
                }))
                all.category.labels <- unlist(lapply(cat.ind, function (j) {
                    names(attr(v.list[[j]], "labels", exact = TRUE))
                }))

                num.ind <- ind[v.types == "Numeric"]
                num.merge.ind <- num.ind[vapply(num.ind, function(j) {
                    vals <- unique(v.list[[j]])
                    vals <- vals[!is.na(vals)]
                    all(vals %in% all.category.values)
                }, logical(1))]

                text.ind <- ind[v.types == "Text"]
                text.merge.ind <- text.ind[vapply(text.ind, function(j) {
                    vals <- unique(v.list[[j]])
                    vals <- vals[!isMissingValue(vals)]
                    all(vals %in% all.category.values) ||
                        all(vals %in% all.category.labels)
                }, logical(1))]

                merge.ind <- c(cat.ind, num.merge.ind, text.merge.ind)
            }
            # Text to numeric if possible
            else if (any(v.types %in% "Numeric") && any(v.types %in% "Text"))
            {
                text.ind <- ind[v.types == "Text"]
                parsable.ind <- text.ind[vapply(text.ind, function(j) {
                    isParsableAsNumeric(v.list[[j]])
                }, logical(1))]
                merge.ind <- c(ind[v.types == "Numeric"], parsable.ind)
            }
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
        }
    }
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
mergeMap <- function(matched.names, variable.metadata,
                     prioritize.early.data.sets)
{
    unordered.merged.names <- mergedNamesFromMatchedNames(matched.names,
                                                          prioritize.early.data.sets)
    merged.names <- orderMergedNames(unordered.merged.names,
                                     matched.names,
                                     variable.metadata,
                                     prioritize.early.data.sets)
    input.names <- orderMatchedNames(matched.names,
                                     unordered.merged.names,
                                     merged.names)
    list(input.names = input.names,
         merged.names = merged.names,
         deduplicated.names = attr(unordered.merged.names, "deduplicated.names"))
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
        function(nms) nms[!is.na(nms)][1]
    else
        function(nms) rev(nms[!is.na(nms)])[1]

    result <- apply(matched.names, 1, .select.name)

    # As a result of calling unmatchVariablesOfDifferentTypes, two variables
    # with the same name from different data sets could not be merged if they
    # have incompatible types, and therefore become two separate variables in
    # the merged data set. We need to rename one of these variables in the
    # merged data set as names need to be unique.
    deduplicated.names <- matrix(nrow = 0, ncol = 2)
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
                    deduplicated.names <- rbind(deduplicated.names,
                                                c(base.name, nm))
                    break
                }
                else
                    j <- j + 1

            }
        }
    }

    attr(result, "deduplicated.names") <- deduplicated.names
    result
}

# Produce an ordering of the matched variables based on the order if the
# variables in the data set
orderMergedNames <- function(unordered.merged.names, matched.names,
                             variable.metadata, prioritize.early.data.sets)
{
    n.data.sets <- variable.metadata$n.data.sets
    names.list <- lapply(seq_len(n.data.sets), function(i) {
        ind <- match(variable.metadata$variable.names[[i]], matched.names[, i])
        unordered.merged.names[ind[!is.na(ind)]]
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
#
# The algorithm works by first considering the first names in each list
# element. A matrix is constructed where each row corresponds to the
# ranks of a first name in the list elements, with the ranks sorted in
# ascending numeric order (descending rank).
#
# The rows of the matrix are then sorted by the first column, using subsequent
# columns to break ties. The first sorted row corresponds to the name
# which is chosen to be first name in the output vector. The chosen name is
# removed from the names.list and the process is repeated to chose the second
# name in the output until the names.list is exhausted of all elements.
#
# When constructing a row, if a first name does not appear in a list element,
# it is given a value equal to the worst rank in the row. This means that such
# a name would have precedence over other names that are not missing with a
# worse rank. But if it is tied with names with the same worst rank, the tie
# will be broken by the order of the lists of the names.
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

        first.names <- vapply(names.list, `[`, character(1), 1)

        name.ranks.matrix <- t(vapply(seq_along(names.list), function(i) {
            sorted <- sort(vapply(names.list, function(nms) {
                min(which(nms == first.names[i]), Inf)
            }, numeric(1)))
            sorted[sorted == Inf] <- max(sorted[sorted < Inf])
            sorted
        }, numeric(length(names.list))))

        selected.name <- first.names[do.call(order, data.frame(name.ranks.matrix))[1]]
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

    result <- if ("Categorical" %in% v.types)
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
                                    merged.categories, map)
            merged.categories <- output$merged.categories
            map <- output$map
        }
        if (nrow(map) > 0)
            value.map[[i]] <- map
    }

    n.data.sets <- length(data.sets)
    result <- unlist(lapply(seq_len(n.data.sets), function (i) {
        v <- var.list[[i]]
        if (is.null(v))
            rep(NA, nrow(data.sets[[i]]))
        else if (v.types[i] == "Text")
        {
            is.missing <- isMissingValue(v)
            unique.v <- unique(v[!is.missing])
            if (string.values &&
                all(unique.v %in% merged.categories)) # text are category (string) values
            {
                result <- v
                result[is.missing] <- NA
                result
            }
            else if (!string.values &&
                     all(unique.v %in% as.character(merged.categories))) # text are category (numeric) values
            {
                result <- suppressWarnings(as.numeric(v))
                result[is.missing] <- NA
                result
            }
            else # text are category labels
            {
                result <- numeric(length(v))
                for (text.val in unique.v)
                    result[text.val == v] <- merged.categories[text.val == names(merged.categories)]
                result
            }
        }
        else if (v.types[i] == "Numeric")
        {
            if (string.values)
                as.character(v)
            else
                v
        }
        else
            remapValuesInVariable(v, value.map[[i]])
    }))
    attr(result, "labels") <- merged.categories
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
mergeCategory <- function(category.value, category.label, merged.categories, map)
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
        unlist(lapply(seq_len(n.data.sets), function (i) {
            v <- var.list[[i]]
            if (is.null(v))
                rep(NA, nrow(data.sets[[i]]))
            else if (v.types[i] == "Text")
            {
                missing.ind <- isMissingValue(v)
                result <- rep(NA, length(v))
                result[!missing.ind] <- parser(v[!missing.ind])
                result
            }
            else
                v
        }))
    }

    # Convert Text to Date/Time
    if (setequal(unique(v.types), c("Date/Time", "Text")))
        .convertTextVar(AsDateTime)
    else if (setequal(unique(v.types), c("Duration", "Text")))
        .convertTextVar(as.difftime)
    else if (setequal(unique(v.types), c("Numeric", "Text")))
        .convertTextVar(as.numeric)
    else # all same type
    {
        unlist(lapply(seq_len(n.data.sets), function (i) {
            v <- var.list[[i]]
            if (!is.null(v))
                v
            else
                rep(NA, nrow(data.sets[[i]]))
        }))
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

outputForMergeDataSetsByCase <- function(merged.data.set, variable.metadata,
                                         merge.map,
                                         include.merged.data.set.in.output,
                                         merged.data.set.name)
{
    result <- list()
    if (include.merged.data.set.in.output)
        result$merged.data.set <- merged.data.set

    result$variable.metadata <- variable.metadata
    result$merged.variable.metadata <- extractMergedVariableMetadata(merged.data.set)
    result$merge.map <- merge.map
    result$merged.data.set.name <- extractDataSetName(merged.data.set.name)
    result$omitted.variables <- omittedVariables(variable.metadata, merge.map)
    class(result) <- "MergeDataSetByCase"
    result
}

omittedVariables <- function(variable.metadata, merge.map)
{
    lapply(seq_len(variable.metadata$n.data.sets), function(i) {
        nms <- variable.metadata$variable.names[[i]]
        ind <- !(nms %in% merge.map$input.names[, i])
        nms[ind]
    })
}

#' @importFrom flipFormat DataSetMergingWidget
#' @export
print.MergeDataSetByCase <- function(x, ...)
{
    DataSetMergingWidget(x$variable.metadata,
                         x$merged.variable.metadata,
                         x$merge.map,
                         x$merged.data.set.name,
                         x$omitted.variables)
}
