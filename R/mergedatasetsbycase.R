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
#' @importFrom verbs Sum
#' @export
MergeDataSetsByCase <- function(data.set.names,
                                merged.data.set.name = NULL,
                                match.by = c("Automatic",
                                             "All",
                                             "Variable names",
                                             "Variable labels",
                                             "Variable and value labels"),
                                min.match.percentage = 100,
                                manual.matches = NULL,
                                variables.to.omit = NULL,
                                include.merged.data.set.in.output = FALSE)
{
    data.sets <- readDataSets(data.set.names)
    variable.metadata <- extractVariableMetadata(data.sets)
    matched.names <- matchVariables(variable.metadata, match.by,
                                         min.match.percentage, manual.matches,
                                         variables.to.omit)
    merged.data.set <- mergeDataSets(matched.names, variable.metadata,
                                     data.sets)
    if (!is.null(merged.data.set.name))
        writeMergedDataSet(merged.data.set, merged.data.set.name)
    outputForMergeDataSetsByCase(merged.data.set, variable.metadata,
                                 matched.variables,
                                 include.merged.data.set.in.output)
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

# Need to ensure any new names we generate are valid for sav files

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
writeMergedDataSet <- function(merged.data.set, merged.data.set.name)
{
    if (canAccessDisplayrCloudDrive())
        QSaveData(merged.data.set, merged.data.set.name)
    else
        write_sav(merged.data.set, merged.data.set.name)
}

extractVariableMetadata <- function(data.sets)
{
    list(variable.names = lapply(data.sets, names),
         variable.labels = lapply(data.sets, attr, "label"),
         value.labels = lapply(data.sets, attr, "labels"),
         data.set.names = names(data.sets),
         n.data.sets = length(data.sets))
}

matchVariables <- function(variable.metadata, match.by, min.match.percentage,
                           manual.matches, variables.to.omit)
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

mergeDataSets <- function(matched.names, variable.metadata, data.sets)
{

    final.names <- finalNamesFromMatchedNames(matched.names)
    ordered.final.names <- orderFinalNames(matched.names, final.names,
                                           variable.metadata)
    ordered.matched.names <- orderMatchedNames(matched.names, final.names,
                                               ordered.final.names)

    n.data.set.cases <- vapply(data.sets, nrow, integer(1))
    n.cases <- sum(n.data.set.cases)

    result <- data.frame(lapply(seq_len(nrow(ordered.matched.names)), function(i) {
        compositeVariable(ordered.matched.names[i, ], data.sets)
    }))

    names(result) <- ordered.final.names

    result[["mergesrc"]] <- mergeSrc(n.data.set.cases)

    result
}

# Final names are the names shown in the merged data set.
# They are obtained from the latest non-missing name in each row of
# matched.names.
finalNamesFromMatchedNames <- function(matched.names)
{
    apply(matched.names, 1, function(nms) {
        rev(nms[!is.na(nms)])[1]
    })
}

# Produce an ordering of the matched variables based on the order if the
# variables in the data set
orderFinalNames <- function(matched.names, final.names, variable.metadata)
{
    n.data.sets <- variable.metadata$n.data.sets
    names.list <- lapply(seq_len(n.data.sets), function(i) {
        ind <- match(variable.metadata$variable.names[[i]], matched.names[, i])
        final.names[ind[!is.na(ind)]]
    })

    # reverse names.list to prioritize later data sets
    mergeNamesListRespectingOrder(rev(names.list))
}

# Order the rows in the matched names matrix according to the ordered.final.names
orderMatchedNames <- function(matched.names, final.names, ordered.final.names)
{
    n.data.sets <- ncol(matched.names)
    t(vapply(ordered.final.names, function(nm) {
        matched.names[match(nm, final.names), ]
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
# columns to break ties. The first sorted row corresponds to the final name
# which is chosen to be first name in the output vector. The chosen name is
# removed from the names.list and the process is repeated to chose the second
# name in the output until the names.list is exhausted of all elements.
#
# When constructing a row, if a first name does not appear in a list element,
# it is given a value equal to the worst rank in the row. This means that such
# a name would have precedence over other names that are not missing with a
# worse rank. But if it is tied with names with the same worst rank, the tie
# will be broken by the order of the lists of the names.
mergeNamesListRespectingOrder <- function(names.list)
{
    merged.names <- character()
    while (TRUE)
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
compositeVariable <- function(variable.names, data.sets)
{
    n.data.sets <- length(data.sets)
    var.list <- lapply(seq_len(n.data.sets), function(i) {
         if(!is.na(variable.names[i]))
             data.sets[[i]][[variable.names[i]]]
        else
            NULL
    })
    v.type <- vapply(var.list, variableType, character(1))
    v.type <- unique(v.type[!is.na(v.type)])

    if (length(v.type) > 1)
        stop("The variables for ",
             paste0(paste0("'", unique(variable.names[!is.na(variable.names)]), "'"), collapse = ", "),
             " could not be combined as they have different types: ",
             paste0(v.type, collapse = ", "), ".")

    if (v.type == "Categorical")
    {
        output <- mergeCategories(var.list)
        result <- unlist(lapply(seq_len(n.data.sets), function (i) {
            v <- var.list[[i]]
            if (!is.null(v))
                remapValuesInVariable(v, output$value.mapping[[i]])
            else
                rep(NA, nrow(data.sets[[i]]))
        }))
        attr(result, "labels") <- output$merged.categories
        class(result) <- c(class(result), "haven_labelled")
    }
    else
    {
        result <- unlist(lapply(seq_len(n.data.sets), function (i) {
            v <- var.list[[i]]
            if (!is.null(v))
                v
            else
                rep(NA, nrow(data.sets[[i]]))
        }))
    }

    attr(result, "label") <- variableLabelFromDataSets(variable.names, data.sets)

    result
}

variableType <- function(variable)
{
    if (is.null(variable))
        NA_character_
    else if (!is.null(attr(variable, "labels")))
        "Categorical"
    else if (is.numeric(variable))
        "Numeric"
    else if (is.character(variable))
        "Text"
    else
        stop("Variable type not recognised")
}

# Merge categories from categorical variables
mergeCategories <- function(variable.list)
{
    categories.list <- lapply(variable.list, attr, "labels")

    # reverse indices to prioritize categories from later data sets
    indices <- rev(which(!vapply(categories.list, is.null, logical(1))))

    merged.categories <- categories.list[[indices[1]]]
    value.mapping <- vector("list", length = length(variable.list))

    for (i in indices[-1])
    {
        # 2-column matrix representing a remapping of values where the
        # 1st column contains the original value and the 2nd column contains
        # the new value
        map <- matrix(nrow = 0, ncol = 2)

        categories <- categories.list[[i]]
        for (lbl in names(categories))
        {
            if (lbl %in% names(merged.categories))
            {
                if (categories[lbl] != merged.categories[lbl]) # same label with different values
                {
                    map <- rbind(map, c(categories[lbl], merged.categories[lbl])) # use the value in merged.categories
                }
                # else: same label, same value, no action required
            }
            else
            {
                if (categories[lbl] %in% merged.categories) # different labels with same value
                {
                    merged.categories[lbl] <- max(merged.categories) + 1 # create new value for label
                    map <- rbind(map, c(categories[lbl], merged.categories[lbl])) # use the value in merged.categories
                }
                else # value and label not in merged.categories
                {
                    merged.categories[lbl] <- categories[lbl] # create new category in merged.categories
                }
            }
        }
        if (nrow(map) > 0)
            value.mapping[[i]] <- map
    }

    # We need to return value.mapping since it determines the new values to use
    # when creating the composite variable
    list(merged.categories = merged.categories, value.mapping = value.mapping)
}

remapValuesInVariable <- function(variable, map)
{
    result <- variable
    if (!is.null(map))
        for (i in seq_len(nrow(map)))
            result[variable == map[i, 1]] <- map[i, 2]
    result
}

variableLabelFromDataSets <- function(variable.names, data.sets)
{
    n.data.sets <- length(data.sets)
    for (i in rev(seq_along(data.sets))) # start from last data set
    {
        data.set <- data.sets[[i]]

        if (!is.na(variable.names[i]))
        {
            v <- data.set[[variable.names[i]]]
            lbl <- attr(v, "label")
            if (!is.null(lbl))
                return(attr(v, "label"))
        }
    }
    stop("Label could not be found for variable(s) ",
         paste0(paste0("'", unique(variable.names[!is.na(variable.names)]), "'"), collapse = ", "),
         " in the supplied data sets.")
}

mergeSrc <- function( n.data.set.cases)
{
    result <- rep(seq_along(n.data.set.cases), n.data.set.cases)
    attr(result, "label") <- "Source of cases"
    result
}

outputForMergeDataSetsByCase <- function(merged.data.set, variable.metadata,
                                         matched.variables,
                                         include.merged.data.set.in.output)
{
    result <- list()
    if (include.merged.data.set.in.output)
        result$merged.data.set <- merged.data.set
    # result$match.table <- matchTable(matched.variables)
    # result$omitted.variables <- omittedVariables(variable.metadata,
    #                                              matched.variables)
    class(result) <- "MergeDataSetByCase"
    result
}

matchTable <- function(matched.variables)
{

}

# omittedVariables <- function(variable.metadata, matched.variables)
# {
#
# }
