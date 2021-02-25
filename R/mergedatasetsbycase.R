#' @title Merge Data Sets by Case
#' @description Merges multiple data sets by case where the data sets contain
#'  similar variables but different cases, e.g., data sets from different time
#'  periods.
#' @param data.set.names A character vector of names of data sets from the
#'  Displayr cloud drive to merge or file paths of local data sets.
#' @param match.by One of Automatic, All, Variable names, Variable labels,
#'  Variable and value labels
#' @param min.match.percentage To be decided, possibly a percentage.
#' @param manual.matches A character vector of comma-separated variable names
#'  indicating which variables are to be matched together, e.g.,
#'  "var_name_from_data_set_1, var_name_from_data_set_2, ..."
#' @param variables.to.omit A list of character vectors containing names of
#'   variables to omit from the merged data set. The vectors in the list
#'   correspond to the input data sets.
#' @export
MergeDataSetsByCase <- function(data.set.names,
                                match.by = c("Automatic",
                                             "All",
                                             "Variable names",
                                             "Variable labels",
                                             "Variable and value labels"),
                                min.match.percentage = 100,
                                manual.matches = NULL,
                                variables.to.omit = NULL)
{
    data.sets <- readDataSets(data.set.names)
    variable.metadata <- extractVariableMetadata(data.sets)
    matched.variables <- matchVariables(variable.metadata, match.by,
                                        min.match.percentage, manual.matches,
                                        variables.to.omit)
    merged.data.set <- mergeDataSet(matched.variables)
    writeMergedDataSet(merged.data.set)
    outputForMergeDataSetsByCase(variable.metadata, matched.variables)
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

#' @importFrom flipAPI QFileExists
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
    # matched.variables is a matrix where the columns correspond to the data
    # sets and each row contains the names of variables that have been matched
    # together. NA is used if a variable is absent in a match.
    matched.variables <- matrix(nrow = 0, ncol = variable.metadata$n.data.sets)
    matched.variables <- applyManualMatches(matched.variables,
                                            variable.metadata,
                                            manual.matches,
                                            variables.to.omit)
    checkVariablesToOmit(variable.metadata, variables.to.omit)

    if (match.by == "Variable names")
        matched.variables <- matchVariableNames(matched.variables,
                                                variable.metadata,
                                                variables.to.omit)
    else if (match.by == "Variable labels")
    {
        matched.variables <- matchVariableLabels(matched.variables,
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

    matched.variables
}

applyManualMatches <- function(matched.variables, variable.metadata,
                               manual.matches, variables.to.omit)
{
    if (is.null(manual.matches))
        return (matched.variables)

    length(variable.metadata$variable.names)
    for (match.text in manual.matches)
    {
        manual.match.variables <- parseManualMatchText(match.text,
                                                       variable.metadata,
                                                       variables.to.omit)
        matched.variables <- rbind(matched.variables, manual.match.variables)
    }
    checkMatchForDuplication(matched.variables, manual.matches)

    matched.variables
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
                     " or the variables to be omitted.")
            else if (sum(is.omitted) > 1)
                stop("The variable(s) ",
                     paste0(parsed.names[[i]][is.omitted], collapse = ", "),
                     " have been specified in both manual matches both and ",
                     "to be omitted. They need to be removed from the manual ",
                     "matches or the variables to be omitted.")
        }

    matrix(unlist(parsed.names), ncol = n.data.sets)
}

parseNameRangeText <- function(range.text, variable.names, data.set.name)
{
    msg <- paste0("The input range '", range.text,
                  "' could not be recognized. It needs to contain the ",
                  "start and end variable names separated by a dash (-).")

    dash.ind <- which(strsplit(range.text, "")[[1]] == "-")
    if (length(dash.ind) == 0)
    {
        # check that this is a valid variable?
        return(range.text)
    }
    else if (length(dash.ind) == 1)
    {
        start.variable <- substr(range.text, 1, dash.ind - 1)
        end.variable <- substr(range.text, dash.ind + 1, nchar(range.text))

        if (start.variable == "" || end.variable == "")
            stop(msg)

        start.ind <- which(variable.names == start.variable)
        end.ind <- which(variable.names == end.variable)
        if (length(start.ind) == 0)
            stop("The variable '", start.variable, "' from the input range '",
                 range.text, "' could not be found in the data set '",
                 data.set.name, "'.")
        if (length(end.ind) == 0)
            stop("The variable '", end.variable, "' from the input range '",
                 range.text, "' could not be found in the data set '",
                 data.set.name, "'.")

        return(variable.names[start.ind:end.ind])
    }
    else
        stop(msg)
}

checkMatchForDuplication <- function(matched.variables, match.source)
{
    n.data.sets <- ncol(matched.variables)
    for (i in seq_len(n.data.sets))
    {
        vars <- matched.variables[, i]
        var.table <- table(vars)
        if (any(var.table > 1))
        {
            duplicate.var <- name(var.table)[var.table > 1][1]
            duplicate.match.source <- match.sources[vars == duplicate.var]
            stop("The variable '", duplicate.var, "' has been specified in ",
                 "multiple manual match inputs: ",
                 paste0(duplicate.match.source, collapse = ", "),
                 ". Ensure that all variables are only specified in at most ",
                 "one manual match.")
        }
    }
}

checkVariablesToOmit <- function(variable.metadata, variables.to.omit)
{
    if (is.null(variables.to.omit))
        return()

    for (i in seq_len(variable.metadata$n.data.sets))
    {
        var.names <- variable.metadata$variable.names[[i]]
        ind <- which(!(variables.to.omit %in% var.names))
        if (length(ind) > 0)
        {
            stop("The following variable(s) were specified to be omitted but ",
                 "could not be found in the data set ",
                 variable.metadata$data.set.names[i], ": ",
                 paste0(var.names, collapse = ", "), ".")
        }
    }
}

matchVariableNames <- function(matched.variables, variable.metadata,
                               variables.to.omit)
{
    unmatched.var <- unmatchedVariables(matched.variables, variable.metadata,
                                        variables.to.omit)

    var.names <- variable.metadata$variable.names

    n.data.sets <- variable.metadata$n.data.sets

    for (i in seq_len(n.data.sets))
    {

    }

    matched.variables
}

unmatchedVariables <- function(matched.variables, variable.metadata,
                               variables.to.omit)
{
    unmatched.var <- variable.metadata$variable.names
    for (i in seq_len(variable.metadata$n.data.sets))
    {
        excluded.variables <- matched.variables[, i]
        if (!is.null(variables.to.omit))
            excluded.variables <- c(excluded.variables, variables.to.omit[[i]])
        variable.names[[i]] <- setdiff(variable.names[[i]], excluded.variables)
    }
    variable.names
}

matchVariableLabels <- function(matched.variables, variable.metadata,
                                variables.to.omit)
{

}

matchVariableAndValueLabels <- function(variable.metadata)
{

}

mergeDataSet <- function(matched.variables)
{

}

writeMergedDataSet <- function(merged.data.set)
{

}

outputForMergeDataSetsByCase <- function(variable.metadata, matched.variables)
{
    result <- list()
    result$match.table <- matchTable(matched.variables)
    result$omitted.variables <- omittedVariables(variable.metadata,
                                                 matched.variables)
    class(result) <- "MergeDataSetByCase"
    result
}

matchTable <- function(matched.variables)
{

}

omittedVariables <- function(variable.metadata, matched.variables)
{

}
