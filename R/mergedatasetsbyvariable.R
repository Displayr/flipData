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
#' @param id.variables A character vector of ID variable names corresponding
#'  to each data set. NULL if no ID variables supplied.
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
#' @examples
#' path <- c(system.file("examples", "cola15.sav", package = "flipData"),
#'           system.file("examples", "Cola16.sav", package = "flipData"))
#' print(MergeDataSetsByVariable(path, id.variables = c("Attr1","PartyID")))
#' @export
MergeDataSetsByVariable <- function(data.set.names,
                                    merged.data.set.name = NULL,
                                    id.variables = NULL,
                                    include.or.omit.variables = rep("Include all variables except those manually omitted", length(data.set.names)),
                                    variables.to.include.or.omit = NULL,
                                    only.keep.cases.matched.to.all.data.sets = FALSE,
                                    include.merged.data.set.in.output = FALSE)
{
    data.sets <- readDataSets(data.set.names, 2)
    input.data.sets.metadata <- metadataFromDataSets(data.sets)

    matched.cases <- matchCases(input.data.sets.metadata, id.variables,
                                data.sets,
                                only.keep.cases.matched.to.all.data.sets)
    merged.data.set.var.names <- mergedDataSetVariableNames(input.data.sets.metadata,
                                                            include.or.omit.variables,
                                                            variables.to.include.or.omit,
                                                            matched.cases)
    merged.data.set <- mergedDataSetByVariable(data.sets, matched.cases,
                                               merged.data.set.var.names,
                                               input.data.sets.metadata)
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

# Returns the matched.cases matrix (number of merged cases * number of input data sets)
# containing the input case indices in the merged data set
matchCases <- function(input.data.sets.metadata, id.variables,
                       data.sets, only.keep.cases.matched.to.all.data.sets)
{
    if (!is.null(id.variables))
        matchCasesWithIDVariables(input.data.sets.metadata, id.variables,
                                  data.sets,
                                  only.keep.cases.matched.to.all.data.sets)
    else
        matchCasesWithoutIDVariables(input.data.sets.metadata)
}

# Returns the matched.cases matrix (number of merged cases * number of input data sets)
# containing the input case indices in the merged data set, after merging
# cases by ID variables
matchCasesWithIDVariables <- function(input.data.sets.metadata, id.variables,
                                      data.sets,
                                      only.keep.cases.matched.to.all.data.sets)
{
    id.var.names <- parseIDVariables(id.variables, input.data.sets.metadata)
    n.data.sets <- input.data.sets.metadata$n.data.sets
    n.cases <- input.data.sets.metadata$n.cases

    id.var.types <- vapply(seq_len(n.data.sets), function(i) {
        variableType(data.sets[[i]][[id.var.names[i]]])
    }, character(1))

    merged.id.var.type <- mergedIDVariableType(id.var.types)

    ids.list <- lapply(seq_len(n.data.sets), function(i) {
        ids <- data.sets[[i]][[id.var.names[i]]]

        if (id.var.types[i] != merged.id.var.type)
            ids <- convertIDVariableType(ids, id.var.types[i],
                                         merged.id.var.type)

        if (all(is.na(ids)))
            stop("The id variable '", id.var.names[i], "' from data set ", i,
                 " does not contain any non-missing IDs.")
        ids
    })

    non.missing.ids.list <- lapply(ids.list, removeNA)

    # Warn if no overlap exists
    if (n.data.sets == 2)
    {
        if (all(!(non.missing.ids.list[[1]] %in% non.missing.ids.list[[2]])))
            warning("There are no common IDs between the two input data sets. ",
                    "Ensure that the ID variable names have been correctly specified.")
    }
    else # n.data.sets > 2
    {
        for (i in seq_len(n.data.sets))
        {
            if (all(!(non.missing.ids.list[[i]] %in% unlist(non.missing.ids.list[-i]))))
                warning("The IDs in data set ", i, " from variable '",
                        id.var.names[i],
                        "' are not present in the other data sets. ",
                        "Ensure that the ID variable names have been correctly specified.")
        }
    }

    # Merge ID values
    unique.ids <- removeNA(unique(unlist(non.missing.ids.list)))
    merged.ids <- NULL
    for (unique.id in unique.ids)
    {
        id.frequency <- vapply(non.missing.ids.list, function(ids) {
            sum(ids == unique.id)
        }, integer(1))

        # Check that there are no IDs that are duplicated in more than one data set
        if (sum(id.frequency > 1) > 1)
            stop("The data sets cannot be merged by the specified ID variables as the ID '",
                 unique.id, "' is duplicated in multiple data sets.")

        merged.ids <- c(merged.ids, rep(unique.id, max(id.frequency)))
    }

    result <- matrix(NA_integer_, nrow = length(merged.ids), ncol = n.data.sets)
    for (id in unique.ids)
    {
        merged.id.ind <- which(id == merged.ids)
        for (i in seq_len(n.data.sets))
        {
            id.ind <- which(id == ids.list[[i]])
            if (length(id.ind) == 0)
                next

            result[merged.id.ind, i] <- id.ind
        }
    }

    if (only.keep.cases.matched.to.all.data.sets)
    {
        is.incomplete.case <- rowSums(is.na(result)) > 0
        if (all(is.incomplete.case))
            stop("The merged data set has no cases as there are no IDs that appear in all data sets. ",
                 "Ensure that the ID variable names have been correctly specified.")
        result <- result[!is.incomplete.case, , drop = FALSE]
        merged.ids <- merged.ids[!is.incomplete.case]
    }

    attr(result, "id.variable.names") <- id.var.names
    attr(result, "merged.ids") <- merged.ids
    result
}

# Returns a vector containing ID variable names corresponding to each data set.
# If an ID variable is not found for a data set, the element will be NA.
parseIDVariables <- function(id.variables, input.data.sets.metadata)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    v.names <- input.data.sets.metadata$variable.names

    for (i in seq_len(n.data.sets))
    {
        t <- id.variables[i]
        if (!(t %in% v.names[[i]]))
            variableNotFoundError(t, i)
    }
    id.variables
}

# The variable type of the merged ID given the input variable types
mergedIDVariableType <- function(id.variable.types)
{
    if (allIdentical(id.variable.types))
        id.variable.types[1]
    else if (all(isDateType(id.variable.types)))
        "Date/Time"
    else # Combination of 2 or more variable types (except "Date" + "Date/Time")
        "Text"
}

# Convert ID variable to have type merged.id.variable.type
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

# Returns the matched.cases matrix (number of merged cases * number of input data sets)
# containing the input case indices in the merged data set, after merging
# cases by simply joining cases side-by-side (no matching of IDs)
matchCasesWithoutIDVariables <- function(input.data.sets.metadata)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    n.cases <- input.data.sets.metadata$n.cases

    if (!allIdentical(n.cases))
        stop("The data sets could not be combined without ID variables (side-by-side, no matching) as they have differing numbers of cases. ",
             "To combine them, ID variables need to be specified.")

    matrix(rep(seq_len(n.cases[1]), n.data.sets), ncol = n.data.sets)
}

# The names of the variables in the merged data set
mergedDataSetVariableNames <- function(input.data.sets.metadata,
                                       include.or.omit.variables,
                                       variables.to.include.or.omit,
                                       matched.cases)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    v.names <- input.data.sets.metadata$variable.names
    id.var.names <- attr(matched.cases, "id.variable.names")

    v.names.to.include.or.omit <-  lapply(seq_len(n.data.sets), function(i) {
        parseInputVariableTextForDataSet(variables.to.include.or.omit[[i]],
                                         v.names[[i]], i)
    })

    input.var.names <- rep(list(character(0)), n.data.sets)
    omitted.var.names <- rep(list(character(0)), n.data.sets)
    merged.id.var.name <- NA_character_

    for (i in seq_len(n.data.sets))
    {
        if (include.or.omit.variables[i] == "Only include manually specified variables")
        {
            input.var.names[[i]] <- v.names.to.include.or.omit[[i]]
            omitted.var.names[[i]] <- setdiff(v.names[[i]],
                                              v.names.to.include.or.omit[[i]])
        }
        else # include.or.omit.variables[i] == "Include all variables except those manually omitted"
        {
            input.var.names[[i]] <- setdiff(v.names[[i]],
                                            v.names.to.include.or.omit[[i]])
            omitted.var.names[[i]] <- v.names.to.include.or.omit[[i]]
        }

        # Keep ID variable from first data set
        if (!is.null(id.var.names))
        {
            if (i == 1)
            {
                input.var.names[[i]] <- union(input.var.names[[i]],
                                              id.var.names[i])
                merged.id.var.name <- id.var.names[i]
            }
            else if (i > 1)
                input.var.names[[i]] <- setdiff(input.var.names[[i]],
                                                id.var.names[i])
        }

        if (length(input.var.names[[i]]) == 0)
            stop("All variables in data set ", i, "were specified to be omitted. ",
                 "Ensure that the variables to be omitted have been correctly specified.")

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

# Order a character vector of variable names according to their order in their
# data set
orderVariablesUsingInputDataSet <- function(var.names.to.order,
                                            data.set.var.names)
{
    var.names.to.order[order(match(var.names.to.order, data.set.var.names))]
}

# Parse character vector of comma-separated variable names
parseInputVariableTextForDataSet <- function(input.text,
                                             data.set.variable.names,
                                             data.set.index)
{
    split.text <- unlist(lapply(input.text, splitByComma),
                         use.names = FALSE)

    parsed.names <- character(0)
    for (j in seq_along(split.text))
    {
        t <- split.text[j]
        dash.ind <- match("-", strsplit(t, "")[[1]])
        if (is.na(dash.ind)) # single variable (not range)
        {
            if (!(t %in% data.set.variable.names))
                variableNotFoundError(t, data.set.index)
            parsed.names <- union(parsed.names, t)
        }
        else # range of variables
        {
            range.start <- trimws(substr(t, 1, dash.ind - 1))
            range.end <- trimws(substr(t, dash.ind + 1, nchar(t)))
            range.var.names <- variablesFromRange(data.set.variable.names,
                                                  range.start,
                                                  range.end,
                                                  data.set.index, t)
            parsed.names <- union(parsed.names, range.var.names)
        }
    }
    parsed.names
}

# TODO: to be moved to merging and stacking utilities, removing the copy in merging by case
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

# TODO: to be moved to merging and stacking utilities, removing the copy in merging by case
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

# Create the merged data set (as a data frame) by merging by variable
mergedDataSetByVariable <- function(data.sets, matched.cases,
                                    merged.data.set.variable.names,
                                    input.data.sets.metadata)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    n.merged.cases <- nrow(matched.cases)
    input.variable.names <- attr(merged.data.set.variable.names,
                                 "input.variable.names")
    id.variable.names <- attr(matched.cases, "id.variable.names")
    merged.id.var.name <- attr(merged.data.set.variable.names,
                               "merged.id.variable.name")
    merged.ids <- attr(matched.cases, "merged.ids")

    merged.data.set.variables <- vector(mode = "list", length = 0)
    merged.data.set.size <- 0

    j <- 1
    for (i in seq_len(n.data.sets))
    {
        for (nm in input.variable.names[[i]])
        {
            if (!is.null(merged.id.var.name) && i == 1 &&
                nm == merged.id.var.name) # ID variable
            {
                merged.var <- merged.ids
                attr(merged.var, "label") <- attr(data.sets[[i]][[nm]], "label")
            }
            else # Non-ID variable
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

# Character vector of the first ID values from each ID variable
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
            as.character(removeNA(v)[1])
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
