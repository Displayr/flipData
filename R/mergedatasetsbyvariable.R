#' @title Merge Data Sets by Variable
#' @description Merges multiple data sets by combining variables, matching
#'  cases either using ID variables or by simply joining data sets side-by-side.
#' @param data.set.names A character vector of names of data sets from the
#'  Displayr cloud drive to merge (if run from Displayr) or file paths of local
#'  data sets.
#' @param merged.data.set.name A character scalar of the name of the merged
#'  data set in the Displayr cloud drive (if run from Displayr) or the local
#'  file path of the merged data set.
#' @param id.variables A character vector of ID variable names corresponding
#'  to each data set. ID variables should generally contain unique IDs, but
#'  otherwise an ID can only be duplicated in at most one data set. The ID
#'  variable in the merged data set will use the name and label from the ID
#'  variable from the first input data set.
#'  NULL if ID variables are not used, in which case the input data sets are
#'  simply combined side-by-side, and the input data sets are required to have
#'  the same number of cases.
#' @param include.or.omit.variables A character vector where each element
#'  corresponds to an input data set, and indicates whether variables from the
#'  input data set are to be specified in the merged data set by specifying
#'  the variables to include ("Only include manually specified variables") or the
#'  variables to omit ("Include all variables except those manually omitted").
#' @param variables.to.include.or.omit A list of character vectors corresponding
#'  to each data set. Each element in a character vector contains comma-separated
#'  names of variables to include or omit (depending on the option for the data
#'  set in \code{include.or.omit.variables}). Ranges of variables can be specified
#'  by separating variable names by '-'. Wildcard matching of names is supported
#'  using the asterisk character '*'.
#' @param only.keep.cases.matched.to.all.data.sets A logical scalar which
#'  controls whether to only keep cases if they are present in all data sets,
#'  and discard the rest.
#' @param include.merged.data.set.in.output A logical scalar which controls
#'  whether to include the merged data set in the output object, which can be
#'  used for diagnostic purposes in R.
#' @return A list of class MergeDataSetByVariable with the following elements:
#' \itemize{
#'   \item \code{merged.data.set} If \code{include.merged.data.set.in.output},
#'   is TRUE, this is a data frame of the merged data set.
#'   \item \code{input.data.sets.metadata} A list containing metadata on the
#'     the input data sets such as variable names, labels etc. See the function
#'     \code{metadataFromDataSets} for more information.
#'   \item \code{merged.data.set.metadata} A list containing metadata on the
#'     the merged data set such as variable names, labels etc. See the function
#'     \code{metadataFromDataSet} for more information.
#'   \item \code{source.data.set.indices} An integer vector corresponding to the
#'   variables in the merged data set. Each element contains the index of the
#'   input data set from which the variable originated. The data set index
#'   for the ID variable will be 1 even though ID variables are present in all
#'   data sets when ID variables are specified.
#'   \item \code{omitted.variable.names.list} A list whose elements correspond
#'   to the input data sets. Each element contains the names of variables from a
#'   data set that were omitted from the merged data set.
#'   \item \code{merged.id.variable.name} A character scalar of the name of the
#'   ID variable in the merged data set. It is NULL if there is no ID variable.
#'   \item \code{id.variable.names} A character vector corresponding to the
#'   input data sets. Each element is an ID variable name from an input data set.
#'   \item \code{example.id.values} A character vector corresponding to the
#'   input data sets. Each element is an example ID value from an ID variable
#'   from an input data set.
#'   \item \code{is.saved.to.cloud} A logical scalar indicating whether the
#'   merged data set was saved to the Displayr cloud drive.
#' }
#' @examples
#' path <- c(system.file("examples", "cola15.sav", package = "flipData"),
#'           system.file("examples", "cola16.sav", package = "flipData"))
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
    # === Data dictionary ===
    # data.sets: A list of data frames, with each representing an input data set.
    # input.data.sets.metadata: A list of containing metadata on the input data
    #                           sets. See the function metadataFromDataSets.
    # matched.cases.matrix: An integer matrix whose rows correspond to the cases
    #                       in the merged data set and whose columns correspond
    #                       to the input data sets. Each row contains the input
    #                       data set case indices that map to a case in the
    #                       merged data set. Has the attributes id.variable.names
    #                       and merged.id.variable (see below).
    # id.variable.names: Character vector of the names of the ID variables from
    #                    each input data set.
    # merged.id.variable: A vector containing the values of the ID variable in the merged
    #                     data set, which was created by merging the ID
    #                     variables from the input data sets.
    # merged.data.set.var.names: A character vector containing the names of the
    #                            variables in the merged data set. Has the
    #                            attributes included.variable.names.list,
    #                            omitted.variable.names, merged.id.variable.name,
    #                            source.data.set.indices.
    # included.variable.names.list: A list of character vectors corresponding to
    #                               the input data sets. Each element contains the
    #                               names of the variables from an input data set
    #                               that will be present in the merged data set.
    # omitted.variable.names.list: A list whose elements correspond to the input
    #                              data sets. Each element contains the names of
    #                              variables from a data set that were omitted
    #                              from the merged data set.
    # merged.id.variable.name: A character scalar of the name of the ID
    #                          variable in the merged data set. It is NULL if
    #                          there is no ID variable.
    # source.data.set.indices: An integer vector corresponding to the variables
    #                          in the merged data set. Each element contains
    #                          the index of the input data set from which the
    #                          variable originated. The data set index for the
    #                          ID variable will be 1 even though ID variables
    #                          are present in all data sets when ID variables
    #                          are specified.
    # merged.data.set: A data frame representing the merged data set.

    data.sets <- readDataSets(data.set.names, 2)
    input.data.sets.metadata <- metadataFromDataSets(data.sets)

    matched.cases.matrix <- matchCases(input.data.sets.metadata, id.variables,
                                       data.sets,
                                       only.keep.cases.matched.to.all.data.sets)
    merged.data.set.var.names <- mergedDataSetVariableNames(input.data.sets.metadata,
                                                            include.or.omit.variables,
                                                            variables.to.include.or.omit,
                                                            matched.cases.matrix)
    merged.data.set <- doMergeByVariable(data.sets, matched.cases.matrix,
                                         merged.data.set.var.names,
                                         input.data.sets.metadata)
    merged.data.set.name <- correctDataSetName(merged.data.set.name,
                                               "Merged data set.sav")
    is.saved.to.cloud <- IsDisplayrCloudDriveAvailable()
    writeDataSet(merged.data.set, merged.data.set.name,
                 is.saved.to.cloud = is.saved.to.cloud)

    result <- list()
    if (include.merged.data.set.in.output)
        result$merged.data.set <- merged.data.set

    result$input.data.sets.metadata <- input.data.sets.metadata
    result$merged.data.set.metadata <- metadataFromDataSet(merged.data.set,
                                                           merged.data.set.name)
    result$source.data.set.indices <- attr(merged.data.set.var.names,
                                           "source.data.set.indices")
    result$omitted.variable.names.list <- attr(merged.data.set.var.names,
                                               "omitted.variable.names.list")
    result$merged.id.variable.name <- attr(merged.data.set.var.names,
                                           "merged.id.variable.name")
    result$id.variable.names <- attr(matched.cases.matrix, "id.variable.names")
    result$example.id.values <- exampleIDValues(result$id.variable.names,
                                                data.sets)
    result$is.saved.to.cloud <- is.saved.to.cloud
    class(result) <- "MergeDataSetByVariable"
    result
}

#' @param input.data.sets.metadata See data dictionary.
#' @param id.variables See documentation for id.variables in MergeDataSetsByVariable.
#' @param data.sets See data dictionary.
#' @param only.keep.cases.matched.to.all.data.sets See documentation for
#'  only.keep.cases.matched.to.all.data.sets in MergeDataSetsByVariable.
#' @return Returns matched.cases.matrix, see data dictionary.
#' @noRd
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

# See params for the function matchCases.
#' @return Returns the matched.cases matrix, see data dictionary.
#' @noRd
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

    # List whose elements correspond to the input data sets.
    # Each element contains the ID variable from a data set, after converting
    # its type to merged.id.var.type
    ids.list <- lapply(seq_len(n.data.sets), function(i) {
        ids <- data.sets[[i]][[id.var.names[i]]]
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
    unique.ids <- unique(unlist(non.missing.ids.list))
    merged.id.variable <- NULL
    for (unique.id in unique.ids)
    {
        id.frequency <- vapply(non.missing.ids.list, function(ids) {
            sum(ids == unique.id)
        }, integer(1))

        # Check that there are no IDs that are duplicated in more than one data set
        if (sum(id.frequency > 1) > 1)
            stop("The data sets cannot be merged by the specified ID variables as the ID '",
                 unique.id, "' is duplicated in multiple data sets.")

        merged.id.variable <- c(merged.id.variable,
                                rep(unique.id, max(id.frequency)))
    }

    result <- matrix(NA_integer_, nrow = length(merged.id.variable),
                     ncol = n.data.sets)
    for (id in unique.ids)
    {
        merged.id.ind <- which(id == merged.id.variable)
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
        merged.id.variable <- merged.id.variable[!is.incomplete.case]
    }

    attr(result, "id.variable.names") <- id.var.names
    attr(result, "merged.id.variable") <- merged.id.variable
    result
}

#' @param id.variables See documentation for id.variables in
#'  MergeDataSetsByVariable.
#' @param input.data.sets.metadata See data dictionary.
#' @return A character vector containing ID variable names corresponding to
#'  each data set.
#' @noRd
parseIDVariables <- function(id.variables, input.data.sets.metadata)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    v.names.list <- input.data.sets.metadata$variable.names.list

    for (i in seq_len(n.data.sets))
    {
        t <- id.variables[i]
        if (!(t %in% v.names.list[[i]]))
            throwVariableNotFoundError(t, i)
    }
    id.variables
}

#' @param id.variable.types A character vector of variable types
#'  (see function variableType) of the ID variables to be merged.
#' @return A character scalar of the variable type of the merged ID variable.
#' @noRd
mergedIDVariableType <- function(id.variable.types)
{
    if (allIdentical(id.variable.types))
        id.variable.types[1]
    else if (all(isDateType(id.variable.types)))
        DATE.TIME.VARIABLE.TYPE
    else # Combination of 2 or more variable types (except date and date/time types)
        TEXT.VARIABLE.TYPE
}

#' @description Convert ID variable to have type merged.id.variable.type
#' @param ids A vector representing the ID variable to be converted.
#' @param id.variable.type The variable type (see function variableType) of the
#'  ID variable to be converted.
#' @param merged.id.variable.type The variable type that the ID variable will
#'  be converted to.
#' @return A vector representing the converted ID variable.
#' @noRd
convertIDVariableType <- function(ids, id.variable.type,
                                  merged.id.variable.type)
{
    if (id.variable.type == merged.id.variable.type) # no conversion necessary
    {
        ids
    }
    else if (merged.id.variable.type == DATE.TIME.VARIABLE.TYPE &&
             id.variable.type == DATE.VARIABLE.TYPE)
    {
             AsDateTime(as.character(ids))
    }
    else # merged.id.variable.type == TEXT.VARIABLE.TYPE (final possibility, see mergedIDVariableType)
    {
        if (id.variable.type == CATEGORICAL.VARIABLE.TYPE)
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

#' @description Match cases side-by-side (no matching of IDs).
#' @param input.data.sets.metadata See data dictionary.
#' @return Returns the matched.cases matrix, see data dictionary.
#' @noRd
matchCasesWithoutIDVariables <- function(input.data.sets.metadata)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    n.cases <- input.data.sets.metadata$n.cases

    if (!allIdentical(n.cases))
        stop("The data sets could not be combined without ID variables (side-by-side, no matching) as they have differing numbers of cases. ",
             "To combine them, ID variables need to be specified.")

    matrix(rep(seq_len(n.cases[1]), n.data.sets), ncol = n.data.sets)
}

#' @param input.data.sets.metadata See data dictionary.
#' @param include.or.omit.variables See documentation for
#'  include.or.omit.variables in MergeDataSetsByVariable.
#' @param variables.to.include.or.omit See documentation for
#'  variables.to.include.or.omit in MergeDataSetsByVariable.
#' @param matched.cases See data dictionary.
#' @return Character vector of the names of the variables in the merged data set.
#' @noRd
mergedDataSetVariableNames <- function(input.data.sets.metadata,
                                       include.or.omit.variables,
                                       variables.to.include.or.omit,
                                       matched.cases)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    v.names.list <- input.data.sets.metadata$variable.names.list
    id.var.names <- attr(matched.cases, "id.variable.names")

    v.names.to.include.or.omit.list <- lapply(seq_len(n.data.sets), function(i) {
        parseInputVariableTextForDataSet(variables.to.include.or.omit[[i]],
                                         v.names.list[[i]], i)
    })

    included.variable.names.list <- rep(list(character(0)), n.data.sets)
    omitted.var.names.list <- rep(list(character(0)), n.data.sets)
    merged.id.var.name <- NA_character_

    for (data.set.ind in seq_len(n.data.sets))
    {
        if (include.or.omit.variables[data.set.ind] == "Only include manually specified variables")
        {
            included.variable.names.list[[data.set.ind]] <- v.names.to.include.or.omit.list[[data.set.ind]]
            omitted.var.names.list[[data.set.ind]] <- setdiff(v.names.list[[data.set.ind]],
                                                              v.names.to.include.or.omit.list[[data.set.ind]])
        }
        else # include.or.omit.variables[data.set.ind] == "Include all variables except those manually omitted"
        {
            included.variable.names.list[[data.set.ind]] <- setdiff(v.names.list[[data.set.ind]],
                                                            v.names.to.include.or.omit.list[[data.set.ind]])
            omitted.var.names.list[[data.set.ind]] <- v.names.to.include.or.omit.list[[data.set.ind]]
        }

        # Keep ID variable from first data set,  because they must be included regardless of v.names.to.include.or.omit.list
        if (!is.null(id.var.names))
        {
            if (data.set.ind == 1)
            {
                included.variable.names.list[[data.set.ind]] <- union(included.variable.names.list[[data.set.ind]],
                                                                      id.var.names[data.set.ind])
                merged.id.var.name <- id.var.names[data.set.ind]
            }
            else if (data.set.ind > 1)
                included.variable.names.list[[data.set.ind]] <- setdiff(included.variable.names.list[[data.set.ind]],
                                                                        id.var.names[data.set.ind])
        }

        if (length(included.variable.names.list[[data.set.ind]]) == 0)
            stop("All variables in data set ", data.set.ind, "were specified to be omitted. ",
                 "Ensure that the variables to be omitted have been correctly specified.")

        included.variable.names.list[[data.set.ind]] <- orderVariablesUsingInputDataSet(included.variable.names.list[[data.set.ind]],
                                                                                v.names.list[[data.set.ind]])
        omitted.var.names.list[[data.set.ind]] <- orderVariablesUsingInputDataSet(omitted.var.names.list[[data.set.ind]],
                                                                                  v.names.list[[data.set.ind]])
    }

    merged.data.set.var.names <- character(0)
    for (i in seq_len(n.data.sets))
        for (nm in included.variable.names.list[[i]])
            merged.data.set.var.names <- c(merged.data.set.var.names,
                                           uniqueName(nm, merged.data.set.var.names, delimiter = "_"))

    attr(merged.data.set.var.names, "included.variable.names.list") <- included.variable.names.list
    attr(merged.data.set.var.names, "omitted.variable.names.list") <- omitted.var.names.list

    if (!is.null(id.var.names))
    {
        merged.id.var.name <- merged.data.set.var.names[match(merged.id.var.name,
                                                              unlist(included.variable.names.list))]
        attr(merged.data.set.var.names, "merged.id.variable.name") <- merged.id.var.name
    }

    source.data.set.indices <- unlist(lapply(seq_along(included.variable.names.list), function(i) {
        rep(i, length(included.variable.names.list[[i]]))
    }))
    attr(merged.data.set.var.names, "source.data.set.indices") <- source.data.set.indices

    merged.data.set.var.names
}

#' @param var.names.to.order Character vector of variable names to be reordered.
#' @param data.set.var.names Character vector of variable names from a data set
#'  that will be used to order the names in var.names.to.order.
#' @return Character vector of the names in var.names.to.order reordered
#'  according to their order in data.set.var.names.
#' @noRd
orderVariablesUsingInputDataSet <- function(var.names.to.order,
                                            data.set.var.names)
{
    var.names.to.order[order(match(var.names.to.order, data.set.var.names))]
}

#' @param input.text Character vector containing comma-separated variable names
#'  or variable ranges (see documentation for variables.to.include.or.omit in
#'  MergeDataSetsByVariable).
#' @param data.set.variable.names Character vector of variable names from a
#'  data set. The variables in input.text are expected to be from this data set.
#' @param data.set.index Integer scalar of the index of the data set among the
#'  input data sets.
#' @return A character vector of the names of variables parsed from input text.
#' @noRd
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
        if (is.na(dash.ind)) # not range
        {
            if (!grepl("\\*", t)) # single variable, not wildcard
            {
                if (!(t %in% data.set.variable.names))
                    throwVariableNotFoundError(t, data.set.index)
                parsed.names <- union(parsed.names, t)
            }
            else # wildcard
            {
                nms <-  parseVariableWildcardForMerging(t,
                                                        data.set.variable.names,
                                                        data.set.index,
                                                        error.if.not.found = TRUE)
                parsed.names <- union(parsed.names, nms)
            }
        }
        else # range of variables
        {
            if (grepl("\\*", t))
                stop("The input '", t,
                     "' is invalid as wildcard characters are not supported for variable ranges.")

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

#' @param data.sets See data dictionary.
#' @param matched.cases.matrix See data dictionary.
#' @param merged.data.set.variable.names See data dictionary.
#' @param input.data.sets.metadata See data dictionary.
#' @return A data frame representing the merged data set.
#' @noRd
doMergeByVariable <- function(data.sets, matched.cases.matrix,
                              merged.data.set.variable.names,
                              input.data.sets.metadata)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    n.merged.cases <- nrow(matched.cases.matrix)
    included.variable.names.list <- attr(merged.data.set.variable.names,
                                         "included.variable.names.list")
    id.variable.names <- attr(matched.cases.matrix, "id.variable.names")
    merged.id.var.name <- attr(merged.data.set.variable.names,
                               "merged.id.variable.name")
    merged.id.variable <- attr(matched.cases.matrix, "merged.id.variable")

    merged.data.set.variables <- vector(mode = "list",
                                        length = length(merged.data.set.variable.names))
    merged.data.set.size <- 0

    j <- 1
    for (data.set.ind in seq_len(n.data.sets))
    {
        for (nm in included.variable.names.list[[data.set.ind]])
        {
            if (!is.null(merged.id.var.name) && data.set.ind == 1 &&
                nm == merged.id.var.name) # ID variable
            {
                merged.var <- merged.id.variable
                attr(merged.var, "label") <- attr(data.sets[[data.set.ind]][[nm]], "label")
            }
            else # Non-ID variable
            {
                input.var <- data.sets[[data.set.ind]][[nm]]
                non.missing.ind <- which(!is.na(matched.cases.matrix[, data.set.ind]))
                # Initialize with missing values of the same type as input.var
                merged.var <- rep(c(input.var[1], NA)[2], n.merged.cases)
                merged.var[non.missing.ind] <- input.var[matched.cases.matrix[non.missing.ind,
                                                                              data.set.ind]]

                if (isIntegerValued(merged.var))
                    merged.var <- as.integer(merged.var)
                v.type <- variableType(input.var)
                if (v.type == CATEGORICAL.VARIABLE.TYPE)
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

#' @param id.variable.names See data dictionary.
#' @param data.sets See data dictionary.
#' @return Character vector of the first ID values from the ID variables from
#'  each input data set.
#' @noRd
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

#' @description Produces a widget output when printing a MergeDataSetByVariable
#'  object.
#' @param x A list of class MergeDataSetByVariable
#' @noRd
#' @importFrom flipFormat DataSetMergingByVariableWidget
#' @export
print.MergeDataSetByVariable <- function(x, ...)
{
    DataSetMergingByVariableWidget(x$input.data.sets.metadata,
                                   x$merged.data.set.metadata,
                                   x$source.data.set.indices,
                                   x$omitted.variable.names.list,
                                   x$merged.id.variable.name,
                                   x$id.variable.names,
                                   x$example.id.values,
                                   x$is.saved.to.cloud)
}
