#' @title Merge Data Sets by Case
#' @description Merges multiple data sets by case where the data sets contain
#'  similar variables but different cases, e.g., data sets from different time
#'  periods.
#' @param data.set.names A character vector of names of data sets from the
#'  Displayr cloud drive to merge (if run from Displayr) or file paths of local
#'  data sets.
#' @param merged.data.set.name A character scalar of the name of the merged data
#'  set in the Displayr cloud drive (if run from Displayr) or the local file
#'  path of the merged data set.
#' @param auto.select.what.to.match.by If TRUE, the metadata to match by is
#'  chosen automatically, whereas if FALSE, the metadata to match by is
#'  specified by setting the flags \code{match.by.variable.names},
#'  \code{match.by.variable.labels} and match.by.value.labels.
#' @param match.by.variable.names Logical scalar indicating whether to match
#'  using variable names.
#' @param match.by.variable.labels Logical scalar indicating whether to match
#'  using variable labels.
#' @param match.by.value.labels Logical scalar indicating whether to match using
#'  value labels of categorical variables.
#' @param ignore.case Logical scalar indicating whether to ignore case when
#'  matching text (variable names and labels and value labels).
#' @param ignore.non.alphanumeric Logical scalar indicating whether to ignore
#'  non-alphanumeric characters when matching text (variable names and labels
#'  and value labels) except when numeric characters appear both before and
#'  after non-alphanumeric characters e.g., "24 - 29", in which case the
#'  characters are still ignored but the separation between the numbers is noted.
#' @param min.match.percentage A numeric scalar of a percentage (number from 0
#'  to 100) which determines how close matches need to be in order for matches
#'  to be accepted. Applies to variable names and labels and value labels.
#' @param variables.to.combine A character vector of comma-separated
#'  variable names indicating which variables are to appear together.
#'  Ranges of variables can be specified by separating variable names by '-'.
#'  Variables can be specified from specific data sets by appending '(x)' to
#'  the variable name where x is the data set index.
#' @param variables.to.not.combine A character vector of comma-separated variable
#'  names specifying variables that should never be combined together.
#'  To specify variables from a specific data set, suffix variable names
#'  with the data set index in parentheses, e.g., 'Q2(3)'.
#' @param variables.to.keep Character vector of variable names to keep in
#'  the merged data set. To specify variables from a specific data set,
#'  suffix the name with the data set index in parentheses, e.g., 'Q2(3)'.
#'  Ranges of variables can be specified by separating variable names by '-'.
#'  Wildcard matching of names is supported using the asterisk character '*'.
#'  This parameter is only useful when data.sets.whose.variables.are.kept is
#'  used (i.e., when variables are left out).
#' @param variables.to.omit Character vector of variable names to omit from
#'  the merged data set. To specify variables from a specific data set,
#'  suffix the name with the data set index in parentheses, e.g., 'Q2(3)'.
#'  Ranges of variables can be specified by separating variable names by '-'.
#'  Wildcard matching of names is supported using the asterisk character '*'.
#' @param include.merged.data.set.in.output A logical scalar which controls
#'  whether to include the merged data set in the output object, which can be
#'  used for diagnostic purposes in R.
#' @param when.multiple.labels.for.one.value Character scalar that is either
#'  "Use one of the labels" or "Create new values for the labels". When the
#'  former is the case, the label from the earliest/latest data set will be
#'  chosen if use.names.and.labels.from is "First data set"/"Last data set".
#'  If the latter is the case, new values are generated for the extra labels.
#' @param use.names.and.labels.from Character scalar that is either
#'  "First data set" or "Last data set". This sets the preference for either the
#'  first or last data set when choosing which names and labels to use in the
#'  merged data set.
#' @param data.sets.whose.variables.are.kept An integer vector of indices of
#'  data sets where merged variables are only included if they contain input
#'  variables from these data sets.
#' @param min.value.label.match.percentage Numeric scalar of the minimum
#'  percentage match for value labels to be considered the same when combining
#'  value attributes from different variables.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{merged.data.set} If \code{include.merged.data.set.in.output},
#'   is TRUE, this is a data frame of the merged data set.
#'   \item \code{input.data.sets.metadata} A list containing metadata on the
#'     the input data sets such as variable names, labels etc. See the function
#'     \code{metadataFromDataSets} for more information.
#'   \item \code{merged.data.set.metadata} A list containing metadata on the
#'     the merged data set such as variable names, labels etc. See the function
#'     \code{metadataFromDataSet} for more information.
#'   \item \code{matched.names} A character matrix whose rows correspond to the
#'     variables in the merged data set. The elements in each row correspond to
#'     the input data sets and contain the names of the variables from the input
#'     data sets that have been combined together to create a merged variable.
#'     This matrix also has the attributes "is.fuzzy.match" and "matched.by".
#'     is.fuzzy.match is a logical matrix of the same size as matched.names
#'     indicating if an input variable was matched using fuzzy matching.
#'     matched.by is a character matrix of the same size as matched.names
#'     containing the strings "Variable name", "Variable label", "Value label"
#'     and "Manual" indicating what data was used to match an input variable or
#'     if the variable was matched manually.
#'   \item \code{merged.names} A character vector containing the names of the
#'     variables in the merged data set.
#'   \item \code{omitted.variable.names} A list whose elements correspond to the
#'    input data sets. Each element is a character vector that contains the
#'    names of variables from an input data set that have been omitted from the
#'    merged data set.
#'   \item \code{input.value.attributes} A list whose elements correspond to the
#'   variables in the merged data set. Each element is another list whose
#'   elements correspond to the input data sets, which each of these elements
#'   containing a named numeric vector representing the values and value labels
#'   of a categorical input variable. This is NULL if the input variable is
#'   not categorical.
#'   \item \code{is.saved.to.cloud} Logical scalar that indicates whether the
#'   merged data set was saved to the Displayr cloud drive.
#' }
#' @examples
#' data.set.names <- c(system.file("examples", "Cola1.sav", package = "flipData"),
#'                     system.file("examples", "Cola2.sav", package = "flipData"),
#'                     system.file("examples", "Cola5.sav", package = "flipData"),
#'                     system.file("examples", "Cola8.sav", package = "flipData"))
#'
#' print(MergeDataSetsByCase(data.set.names = data.set.names,
#'                           data.sets.whose.variables.are.kept = 1,
#'                           variables.to.combine = "Q4_A_3,Q4_A_3_new"))
#' @export
MergeDataSetsByCase <- function(data.set.names,
                                merged.data.set.name = NULL,
                                auto.select.what.to.match.by = TRUE,
                                match.by.variable.names = TRUE,
                                match.by.variable.labels = TRUE,
                                match.by.value.labels = TRUE,
                                ignore.case = TRUE,
                                ignore.non.alphanumeric = TRUE,
                                min.match.percentage = 90,
                                variables.to.combine = NULL,
                                variables.to.not.combine = NULL,
                                variables.to.keep = NULL,
                                variables.to.omit = NULL,
                                include.merged.data.set.in.output = FALSE,
                                when.multiple.labels.for.one.value = "Create new values for the labels",
                                use.names.and.labels.from = "First data set",
                                data.sets.whose.variables.are.kept = seq_along(data.set.names),
                                min.value.label.match.percentage = 90)
{
    data.sets <- readDataSets(data.set.names, 2)
    input.data.sets.metadata <- metadataFromDataSets(data.sets)

    match.parameters <- list(auto.select.what.to.match.by = auto.select.what.to.match.by,
                             match.by.variable.names = match.by.variable.names,
                             match.by.variable.labels = match.by.variable.labels,
                             match.by.value.labels = match.by.value.labels,
                             ignore.case = ignore.case,
                             ignore.non.alphanumeric = ignore.non.alphanumeric,
                             min.match.percentage = min.match.percentage,
                             min.value.label.match.percentage = min.value.label.match.percentage)

    matched.names <- matchVariables(input.data.sets.metadata,
                                    match.parameters,
                                    variables.to.combine,
                                    variables.to.not.combine,
                                    variables.to.keep,
                                    variables.to.omit, data.sets,
                                    data.sets.whose.variables.are.kept,
                                    use.names.and.labels.from)
    merged.names <- mergedVariableNames(matched.names,
                                        use.names.and.labels.from)
    merged.data.set <- mergedDataSet(data.sets, matched.names, merged.names,
                                     use.names.and.labels.from,
                                     when.multiple.labels.for.one.value,
                                     match.parameters)
    merged.data.set.name <- correctDataSetName(merged.data.set.name,
                                               "Merged data set.sav")

    is.saved.to.cloud <- IsDisplayrCloudDriveAvailable()
    writeDataSet(merged.data.set, merged.data.set.name, is.saved.to.cloud)

    result <- list()
    if (include.merged.data.set.in.output)
        result$merged.data.set <- merged.data.set

    result$input.data.sets.metadata <- input.data.sets.metadata
    result$merged.data.set.metadata <- metadataFromDataSet(merged.data.set,
                                                           merged.data.set.name)
    result$matched.names <- matched.names
    result$merged.names <- merged.names
    result$omitted.variable.names <- omittedVariables(input.data.sets.metadata,
                                                      matched.names)
    result$input.value.attributes <- lapply(merged.data.set, attr,
                                            "input.value.attributes")
    result$is.saved.to.cloud <- is.saved.to.cloud
    class(result) <- "MergeDataSetByCase"
    result
}

# Performs matching of variables and returns the matched.names character matrix
# which represents the matching to be done. The rows of the matrix correspond to
# merged variables and the columns correspond to input data sets. Each row of
# the matrix contains the names of the input variables that will be merged into
# an output variable.
#
# The algorithm starts by constructing the rows of the matched.names matrix from
# the manually input variables.to.combine and then finding matches for these
# variables from the remaining input variables using the function findMatchesForRows.
#
# Next, the algorithm iterates through each input data set and gets the names
# of the variables that are to appear in the merged data set. It finds matches
# for these variables from the remaining input variables in the other data sets
# using the function findMatchesForRows.
#
# The output also has the attributes "is.fuzzy.match", "matched.by" and
# "match.parameters".
# is.fuzzy.match is a logical matrix of the same size as matched.names
# indicating if an input variable was matched using fuzzy matching.
# matched.by is a character matrix of the same size as matched.names
# containing the strings "Variable name", "Variable label", "Value label"
# and "Manual" indicating what data was used to match an input variable or
# if the variable was matched manually.
# match.parameters is a list containing the settings used for fuzzy matching
# (see MergeDataSetsByCase). It is returned because if auto.select.what.to.match.by
# is TRUE, then the properties that are matched by (variable name, label or
# value label) are set in match.parameters and this information is passed on.
matchVariables <- function(input.data.sets.metadata, match.parameters,
                           variables.to.combine, variables.to.not.combine,
                           variables.to.keep, variables.to.omit, data.sets,
                           data.sets.whose.variables.are.kept,
                           use.names.and.labels.from)
{
    v.names.to.combine <- parseVariablesToCombine(variables.to.combine,
                                                  input.data.sets.metadata)
    v.names.to.not.combine <- parseVariablesToNotCombine(variables.to.not.combine,
                                                         input.data.sets.metadata)
    v.names.to.keep <- parseVariablesToKeep(variables.to.keep,
                                            input.data.sets.metadata)
    v.names.to.omit <- parseVariablesToOmit(variables.to.omit,
                                            input.data.sets.metadata)
    checkMatchVariablesInputs(v.names.to.combine, v.names.to.not.combine,
                              v.names.to.keep, v.names.to.omit,
                              data.sets.whose.variables.are.kept,
                              input.data.sets.metadata)

    v.names <- input.data.sets.metadata$variable.names
    n.data.sets <- input.data.sets.metadata$n.data.sets

    remaining.ind <- lapply(seq_len(n.data.sets), function(i) {
        nms <- v.names[[i]]
        if (!is.null(v.names.to.combine))
            nms <- setdiff(nms, v.names.to.combine[, i])
        if (!is.null(v.names.to.omit))
            nms <- setdiff(nms, v.names.to.omit[, i])
        match(nms, v.names[[i]])
    })

    if (match.parameters$auto.select.what.to.match.by)
        match.parameters <- autoSelectWhatToMatchBy(input.data.sets.metadata,
                                                    match.parameters)

    matched.names <- rbind(matrix(nrow = 0, ncol = n.data.sets),
                           v.names.to.combine)
    is.fuzzy.match <- matrix(FALSE, nrow = nrow(matched.names),
                             ncol = n.data.sets)
    matched.by <- matrix(NA_character_, nrow = nrow(matched.names),
                         ncol = n.data.sets)
    matched.by[!is.na(matched.names)] <- "Manual"

    # Find matches to manually specified variables
    if (nrow(matched.names) > 0)
    {
        # If the data set index was specified for a variable in a manual match,
        # we don't find matches for the variable since we are likely to pick up
        # matches that the user explicitly wanted to avoid by specifying the
        # data set index.
        not.used.for.name.matching <- attr(v.names.to.combine,
                                           "is.data.set.specified.matrix")
        output <- findMatchesForRows(matched.names, seqRow(matched.names),
                                     seq_len(n.data.sets),
                                     input.data.sets.metadata,
                                     remaining.ind,
                                     use.names.and.labels.from,
                                     v.names.to.not.combine,
                                     match.parameters, data.sets,
                                     is.fuzzy.match, matched.by,
                                     not.used.for.name.matching)
        matched.names <- output$matched.names
        is.fuzzy.match <- output$is.fuzzy.match
        matched.by <- output$matched.by
        remaining.ind <- output$remaining.ind
    }

    d.ind <- if (use.names.and.labels.from == "First data set")
        seq_len(n.data.sets)
    else
        rev(seq_len(n.data.sets))

    # Find matches for remaining labels
    for (i in d.ind)
    {
        if (i %in% data.sets.whose.variables.are.kept)
        {
            nms.to.find.matches.for <- v.names[[i]][remaining.ind[[i]]]
            remaining.ind[[i]] <- integer(0)
        }
        else if (!is.null(v.names.to.keep) &&
                 any(!is.na(v.names.to.keep[, i])))
        {
            remaining.names <- v.names[[i]][remaining.ind[[i]]]
            nms.to.find.matches.for <- intersect(removeNA(v.names.to.keep[, i]),
                                                 remaining.names)
            ind <- match(nms.to.find.matches.for, remaining.names)
            remaining.ind[[i]] <- remaining.ind[[i]][-ind]
        }
        else
            next

        new.rows <- matrix(NA_character_,
                           nrow = length(nms.to.find.matches.for),
                           ncol = n.data.sets)
        new.rows[, i] <- nms.to.find.matches.for
        row.indices <- seqRow(new.rows) + nrow(matched.names)
        matched.names <- rbind(matched.names, new.rows)
        is.fuzzy.match <- rbind(is.fuzzy.match, matrix(FALSE,
                                                       nrow = nrow(new.rows),
                                                       ncol = n.data.sets))
        matched.by <- rbind(matched.by, matrix(NA_character_,
                                               nrow = nrow(new.rows),
                                               ncol = n.data.sets))

        other.data.set.indices <- d.ind[-i]

        output <- findMatchesForRows(matched.names, row.indices,
                                     other.data.set.indices,
                                     input.data.sets.metadata,
                                     remaining.ind,
                                     use.names.and.labels.from,
                                     v.names.to.not.combine,
                                     match.parameters, data.sets,
                                     is.fuzzy.match, matched.by)
        matched.names <- output$matched.names
        is.fuzzy.match <- output$is.fuzzy.match
        matched.by <- output$matched.by
        remaining.ind <- output$remaining.ind
    }
    attr(matched.names, "is.fuzzy.match") <- is.fuzzy.match
    attr(matched.names, "matched.by") <- matched.by

    matched.names <- orderMatchedNames(matched.names,
                                       input.data.sets.metadata,
                                       use.names.and.labels.from)

    attr(matched.names, "match.parameters") <- match.parameters
    matched.names
}

# For each row in the matched.names matrix that is in row.indices, find matches
# to the variables in the row from the remaining variables. The matched.names
# matrix is updated with the matches and returned in a list along with the
# objects is.fuzzy.match, matched.by and remaining.ind.
# See matchVariables for documentation on the first two.
# remaining.ind is a list whose elements correspond to the input data sets. Each
# element is an integer vector that contains the indices of variables in a data
# set that are still available for matching.
findMatchesForRows <- function(matched.names, row.indices, data.set.indices,
                               input.data.sets.metadata,
                               remaining.ind,
                               use.names.and.labels.from,
                               v.names.to.not.combine,
                               match.parameters, data.sets,
                               is.fuzzy.match, matched.by,
                               not.used.for.name.matching = NULL)
{
    v.names <- input.data.sets.metadata$variable.names
    v.labels <- input.data.sets.metadata$variable.labels
    v.val.attrs <- input.data.sets.metadata$variable.value.attributes
    v.types <- input.data.sets.metadata$variable.types

    n.data.sets <- length(data.sets)
    n.rows <- nrow(matched.names)
    matching.names <- matrix(NA_character_, nrow = n.rows, ncol = n.data.sets)
    matching.names.percentage <- matrix(NA_real_, nrow = n.rows,
                                        ncol = n.data.sets)
    matching.names.is.fuzzy <- matrix(FALSE, nrow = n.rows, ncol = n.data.sets)
    matching.names.matched.by <- matrix(NA_character_, nrow = n.rows,
                                        ncol = n.data.sets)
    if (is.null(not.used.for.name.matching))
        not.used.for.name.matching <- matrix(FALSE, nrow = n.rows, ncol = n.data.sets)

    for (i in row.indices)
    {
        missing.ind <- data.set.indices[is.na(matched.names[i, data.set.indices])]
        for (j in missing.ind)
        {
            if (length(remaining.ind[[j]]) == 0)
                next

            nms <- unique(removeNA(matched.names[i, !not.used.for.name.matching[i, ]]))
            non.missing.ind <- which(!is.na(matched.names[i, ]))
            lbls <- character(length(non.missing.ind))
            val.attrs <- vector(mode = "list", length = length(non.missing.ind))
            for (k in seq_along(non.missing.ind))
            {
                ind <- non.missing.ind[k]
                name.ind <- match(matched.names[i, ind], v.names[[ind]])
                lbls[k] <- v.labels[[ind]][name.ind]
                val.attrs[[k]] <- v.val.attrs[[ind]][[name.ind]]
            }
            lbls <- unique(removeNA(lbls))
            val.attrs <- unique(removeNULL(val.attrs))

            if (use.names.and.labels.from == "Last data set")
            {
                nms <- rev(nms)
                lbls <- rev(lbls)
                val.attrs <- rev(val.attrs)
            }

            remaining.names <- v.names[[j]][remaining.ind[[j]]]
            is.combinable <- vapply(remaining.names,
                                    isVariableCombinableIntoRow,
                                    logical(1), j, matched.names[i, ],
                                    v.names.to.not.combine)
            if (sum(is.combinable) == 0)
                next

            candidate.names <- remaining.names[is.combinable]
            candidate.labels <- v.labels[[j]][remaining.ind[[j]][is.combinable]]
            candidate.val.attrs <- v.val.attrs[[j]][remaining.ind[[j]][is.combinable]]

            matching.name <- findMatchingVariable(nms, lbls, val.attrs,
                                                  candidate.names,
                                                  candidate.labels,
                                                  candidate.val.attrs,
                                                  match.parameters)
            if (is.na(matching.name))
                next

            is.compatible <- isVariableTypeCompatible(matching.name, j,
                                                      matched.names[i, ],
                                                      input.data.sets.metadata,
                                                      data.sets)
            if (!is.compatible)
                next

            matching.names[i, j] <- matching.name
            matching.names.percentage[i, j] <- attr(matching.name,
                                                    "match.percentage")
            matching.names.is.fuzzy[i, j] <- attr(matching.name,
                                                  "is.fuzzy.match")
            matching.names.matched.by[i, j] <- attr(matching.name,
                                                    "matched.by")
        }
    }

    for (i in data.set.indices)
    {
        unique.names <- unique(removeNA(matching.names[, i]))
        matched.ind <- c()
        for (nm in unique.names)
        {
            ind <- which(matching.names[, i] == nm)
            max.ind <- ind[which.max(matching.names.percentage[ind, i])]
            matched.names[max.ind, i] <- nm
            matched.ind <- c(matched.ind,
                             match(nm, v.names[[i]][remaining.ind[[i]]]))
            is.fuzzy.match[max.ind, i] <- matching.names.is.fuzzy[max.ind, i]
            matched.by[max.ind, i] <- matching.names.matched.by[max.ind, i]
        }
        if (!is.null(matched.ind))
            remaining.ind[[i]] <- remaining.ind[[i]][-matched.ind]
    }

    list(matched.names = matched.names,
         is.fuzzy.match = is.fuzzy.match,
         matched.by = matched.by,
         remaining.ind = remaining.ind)
}

# Determine what to match by: variable names, variable labels and/or value labels
autoSelectWhatToMatchBy <- function(input.data.sets.metadata, match.parameters)
{
    v.names <- input.data.sets.metadata$variable.names
    v.labels <- input.data.sets.metadata$variable.labels
    v.val.attrs <- input.data.sets.metadata$variable.value.attributes
    n.data.sets <- input.data.sets.metadata$n.data.sets

    if (match.parameters$ignore.case)
    {
        # Convert to lowercase
        v.names <- lapply(v.names, tolower)
        v.labels <- lapply(v.labels, tolower)
        v.val.attrs <- lapply(v.val.attrs, function(val.attrs) {
            lapply(removeNULL(val.attrs), function(val.attr) {
                names(val.attr) <- tolower(names(val.attr))
                val.attr
            })
        })
    }

    if (match.parameters$ignore.non.alphanumeric)
    {
        # Remove non-alphanumeric characters
        v.names <- lapply(v.names, removeNonAlphaNumericCharacters)
        v.labels <- lapply(v.labels, removeNonAlphaNumericCharacters)
        v.val.attrs <- lapply(v.val.attrs, function(val.attrs) {
            lapply(removeNULL(val.attrs), function(val.attr) {
                names(val.attr) <- removeNonAlphaNumericCharacters(names(val.attr))
                val.attr
            })
        })
    }

    # Don't match by variable labels if more than 25% of labels in a data set
    # map to two or more labels in another data set.
    match.parameters$match.by.variable.labels <- maxOneToManyLabelProportion(v.labels) <= 0.25

    # Don't match by variable names if we are matching by variable labels and
    # variable name matches agree with variable label matches less than half
    # the time, since labels matches are usually better quality.
    if (match.parameters$match.by.variable.labels)
    {
        unique.lbls <- unique(unlist(v.labels))
        unique.lbls <- unique.lbls[unique.lbls != ""]
        duplicated.lbls <- unlist(lapply(v.labels,
                                         function(lbls) lbls[duplicated(lbls)]))
        unique.lbls <- setdiff(unique.lbls, duplicated.lbls)

        n.tested <- 0
        n.matching.names <- 0
        for (lbl in unique.lbls)
        {
            indices <- vapply(v.labels,
                              function(lbls) match(lbl, lbls), integer(1))
            if (sum(!is.na(indices)) < 2)
                next

            all.identical <- allIdentical(removeNA(vapply(seq_len(n.data.sets), function(i) {
                if (!is.na(indices[i]))
                    v.names[[i]][indices[i]]
                else
                    NA_character_
            }, character(1))))

            n.tested <- n.tested + 1
            if (all.identical)
                n.matching.names <- n.matching.names + 1
        }

        match.parameters$match.by.variable.names <- n.tested < 0.5 * length(unique.lbls) ||
                                                    n.matching.names / n.tested >= 0.5
    }
    else
        match.parameters$match.by.variable.names <- TRUE

    # Don't match by value labels if more than 50% of value label sets in a data
    # set maps to two or more sets of value labels in another data set.
    match.parameters$match.by.value.labels <- maxOneToManyValueAttrProportion(v.val.attrs) <= 0.25
    match.parameters
}

# Maximum proportion of labels in a data set that map to multiple labels in
# another data set (maximum over all pairs of data sets).
# See unit tests in test-mergedatasetsbycase.R
maxOneToManyLabelProportion <- function(v.labels)
{
    n.data.sets <- length(v.labels)
    prop <- c()
    for (i in seq_len(n.data.sets))
    {
        for (j in seq_len(n.data.sets))
        {
            if (i == j)
                next

            n <- 0

            lbls.i <- v.labels[[i]]
            lbls.j <- v.labels[[j]]
            unique.lbls.i <- unique(lbls.i)
            for (k in seq_along(unique.lbls.i))
                if (length(which(unique.lbls.i[k] == lbls.j)) > 1)
                    n <- n + 1
            prop <- c(prop, n / length(unique.lbls.i))
        }
    }
    max(prop)
}

# Maximum proportion of value attributes in a data set that map to multiple
# value attributes in another data set (maximum over all pairs of data sets)
# See unit tests in test-mergedatasetsbycase.R
maxOneToManyValueAttrProportion <- function(v.val.attrs)
{
    n.data.sets <- length(v.val.attrs)
    prop <- c()
    for (i in seq_len(n.data.sets))
    {
        for (j in seq_len(n.data.sets))
        {
            if (i == j)
                next

            n <- 0

            v.val.attrs.i <- v.val.attrs[[i]]
            v.val.attrs.j <- v.val.attrs[[j]]

            if (length(v.val.attrs.i) == 0 || length(v.val.attrs.j) == 0)
                next

            for (k in seq_along(v.val.attrs.i))
            {
                n.matches <- sum(vapply(v.val.attrs.j, function(val.attr.j) {
                    identical(v.val.attrs.i[[k]], val.attr.j)
                }, logical(1)))
                if (n.matches > 1)
                    n <- n + 1
            }
            prop <- c(prop, n / length(v.val.attrs.i))
        }
    }
    max(prop)
}

# Parse the character vector variables.to.combine and return a matrix where
# each row contains a set of variables to be combined, with the columns
# corresponding to the input data sets.
# See unit tests in test-mergedatasetsbycase.R
parseVariablesToCombine <- function(variables.to.combine,
                                    input.data.sets.metadata)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    result <- matrix(nrow = 0, ncol = n.data.sets)
    is.data.set.specified.matrix <- matrix(nrow = 0, ncol = n.data.sets)
    for (txt in variables.to.combine)
    {
        new.rows <- parseInputTextForVariableInteraction(txt,
                                                         input.data.sets.metadata,
                                                         "Variables to manually combine")
        result <- rbind(result, new.rows, deparse.level = 0)

        n.new.rows <- nrow(new.rows)
        is.data.set.specified.matrix <- rbind(is.data.set.specified.matrix,
                                              matrix(rep(attr(new.rows, "is.data.set.specified.vector"),
                                                         each = n.new.rows), nrow = n.new.rows))
    }

    # Check that variables to combine have not been specified multiple times
    for (i in seq_len(n.data.sets))
    {
        date.set.vars.names <- result[, i]
        names.table <- table(date.set.vars.names)
        if (any(names.table > 1))
        {
            duplicate.name <- names(names.table)[names.table > 1][1]
            duplicate.match.source <- variables.to.combine[date.set.vars.names == duplicate.name]
            stop("The variable '", duplicate.name, "' has been specified to be combined in multiple inputs: ",
                 paste0(paste0("'", duplicate.match.source, "'"), collapse = ", "),
                 ". Ensure that any of the variables to be combined are specified in at most one input.")
        }
    }

    attr(result, "is.data.set.specified.matrix") <- is.data.set.specified.matrix
    result
}

# Parse the character vector variables.to.not.combine and return a matrix where
# each row contains a set of variables that should not be combined, with the
# columns corresponding to the input data sets.
# See unit tests in test-mergedatasetsbycase.R
parseVariablesToNotCombine <- function(variables.to.not.combine,
                                       input.data.sets.metadata)
{
    do.call("rbind", lapply(variables.to.not.combine,
                            parseInputTextForVariableInteraction,
                            input.data.sets.metadata, "Variables that should not be combined"))
}

# Parse the character vector variables.to.keep and return a matrix where
# each row contains a set of variables that should not be kept, with the
# columns corresponding to the input data sets.
# See unit tests in test-mergedatasetsbycase.R
parseVariablesToKeep <- function(variables.to.keep, input.data.sets.metadata)
{
    split.text <- unlist(lapply(variables.to.keep, splitByComma,
                                ignore.commas.in.parentheses = TRUE),
                         use.names = FALSE)
    do.call("rbind", lapply(split.text, parseInputTextIntoVariableNamesMatrix,
                            input.data.sets.metadata, allow.wildcards = TRUE,
                            "Variables to manually include"))
}

# Parse the character vector variables.to.omit and return a matrix where
# each row contains a set of variables that should be omitted, with the
# columns corresponding to the input data sets.
# See unit tests in test-mergedatasetsbycase.R
parseVariablesToOmit <- function(variables.to.omit,
                                 input.data.sets.metadata)
{
    split.text <- unlist(lapply(variables.to.omit, splitByComma,
                                ignore.commas.in.parentheses = TRUE),
                         use.names = FALSE)
    do.call("rbind", lapply(split.text, parseInputTextIntoVariableNamesMatrix,
                            input.data.sets.metadata, allow.wildcards = TRUE,
                            "Variables to manually omit"))
}

# Parse input text containing a variable name, variable range or variable name
# with a wildcard character into a matrix of variable names. The columns of the
# matrix correspond to the input data sets and contain the parsed variable names
# from a data set.
# The matrix has the attribute is.data.set.specified which is a logical scalar
# indicating if the data set was specified in the input.
# See unit tests in test-mergedatasetsbycase.R
parseInputTextIntoVariableNamesMatrix <- function(input.text,
                                                  input.data.sets.metadata,
                                                  allow.wildcards,
                                                  input.purpose)
{
    v.names <- input.data.sets.metadata$variable.names
    n.data.sets <- input.data.sets.metadata$n.data.sets
    is.data.set.specified <- FALSE

    if (!allow.wildcards && grepl("\\*", input.text))
        stop("The input '", input.text,
             "' could not be parsed as wildcard characters are not supported for '",
             input.purpose, "' inputs.")

    parsed.v.names.list <- vector(mode = "list", length = n.data.sets)

    dash.ind <- match("-", strsplit(input.text, "")[[1]])

    if (is.na(dash.ind)) # single variable (not range)
    {
        data.set.indices <- parseDataSetIndices(input.text, n.data.sets)
        if (length(data.set.indices) == 0) # data set index not supplied
        {
            is.variable.found <- FALSE
            if (!grepl("\\*", input.text)) # no wildcard
            {
                for (j in seq_len(n.data.sets))
                {
                    if (input.text %in% v.names[[j]])
                    {
                        parsed.v.names.list[[j]] <- input.text
                        is.variable.found <- TRUE
                    }
                }
                if (!is.variable.found)
                    throwVariableNotFoundError(input.text)
            }
            else # has wildcard
            {
                for (j in seq_len(n.data.sets))
                {
                    parsed.v.names <- parseVariableWildcardForMerging(input.text,
                                                                      v.names[[j]],
                                                                      j, FALSE)
                    if (length(parsed.v.names) > 0)
                    {
                        parsed.v.names.list[[j]] <- parsed.v.names
                        is.variable.found <- TRUE
                    }
                }
                if (!is.variable.found)
                    stop("No variables matching the wildcard name '",
                         input.text,
                         "' could be found in any of the input data sets. ",
                         "Ensure that the wildcard name has been correctly specified.")
            }
        }
        else # data set indices supplied
        {
            input.text.without.index <- removeDataSetIndices(input.text)
            if (!grepl("\\*", input.text)) # no wildcard
            {
                for (j in data.set.indices)
                {
                    if (!(input.text.without.index %in% v.names[[j]]))
                        throwVariableNotFoundError(input.text.without.index, j)
                    parsed.v.names.list[[j]] <- input.text.without.index
                }
            }
            else # has wildcard
            {
                for (j in data.set.indices)
                    parsed.v.names.list[[j]] <- parseVariableWildcardForMerging(input.text.without.index,
                                                                                v.names[[j]],
                                                                                j, TRUE)
            }
            is.data.set.specified <- TRUE
        }
    }
    else # range of variables
    {
        if (grepl("\\*", input.text))
            stop("The input '", input.text,
                 "' is invalid as wildcard characters are not supported for variable ranges.")

        range.start <- trimws(substr(input.text, 1, dash.ind - 1))
        range.end <- trimws(substr(input.text, dash.ind + 1, nchar(input.text)))

        data.set.ind <- parseDataSetIndicesForRange(range.start,
                                                    range.end,
                                                    n.data.sets)

        if (length(data.set.ind) == 0) # data set index not supplied for range
        {
            is.range.found <- FALSE
            for (j in seq_len(n.data.sets))
            {
                range.var.names <- variablesFromRange(v.names[[j]],
                                                      range.start,
                                                      range.end,
                                                      j, input.text, FALSE)
                if (length(range.var.names) > 0)
                {
                    parsed.v.names.list[[j]] <- range.var.names
                    is.range.found <- TRUE
                }
            }
            if (!is.range.found)
                stop("The input range '", input.text,
                     "' was not found in any of the input data sets. ",
                     "Ensure that the range has been correctly specified.")
        }
        else # data set indices supplied for range
        {
            range.start.without.index <- removeDataSetIndices(range.start)
            range.end.without.index <- removeDataSetIndices(range.end)
            for (j in data.set.ind)
            {
                parsed.v.names.list[[j]] <- variablesFromRange(v.names[[j]],
                                                               range.start.without.index,
                                                               range.end.without.index,
                                                               j, input.text,
                                                               TRUE)
            }
            is.data.set.specified <- TRUE
        }
    }

    # Convert parsed.v.names.list to matrix
    n.var <- max(vapply(parsed.v.names.list, length, integer(1)))
    result <- do.call("cbind", lapply(parsed.v.names.list, function(nms) {
        column <- rep(NA_character_, n.var)
        column[seq_along(nms)] <- nms
        column
    }))

    n.row <- nrow(result)
    attr(result, "is.data.set.specified") <- is.data.set.specified
    result
}

# Parses a string of comma-separated names of variables and returns a matrix
# of names where columns correspond to input data. Ranges of variables can be
# specified with a dash. Variables are specified to be from a data set when
# their names have the suffix consisting of the data set index in parentheses.
# The returned matrix has the attribute is.data.set.specified.vector which is a
# logical vector indicating the columns of the matrix where the data set index
# was specified. See unit tests in test-mergedatasetsbycase.R
parseInputTextForVariableInteraction <- function(input.text,
                                                 input.data.sets.metadata,
                                                 input.purpose)
{


    n.data.sets <- input.data.sets.metadata$n.data.sets
    split.text <- splitByComma(input.text, ignore.commas.in.parentheses = TRUE)
    if (length(split.text) == 0)
        stop("The input '", input.text, "' is invalid. It needs to specify ",
             input.purpose, ".")

    result <- NULL
    is.data.set.specified.vector <- NULL
    for (i in seq_along(split.text))
    {
        v.names.matrix <- parseInputTextIntoVariableNamesMatrix(split.text[i],
                                                                input.data.sets.metadata,
                                                                allow.wildcards = FALSE,
                                                                input.purpose)
        if (is.null(result))
        {
            result <- v.names.matrix
            attr(result, "is.data.set.specified") <- NULL
            is.data.set.specified.vector <- !is.na(v.names.matrix[1, ]) &
                                                   attr(v.names.matrix, "is.data.set.specified")
        }
        else
        {
            if (nrow(v.names.matrix) != nrow(result))
                stop("The input '", input.text,
                     "' contains variable ranges with differing numbers of variables. ",
                     "Ensure that the ranges have been correctly specified so that they all contain the same number of variables.")

            is.data.set.specified <- attr(v.names.matrix, "is.data.set.specified")
            if (is.data.set.specified)
            {
                for (j in which(!is.na(v.names.matrix[1, ])))
                {
                    if (is.na(result[1, j]) || !is.data.set.specified.vector[j])
                    {
                        result[, j] <- v.names.matrix[, j]
                        is.data.set.specified.vector[j] <- TRUE
                    }
                    else # !is.na(result[1, j]) && is.data.set.specified.vector[j]
                        stop("The input '", input.text,
                             "' contains different variables which have been specified for data set ",
                             j, ". Each input for '", input.purpose,
                             "' may only specify at most one variable or variable range per data set.")
                }
            }
            else # is.data.set.specified == FALSE
            {
                for (j in which(!is.na(v.names.matrix[1, ])))
                {
                    if (is.na(result[1, j]))
                    {
                        result[, j] <- v.names.matrix[, j]
                        is.data.set.specified.vector[j] <- FALSE
                    }
                    else if (!is.data.set.specified.vector[j]) # && !is.na(result[1, j])
                    {
                        stop("The input '", input.text, "' for '",
                             input.purpose,
                             "' contains variables which are both present in data set ",
                             j, ". Each input may only specify at most one variable or variable range per data set. ",
                             "Try explicitly specifying the data set index for variables by appending '(x)' to the variable name, ",
                             "where 'x' is replaced with the data set index, e.g., use '(2)' for the 2nd input data set.")
                    }

                }
            }
        }
    }

    if (sum(!is.na(result[1, ])) == 1)
        stop("The input '", input.text, "' for '", input.purpose,
             "' only specifies variables from one data set. ",
             "It needs to specify variables from two or more data sets.")

    attr(result, "is.data.set.specified.vector") <- is.data.set.specified.vector
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
        stop("The manually specified names '",
             source.text[data.set.ind], "' and '", input.text,
             "' were both found in data set ", data.set.ind,
             ". One of the names needs to be specified from a different data set by appending '(x)' to the variable name, ",
             "where 'x' is replaced with the data set index, e.g., use '(2)' for the 2nd input data set.")
}

# Check that variables to combine, variables to not combine, variables to keep
# and variables to omit are all consistent with each other
checkMatchVariablesInputs <- function(v.names.to.combine, v.names.to.not.combine,
                                      v.names.to.keep, v.names.to.omit,
                                      data.sets.whose.variables.are.kept,
                                      input.data.sets.metadata)
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
                     " have been specified to be both combined and not combined. ",
                     "Ensure that they are specified to be either combined or not combined.")
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
                     "Ensure that it is specified to be either combined or omitted.")
            else if (length(v) > 1)
                stop("The variable(s) ",
                     paste0(paste0("'", v, "'"), collapse = ", "),
                     " have been specified to be both combined and omitted. ",
                     "Ensure that they are specified to be either combined or omitted.")
        }
    }

    # Check v.names.to.keep against v.names.to.omit
    if (!is.null(v.names.to.keep) && !is.null(v.names.to.omit))
    {
        for (i in seq_len(ncol(v.names.to.keep)))
        {
            ind <- which(v.names.to.keep[, i] %in%
                             removeNA(v.names.to.omit[, i]))
            v <- unique(v.names.to.keep[ind, i])

            if (length(v) == 1)
                stop("The variable ",
                     paste0("'", v, "'"),
                     " has been specified to be both kept and omitted. ",
                     "Ensure that it is specified to be either kept or omitted.")
            else if (length(v) > 1)
                stop("The variable(s) ",
                     paste0(paste0("'", v, "'"), collapse = ", "),
                     " have been specified to be both kept and omitted. ",
                     "Ensure that they are specified to be either kept or omitted.")
        }
    }

    if (length(data.sets.whose.variables.are.kept) == 0)
        stop("At least one data set needs to be specified in the input for the data sets whose variables are kept")

    n.data.sets <- input.data.sets.metadata$n.data.sets
    if (any(!(data.sets.whose.variables.are.kept %in% seq_len(n.data.sets))))
        stop("The input for 'data.sets.whose.variables.are.kept' contains invalid data set indices. ",
             "Ensure that it contains only indices from 1 to the number of input data sets.")
}

# Find candidate variable with names, labels or value attributes that match
# nms, lbls or val.attrs. Returns the name of the matching variable,
# otherwise NA_character_.
findMatchingVariable <- function(nms, lbls, val.attrs, candidate.names,
                                 candidate.labels, candidate.val.attrs,
                                 match.parameters)
{
    match.by.variable.names <- match.parameters$match.by.variable.names
    match.by.variable.labels <- match.parameters$match.by.variable.labels
    match.by.value.labels <- match.parameters$match.by.value.labels
    ignore.case <- match.parameters$ignore.case
    ignore.non.alphanumeric <- match.parameters$ignore.non.alphanumeric
    min.match.percentage <- match.parameters$min.match.percentage

    n.input.candidate.names <- length(candidate.names)
    is.exact.match <- FALSE

    # Find exact label match
    if (match.by.variable.labels && length(lbls) > 0)
    {
        ind <- unlist(lapply(lbls, function(lbl) {
            which(lbl == candidate.labels)
        }))

        if (length(ind) == 1)
        {
            result <- candidate.names[ind]
            attr(result, "match.percentage") <- 100
            attr(result, "is.fuzzy.match") <- FALSE
            attr(result, "matched.by") <- "Variable label"
            return(result)
        }
        else if (length(ind) > 1)
        {
            candidate.names <- candidate.names[ind]
            candidate.labels <- candidate.labels[ind]
            candidate.val.attrs <- candidate.val.attrs[ind]
            is.exact.match <- TRUE
        }
    }

    # Find fuzzy label match
    if (match.by.variable.labels && length(lbls) > 0)
    {
        match.percentages <- matchPercentages(strings.1 = candidate.labels,
                                              strings.2 = lbls,
                                              ignore.case = ignore.case,
                                              ignore.non.alphanumeric = ignore.non.alphanumeric,
                                              min.match.percentage = min.match.percentage)
        best.match.percentage <- max(match.percentages)
        if (best.match.percentage >= min.match.percentage)
        {
            arr.ind <- which(match.percentages == best.match.percentage,
                             arr.ind = TRUE)
            if (allIdentical(arr.ind[, 1]))
            {
                result <- candidate.names[arr.ind[1, 1]]
                attr(result, "match.percentage") <- best.match.percentage
                attr(result, "is.fuzzy.match") <- !is.exact.match
                attr(result, "matched.by") <- "Variable label"
                return(result)
            }

            candidate.names <- candidate.names[arr.ind[, 1]]
            candidate.labels <- candidate.labels[arr.ind[, 1]]
            candidate.val.attrs <- candidate.val.attrs[arr.ind[, 1]]
        }
    }

    # Find exact name match
    if (match.by.variable.names)
    {
        ind <- removeNA(match(nms, candidate.names))

        if (length(ind) == 1)
        {
            result <- candidate.names[ind]
            attr(result, "match.percentage") <- 100
            attr(result, "is.fuzzy.match") <- FALSE
            attr(result, "matched.by") <- "Variable name"
            return(result)
        }
        else if (length(ind) > 1)
        {
            candidate.names <- candidate.names[ind]
            candidate.labels <- candidate.labels[ind]
            candidate.val.attrs <- candidate.val.attrs[ind]
            is.exact.match <- TRUE
        }
    }

    # Find fuzzy name match
    if (match.by.variable.names)
    {
        match.percentages <- matchPercentages(strings.1 = candidate.names,
                                              strings.2 = nms,
                                              ignore.case = ignore.case,
                                              ignore.non.alphanumeric = ignore.non.alphanumeric,
                                              min.match.percentage = min.match.percentage)
        sorted.match.percentages <- unique(sort(match.percentages, decreasing = TRUE))

        for (p in sorted.match.percentages)
        {
            if (p < min.match.percentage)
                break

            arr.ind <- which(match.percentages == p, arr.ind = TRUE)
            is.numbers.preserved <- vapply(seqRow(arr.ind), function(i) {
                isNumbersPreserved(candidate.names[arr.ind[, 1]], nms[arr.ind[, 2]])
            }, logical(1))
            arr.ind <- arr.ind[is.numbers.preserved, , drop = FALSE]

            if (nrow(arr.ind) == 1)
            {
                result <- candidate.names[arr.ind[1, 1]]
                attr(result, "match.percentage") <- p
                attr(result, "is.fuzzy.match") <- !is.exact.match
                attr(result, "matched.by") <- "Variable name"
                return(result)
            }
            else if (nrow(arr.ind) > 1)
            {
                candidate.names <- candidate.names[arr.ind[, 1]]
                candidate.labels <- candidate.labels[arr.ind[, 1]]
                candidate.val.attrs <- candidate.val.attrs[arr.ind[, 1]]
            }
        }
    }

    # Find exact value labels match
    if (match.by.value.labels && length(val.attrs) > 0)
    {
        ind <- unlist(lapply(val.attrs, function(val.attr) {
            which(vapply(candidate.val.attrs, function(candidate.val.attr) {
                setequal(names(candidate.val.attr), names(val.attr))
            }, logical(1)))
        }))

        if (length(ind) == 1)
        {
            result <- candidate.names[ind]
            attr(result, "match.percentage") <- 100
            attr(result, "is.fuzzy.match") <- FALSE
            attr(result, "matched.by") <- "Value label"
            return(result)
        }
        else if (length(ind) > 1)
        {
            candidate.names <- candidate.names[ind]
            candidate.labels <- candidate.labels[ind]
            candidate.val.attrs <- candidate.val.attrs[ind]
            is.exact.match <- TRUE
        }
    }

    # Find fuzzy value label match
    if (match.by.value.labels && length(val.attrs) > 0)
    {
        match.percentages <- matchPercentagesForValueAttributes(val.attrs.1 = candidate.val.attrs,
                                                                val.attrs.2 = val.attrs,
                                                                ignore.case = ignore.case,
                                                                ignore.non.alphanumeric = ignore.non.alphanumeric,
                                                                min.match.percentage = min.match.percentage)
        best.match.percentage <- max(match.percentages)
        if (best.match.percentage >= min.match.percentage)
        {
            arr.ind <- which(match.percentages == best.match.percentage,
                             arr.ind = TRUE)
            if (nrow(arr.ind) == 1)
            {
                result <- candidate.names[arr.ind[1, 1]]
                attr(result, "match.percentage") <- best.match.percentage
                attr(result, "is.fuzzy.match") <- !is.exact.match
                attr(result, "matched.by") <- "Value label"
                return(result)
            }
        }
    }

    NA_character_
}

# Returns a matrix of percentage matches (similarities) between strings in
# strings.1 and strings.2.
# See unit tests in test-mergedatasetsbycase.R
#' @importFrom stringdist stringdistmatrix
matchPercentages <- function(strings.1, strings.2, ignore.case,
                             ignore.non.alphanumeric,
                             min.match.percentage)
{
    if (ignore.case)
    {
        strings.1 <- tolower(strings.1)
        strings.2 <- tolower(strings.2)
    }
    if (ignore.non.alphanumeric)
    {
        strings.1 <- removeNonAlphaNumericCharacters(strings.1)
        strings.2 <- removeNonAlphaNumericCharacters(strings.2)
    }

    # It is faster to check for equality compared to computing distances
    if (min.match.percentage == 100)
    {
        match.matrix <- outer(strings.1, strings.2, "==")
        return(outer(strings.1, strings.2, "==") * 100)
    }

    n.char.1 <- nchar(strings.1)
    n.char.2 <- nchar(strings.2)

    distances <- do.call("cbind", lapply(strings.2, function(s) {
        d <- rep(Inf, length(n.char.1))
        if (s == "")
            return(d)

        # Find a subset of strings.1 where it is actually useful to compute
        # distances since it is expensive. We don't compute distances for empty
        # strings or when the difference in the number of characters would
        # result in the min.match.percentage being exceeded
        ind <- which(n.char.1 > 0)
        n.char.diff <- abs(nchar(s) - n.char.1[ind])
        max.n.char <- pmax(nchar(s), n.char.1[ind])
        ind <- ind[adjustedMatchPercentage(distances = n.char.diff, max.nchars = max.n.char) >= min.match.percentage]

        d[ind] <- stringdist(s, strings.1[ind])
        d
    }))

    nchar.matrix.1 <- matrix(rep(n.char.1, length(strings.2)),
                      nrow = length(strings.1))
    nchar.matrix.2 <- matrix(rep(n.char.2, each = length(strings.1)),
                      nrow = length(strings.1))
    adjustedMatchPercentage(distances = distances,
                            max.nchars = pmax(nchar.matrix.1, nchar.matrix.2))
}

# Compute an adjusted match percentage that gives higher values when two
# strings have many characters in common.
# The unadjusted match percentage is simply 100 * (1 - (distances / max.nchars).
# The adjusted match percentage only differs from the unadjusted match
# percentage when max.nchars - distances > 20.
# For example, for distance = 2 and max.nchar = 10, the match percentage is 80%
# which is same with and without adjustment.
# But for distance = 20 and max.nchar = 100, the match percentage is also 80%
# but the adjusted match percentage is 99.84%. This makes sense because in the
# latter case, there are a lot more characters in common and the chance of a
# true match is a lot higher.
# See unit tests in test-mergedatasetsbycase.R
adjustedMatchPercentage <- function(distances, max.nchars)
{
    scale.parameter <- 20
    result <- 100 * (1 - (distances / max.nchars) ^ pmax((max.nchars - distances) / scale.parameter, 1))
    # Since the formula above may give 100% for non-zero distances (when
    # max.nchars is much larger than distances), the line below ensures that
    # non-zero distances never result in 100%, so that zero distances will
    # always be chosen over non-zero distances.
    result - 1e-12 * (distances > 0)
}

# Returns a matrix of percentage matches (similarities) between lists of value
# attributes val.attrs.1 and val.attrs.2. This function works by concatenating
# the value labels into one string and comparing these strings.
# See unit tests in test-mergedatasetsbycase.R
matchPercentagesForValueAttributes <- function(val.attrs.1, val.attrs.2,
                                               ignore.case,
                                               ignore.non.alphanumeric,
                                               min.match.percentage)
{
    lbls.combined.1 <- vapply(val.attrs.1, function(x) {
        if (is.null(x))
            return("")
        lbls <- names(x)
        lbls <- lbls[lbls != ""]
        if (ignore.case)
            lbls <- tolower(lbls)
        if (ignore.non.alphanumeric)
            lbls <- removeNonAlphaNumericCharacters(lbls)
        paste0(sort(lbls), collapse = ",")
    }, character(1))

    lbls.combined.2 <- vapply(val.attrs.2, function(x) {
        if (is.null(x))
            return("")
        lbls <- names(x)
        lbls <- lbls[lbls != ""]
        if (ignore.case)
            lbls <- tolower(lbls)
        if (ignore.non.alphanumeric)
            lbls <- removeNonAlphaNumericCharacters(lbls)
        paste0(sort(lbls), collapse = ",")
    }, character(1))

    matchPercentages(strings.1 = lbls.combined.1,
                     strings.2 = lbls.combined.2,
                     ignore.case = FALSE,
                     ignore.non.alphanumeric = FALSE,
                     min.match.percentage = min.match.percentage)
}

# Returns a vector of percentage matches (similarities) between the value label
# lbl and lbls.to.compare.against.
#' @importFrom stringdist stringdist
matchPercentagesForValueLabels <- function(lbl, lbls.to.compare.against,
                                           match.parameters)
{
    nchar.lbls <- pmax(nchar(lbl), nchar(lbls.to.compare.against))
    lbl <- normalizeValueLabels(lbl, match.parameters)
    lbls.to.compare.against <- normalizeValueLabels(lbls.to.compare.against,
                                                    match.parameters)
    if (lbl == "")
        return(rep(0, length(lbls.to.compare.against)))

    distances <- stringdist(lbl, lbls.to.compare.against)
    adjustedMatchPercentage(distances = distances, max.nchars = nchar.lbls)
}

normalizeValueLabels <- function(lbls, match.parameters)
{
    if (match.parameters$ignore.case)
        lbls <- tolower(lbls)
    if (match.parameters$ignore.non.alphanumeric)
        lbls <- removeNonAlphaNumericCharacters(lbls)
    lbls
}

# Remove non-alphanumeric characters from input text, except when the
# removal of the characters results in numeric characters connecting,
# e.g., "20 - 29" becoming "2029". In such a situation we replace the
# characters with an underscore.
# See unit tests in test-mergedatasetsbycase.R
removeNonAlphaNumericCharacters <- function(txt)
{
    # We require this elaborate pattern to avoid selecting the case where
    # non-alphanum characters are between digit characters
    pattern <- paste0("(^[^a-zA-Z0-9]+)|", # non-alphanum characters at start
                      "([^a-zA-Z0-9]+$)|", # non-alphanum characters at end
                      "((?<=[[:alpha:]])[^a-zA-Z0-9]+(?=[[:alpha:]]))|", # non-alphanum characters between alphabet characters
                      "((?<=[[:alpha:]])[^a-zA-Z0-9]+(?=\\d))|", # non-alphanum characters between alphabet character and digit
                      "((?<=\\d)[^a-zA-Z0-9]+(?=[[:alpha:]]))") # non-alphanum characters between digit and alphabet character
    result <- gsub(pattern, "", txt, perl = TRUE)

    # Replace remaining non-alphanum characters with an underscore
    # (those between digit characters)
    result <- gsub("[^a-zA-Z0-9]+", "_", result, perl = TRUE)

    result
}

# Checks that the numbers that appear in string.1 and string.2 are identical
# This is used to ensure that variable names that fuzzy match refer to the
# same numbers. For example, Q_11_2 and Q_1_12 match if the "_" is ignored
# but the numbers in the variables do not match.
# See unit tests in test-mergedatasetsbycase.R
isNumbersPreserved <- function(string.1, string.2)
{
    nums.1 <- strsplit(string.1, "[^0-9]")[[1]]
    nums.1 <- nums.1[nums.1 != ""]
    nums.2 <- strsplit(string.2, "[^0-9]")[[1]]
    nums.2 <- nums.2[nums.2 != ""]

    if (length(nums.1) > length(nums.2))
    {
        temp <- nums.1
        nums.1 <- nums.2
        nums.2 <- temp
    }

    for (num in nums.1)
    {
        ind <- match(num, nums.2)
        if (is.na(ind))
            return(FALSE)
        else
            nums.2 <- nums.2[-ind]
    }
    TRUE
}

# Checks that a variable doesn't violate variables.to.not.combine when it is
# combined into a row of other variables. Doesn't check that variable types
# are compatible (this is done later in unmatchVariablesOfDifferentTypes).
# See unit tests in test-mergedatasetsbycase.R
isVariableCombinableIntoRow <- function(name.to.combine,
                                        data.set.ind,
                                        matched.names.row,
                                        v.names.to.not.combine)
{
    if (is.null(v.names.to.not.combine))
        return(TRUE)

    matched.names.row[data.set.ind] <- name.to.combine

    all(apply(v.names.to.not.combine, 1, function(nms) {
        sum(nms == matched.names.row, na.rm = TRUE) < 2
    }))
}

# Checks that a variable's type is compatible with those of variables in a row
# (representing variables to be merged)
isVariableTypeCompatible <- function(variable.name, data.set.ind, matched.names.row,
                                     input.data.sets.metadata, data.sets)
{
    md <- input.data.sets.metadata
    var.ind <- match(variable.name, md$variable.names[[data.set.ind]])
    var.type <- md$variable.types[[data.set.ind]][var.ind]
    var.vals <- data.sets[[data.set.ind]][[var.ind]]
    var.val.attr <- md$variable.value.attributes[[data.set.ind]][[var.ind]]

    non.missing.ind <- which(!is.na(matched.names.row))
    row.vars.ind <- vapply(non.missing.ind, function(i) {
        match(matched.names.row[i], md$variable.names[[i]])
    }, integer(1))
    row.vars.types <- vapply(seq_along(row.vars.ind), function(i) {
        md$variable.types[[non.missing.ind[i]]][row.vars.ind[i]]
    }, character(1))
    row.vars.vals <- lapply(seq_along(row.vars.ind), function(i) {
        data.sets[[non.missing.ind[i]]][[row.vars.ind[i]]]
    })
    row.vars.val.attr <- lapply(seq_along(row.vars.ind), function(i) {
        md$variable.value.attributes[[non.missing.ind[i]]][[row.vars.ind[i]]]
    })

    if (var.type == NUMERIC.VARIABLE.TYPE)
    {
        if (any(isDateType(row.vars.types)))
            return(isConvertibleToDateTime(var.vals))

        if (DURATION.VARIABLE.TYPE %in% row.vars.types)
            return(FALSE)

        if (CATEGORICAL.VARIABLE.TYPE %in% row.vars.types)
            return(isConvertibleToCategorical(NUMERIC.VARIABLE.TYPE, var.vals,
                                              row.vars.val.attr[row.vars.types == CATEGORICAL.VARIABLE.TYPE],
                                              20))
        return(TRUE)
    }
    else if (var.type == TEXT.VARIABLE.TYPE)
    {
        if (any(isDateType(row.vars.types)))
            return(isParsableAsDateTime(var.vals))

        if (DURATION.VARIABLE.TYPE %in% row.vars.types)
            return(FALSE)

        if (CATEGORICAL.VARIABLE.TYPE %in% row.vars.types)
            return(isConvertibleToCategorical(TEXT.VARIABLE.TYPE, var.vals,
                                              row.vars.val.attr[row.vars.types == CATEGORICAL.VARIABLE.TYPE],
                                              20))
        return(TRUE)
    }
    else if (isDateType(var.type))
    {
        if (any(isDateType(row.vars.types)))
            return(TRUE)
        if (any(c(CATEGORICAL.VARIABLE.TYPE, DURATION.VARIABLE.TYPE) %in% row.vars.types))
            return(FALSE)

        for (i in seq_along(row.vars.types))
        {
            if (row.vars.types[i] == NUMERIC.VARIABLE.TYPE)
            {
                if (!isConvertibleToDateTime(row.vars.vals[[i]]))
                    return(FALSE)
            }
            else # row.vars.types[i] == TEXT.VARIABLE.TYPE
            {
                if (!isParsableAsDateTime(row.vars.vals[[i]]))
                    return(FALSE)
            }
        }
        return(TRUE)
    }
    else if (var.type == DURATION.VARIABLE.TYPE)
        return(all(row.vars.types == DURATION.VARIABLE.TYPE))
    else if (var.type == CATEGORICAL.VARIABLE.TYPE)
    {
        if (DURATION.VARIABLE.TYPE %in% row.vars.types || any(isDateType(row.vars.types)))
            return(FALSE)
        for (i in seq_along(row.vars.types))
        {
            val.attrs <- c(list(var.val.attr),
                           row.vars.val.attr[row.vars.types == CATEGORICAL.VARIABLE.TYPE])
            if(!isConvertibleToCategorical(row.vars.types[i], row.vars.vals[[i]],
                                           val.attrs, 20))
                return(FALSE)
        }
        return(TRUE)
    }
    else
        stop("Variable type not recognised")
}

# See unit tests in test-mergedatasetsbycase.R
isMissingValue <- function(text)
{
    tolower(trimws(text)) %in% c(NA, "", "na", "n/a", "-")
}

# Tests if strings are parsable as numeric.
# See unit tests in test-mergedatasetsbycase.R
isParsableAsNumeric <- function(text)
{
    missing.ind <- isMissingValue(text)
    all(!is.na(suppressWarnings(as.numeric(text[!missing.ind]))))
}

# Tests if strings parsable as date/time.
# See unit tests in test-mergedatasetsbycase.R
#' @importFrom flipTime AsDate AsDateTime
isParsableAsDateTime <- function(text)
{
    missing.ind <- isMissingValue(text)
    all(!is.na(AsDateTime(text[!missing.ind], on.parse.failure = "silent")))
}

# Are numeric values seconds that can be converted to date time
# See unit tests in test-mergedatasetsbycase.R
isConvertibleToDateTime <- function(num)
{
    missing.ind <- is.na(num)
    # seconds from 1970/1/1 between years 1990 and 2050
    num[!missing.ind] >= 631152000 && num[!missing.ind] <= 2524608000
}

isConvertibleToCategorical <- function(variable.type, values, val.attrs,
                                       max.unique.values)
{
    if (variable.type == CATEGORICAL.VARIABLE.TYPE)
        return(TRUE)

    if (variable.type %in% c(DATE.VARIABLE.TYPE, DATE.TIME.VARIABLE.TYPE,
                             DURATION.VARIABLE.TYPE))
        return(FALSE)

    n.category.values <- length(unique(unlist(lapply(val.attrs, as.character))))
    return(length(unique(values)) <= max(2 * n.category.values,
                                           max.unique.values))
}

# Determine merged variable names from the matrix of matched names
# See unit tests in test-mergedatasetsbycase.R
mergedVariableNames <- function(matched.names, use.names.and.labels.from)
{
    merged.names <- namesFromEarliestDataSet(matched.names,
                                             use.names.and.labels.from)

    # Merged names may contain duplicate variable names due to the user
    # specifying variables with the same name to not be combined or variables
    # with the same name not being combined as their types are incompatible.
    # We rename variables so that the names are unique.
    dup <- which(duplicated(merged.names))
    renamed.variables <- matrix(nrow = length(dup), ncol = 2)
    colnames(renamed.variables) <- c("Original name", "New name")
    for (i in seq_along(dup))
    {
        new.name <- uniqueName(merged.names[dup[i]], merged.names, "_")
        renamed.variables[i, ] <- c(merged.names[dup[i]], new.name)
        merged.names[dup[i]] <- new.name
    }
    attr(merged.names, "renamed.variables") <- renamed.variables

    merged.names
}

# Get the names from the earliest data set for each row in matched.names
# (use the latest data set if use.names.and.labels.from == "Last data set")
namesFromEarliestDataSet <- function(matched.names, use.names.and.labels.from)
{
    apply(matched.names, 1, function(nms) {
        if (use.names.and.labels.from == "First data set")
            removeNA(nms)[1]
        else
            removeNA(rev(nms))[1]
    })
}

# Order matched names based on the original order of the input variables
orderMatchedNames <- function(matched.names, input.data.sets.metadata,
                              use.names.and.labels.from)
{
    n.data.sets <- input.data.sets.metadata$n.data.sets
    v.names <- input.data.sets.metadata$variable.names

    # Convert list of variable names to list of row indices relative to the
    # matched names matrix, removing any names that do not appear in
    # matched.names (i.e., those that have been omitted).
    v.indices <- lapply(seq_len(n.data.sets), function(i) {
        removeNA(match(v.names[[i]], matched.names[, i]))
    })

    # We want to keep the variables that will have the same merged names
    # (before deduplication) together.
    nms <- namesFromEarliestDataSet(matched.names,
                                    use.names.and.labels.from)
    tab <- table(nms)
    duplicated.names <- names(tab[tab > 1])
    indices.to.keep.togther <- lapply(duplicated.names,
                                      function(nm) which(nm == nms))

    ordering <- mergeIndicesList(indices.list = v.indices,
                                 prefer.first.element = use.names.and.labels.from == "First data set",
                                 indices.to.keep.togther = indices.to.keep.togther)

    ordered.matched.names <- matched.names[ordering, , drop = FALSE]
    attr(ordered.matched.names, "non.combinable.variables") <- attr(matched.names, "non.combinable.variables")
    attr(ordered.matched.names, "is.fuzzy.match") <- attr(matched.names, "is.fuzzy.match")[ordering, , drop = FALSE]
    attr(ordered.matched.names, "matched.by") <- attr(matched.names, "matched.by")[ordering, , drop = FALSE]
    ordered.matched.names
}

# Takes a list of character vectors each containing indices in a certain order
# and merges them into a single integer vector, respecting the order in
# each vector as much as possible, with earlier vectors taking precedence
# in case of ties (or later vectors if use.names.and.labels.from == "Last data set").
# See unit tests in test-mergedatasetsbycase.R
mergeIndicesList <- function(indices.list, prefer.first.element,
                             indices.to.keep.togther = NULL)
{
    # A set of indices are kept together by replacing all in a set with a
    # representative, which is then replaced with the set after merging
    if (!is.null(indices.to.keep.togther))
    {
        representative.indices <- vapply(indices.to.keep.togther, `[`,
                                         integer(1), 1)
        indices.list <- lapply(seq_along(indices.list), function(j) {
            indices <- indices.list[[j]]
            for (i in seq_along(indices.to.keep.togther))
                indices[indices %in% indices.to.keep.togther[[i]]] <- representative.indices[i]
            indices
        })
    }
    else
        representative.indices <- integer(0)

    if (!prefer.first.element)
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
                                indices.to.keep.togther[[ind]])
        else
            merged.indices <- c(merged.indices, selected.index)

        # Remove selected index from indices.list
        indices.list <- lapply(indices.list, setdiff, selected.index)

        # Remove empty list elements
        indices.list <- indices.list[vapply(indices.list, length, integer(1)) > 0]
    }
    merged.indices
}

# Construct the merged data set as a data frame
#' @importFrom utils object.size
mergedDataSet <- function(data.sets, matched.names, merged.names,
                          use.names.and.labels.from,
                          when.multiple.labels.for.one.value,
                          match.parameters)
{
    n.vars <- nrow(matched.names)
    n.data.set.cases <- vapply(data.sets, nrow, integer(1))

    merged.data.set <- vector(mode = "list", length = n.vars)
    data.set.size <- 0
    for (i in seq_len(n.vars))
    {
        v <- compositeVariable(matched.names[i, ], data.sets,
                               use.names.and.labels.from,
                               when.multiple.labels.for.one.value,
                               match.parameters)
        data.set.size <- data.set.size + object.size(v)
        if (data.set.size > DATA.SET.SIZE.LIMIT)
            stop("The merged data set is too large to create. ",
                 "Consider omitting variables or only keeping merged variables that contain input variables from a few data sets.")
        merged.data.set[[i]] <- v
    }
    merged.data.set <- data.frame(merged.data.set)
    names(merged.data.set) <- merged.names

    mergesrc.name <- uniqueName("mergesrc", names(merged.data.set), "_")
    merged.data.set[[mergesrc.name]] <- mergeSrc(n.data.set.cases,
                                                 names(data.sets))
    merged.data.set
}

# Combine variables from different data sets (end-to-end) to create a
# composite variable.
# matched.names.row is an integer vector with elements corresponding to the
# input data sets. Each element is a variable index in an input data set.
# The variables in these indices are to be combined into one variable.
# The vector is a row from the matched.names matrix.
compositeVariable <- function(matched.names.row, data.sets,
                              use.names.and.labels.from,
                              when.multiple.labels.for.one.value,
                              match.parameters)
{
    n.data.sets <- length(data.sets)
    var.list <- lapply(seq_len(n.data.sets), function(i) {
         if(!is.na(matched.names.row[i]))
             data.sets[[i]][[matched.names.row[i]]]
        else
            NULL
    })
    v.types <- vapply(var.list, variableType, character(1))



    result <- if (isCombinedAsCategoricalVariable(var.list, v.types))
        combineAsCategoricalVariable(var.list, data.sets,
                                     use.names.and.labels.from, v.types,
                                     when.multiple.labels.for.one.value,
                                     match.parameters)
    else
        combineAsNonCategoricalVariable(var.list, data.sets, v.types)

    attr(result, "label") <- variableLabelFromDataSets(matched.names.row,
                                                       data.sets,
                                                       use.names.and.labels.from)

    result
}

isCombinedAsCategoricalVariable <- function(var.list, v.types)
{
    if (CATEGORICAL.VARIABLE.TYPE %in% removeNA(v.types))
    {
        combine.as.categorical.var <- TRUE
        val.attrs <- lapply(which(v.types == CATEGORICAL.VARIABLE.TYPE), function(i) {
            attr(var.list, "labels", exact = TRUE)
        })
        for (i in seq_along(v.types))
        {
            if (!is.null(var.list[[i]]) &&
                !isConvertibleToCategorical(v.types[i], var.list[[i]], val.attrs,
                                            100))
            {
                combine.as.categorical.var <- FALSE
                break
            }
        }
        combine.as.categorical.var
    }
    else
        FALSE
}

# Combine variables in var.list as a categorical variable
# matched.names.row is an integer vector with elements corresponding to the
# input data sets. Each element is a variable index in an input data set.
# The variables in these indices are to be combined into one variable.
# The vector is a row from the matched.names matrix.
combineAsCategoricalVariable <- function(var.list, data.sets,
                                         use.names.and.labels.from, v.types,
                                         when.multiple.labels.for.one.value,
                                         match.parameters)
{
    val.attr.list <- lapply(var.list, attr, "labels")

    cat.ind <- which(v.types == CATEGORICAL.VARIABLE.TYPE)
    if (use.names.and.labels.from == "Last data set")
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
                                                   when.multiple.labels.for.one.value,
                                                   match.parameters)
            map <- attr(merged.val.attr, "map")
        }
        if (nrow(map) > 0)
            value.map[[i]] <- map
    }
    attr(merged.val.attr, "map") <- NULL

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
        input.val.attr[[i]] <- rep(NA_real_, length(merged.val.attr))

        if (is.null(v))
            result <- c(result, rep(NA, nrow(data.sets[[i]])))
        else if (v.types[i] == TEXT.VARIABLE.TYPE)
        {
            is.missing <- isMissingValue(v)
            # It is necessary to call as.character to remove potential excess
            # classes that cause issues with isParsableAsNumeric
            unique.v <- unique(as.character(v[!is.missing]))

            # text becomes categorical (numeric) values
            if (isParsableAsNumeric(unique.v))
            {
                var.values <- rep(NA_integer_, length(v))
                if (any(!is.missing))
                    var.values[!is.missing] <- as.numeric(as.character(v[!is.missing]))
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
            else
            {
                var.values <- numeric(length(v))

                for (text.val in unique.v)
                {
                    if (text.val %in% names(merged.val.attr)) # match value label in merged.val.attr
                    {
                        ind <- text.val == names(merged.val.attr)
                        merged.val <- unname(merged.val.attr[ind])
                        input.val.attr[[i]][ind] <- merged.val
                        var.values[text.val == v] <- merged.val
                    }
                    else # not found in merged.val.attr, add as numeric value
                    {
                        new.ind <- length(merged.val.attr) + 1
                        new.val <- ceiling(max(merged.val.attr)) + 1
                        merged.val.attr[new.ind] <- new.val
                        names(merged.val.attr)[new.ind] <- text.val
                        input.val.attr[[i]][new.ind] <- text.val
                        var.values[text.val == v] <- new.val
                    }
                }
                result <- c(result, var.values)
            }
        }
        else if (v.types[i] == NUMERIC.VARIABLE.TYPE)
        {
            unique.v <- unique(removeNA(v))
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
                    if (!is.na(ind)) # input value was mapped to val
                        return(map[ind, 1])
                    else
                    {
                        ind <- match(val, map[, 1])
                        # input value was mapped away from val, so input corresponding to val is NA
                        if (!is.na(ind))
                            return(NA_real_)
                    }
                }
                if (val %in% val.attr.list[[i]])
                    return(val)
                else
                    return(NA_real_)
            }, numeric(1), USE.NAMES = FALSE)
        }
    }

    if (isIntegerValued(result))
    {
        result <- as.integer(result)
        nms <- names(merged.val.attr)
        merged.val.attr <- as.integer(merged.val.attr)
        names(merged.val.attr) <- nms
    }

    attr(result, "labels") <- merged.val.attr
    attr(result, "input.value.attributes") <- input.val.attr
    class(result) <- c(class(result), "haven_labelled")

    result
}

#' @description  Extract the value from val.attr given a label, also dealing
#'  with the special case where label == "". If it wasn't for this special case
#'  we could just call unname(val.attr[label]).
#' @param val.attr A named numeric vector representing value attributes, where
#'  the value names are the labels.
#' @param label A character scalar of a label for which a value is to be returned.
#' @return A numeric scalar of the value corresponding to the label.
#' @noRd
labelValue <- function(val.attr, label)
{
    if (label != "")
        unname(val.attr[label])
    else
        # need to do this since val.attr[""] will return NA
        unname(val.attr[names(val.attr) == ""])
}

#' @description  Merge a value attribute (val and lbl) into merged.val.attr.
#'  val and lbl come from an input categorical variable and this
#'  function tries to merge it into merged.val.attr, which will be the value
#'  attributes for the merged variable. This function is run iteratively
#'  over each value attribute in each categorical input variable to build
#'  merged.val.attr and the mapping matrix map.
#'
#'  If val and lbl match (or lbl fuzzy matches) those of a value in
#'  merged.val.attr, then merged.val.attr is unchanged.
#'  If lbl matches (or fuzzy matches) a label in merged.val.attr but the
#'  values are different, merged.val.attr is unchanged as well but a row is
#'  added to map representing a mapping from val to the value in merged.val.attr
#'  corresponding to the label.
#'  If val matches a value in merged.val.attr but the labels don't match (or fuzzy
#'  match), and when.multiple.labels.for.one.value is "Create new values for the labels",
#'  then a new value is generated and added to merged.val.attr with the label lbl.
#'  Otherwise if when.multiple.labels.for.one.value is "Use one of the labels",
#'  then the existing label in merged.val.attr is used (merged.val.attr is unchanged).
#'  If neither val or lbl matches anything in merged.val.attr, then they are added
#'  to merged.val.attr.
#' @param val Numeric scalar of the value to be merged
#' @param lbl Character scalar of the label to be merged
#' @param merged.val.attr Named numeric vector of (incomplete) value attributes
#' (values and labels) of the merged categorical variable. This is iteratively
#' added to with each call of this function and it starts out empty.
#' @param map Numeric matrix where each row represents a mapping from one value
#'  to another. The first column contains the original values and the second column
#'  contains the new values. The contents of this matrix are not used in the function,
#'  it is only augmented with a mapping if necessary and returned as an attribute
#'  of the output object. This matrix is used to map input variable values to
#'  different values when creating the merged variable.
#' @param when.multiple.labels.for.one.value See documentation for this in MergeDataSetsByCase
#' @param match.parameters Parameters used for fuzzy matching of names and labels.
#' @return Returns a possibly augmented merged.val.attr, with the attribute "map"
#'  containing the matrix map.
#' @noRd
mergeValueAttribute <- function(val, lbl, merged.val.attr, map,
                                when.multiple.labels.for.one.value,
                                match.parameters)
{
    if (lbl %in% names(merged.val.attr))
    {
        merged.val <- labelValue(merged.val.attr, lbl)
        if (val != merged.val) # same label with different values
        {
            map <- rbind(map, c(val, merged.val), deparse.level = 0) # use the value in merged.val.attr
        }
        # else: same label, same value, no action required as it is already in merged.val.attr
    }
    else
    {
        match.percentages <- matchPercentagesForValueLabels(lbl = lbl,
                                                            lbls.to.compare.against = names(merged.val.attr),
                                                            match.parameters = match.parameters)
        if (max(match.percentages) >= match.parameters$min.value.label.match.percentage) # label is close enough
        {
            merged.val <- unname(merged.val.attr[which.max(match.percentages)])
            if (merged.val != val)
            {
                map <- rbind(map, c(val, merged.val), deparse.level = 0) # use the value in merged.val.attr
            }
            # else: similar label, same value, no action required as we treat
            #       them as the same and one of them is already in merged.val.attr
        }
        else if (val %in% merged.val.attr) # different labels with same value
        {
            if (when.multiple.labels.for.one.value == "Create new values for the labels")
            {
                new.value <- ceiling(max(merged.val.attr)) + 1
                merged.val.attr[lbl] <- new.value
                map <- rbind(map, c(val, new.value), deparse.level = 0)
            }
            # else "Use one of the labels", no action required as it is already in merged.val.attr
        }
        else # value and label not in merged.val.attr
            merged.val.attr[lbl] <- val # create new value in merged.val.attr
    }

    attr(merged.val.attr, "map") <- map
    merged.val.attr
}

# Combine variables in var.list into a (non-categorical) variable
combineAsNonCategoricalVariable <- function(var.list, data.sets, v.types)
{
    unique.v.types <- unique(removeNA(v.types))

    if (DATE.TIME.VARIABLE.TYPE %in% unique.v.types &&
        all(unique.v.types %in% c(DATE.VARIABLE.TYPE, DATE.TIME.VARIABLE.TYPE,
                                  NUMERIC.VARIABLE.TYPE, TEXT.VARIABLE.TYPE)))
    {
        return(combineAsDateTimeVariable(var.list, data.sets, v.types))
    }
    else if (DATE.VARIABLE.TYPE %in% unique.v.types &&
             all(unique.v.types %in% c(DATE.VARIABLE.TYPE, NUMERIC.VARIABLE.TYPE,
                                       TEXT.VARIABLE.TYPE)))
    {
        return(combineAsDateVariable(var.list, data.sets, v.types))
    }
    else if (DURATION.VARIABLE.TYPE %in% unique.v.types &&
             all(unique.v.types %in% c(DURATION.VARIABLE.TYPE, TEXT.VARIABLE.TYPE)))
    {
        return(combineAsDurationVariable(var.list, data.sets, v.types))
    }
    else if (NUMERIC.VARIABLE.TYPE %in% unique.v.types &&
             all(unique.v.types %in% c(NUMERIC.VARIABLE.TYPE, TEXT.VARIABLE.TYPE)))
    {
        text.ind <- which(v.types == TEXT.VARIABLE.TYPE)
        is.parsable.as.numeric <- all(vapply(text.ind, function(j) {
            isParsableAsNumeric(var.list[[j]])
        }, logical(1)))

        if (is.parsable.as.numeric)
            return(combineAsNumericVariable(var.list, data.sets, v.types))
        else
            return(combineAsTextVariable(var.list, data.sets, v.types))
    }
    else if (unique.v.types == TEXT.VARIABLE.TYPE)
    {
        return(combineAsTextVariable(var.list, data.sets, v.types))
    }
    else if (CATEGORICAL.VARIABLE.TYPE %in% unique.v.types)
    {
        # If there are any categorical variables, convert everything into text.
        # This only occurs when categorical is combined with date, date/time or
        # duration variables or there are too many unique values in numeric or
        # text variables.
        return(combineAsTextVariable(var.list, data.sets, v.types))
    }
    else
    {
        # Don't expect this to ever occur
        stop("Unhandled variable types combination: ",
             paste0(unique.v.types, collapse = ", "))
    }
}

# Combine variables in var.list into a date-time variable
# v.types can be text, numeric, date and date/time
#' @importFrom lubridate as_datetime
combineAsDateTimeVariable <- function(var.list, data.sets, v.types)
{
    do.call("c", lapply(seq_along(data.sets), function(i) {
        v <- var.list[[i]]
        if (is.null(v))
            AsDateTime(rep(NA_character_, nrow(data.sets[[i]])))
        else if (v.types[i] == TEXT.VARIABLE.TYPE)
            parseTextVariable(v, AsDateTime)
        else if (v.types[i] == NUMERIC.VARIABLE.TYPE)
            parseNumericVariable(v, as_datetime)
        else if (v.types[i] == DATE.VARIABLE.TYPE)
            AsDateTime(as.character(v))
        else # v.types[i] == DATE.TIME.VARIABLE.TYPE
            v
    }))
}

# Combine variables in var.list into a date variable
# v.types can be text, numeric and date
#' @importFrom lubridate as_date
combineAsDateVariable <- function(var.list, data.sets, v.types)
{
    do.call("c", lapply(seq_along(data.sets), function(i) {
        v <- var.list[[i]]
        if (is.null(v))
            AsDate(rep(NA_character_, nrow(data.sets[[i]])))
        else if (v.types[i] == TEXT.VARIABLE.TYPE)
            parseTextVariable(v, AsDate)
        else if (v.types[i] == NUMERIC.VARIABLE.TYPE)
            parseNumericVariable(v, as_date)
        else # v.types[i] == DATE.VARIABLE.TYPE
            v
    }))
}

# Combine variables in var.list into a duration variable
# v.types can be text and duration
combineAsDurationVariable <- function(var.list, data.sets, v.types)
{
    do.call("c", lapply(seq_along(data.sets), function(i) {
        v <- var.list[[i]]
        if (is.null(v))
            as.difftime(rep(NA_character_, nrow(data.sets[[i]])))
        else if (v.types[i] == TEXT.VARIABLE.TYPE)
            parseTextVariable(v, as.difftime)
        else # v.types[i] == DURATION.VARIABLE.TYPE
            v
    }))
}

# Combine variables in var.list into a numeric variable
# v.types can be text and numeric
combineAsNumericVariable <- function(var.list, data.sets, v.types)
{
    result <- do.call("c", lapply(seq_along(data.sets), function(i) {
        v <- var.list[[i]]
        if (is.null(v))
            rep(NA_real_, nrow(data.sets[[i]]))
        else if (v.types[i] == TEXT.VARIABLE.TYPE)
            parseTextVariable(v, as.numeric)
        else # v.types[i] == NUMERIC.VARIABLE.TYPE
            v
    }))

    if (isIntegerValued(result))
        result <- as.integer(result)

    return(result)
}

# Combine variables in var.list into a text variable
# v.types can be text, numeric and categorical
combineAsTextVariable <- function(var.list, data.sets, v.types)
{
    do.call("c", lapply(seq_along(data.sets), function(i) {
        v <- var.list[[i]]
        if (is.null(v))
            rep(NA_character_, nrow(data.sets[[i]]))
        else if (v.types[i] == CATEGORICAL.VARIABLE.TYPE)
        {
            result <- rep(NA_character_, nrow(data.sets[[i]]))
            val.attr <- attr(var.list[[i]], "labels")
            for (j in seq_along(val.attr))
                result[v == val.attr[j]] <- names(val.attr)[j]
            result
        }
        else if (v.types[i] == NUMERIC.VARIABLE.TYPE)
            as.character(v)
        else # v.types[i] == TEXT.VARIABLE.TYPE
            v
    }))
}

#' @param text.variable A character vector representing a text variable.
#' @param parser A function that parses a character vector into a vector of
#'  some other type.
#' @return A vector of type determined by the parser.
#' @noRd
parseTextVariable <- function(text.variable, parser)
{
    missing.ind <- isMissingValue(text.variable)
    result <- parser(rep(NA_character_, length(text.variable)))
    result[!missing.ind] <- parser(text.variable[!missing.ind])
    result
}

#' @param numeric.variable A numeric vector representing a text variable.
#' @param parser A function that parses a numeric vector into a vector of
#'  some other type.
#' @return A vector of type determined by the parser.
#' @noRd
parseNumericVariable <- function(numeric.variable, parser)
{
    missing.ind <- is.na(numeric.variable)
    parsed <- parser(rep(NA_real_, length(numeric.variable)))
    parsed[!missing.ind] <- parser(numeric.variable[!missing.ind])
    parsed
}

# Remap values in variable given a value map
remapValuesInVariable <- function(variable, map)
{
    result <- variable
    for (i in seqRow(map))
        result[variable == map[i, 1]] <- map[i, 2]
    result
}

# Get a variable label from matched.names.row
variableLabelFromDataSets <- function(matched.names.row, data.sets,
                                      use.names.and.labels.from)
{
    ind <- if (use.names.and.labels.from == "First data set")
        seq_along(data.sets)
    else
        rev(seq_along(data.sets))

    for (i in ind)
    {
        data.set <- data.sets[[i]]

        if (!is.na(matched.names.row[i]))
        {
            v <- data.set[[matched.names.row[i]]]
            lbl <- attr(v, "label", exact = TRUE)
            if (!is.null(lbl))
                return(lbl)
        }
    }
    return("")
}

# Create `Source of cases` variable for the output file, which allows the consumer to determine which input file each variable came from.
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

# Return a list of names of variables in each data set omitted from the merged
# data set
omittedVariables <- function(input.data.sets.metadata, matched.names)
{
    lapply(seq_len(input.data.sets.metadata$n.data.sets), function(i) {
        nms <- input.data.sets.metadata$variable.names[[i]]
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

removeNULL <- function(x)
{
    x[!vapply(x, is.null, logical(1))]
}

#' @importFrom flipFormat DataSetMergingByCaseWidget
#' @export
print.MergeDataSetByCase <- function(x, ...)
{
    DataSetMergingByCaseWidget(x$input.data.sets.metadata,
                               x$merged.data.set.metadata,
                               x$matched.names,
                               x$merged.names,
                               x$omitted.variable.names,
                               x$input.value.attributes,
                               x$is.saved.to.cloud)
}
