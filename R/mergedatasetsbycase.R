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
#' @param auto.select.what.to.match.by If TRUE, the metadata to match by is
#'  chosen automatically, whereas if FALSE, the metadata to match by is
#'  specified by setting the flags \code{match.by.variable.names},
#'  \code{match.by.variable.labels} and match.by.value.labels.
#' @param match.by.variable.names Whether to match using variable names.
#' @param match.by.variable.labels Whether to match using variable names.
#' @param match.by.value.labels Whether to match using value labels of
#'  categorical variables.
#' @param ignore.case Ignore case when matching text.
#' @param ignore.non.alphanumeric Ignore non-alphanumeric characters when
#'  matching text except when numeric characters appear both before and after
#'  non-alphanumeric characters e.g., "24 - 29", in which case the characters
#'  are still ignored but the separation between the numbers is noted.
#' @param min.match.percentage To be decided, possibly a percentage.
#' @param variables.to.combine A character vector of comma-separated
#'  variable names indicating which variables are to appear together.
#'  Ranges of variables can be specified by separating variable names by '-'.
#'  Variables can be specified from specific data sets by appending '(x)' to
#'  the variable name where x is the data set index.
#' @param variables.to.not.combine A character vector of comma-separated variable
#'  names specifying variables that should never be combined together.
#'  To specify variables from a specific data set, suffix variable names
#'  with the data set index in parentheses, e.g., 'Q2(3)'.
#' @param varaibles.to.keep Character vector of variable names to keep in
#'  the merged data set. To specify variables from a specific data set,
#'  suffix the name with the data set index in parentheses, e.g., 'Q2(3)'.
#'  Ranges of variables can be specified by separating variable names by '-'.
#' @param variables.to.omit Character vector of variable names to omit from
#'  the merged data set. To specify variables from a specific data set,
#'  suffix the name with the data set index in parentheses, e.g., 'Q2(3)'.
#'  Ranges of variables can be specified by separating variable names by '-'.
#' @param include.merged.data.set.in.output Whether to include the merged data
#'  set in the output.
#' @param when.multiple.labels.for.one.value Either "Use one of the labels" or
#'  "Create new values for the labels". When the former is chosen, the label
#'  from the earliest/latest data set will be chosen if use.names.and.labels.from
#'  is "First data set"/"Last data set". If the latter is chosen, new values
#'  are generated for the extra labels.
#' @param use.names.and.labels.from Either "First data set" or "Last data set".
#'  This sets the preference for either the first or last data set when
#'  choosing which names and labels to use in the merged data set.
#' @param data.sets.whose.variables.are.kept An integer vector of indices of data
#'  sets whose variables are to be kept. Any variable not in these data sets
#'  will not be included in the merged data set.
#' @param min.value.label.match.percentage The minimum percentage match for
#'  value labels to be considered the same when combining value attributes
#'  from different variables.
#'
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
                                     input.data.sets.metadata$data.set.names,
                                     when.multiple.labels.for.one.value,
                                     match.parameters)
    merged.data.set.name <- cleanMergedDataSetName(merged.data.set.name,
                                                   data.set.names)

    writeDataSet(merged.data.set, merged.data.set.name)

    outputForMergeDataSetsByCase(merged.data.set, input.data.sets.metadata,
                                 matched.names, merged.names,
                                 include.merged.data.set.in.output,
                                 merged.data.set.name)
}

# TODO

# Need to ensure any new variable names we generate are valid for sav files, e.g. not too long

matchVariables <- function(input.data.set.metadata, match.parameters,
                           variables.to.combine, variables.to.not.combine,
                           variables.to.keep, variables.to.omit, data.sets,
                           data.sets.whose.variables.are.kept,
                           use.names.and.labels.from)
{
    v.names.to.combine <- parseVariablesToCombine(variables.to.combine,
                                                  input.data.set.metadata)
    v.names.to.not.combine <- parseVariablesToNotCombine(variables.to.not.combine,
                                                         input.data.set.metadata)
    v.names.to.keep <- parseVariablesToKeep(variables.to.keep,
                                            input.data.set.metadata)
    v.names.to.omit <- parseVariablesToOmit(variables.to.omit,
                                            input.data.set.metadata)
    checkMatchVariablesInputs(v.names.to.combine, v.names.to.not.combine,
                              v.names.to.keep, v.names.to.omit,
                              data.sets.whose.variables.are.kept,
                              input.data.set.metadata)

    v.names <- input.data.set.metadata$variable.names
    v.labels <- input.data.set.metadata$variable.labels
    v.val.attrs <- input.data.set.metadata$variable.value.attributes
    v.types <- input.data.set.metadata$variable.types
    n.data.sets <- input.data.set.metadata$n.data.sets

    remaining.names <- lapply(seq_len(n.data.sets), function(i) {
        result <- v.names[[i]]
        if (!is.null(v.names.to.combine))
            result <- setdiff(result, v.names.to.combine[, i])
        if (!is.null(v.names.to.omit))
            result <- setdiff(result, v.names.to.omit[, i])
        result
    })
    remaining.ind <- mapply(match, remaining.names, v.names, SIMPLIFY = FALSE)
    remaining.labels <- mapply(function(x, y) x[y], v.labels, remaining.ind,
                               SIMPLIFY = FALSE)
    remaining.val.attrs <-  mapply(function(x, y) x[y], v.val.attrs, remaining.ind,
                                   SIMPLIFY = FALSE)

    if (match.parameters$auto.select.what.to.match.by)
        match.parameters <- autoSelectWhatToMatchBy(input.data.set.metadata,
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
        not.used.for.matching <- attr(v.names.to.combine,
                                      "is.data.set.specified")
        output <- findMatchesForRows(matched.names, seqRow(matched.names),
                                     seq_len(n.data.sets),
                                     v.names, v.labels, v.val.attrs, v.types,
                                     remaining.names, remaining.labels,
                                     remaining.val.attrs,
                                     use.names.and.labels.from,
                                     v.names.to.not.combine,
                                     match.parameters, data.sets,
                                     is.fuzzy.match, matched.by,
                                     not.used.for.matching)
        matched.names <- output$matched.names
        is.fuzzy.match <- output$is.fuzzy.match
        matched.by <- output$matched.by
        remaining.names <- output$remaining.names
        remaining.labels <- output$remaining.labels
        remaining.val.attrs <- output$remaining.val.attrs
    }

    # d.ind <- data.sets.whose.variables.are.kept # shorten name
    # d.ind <- sort(d.ind, decreasing = use.names.and.labels.from == "Last data set")
    d.ind <- if (use.names.and.labels.from == "First data set")
        seq_len(n.data.sets)
    else
        rev(seq_len(n.data.sets))

    # Find matches for remaining labels
    for (i in d.ind)
    {
        if (i %in% data.sets.whose.variables.are.kept)
        {
            nms.to.find.matches.for <- remaining.names[[i]]
            remaining.names[[i]] <- character(0)
            remaining.labels[[i]] <- character(0)
            remaining.val.attrs[[i]] <- list()
        }
        else if (!is.null(variables.to.keep) &&
                 any(!is.na(variables.to.keep[, i])))
        {
            nms.to.find.matches.for <- intersect(removeNA(variables.to.keep[, i]),
                                                 remaining.names[[i]])
            ind <- match(nms.to.find.matches.for, remaining.names[[i]])
            remaining.names[[i]] <- remaining.names[[i]][-ind]
            remaining.labels[[i]] <- remaining.labels[[i]][-ind]
            remaining.val.attrs[[i]] <- remaining.val.attrs[[i]][-ind]
        }
        else
            next

        other.data.set.indices <- d.ind[-i]

        new.rows <- matrix(NA_character_, nrow = length(nms.to.find.matches.for),
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

        output <- findMatchesForRows(matched.names, row.indices,
                                     other.data.set.indices,
                                     v.names, v.labels, v.val.attrs, v.types,
                                     remaining.names, remaining.labels,
                                     remaining.val.attrs,
                                     use.names.and.labels.from,
                                     v.names.to.not.combine,
                                     match.parameters, data.sets,
                                     is.fuzzy.match, matched.by)
        matched.names <- output$matched.names
        is.fuzzy.match <- output$is.fuzzy.match
        matched.by <- output$matched.by
        remaining.names <- output$remaining.names
        remaining.labels <- output$remaining.labels
        remaining.val.attrs <- output$remaining.val.attrs
    }
    attr(matched.names, "is.fuzzy.match") <- is.fuzzy.match
    attr(matched.names, "matched.by") <- matched.by

    matched.names <- orderMatchedNames(matched.names,
                                       input.data.set.metadata,
                                       use.names.and.labels.from)

    attr(matched.names, "match.parameters") <- match.parameters
    matched.names
}

findMatchesForRows <- function(matched.names, row.indices, data.set.indices,
                               v.names, v.labels, v.val.attrs, v.types,
                               remaining.names, remaining.labels,
                               remaining.val.attrs,
                               use.names.and.labels.from,
                               v.names.to.not.combine,
                               match.parameters, data.sets,
                               is.fuzzy.match, matched.by,
                               not.used.for.matching = NULL)
{
    n.data.sets <- length(data.sets)
    n.rows <- nrow(matched.names)
    matching.names <- matrix(NA_character_, nrow = n.rows, ncol = n.data.sets)
    matching.names.percentage <- matrix(NA_real_, nrow = n.rows,
                                        ncol = n.data.sets)
    matching.names.is.fuzzy <- matrix(FALSE, nrow = n.rows, ncol = n.data.sets)
    matching.names.matched.by <- matrix(NA_character_, nrow = n.rows,
                                        ncol = n.data.sets)
    if (is.null(not.used.for.matching))
        not.used.for.matching <- matrix(FALSE, nrow = n.rows, ncol = n.data.sets)

    for (i in row.indices)
    {
        missing.ind <- data.set.indices[is.na(matched.names[i, data.set.indices])]
        for (j in missing.ind)
        {
            if (length(remaining.names[[j]]) == 0)
                next

            ind.for.matching <- which(!is.na(matched.names[i, ]) &
                                      !not.used.for.matching[i, ])
            nms <- matched.names[i, ind.for.matching]
            nms.ind <- vapply(seq_along(nms), function(k) {
                data.set.ind <- ind.for.matching[k]
                match(nms[k], v.names[[data.set.ind]])
            }, integer(1))
            nms <- unique(nms)
            lbls <- unique(removeNA(vapply(seq_along(nms), function(k) {
                data.set.ind <- ind.for.matching[k]
                v.labels[[data.set.ind]][nms.ind[k]]
            }, character(1))))
            val.attrs <- unique(removeNULL(lapply(seq_along(nms), function(k) {
                data.set.ind <- ind.for.matching[k]
                v.val.attrs[[data.set.ind]][[nms.ind[k]]]
            })))

            if (use.names.and.labels.from == "Last data set")
            {
                nms <- rev(nms)
                lbls <- rev(lbls)
                val.attrs <- rev(val.attrs)
            }

            candidates <- candidateMetadata(remaining.names,
                                            remaining.labels,
                                            remaining.val.attrs,
                                            j, matched.names[i, ],
                                            v.names.to.not.combine)

            if (length(candidates$names) == 0)
                next

            matching.name <- findMatchingVariable(nms, lbls, val.attrs,
                                                  candidates,
                                                  match.parameters)

            if (!is.na(matching.name))
            {
                is.compatible <- isVariableCompatible(matching.name, j,
                                                      matched.names[i, ],
                                                      v.names, v.types,
                                                      v.val.attrs,
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
            matched.ind <- c(matched.ind, match(nm, remaining.names[[i]]))
            is.fuzzy.match[max.ind, i] <- matching.names.is.fuzzy[max.ind, i]
            matched.by[max.ind, i] <- matching.names.matched.by[max.ind, i]
        }
        if (!is.null(matched.ind))
        {
            remaining.names[[i]] <- remaining.names[[i]][-matched.ind]
            remaining.labels[[i]] <- remaining.labels[[i]][-matched.ind]
            remaining.val.attrs[[i]] <- remaining.val.attrs[[i]][-matched.ind]
        }
    }

    list(matched.names = matched.names,
         is.fuzzy.match = is.fuzzy.match,
         matched.by = matched.by,
         remaining.names = remaining.names,
         remaining.labels = remaining.labels,
         remaining.val.attrs = remaining.val.attrs)
}

autoSelectWhatToMatchBy <- function(input.data.set.metadata, match.parameters)
{
    v.names <- input.data.set.metadata$variable.names
    v.labels <- input.data.set.metadata$variable.labels
    v.val.attrs <- input.data.set.metadata$variable.value.attributes
    n.data.sets <- input.data.set.metadata$n.data.sets

    if (match.parameters$ignore.case)
    {
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
    match.parameters$match.by.value.labels <- maxOneToManyValueLabelProportion(v.val.attrs) <= 0.25
    match.parameters
}

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
            for (k in seq_along(lbls.i))
                if (length(which(lbls.i[k] == lbls.j)) > 1)
                    n <- n + 1
            prop <- c(prop, n / length(lbls.i))
        }
    }
    max(prop)
}

maxOneToManyValueLabelProportion <- function(v.val.attrs)
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
                {
                    n <- n + 1
                }
            }
            prop <- c(prop, n / length(v.val.attrs.i))
        }
    }
    max(prop)
}

# Parse the character vector variables.to.combine and return a matrix where
# each row contains a set of variables to be combined, with the columns
# corresponding to the input data sets.
parseVariablesToCombine <- function(variables.to.combine,
                                    input.data.set.metadata)
{
    n.data.sets <- input.data.set.metadata$n.data.sets
    result <- matrix(nrow = 0, ncol = n.data.sets)
    is.data.set.specified <- matrix(nrow = 0, ncol = n.data.sets)
    for (txt in variables.to.combine)
    {
        rows <- parseInputVariableText(txt, input.data.set.metadata)
        result <- rbind(result, rows, deparse.level = 0)
        is.data.set.specified <- rbind(is.data.set.specified,
                                       attr(rows, "is.data.set.specified"))
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
            stop("The variable '", duplicate.name, "' has been specified to ",
                 "be combined in multiple inputs: ",
                 paste0(paste0("'", duplicate.match.source, "'"), collapse = ", "),
                 ". Ensure that any of the variables to be combined are ",
                 "specified in at most one input.")
        }
    }

    attr(result, "is.data.set.specified") <- is.data.set.specified
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

parseVariablesToKeep <- function(variables.to.keep, input.data.set.metadata)
{
    split.text <- unlist(lapply(variables.to.keep, splitByComma,
                                ignore.commas.in.parentheses = TRUE),
                         use.names = FALSE)
    do.call("rbind", lapply(split.text, parseInputVariableText,
                            input.data.set.metadata))
}

# Parse the character vector variables.to.omit and return a matrix where
# each row contains a set of variables that should be omitted, with the
# columns corresponding to the input data sets.
parseVariablesToOmit <- function(variables.to.omit,
                                           input.data.set.metadata)
{
    split.text <- unlist(lapply(variables.to.omit, splitByComma,
                                ignore.commas.in.parentheses = TRUE),
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
    if (is.na(dash.ind)) # single variable (not range)
    {
        data.set.ind <- parseDataSetIndices(input.text, n.data.sets)
        if (length(data.set.ind) > 0) # data set indices supplied
        {
            input.text.without.index <- removeDataSetIndices(input.text)
            result <- matrix(nrow = 1, ncol = n.data.sets)
            for (i in data.set.ind)
            {
                if (!(input.text.without.index %in% var.names[[i]]))
                    variableNotFoundError(input.text.without.index,
                                          data.set.names[i])

                result[i] <- input.text.without.index
            }
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
    else # range of variables
    {
        range.start <- trimws(substr(input.text, 1, dash.ind - 1))
        range.end <- trimws(substr(input.text, dash.ind + 1, nchar(input.text)))

        data.set.ind <- parseDataSetIndicesForRange(range.start,
                                                    range.end,
                                                    n.data.sets)
        if (length(data.set.ind) > 0) # data set indices supplied for range
        {
            range.start.without.index <- removeDataSetIndices(range.start)
            range.end.without.index <- removeDataSetIndices(range.end)
            result <- matrix(NA_character_, nrow = end.ind - start.ind + 1,
                             ncol = n.data.sets)
            for (i in data.set.ind)
                result[, i] <- variablesFromRange(var.names[[i]],
                                                  range.start.without.index,
                                                  range.end.without.index,
                                                  data.set.names[i],
                                                  input.text)
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
}

checkMatchVariablesInputs <- function(v.names.to.combine, v.names.to.not.combine,
                                      v.names.to.keep, v.names.to.omit,
                                      data.sets.whose.variables.are.kept,
                                      input.data.set.metadata)
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
                     "Ensure that it is specified to be either kept or ",
                     "or omitted.")
            else if (length(v) > 1)
                stop("The variable(s) ",
                     paste0(paste0("'", v, "'"), collapse = ", "),
                     " have been specified to be both kept and omitted. ",
                     "Ensure that they are specified to be either kept ",
                     "or omitted.")
        }
    }

    if (length(data.sets.whose.variables.are.kept) == 0)
        stop("At least one data set needs to be specified in the input for the data sets whose variables are kept")

    n.data.sets <- input.data.set.metadata$n.data.sets
    if (any(!(data.sets.whose.variables.are.kept %in% seq_len(n.data.sets))))
        stop("The input for 'data.sets.whose.variables.are.kept' contains ",
             "invalid data set indices. Ensure that it contains only indices ",
             "from 1 to the number of input data sets.")
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
                        variableNotFoundError(t.without.index,
                                              data.set.names[j])

                    parsed.names <- addToParsedNames(parsed.names,
                                                     t.without.index,
                                                     j, data.set.names,
                                                     source.text, t)
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
                                                     j, data.set.names,
                                                     source.text, t)
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
                                                          data.set.names[j],
                                                          t)
                    parsed.names <- addToParsedNames(parsed.names, range.var.names,
                                                     j, data.set.names,
                                                     source.text, t)
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
                                                          data.set.names[j],
                                                          t, FALSE)
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
                                                     data.set.names,
                                                     source.text, t)
                    source.text[j] <- t
                }
                if (!is.range.found)
                    stop("The input range '", t, "' was not found ",
                         "in any of the input data sets. Ensure that the ",
                         "range has been correctly specified.")
            }
        }
    }

    if (sum(!vapply(parsed.names, is.null, logical(1))) == 0)
        stop("The input '", input.text, "' does not specify any variables. ",
             "This input needs to specify variables from two or more data sets.")

    if (sum(!vapply(parsed.names, is.null, logical(1))) == 1)
      stop("The input '", input.text, "' only specifies variables from one data set. ",
           "This input needs to specify variables from two or more data sets.")

    n.vars <- vapply(parsed.names, length, integer(1))
    if (!allIdentical(n.vars[n.vars > 0]))
        stop("The input '", input.text, "' contains ",
             "variable ranges with differing numbers of variables. ",
             "Ensure that the ranges have been correctly specified ",
             "so that they all contain the same number of variables.")

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

# Parse variable names with '(x)' appended where 'x' is a data set index
parseDataSetIndices <- function(input.text, n.data.sets)
{
    if (grepl("\\(.+\\)$", input.text))
    {
        split.char <- strsplit(input.text, "")[[1]]
        start.ind <- match("(", split.char) + 1
        end.ind <- match(")", split.char) - 1
        data.set.ind <- as.integer(trimws(strsplit(substr(input.text,
                                                          start.ind,
                                                          end.ind), ",")[[1]]))
        if (any(is.na(data.set.ind)))
            stop("The data set indices in the input '", input.text,
                 "' could not be parsed. ",
                 "They need to be numbers corresponding to the data sets, e.g., 'Q2(3)'.")

        if (any(data.set.ind < 1) || any(data.set.ind > n.data.sets))
        {
            if (length(data.set.ind) == 1)
                stop("The data set index in the input '", input.text,
                     "' is out of range.")
            else
                stop("The data set indices in the input '", input.text,
                     "' are out of range.")
        }
        data.set.ind
    }
    else
        integer(0)
}

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

# Data set index for a range can either be specified for start or end or both
# as long as they are consistent
parseDataSetIndicesForRange <- function(input.text.start, input.text.end, n.data.sets)
{
    data.set.ind.start <- parseDataSetIndices(input.text.start, n.data.sets)
    data.set.ind.end <- parseDataSetIndices(input.text.end, n.data.sets)

    if (!setequal(data.set.ind.start, data.set.ind.end))
        stop("The following specified variable range contains two different data set indices: '",
             input.text.start, "-", input.text.end,
             "'. The indices need refer to the same data sets.")
    data.set.ind.start
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

candidateMetadata <- function(remaining.names,
                              remaining.labels,
                              remaining.val.attrs,
                              data.set.ind, row.variables,
                              v.names.to.not.combine)
{
    is.combinable <- vapply(remaining.names[[data.set.ind]],
                            isVariableCombinableIntoRow,
                            logical(1), data.set.ind, row.variables,
                            v.names.to.not.combine)
    list(names = remaining.names[[data.set.ind]][is.combinable],
         labels = remaining.labels[[data.set.ind]][is.combinable],
         val.attrs = remaining.val.attrs[[data.set.ind]][is.combinable])
}


findMatchingVariable <- function(nms, lbls, val.attrs, candidates,
                                 match.parameters)
{
    candidate.names <- candidates$names
    candidate.labels <- candidates$labels
    candidate.val.attrs <- candidates$val.attrs

    match.by.variable.names <- match.parameters$match.by.variable.names
    match.by.variable.labels <- match.parameters$match.by.variable.labels
    match.by.value.labels <- match.parameters$match.by.value.labels
    ignore.case <- match.parameters$ignore.case
    ignore.non.alphanumeric <- match.parameters$ignore.non.alphanumeric
    min.match.percentage <- match.parameters$min.match.percentage

    if (!match.by.variable.names &&
        !match.by.variable.labels &&
        !match.by.value.labels)
        stop("Matching needs to be done with at least one of the following: ",
             "variable names, variable labels or value labels.")

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
        match.percentages <- matchPercentages(candidate.labels, lbls,
                                              ignore.case,
                                              ignore.non.alphanumeric,
                                              min.match.percentage)
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
        match.percentages <- matchPercentages(candidate.names, nms, ignore.case,
                                              ignore.non.alphanumeric,
                                              min.match.percentage)
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
        match.percentages <- matchPercentagesForValueAttributes(candidate.val.attrs,
                                                                val.attrs,
                                                                ignore.case,
                                                                ignore.non.alphanumeric,
                                                                min.match.percentage)
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

#' @importFrom stringdist stringdistmatrix
#' @importFrom stringi stri_detect_fixed
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
        ind <- which(n.char.1 > 0)
        ind <- ind[adjustedMatchPercentage(abs(nchar(s) - n.char.1[ind]), pmax(nchar(s), n.char.1[ind])) >= min.match.percentage]
        d[ind] <- stringdist(s, strings.1[ind])
        d
    }))

    nchar.matrix.1 <- matrix(rep(n.char.1, length(strings.2)),
                      nrow = length(strings.1))
    nchar.matrix.2 <- matrix(rep(n.char.2, each = length(strings.1)),
                      nrow = length(strings.1))
    adjustedMatchPercentage(distances, pmax(nchar.matrix.1, nchar.matrix.2))
}

adjustedMatchPercentage <- function(distances, denominators)
{
    scale.parameter <- 20
    result <- 100 * (1 - (distances / denominators) ^ pmax((denominators - distances) / scale.parameter, 1))
    ind <- result == 100
    result[ind] <- result[ind] - 1e-12 * distances[ind]
    result
}

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

    matchPercentages(lbls.combined.1, lbls.combined.2, FALSE, FALSE,
                     min.match.percentage)
}

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

    adjustedMatchPercentage(distances, nchar.lbls)
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
removeNonAlphaNumericCharacters <- function(txt)
{
    pattern <- paste0("(^[^a-zA-Z0-9]+)|", # non-alphanum characters at start
                      "([^a-zA-Z0-9]+$)|", # non-alphanum characters at end
                      "((?<=[[:alpha:]])[^a-zA-Z0-9]+(?=[[:alpha:]]))|", # non-alphanum characters between alphabet characters
                      "((?<=[[:alpha:]])[^a-zA-Z0-9]+(?=\\d))|", # non-alphanum characters between alphabet character and digit
                      "((?<=\\d)[^a-zA-Z0-9]+(?=[[:alpha:]]))") # non-alphanum characters between digit and alphabet character
    result <- gsub(pattern, "", txt, perl = TRUE)

    # Replace remaining non-alphanum characters with an underscore
    result <- gsub("[^a-zA-Z0-9]+", "_", result, perl = TRUE)

    result
}

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
isVariableCombinableIntoRow <- function(name.to.combine,
                                        data.set.ind,
                                        row.variables,
                                        v.names.to.not.combine)
{
    if (is.null(v.names.to.not.combine))
        return(TRUE)

    row.variables[data.set.ind] <- name.to.combine

    all(apply(v.names.to.not.combine, 1, function(nms) {
        sum(nms == row.variables, na.rm = TRUE) < 2
    }))
}

isVariableCompatible <- function(variable.name, data.set.ind, row.variables,
                                 variable.names, variable.types,
                                 variable.value.attributes, data.sets)
{
    var.ind <- match(variable.name, variable.names[[data.set.ind]])
    var.type <- variable.types[[data.set.ind]][var.ind]
    var.vals <- data.sets[[data.set.ind]][[var.ind]]
    var.val.attr <- variable.value.attributes[[data.set.ind]][[var.ind]]

    non.missing.ind <- which(!is.na(row.variables))
    row.vars.ind <- vapply(non.missing.ind, function(i) {
        match(row.variables[i], variable.names[[i]])
    }, integer(1))
    row.vars.types <- vapply(seq_along(row.vars.ind), function(i) {
        variable.types[[non.missing.ind[i]]][row.vars.ind[i]]
    }, character(1))
    row.vars.vals <- lapply(seq_along(row.vars.ind), function(i) {
        data.sets[[non.missing.ind[i]]][[row.vars.ind[i]]]
    })
    row.vars.val.attr <- lapply(seq_along(row.vars.ind), function(i) {
        variable.value.attributes[[non.missing.ind[i]]][[row.vars.ind[i]]]
    })

    if (var.type == "Numeric")
    {
        if (any(isDateType(row.vars.types)))
            return(isConvertibleToDateTime(var.vals))

        if ("Duration" %in% row.vars.types)
            return(FALSE)

        if (any(isCatType(row.vars.types)))
            return(isConvertibleToCategorical("Numeric", var.vals,
                                              row.vars.val.attr[isCatType(row.vars.types)],
                                              20))

        return(TRUE)
    }
    else if (var.type == "Text")
    {
        if (any(isDateType(row.vars.types)))
            return(isParsableAsDateTime(var.vals))

        if ("Duration" %in% row.vars.types)
            return(isParsableAsDiffTime(var.vals))

        if (any(isCatType(row.vars.types)))
            return(isConvertibleToCategorical("Text", var.vals,
                                              row.vars.val.attr[isCatType(row.vars.types)],
                                              20))

        return(TRUE)
    }
    else if (isDateType(var.type))
    {
        if (any(isDateType(row.vars.types)))
            return(TRUE)
        if (any(c("Categorical", "Duration") %in% row.vars.types))
            return(FALSE)

        for (i in seq_along(row.vars.types))
        {
            if (row.vars.types[i] == "Numeric")
            {
                if (!isConvertibleToDateTime(row.vars.vals[[i]]))
                    return(FALSE)
            }
            else # row.vars.types[i] == "Text"
            {
                if (!isParsableAsDateTime(row.vars.vals[[i]]))
                    return(FALSE)
            }
        }
        return(TRUE)
    }
    else if (var.type == "Duration")
    {
        if ("Numeric" %in% row.vars.types ||
            any(isDateType(row.vars.types)) ||
            any(isCatType(row.vars.types)))
            return(FALSE)

        for (i in seq_along(row.vars.types))
        {
            if (row.vars.types[i] == "Text" &&
                !isParsableAsDiffTime(row.vars.vals[[i]]))
                return(FALSE)
        }
        return(TRUE)
    }
    else if (isCatType(var.type))
    {
        if ("Duration" %in% row.vars.types || any(isDateType(row.vars.types)))
            return(FALSE)
        for (i in seq_along(row.vars.types))
        {
            val.attrs <- c(list(var.val.attr),
                           row.vars.val.attr[isCatType(row.vars.types)])
            if(!isConvertibleToCategorical(row.vars.types[i], row.vars.vals[[i]],
                                           val.attrs, 20))
                return(FALSE)
        }
        return(TRUE)
    }
    else
        stop("Variable type not recognised")
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

isConvertibleToDateTime <- function(num)
{
    missing.ind <- is.na(num)
    # seconds from 1970/1/1 between years 2000 and 2050
    num[!missing.ind] >= 946684800 && num[!missing.ind] <= 2524608000
}

isConvertibleToDate <- function(num)
{
    missing.ind <- is.na(num)
    # days from 1970/1/1 between years 2000 and 2050
    num[!missing.ind] >= 10957 && num[!missing.ind] <= 29220
}

isConvertibleToCategorical <- function(variable.type, values, val.attrs,
                                       max.unique.values)
{
    if (variable.type %in% c("Categorical"))
        return(TRUE)

    if (variable.type %in% c("Date", "Date/Time", "Duration"))
        return(FALSE)

    n.category.values <- length(unique(unlist(lapply(val.attrs, as.character))))
    return(length(unique(values)) <= max(2 * n.category.values,
                                           max.unique.values))
}

mergedVariableNames <- function(matched.names, use.names.and.labels.from)
{
    merged.names <- namesFromEarliestDataSet(matched.names,
                                             use.names.and.labels.from)

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

namesFromEarliestDataSet <- function(matched.names, use.names.and.labels.from)
{
    apply(matched.names, 1, function(nms) {
        if (use.names.and.labels.from == "First data set")
            removeNA(nms)[1]
        else
            removeNA(rev(nms))[1]
    })
}

orderMatchedNames <- function(matched.names, input.data.set.metadata,
                              use.names.and.labels.from)
{
    n.data.sets <- input.data.set.metadata$n.data.sets
    v.names <- input.data.set.metadata$variable.names

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

    ordering <- mergeIndicesList(v.indices, use.names.and.labels.from,
                                 indices.to.keep.togther)

    ordered.matched.names <- matched.names[ordering, , drop = FALSE]
    attr(ordered.matched.names, "non.combinable.variables") <- attr(matched.names, "non.combinable.variables")
    attr(ordered.matched.names, "is.fuzzy.match") <- attr(matched.names, "is.fuzzy.match")[ordering, , drop = FALSE]
    attr(ordered.matched.names, "matched.by") <- attr(matched.names, "matched.by")[ordering, , drop = FALSE]
    ordered.matched.names
}

# Takes a list of character vectors each containing indices in a certain order
# and merges them into a single integer vector, respecting the order in
# each vector as much as possible, with earlier vectors taking precedence
# in case of ties.
mergeIndicesList <- function(indices.list, use.names.and.labels.from,
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

    if (use.names.and.labels.from == "Last data set")
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

mergedDataSet <- function(data.sets, matched.names, merged.names,
                          use.names.and.labels.from, data.set.names,
                          when.multiple.labels.for.one.value,
                          match.parameters)
{
    n.vars <- nrow(matched.names)
    n.data.set.cases <- vapply(data.sets, nrow, integer(1))

    merged.data.set <- data.frame(lapply(seq_len(n.vars), function(i) {
        compositeVariable(matched.names[i, ], data.sets,
                          use.names.and.labels.from,
                          when.multiple.labels.for.one.value,
                          match.parameters)
    }))

    names(merged.data.set) <- merged.names

    mergesrc.name <- uniqueName("mergesrc", names(merged.data.set), "_")
    merged.data.set[[mergesrc.name]] <- mergeSrc(n.data.set.cases, data.set.names)

    merged.data.set
}

# Combine variables from different data sets (end-to-end) to create a
# composite variable
compositeVariable <- function(variable.names, data.sets,
                              use.names.and.labels.from,
                              when.multiple.labels.for.one.value,
                              match.parameters)
{
    n.data.sets <- length(data.sets)
    var.list <- lapply(seq_len(n.data.sets), function(i) {
         if(!is.na(variable.names[i]))
             data.sets[[i]][[variable.names[i]]]
        else
            NULL
    })
    v.types <- vapply(var.list, variableType, character(1))

    if (any(isCatType(v.types)))
    {
        combine.as.categorical.var <- TRUE
        val.attrs <- lapply(which(isCatType(v.types)), function(i) {
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
    }
    else
        combine.as.categorical.var <- FALSE

    result <- if (combine.as.categorical.var)
        combineAsCategoricalVariable(var.list, data.sets,
                                     use.names.and.labels.from, v.types,
                                     when.multiple.labels.for.one.value,
                                     match.parameters)
    else
        combineAsNonCategoricalVariable(var.list, data.sets, v.types)

    attr(result, "label") <- variableLabelFromDataSets(variable.names,
                                                       data.sets,
                                                       use.names.and.labels.from)

    result
}

combineAsCategoricalVariable <- function(var.list, data.sets,
                                         use.names.and.labels.from, v.types,
                                         when.multiple.labels.for.one.value,
                                         match.parameters)
{
    val.attr.list <- lapply(var.list, attr, "labels")

    cat.ind <- which(isCatType(v.types))
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
        else if (v.types[i] == "Text")
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
                    var.values[!is.missing] <- as.numeric(v[!is.missing])
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
        else if (v.types[i] == "Numeric")
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
        match.percentages <- matchPercentagesForValueLabels(lbl,
                                                            names(merged.val.attr),
                                                            match.parameters)
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

#' @importFrom lubridate as_date as_datetime
combineAsNonCategoricalVariable <- function(var.list, data.sets, v.types)
{
    n.data.sets <- length(data.sets)

    .combineVar <- function(parser)
    {
        parser.name <- as.character(sys.call()[2])

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
            else if (v.types[i] == "Numeric" && parser.name == "AsDateTime")
            {
                missing.ind <- is.na(v)
                converted <- parser(rep(NA_character_, length(v)))
                converted[!missing.ind] <- as_datetime(v[!missing.ind])
                converted
            }
            else if (v.types[i] == "Numeric" && parser.name == "AsDate")
            {
                missing.ind <- is.na(v)
                converted <- parser(rep(NA_character_, length(v)))
                converted[!missing.ind] <- as_date(v[!missing.ind])
                converted
            }
            else if (v.types[i] == "Date" && parser.name == "AsDateTime")
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
        setequal(unique.v.types, c("Date/Time", "Date")) ||
        setequal(unique.v.types, c("Date/Time", "Numeric")) ||
        setequal(unique.v.types, c("Date/Time", "Text", "Date")) ||
        setequal(unique.v.types, c("Date/Time", "Text", "Numeric")) ||
        setequal(unique.v.types, c("Date/Time", "Date", "Numeric")) ||
        setequal(unique.v.types, c("Date/Time", "Text", "Date", "Numeric")))
        .combineVar(AsDateTime)
    else if (setequal(unique.v.types, c("Date")) ||
             setequal(unique.v.types, c("Date", "Text")) ||
             setequal(unique.v.types, c("Date", "Numeric")) ||
             setequal(unique.v.types, c("Date", "Text", "Numeric")))
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
    else if (any(isCatType(unique.v.types)))
    {
        # If there are any categorical variables, convert everything into text.
        # This only occurs when categorical is combined with date, date/time or
        # duration variables or there are too many unique values in numeric or
        # text variables.
        unlist(lapply(seq_len(n.data.sets), function(i) {
            v <- var.list[[i]]
            if (is.null(v))
                rep(NA_character_, nrow(data.sets[[i]]))
            else if (isCatType(v.types[i]))
            {
                result <- rep(NA_character_, nrow(data.sets[[i]]))
                val.attr <- attr(var.list[[i]], "labels")
                for (j in seq_along(val.attr))
                    result[v == val.attr[i]] <- names(val.attr)[i]
                result
            }
            else
                as.character(v)
        }))
    }
    else
    {
        # Don't expect this to ever occur
        stop("Unhandled variable types combination: ",
             paste0(unique.v.types, collapse = ", "))
    }
}

remapValuesInVariable <- function(variable, map)
{
    result <- variable
    for (i in seqRow(map))
        result[variable == map[i, 1]] <- map[i, 2]
    result
}

variableLabelFromDataSets <- function(variable.names, data.sets,
                                      use.names.and.labels.from)
{
    ind <- if (use.names.and.labels.from == "First data set")
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

removeNULL <- function(x)
{
    x[!vapply(x, is.null, logical(1))]
}

#' @importFrom flipFormat DataSetMergingByCaseWidget
#' @export
print.MergeDataSetByCase <- function(x, ...)
{
    DataSetMergingByCaseWidget(x$input.data.set.metadata,
                               x$merged.data.set.metadata,
                               x$matched.names,
                               x$merged.names,
                               x$omitted.variables,
                               x$input.value.attributes,
                               x$is.saved.to.cloud)
}
