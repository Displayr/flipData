#' @title Stack data set
#' @description Stacks variables in a data set.
#' @param input.data.set.name Name of data file to stack, either as a path to a
#'   local file or file in the Displayr Cloud Drive
#' @param stacked.data.set.name Name of the stacked data file to be saved in
#'   the Displayr Cloud Drive (if run from Displayr) or saved locally.
#' @param stack.with.common.labels \code{"Automatically"},
#'   \code{"Using a set of variables to stack as reference"},
#'   \code{"Using manually input common labels"},
#'   \code{"Disabled"}.
#' @param reference.variables.to.stack A character vector of sets of variables
#'   to stack to be used as a reference to generate the common labels used for
#'   stacking. This can be a combination of comma-separated names, wildcards
#'   and ranges. Variable ranges can be specified by supplying the start and
#'   end variables separated by a dash '-'. If the start or end variables are
#'   left out, then the range is assumed to start from the first variable or
#'   end at the last variable respectively. Wildcards in variable names can be
#'   specified with an asterisk '*'.
#' @param manual.common.labels A list of sets of common labels to be used to
#'   identify variables to stack. Only used when \code{stack.with.common.labels}
#'   is \code{"Using manually input common labels"}. To be identified, a set of
#'   variables to be stacked must contain these labels, have the same prefix
#'   and suffix before and after these labels.
#' @param specify.by "Variable" or "Observation". See \code{manual.stacking}.
#' @param manual.stacking If \code{specify.by} is "Variable", this is a
#'   character vector where each string corresponds to variables to be stacked
#'   into a new variable. If \code{specify.by} is "Observation", this is a
#'   character vector where each string corresponds to an observation in the
#'   stacked variables. The strings in both cases can be a combination of
#'   comma-separated names, wildcards and ranges.
#'   See \code{reference.variables.to.stack} for more details on the format.
#' @param variables.to.omit Character vector of comma-separated names of
#'   variables to omit. Each string can be a combination of comma-separated
#'   names, wildcards and ranges. See \code{reference.variables.to.stack} for
#'   more details on the format.
#' @param write.data.set Whether to write the stacked data set.
#' @param include.stacked.data.set.in.output Whether to include the stacked
#'   data set in the output.
#' @param include.original.case.variable Whether to include the \code{original_case}
#'   variable in the stacked data set.
#' @param include.observation.variable Whether to include the \code{observation}
#'   variable in the stacked data set.
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{stacked.data.set.metadata} A list containing metadata on the
#'     the stacked data set such as variable names, labels etc.
#'   \item \code{unstackable names} A list of character vectors containing
#'     names of the variables that could not be stacked using common labels due
#'     to mismatching types or value attributes.
#'   \item \code{omitted.variables} A character vector of omitted variables.
#'   \item \code{omitted.stacked.variables} A character vector of omitted
#'     stacked variables.
#'   \item \code{common.labels} A character vector of common labels used for
#'     stacking.
#'   \item \code{is.saved.to.cloud} Whether the stacked data set was saved to
#'     the Displayr cloud drive.
#'  }
#' @export
StackData <- function(input.data.set.name,
                      stacked.data.set.name = NULL,
                      stack.with.common.labels = "Automatically",
                      reference.variables.to.stack = NULL,
                      manual.common.labels = NULL,
                      specify.by = "Variable",
                      manual.stacking = NULL,
                      variables.to.omit = NULL,
                      write.data.set = TRUE,
                      include.stacked.data.set.in.output = FALSE,
                      include.original.case.variable = TRUE,
                      include.observation.variable = TRUE)
{
    input.data.set <- readDataSets(input.data.set.name, 1)[[1]]
    variables.to.omit <- splitVariablesToOmitText(variables.to.omit)
    omitted.variables <- parseVariablesToOmit(variables.to.omit,
                                              names(input.data.set),
                                              FALSE)

    input.data.set <- omitVariablesFromDataSet(input.data.set,
                                               omitted.variables)
    input.data.set.metadata <- metadataFromDataSet(input.data.set,
                                                   input.data.set.name)

    common.labels.list <- commonLabels(manual.common.labels,
                                       stack.with.common.labels,
                                       input.data.set.metadata,
                                       reference.variables.to.stack)

    stacking.groups <- stackWithCommonLabels(common.labels.list,
                                             input.data.set.metadata)
    stacking.groups <- stackManually(stacking.groups, manual.stacking,
                                     specify.by, input.data.set.metadata)

    stacked.data.set <- stackedDataSet(input.data.set, input.data.set.metadata,
                                       stacking.groups,
                                       include.original.case.variable,
                                       include.observation.variable,
                                       common.labels.list)

    omitted.variables <- parseVariablesToOmitAfterStacking(variables.to.omit,
                                                           omitted.variables,
                                                           stacked.data.set)
    omitted.stacked.variables <- omittedStackedVariables(omitted.variables,
                                                         stacked.data.set)
    stacked.data.set <- omitVariablesFromDataSet(stacked.data.set,
                                                 omitted.variables)

    stacked.data.set.name <- cleanStackedDataSetName(stacked.data.set.name,
                                                     input.data.set.name)
    if (write.data.set)
        writeDataSet(stacked.data.set, stacked.data.set.name)

    stacked.data.set.metadata <- metadataFromStackedDataSet(stacked.data.set,
                                                            stacked.data.set.name)

    result <- list()
    result$stacked.data.set.metadata <- stacked.data.set.metadata
    result$unstackable.names <- attr(stacking.groups, "unstackable.names")
    result$omitted.variables <- omitted.variables
    result$omitted.stacked.variables <- omitted.stacked.variables
    result$common.labels.list <- common.labels.list
    result$is.saved.to.cloud <- write.data.set && canAccessDisplayrCloudDrive()

    if (include.stacked.data.set.in.output)
        result$stacked.data.set <- stacked.data.set

    class(result) <- "StackedData"
    result
}

readDataSets <- function(data.set.names, min.data.sets = 1)
{
    if (length(data.set.names) < min.data.sets)
        stop("At least ", min.data.sets, " data set(s) are required.")

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
            stop("The data file '", nm, "' does not exist in the Displayr ",
                 "cloud drive. Ensure that the data file is in the Displayr ",
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

dataSetNameWithoutPath <- function(data.set.name.or.path)
{
    if (canAccessDisplayrCloudDrive())
        data.set.name.or.path
    else
        basename(data.set.name.or.path)
}

cleanMergedDataSetName <- function(merged.data.set.name, data.set.names)
{
    if (is.null(merged.data.set.name) ||
        trimws(merged.data.set.name) == "")
        "Merged data set.sav"
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

# Get common labels ready to be used for stacking. The actions of this function
# depend on the value of stack.with.common.labels.
# Returns a list where each element is a set of common labels
commonLabels <- function(manual.common.labels, stack.with.common.labels,
                         input.data.set.metadata, reference.variables.to.stack)
{
    if (stack.with.common.labels == "Automatically")
    {
        if (!is.null(manual.common.labels) && length(manual.common.labels) > 0)
            warning("Input common labels have been ignored as common labels ",
                    "are to be generated automatically.")
        return(automaticCommonLabels(input.data.set.metadata))
    }
    else if (stack.with.common.labels == "Using a set of variables to stack as reference")
    {
        if (!is.null(manual.common.labels) && length(manual.common.labels) > 0)
            warning("Input common labels have been ignored as common labels ",
                    "are to be obtained from a set of variables.")
        return(commonLabelsFromVariables(reference.variables.to.stack,
                                         input.data.set.metadata))
    }
    else if (stack.with.common.labels == "Using manually input common labels")
        tidyManualCommonLabels(manual.common.labels)
    else if (stack.with.common.labels == "Disabled")
    {
        if (!is.null(manual.common.labels) && length(manual.common.labels) > 0)
            warning("Input common labels have been ignored.")
        return(NULL)
    }
    else
        stop("Input for stack.with.common.labels not recognised: ",
             stack.with.common.labels)
}

# Finds a set of common labels from the variable labels. This works by
# 1. Finding all the complete-word prefixes between every pair of labels that
#    appear at least 6 times.
# 2. For each prefix, extract candidate common labels after removing the prefix
#    from labels.
# 3. For each set of candidate common labels, form stacking groups
# 4. Score stacking groups based on size, removing ones with many missing values
# 5. Choose common labels with highest score
automaticCommonLabels <- function(input.data.set.metadata)
{
    v.names <- input.data.set.metadata$variable.names
    v.labels <- input.data.set.metadata$variable.labels

    words <- strsplit(v.labels," ")
    is.multiple.words <- vapply(words, length, integer(1)) > 1
    words <- words[is.multiple.words]
    first.words <- vapply(words, `[`, character(1), 1)

    prefixes <- character(0)
    prefix.count <- integer(0)

    for (i in 1:(length(first.words) - 1))
    {
        words.i <- words[[i]]
        n.words.i <- length(words.i)
        ind <- (i + 1):length(first.words)
        match.ind <- ind[first.words[i] == first.words[ind]]
        for (j in match.ind)
        {
            words.j <- words[[j]]

            min.ind <- 1:min(n.words.i, length(words.j))
            last.match <- match(TRUE, words.i[min.ind] != words.j[min.ind]) - 1
            prefix <- if (!is.na(last.match))
                paste(words.i[seq_len(last.match)], collapse = " ")
            else
                next # not really a prefix since one is a subset of another

            prefix.ind <- match(prefix, prefixes)
            if (is.na(prefix.ind))
            {
                prefixes <- rbind(prefixes, prefix)
                prefix.count <- c(prefix.count, 1)
            }
            else
                prefix.count[prefix.ind] <- prefix.count[prefix.ind] + 1
        }
    }
    prefixes <- prefixes[prefix.count > 5]

    candidate.common.labels <- lapply(prefixes, commonLabelsByRemovingPrefix,
                                      v.labels)
    candidate.common.labels <- candidate.common.labels[!duplicated(candidate.common.labels)]

    score <- vapply(candidate.common.labels, function(common.labels)
    {
        if (length(common.labels) == 1)
            return(-Inf)

        stacking.groups <- stackingGroupFromCommonLabels(common.labels,
                                                         v.labels, v.names)
        if (nrow(stacking.groups) == 0)
            return(-Inf)

        whole.column.missing <- apply(stacking.groups, 2,
                                      function(column) all(is.na(column)))
        stacking.groups <- stacking.groups[, !whole.column.missing, drop = FALSE]

        if (sum(!is.na(stacking.groups)) / length(stacking.groups) < 0.75)
            return(-Inf)

        sum(!is.na(stacking.groups))
    }, numeric(1))

    if (max(score) == -Inf)
    {
        warning("Common labels could not be found automatically.")
        return(NULL)
    }

    common.labels <- candidate.common.labels[[which.max(score)]]
    stacking.groups <- stackingGroupFromCommonLabels(common.labels,
                                                     v.labels, v.names)
    whole.column.missing <- apply(stacking.groups, 2,
                                  function(column) all(is.na(column)))
    list(common.labels[!whole.column.missing])
}

commonLabelsByRemovingPrefix <- function(prefix, v.labels)
{
    prefix <- paste0(prefix, " ")
    nchar.prefix <- nchar(prefix)
    lbls.with.prefix <- v.labels[substr(v.labels, 1, nchar.prefix) == prefix]
    common.labels <- substr(lbls.with.prefix, nchar.prefix + 1,
                            max(nchar(lbls.with.prefix)))
    common.labels <- unique(common.labels)
}

# Given a user-input string of variable names, parse it and extract common
# labels from the variable labels by removing common prefixes and suffixes
commonLabelsFromVariables <- function(reference.variables.to.stack,
                                      input.data.set.metadata)
{
    if (is.null(reference.variables.to.stack) ||
        length(reference.variables.to.stack) == 0)
    {
        warning("No reference variables to stack were supplied for common ",
                "labels. No stacking was conducted using common labels.")
        return(NULL)
    }

    v.names <- input.data.set.metadata$variable.names
    v.labels <- input.data.set.metadata$variable.labels

    common.labels.list <- list()
    for (i in seq_along(reference.variables.to.stack))
    {
        split.text <- splitByComma(reference.variables.to.stack[i])

        if (length(split.text) == 0)
            next

        on.fail <- paste0("Common labels could not be obtained from the input '",
                          reference.variables.to.stack[i], "'.")

        parsed.var.names <- character(0)
        for (t in split.text)
        {
            parsed <- if (grepl("-", t, fixed = TRUE)) # contains range
                parseRange(t, v.names, "common labels", on.fail)
            else if (grepl("*", t, fixed = TRUE)) # contains wildcard
                parseWildcard(t, v.names, "common labels", on.fail)
            else
                parseVariableName(t, v.names, "common labels",
                                  on.fail)

            if (length(parsed) == 0)
                next

            parsed.var.names <- c(parsed.var.names, parsed)
        }

        if (length(parsed.var.names) == 1)
        {
            warning("Only one variable is present in the input '",
                    reference.variables.to.stack[i],
                    "' for extracting common labels. It has been ignored as ",
                    "more than one variable is required.")
            next
        }

        lbls.containing.common.lbls <- vapply(parsed.var.names, function(nm) {
            v.labels[match(nm, v.names)]
        }, character(1))

        nchar.prefix <- nchar(getCommonPrefix(lbls.containing.common.lbls, whole.words = TRUE))
        nchar.suffix <- nchar(getCommonSuffix(lbls.containing.common.lbls, whole.words = TRUE))

        common.labels <- vapply(lbls.containing.common.lbls, function(lbl) {
            substr(lbl, nchar.prefix + 1, nchar(lbl) - nchar.suffix)
        }, character(1))

        common.labels.list <- c(common.labels.list, list(common.labels))
    }

    if (length(common.labels.list) > 0)
        common.labels.list
    else
    {
        warning("No common labels could be extracted from the input reference ",
                "variables.")
        NULL
    }
}

# Tidy up user-input common labels and check for issues
tidyManualCommonLabels <- function(manual.common.labels)
{
    if (is.null(manual.common.labels))
    {
        warning("No common labels were manually supplied. No stacking ",
                "was conducted using common labels.")
        return(NULL)
    }

    common.labels.list <- lapply(seq_along(manual.common.labels), function(i) {
        lbls <- manual.common.labels[[i]]
        lbls <- trimws(lbls)
        lbls <- lbls[lbls != ""]
        if (length(lbls) == 0)
        {
            warning("Set ", i, " of the manually-entered common labels does ",
                    "not contain any labels. Ensure that the common labels ",
                    "are correctly entered.")
            return(NULL)
        }
        else if (length(lbls) == 1)
        {
            warning("Set ", i, " of the manually-entered common labels ",
                    "contains only one label when more than one is required.")
            return(NULL)
        }
        lbls
    })

    common.labels.list <- common.labels.list[!vapply(common.labels.list,
                                                     is.null, logical(1))]
    if (length(common.labels.list) > 0)
        common.labels.list
    else
        NULL
}

# Perform stacking given a character vector of common labels.
# A matrix of variable indices (stacking.group) is returned where each row
# represents a single stacking of variables into one variable.
stackWithCommonLabels <- function(common.labels.list, input.data.set.metadata)
{
    if (is.null(common.labels.list) || length(common.labels.list) == 0)
        return(NULL)

    v.names <- input.data.set.metadata$variable.names
    v.labels <- input.data.set.metadata$variable.labels
    v.types <- input.data.set.metadata$variable.types
    v.val.attr <- input.data.set.metadata$variable.value.attributes

    stacking.groups.list <- lapply(common.labels.list,
                                   stackingGroupFromCommonLabels,
                                   v.labels, v.names)

    # Combine stacking groups in stacking.groups.list
    n.stacking <- max(vapply(stacking.groups.list, ncol, integer(1)))
    stacking.groups <- matrix(nrow = 0, ncol = n.stacking)
    for (element in stacking.groups.list)
    {
        for (i in seq_len(nrow(element)))
        {
            rw <- element[i, ]
            rw.without.missing <- removeNA(rw)
            is.overlapping <- apply(stacking.groups, 1, function(group.ind) {
                any(rw.without.missing %in% removeNA(group.ind))
            })
            n.overlapping <- sum(is.overlapping)
            if (n.overlapping == 0)
            {
                new.row <- rep(NA_integer_, n.stacking)
                new.row[seq_along(rw)] <- rw
                stacking.groups <- rbind(stacking.groups, new.row)
            }
            else if (n.overlapping == 1)
            {
                # Use the new row if it has less missing indices than the row
                # it overlaps with
                if (length(rw.without.missing) >
                    sum(!is.na(stacking.groups[is.overlapping, ])))
                {
                    stacking.groups <- stacking.groups[!is.overlapping, ,
                                                       drop = FALSE]
                    new.row <- rep(NA_integer_, n.stacking)
                    new.row[seq_along(rw)] <- rw
                    stacking.groups <- rbind(stacking.groups, new.row)
                }
            }
            # else (n.overlapping > 1): don't add row to stacking.groups
        }
    }

    # Remove groups with mismatching variable types and value attributes
    unstackable.ind <- which(apply(stacking.groups, 1, function(ind) {
        ind <- removeNA(ind)
        !allIdentical(v.types[ind]) || !allIdentical(v.val.attr[ind])
    }))
    unstackable.names <- lapply(unstackable.ind, function(ind) {
        v.names[removeNA(stacking.groups[ind, ])]
    })
    if (length(unstackable.names) > 0)
        warning("Variables could not be stacked due ",
                "to mismatching variable types or value attributes. ",
                "See Notes section in output for more details.")

    if (length(unstackable.ind) > 0)
        stacking.groups <- stacking.groups[-unstackable.ind, , drop = FALSE]
    attr(stacking.groups, "unstackable.names") <- unstackable.names
    attr(stacking.groups, "is.manually.stacked") <- rep(FALSE,
                                                       nrow(stacking.groups))
    stacking.groups
}

# Creates a stacking group matrix from common labels. This does the
# heavy-lifting for stackWithCommonLabels, i.e., determining which variables
# to stack together.
# This works by finding all the prefixes and suffixes from labels containing
# common labels and grouping together variables with common labels and the same
# prefix and suffix, also considering common prefixes and suffixes in variable
# names if necessary.
stackingGroupFromCommonLabels <- function(common.labels, v.labels, v.names)
{
    v.labels.lowercase <- tolower(v.labels)
    n.common.labels <- length(common.labels)
    common.label.prefixes.suffixes <- list()
    match.ind <- list()

    for (i in seq_len(n.common.labels))
    {
        common.lbl <- common.labels[i]
        matches <- gregexpr(tolower(common.lbl), v.labels.lowercase, # case insensitive match
                            fixed = TRUE)
        ind <- which(vapply(matches, function(m) m[[1]][1], integer(1)) != -1)
        common.label.prefixes.suffixes[[i]] <- t(vapply(ind, function(j) {
            lbl <- v.labels[j]
            start.ind <- matches[[j]][1]
            c(substr(lbl, 1, start.ind - 1),
              substr(lbl, start.ind + nchar(common.lbl), nchar(lbl)))
        }, character(2)))
        match.ind[[i]] <- ind
    }

    unique.prefixes.suffixes <- do.call("rbind", common.label.prefixes.suffixes)
    unique.prefixes.suffixes <- unique.prefixes.suffixes[!duplicated(unique.prefixes.suffixes,
                                                                     MARGIN = 1), , drop = FALSE]
    ord <- do.call("order", data.frame(nchar(unique.prefixes.suffixes)))
    unique.prefixes.suffixes <- unique.prefixes.suffixes[ord, , drop = FALSE]

    stacking.groups <- matrix(nrow = 0, ncol = n.common.labels)
    for (i in seq_len(nrow(unique.prefixes.suffixes)))
    {
        prefix.suffix <- unique.prefixes.suffixes[i, ]
        common.labels.ind <- lapply(seq_len(n.common.labels), function(j) {
            common.label.prefix.suffix <- common.label.prefixes.suffixes[[j]]
            ind <- which(colSums(t(common.label.prefix.suffix) == prefix.suffix) == 2)
            if (length(ind) > 0)
                match.ind[[j]][ind]
            else
                NULL
        })

        new.rows <- matchIndicesBasedOnName(common.labels.ind, v.names)
        for (j in seq_len(nrow(new.rows)))
        {
            if (!(any(removeNA(new.rows[j, ]) %in% removeNA(stacking.groups))))
                stacking.groups <- rbind(stacking.groups, new.rows[j, ])
        }
    }

    # Remove groups with only one element
    stacking.groups <- stacking.groups[rowSums(!is.na(stacking.groups)) > 1, ,
                                       drop = FALSE]

    stacking.groups
}

# Match together indices from elements in a list based on their names to
# create a rows of the output matrix, where each row represents a stacked
# variable. Names are considered matching if after their common prefixes and
# suffixes are removed, the remaining text is either numbers or letters but
# not both (since enumerations occur with letters or numbers but usually not
# both).
matchIndicesBasedOnName <- function(ind.list, nms)
{
    # Trivial case: each element in the list is at most length 1
    if (all(vapply(ind.list, length, integer(1)) < 2))
        return(t(matrix(vapply(ind.list, function(ind) {
            if (length(ind) == 0)
                NA_integer_
            else
                ind[[1]]
        }, integer(1)))))

    n.list <- length(ind.list)
    result <- matrix(nrow = 0, ncol = n.list)
    repeat
    {
        first.name <- NA_character_
        new.row <- rep(NA_integer_, n.list)
        for (i in seq_len(n.list))
        {
            ind <- ind.list[[i]]
            if (length(ind) == 0)
                next

            if (is.na(first.name))
            {
                first.name <- nms[ind[1]]
                new.row[i] <- ind[1]
                ind.list[[i]] <- ind.list[[i]][-1]
                next
            }

            for (j in seq_along(ind))
            {
                nm <- nms[ind[j]]

                prefix <- getCommonPrefixTwoNames(c(first.name, nm))
                suffix <- getCommonSuffixTwoNames(c(first.name, nm))

                nchar.prefix <- nchar(prefix)
                nchar.suffix <- nchar(suffix)

                if (nchar.prefix == 0)
                    next

                # prefix and suffix removed
                middle <- substr(nm, nchar.prefix + 1,
                                 nchar(nm) - nchar.suffix)

                # The middle part of variable names to stack needs to be either
                # numbers or letters (or empty) and not both
                if (grepl("^[[:digit:]]*$", middle) ||
                    grepl("^[[:alpha:]]*$", middle))
                {
                    new.row[i] <- ind[j]
                    ind.list[[i]] <- ind.list[[i]][-j]
                    break
                }
            }
        }
        result <- rbind(result, new.row)

        if (all(vapply(ind.list, length, integer(1)) == 0))
            break
    }
    result
}

# Parse user-input manual stacking strings and append the variables to manually
# stack to stacking.groups. Most of the work is done by either
# stackingSpecifiedByVariable or stackingSpecifiedByObservation.
stackManually <- function(stacking.groups, manual.stacking,
                          specify.by, input.data.set.metadata)
{
    if (is.null(manual.stacking) || length(manual.stacking) == 0 ||
        setequal(manual.stacking, ""))
        return(stacking.groups)

    na.ind <- match("NA", input.data.set.metadata$variable.names)
    has.na.variable <- !is.na(na.ind)
    if (has.na.variable)
        warning("There is an input variable named 'NA'. To avoid confusion, ",
                "missing stacking variables need to be specified with an ",
                "extra slash for this data set, i.e., N/A")

    stacking.groups <- if (specify.by == "Variable")
        stackingSpecifiedByVariable(stacking.groups, manual.stacking,
                                    input.data.set.metadata, has.na.variable)
    else
        stackingSpecifiedByObservation(stacking.groups, manual.stacking,
                                       input.data.set.metadata, has.na.variable)
}

# Parse user-input manual stacking strings for stacking specified by variable,
# i.e., each string in manual.stacking describes the variables to be stacked
# into one variable.
stackingSpecifiedByVariable <- function(stacking.groups, manual.stacking,
                                        input.data.set.metadata,
                                        has.na.variable)
{
    v.names <- input.data.set.metadata$variable.names
    v.types <- input.data.set.metadata$variable.types
    v.val.attr <- input.data.set.metadata$variable.value.attributes

    permitted.na <- if (has.na.variable) "N/A" else c("NA", "N/A")

    manual.stacking.groups <- list()
    manual.stacking.groups.text <- character(0)

    for (input.text in manual.stacking)
    {
        on.fail <- paste0("The manual stacking input '", input.text,
                          "' has been ignored.")
        split.text <- splitByComma(input.text)

        group.names <- character(0)

        for (t in split.text)
        {
            parsed <- if (t %in% permitted.na)
                NA_character_
            else if (grepl("-", t, fixed = TRUE)) # contains range
                parseRange(t, v.names, "manual stacking", on.fail)
            else if (grepl("*", t, fixed = TRUE)) # contains wildcard
                parseWildcard(t, v.names, "manual stacking", on.fail)
            else
                parseVariableName(t, v.names, "manual stacking",
                                  on.fail)
            if (length(parsed) == 0)
            {
                group.names <- NULL
                break
            }
            else
                group.names <- c(group.names, parsed)
        }
        if (is.null(group.names) || length(group.names) == 0)
            next

        if (all(is.na(group.names)))
        {
            warning("The manual stacking input '", input.text,
                    "' has been ignored as it does not contain any variables.")
            next
        }

        # Check for duplicate variables
        dup <- duplicated(removeNA(group.names))
        if (any(dup))
        {
            warning("The manual stacking input '", input.text,
                    "' has been ignored as it contains duplicate entries for ",
                    paste0("'", removeNA(group.names)[dup], "'",
                           collapse = ", "), ".")
            next
        }

        # Remove trailing NA
        ind <- which(!is.na(group.names))
        group.names <- group.names[seq_len(ind[length(ind)])]

        group.ind <- vapply(group.names, match, integer(1), v.names)

        # Check for mismatching variable types and value attributes
        if (!allIdentical(v.types[removeNA(group.ind)]) ||
            !allIdentical(v.val.attr[removeNA(group.ind)]))
        {
            warning("The manual stacking input '", input.text,
                    "' has been ignored as it contains variables with ",
                    "mismatching types or value attributes.")
            next
        }

        # Check for overlap with previous manual stacking inputs
        if (length(manual.stacking.groups) > 0)
        {
            group.ind.without.na <- removeNA(group.ind)
            overlap.ind <- which(vapply(manual.stacking.groups, function(manual.group) {
                any(group.ind.without.na %in% manual.group)
            }, logical(1)))
            if (length(overlap.ind) > 0)
            {
                warning("The manual stacking input '", input.text,
                        "' has been ignored as it contains variable(s) that ",
                        "overlap with another manual stacking input '",
                        manual.stacking.groups.text[overlap.ind[1]], "'.")
                next
            }
        }

        manual.stacking.groups <- c(manual.stacking.groups, list(group.ind))
        manual.stacking.groups.text <- c(manual.stacking.groups.text,
                                         input.text)
    }
    if (length(manual.stacking.groups) == 0)
        return(stacking.groups)

    if (is.null(stacking.groups))
        stacking.groups <- matrix(nrow = 0, ncol = 0)

    # Remove rows in stacking.groups that contain overlap with
    # manual.stacking.groups
    for (manual.group in manual.stacking.groups)
    {
        manual.group.without.na <- removeNA(manual.group)
        is.overlapping <- apply(stacking.groups, 1, function(group) {
            any(manual.group.without.na %in% group)
        })
        stacking.groups <- stacking.groups[!is.overlapping, , drop = FALSE]
    }

    # Increase number of columns in stacking.groups if necessary to fit
    # manual.stacking.groups
    n.rows <- nrow(stacking.groups)
    n.cols <- ncol(stacking.groups)
    max.stacking <- max(max(vapply(manual.stacking.groups, length, integer(1))),
                        n.cols)
    new.stacking.groups <- matrix(nrow = n.rows, ncol = max.stacking)
    new.stacking.groups[seq_len(n.rows), seq_len(n.cols)] <- stacking.groups
    stacking.groups <- new.stacking.groups

    # Append manual.stacking.groups to stacking.groups
    for (manual.group in manual.stacking.groups)
    {
        new.group <- rep(NA_integer_, max.stacking)
        new.group[seq_along(manual.group)] <- manual.group
        stacking.groups <- rbind(stacking.groups, new.group)
    }

    n.manual.stacking <- length(manual.stacking.groups)
    is.manually.stacked <- c(rep(FALSE, nrow(stacking.groups) - n.manual.stacking),
                             rep(TRUE, n.manual.stacking))
    attr(stacking.groups, "is.manually.stacked") <- is.manually.stacked

    stacking.groups
}

# Parse user-input manual stacking strings for stacking specified by
# observation, i.e., each string in manual.stacking describes the variables in
# an observation to be used in stackings of variables.
stackingSpecifiedByObservation <- function(stacking.groups, manual.stacking,
                                           input.data.set.metadata,
                                           has.na.variable)
{
    if (length(manual.stacking) < 2)
    {
        warning("No manual stacking was conducted as 2 or more manual",
                "stacking inputs (corresponding to obvservations) are ",
                "required.")
        return(stacking.groups)
    }

    v.names <- input.data.set.metadata$variable.names
    v.types <- input.data.set.metadata$variable.types
    v.val.attr <- input.data.set.metadata$variable.value.attributes

    permitted.na <- if (has.na.variable) "N/A" else c("NA", "N/A")

    # obs.group = observation group, which is a group of variables be stacked
    # belonging to an observation, instead of a variable (in which case I just
    # use 'group').
    manual.stacking.obs.groups <- list()
    manual.stacking.obs.groups.text <- list()

    for (input.text in manual.stacking)
    {
        on.fail <- paste0("No manual stacking was conducted.")
        split.text <- splitByComma(input.text)
        obs.group.names <- character(0)

        for (t in split.text)
        {
            parsed <- if (t %in% permitted.na)
                NA_character_
            else if (grepl("-", t, fixed = TRUE)) # contains range
                parseRange(t, v.names, "manual stacking", on.fail)
            else if (grepl("*", t, fixed = TRUE)) # contains wildcard
                parseWildcard(t, v.names, "manual stacking", on.fail)
            else
                parseVariableName(t, v.names, "manual stacking",
                                  on.fail)
            if (length(parsed) == 0)
                return(stacking.groups)
            else
                obs.group.names <- c(obs.group.names, parsed)
        }

        if (all(is.na(obs.group.names)))
        {
            warning("No manual stacking was conducted as the manual stacking ",
                    "input '", input.text, "' does not contain any variables.")
            return(stacking.groups)
        }

        # Check for duplicate variables
        dup <- duplicated(removeNA(obs.group.names))
        if (any(dup))
        {
            warning("No manual stacking was conducted as the manual stacking ",
                    "input '", input.text, "' contains duplicate entries for ",
                    paste0("'", removeNA(obs.group.names)[dup], "'",
                           collapse = ", "), ".")
            return(stacking.groups)
        }

        # Remove trailing NA
        ind <- which(!is.na(obs.group.names))
        obs.group.names <- obs.group.names[seq_len(ind[length(ind)])]

        obs.group.ind <- vapply(obs.group.names, match, integer(1),
                                v.names)

        # Check for overlap with previous manual stacking inputs
        if (length(manual.stacking.obs.groups) > 0)
        {
            obs.group.ind.without.na <- removeNA(obs.group.ind)
            overlap.ind <- which(vapply(manual.stacking.obs.groups, function(manual.obs.group) {
                any(obs.group.ind.without.na %in% manual.obs.group)
            }, logical(1)))
            if (length(overlap.ind) > 0)
            {
                warning("No manual stacking was conducted as the manual ",
                        "stacking input '", input.text, "' contains ",
                        "variable(s) that overlap with another manual ",
                        "stacking input '",
                        manual.stacking.obs.groups.text[overlap.ind[1]], "'.")
                return(stacking.groups)
            }
        }

        manual.stacking.obs.groups <- c(manual.stacking.obs.groups, list(obs.group.ind))
        manual.stacking.obs.groups.text <- c(manual.stacking.obs.groups.text,
                                             input.text)
    }

    new.stacking.groups <- if (is.null(stacking.groups))
        matrix(nrow = 0, ncol = 0)
    else
        stacking.groups

    n.manual.variables <- max(vapply(manual.stacking.obs.groups, length,
                                     integer(1)))

    # Remove rows in new.stacking.groups that contain overlap with
    # manual.stacking.groups
    for (manual.obs.group in manual.stacking.obs.groups)
    {
        manual.obs.group.without.na <- removeNA(manual.obs.group)
        is.overlapping <- apply(new.stacking.groups, 1, function(obs.group) {
            any(manual.obs.group.without.na %in% obs.group)
        })
        new.stacking.groups <- new.stacking.groups[!is.overlapping, , drop = FALSE]
    }

    # Increase number of columns in new.stacking.groups if necessary to fit
    # manual.stacking.groups
    n.rows <- nrow(new.stacking.groups)
    n.cols <- ncol(new.stacking.groups)
    max.stacking <- max(length(manual.stacking.obs.groups), n.cols)
    m <- matrix(nrow = n.rows, ncol = max.stacking)
    m[seq_len(n.rows), seq_len(n.cols)] <- new.stacking.groups
    new.stacking.groups <- m

    # Append manual.stacking.groups to stacking.groups
    ind <- seq_along(manual.stacking.obs.groups)
    for (i in seq_len(n.manual.variables))
    {
        new.group <- rep(NA_integer_, max.stacking)
        new.group[ind] <- vapply(manual.stacking.obs.groups, `[`,
                                 integer(1), i)
        non.missing.ind <- removeNA(new.group)
        if (!allIdentical(v.types[non.missing.ind]) ||
            !allIdentical(v.val.attr[non.missing.ind]))
        {
            warning("No manual stacking was conducted as the manual ",
                    "stacking input '", input.text, "' would result in the ",
                    "stacking of variables with mismatching types or ",
                    "value attributes.")
            return(stacking.groups)
        }

        new.stacking.groups <- rbind(new.stacking.groups, new.group)
    }

    is.manually.stacked <- c(rep(FALSE, nrow(new.stacking.groups) - n.manual.variables),
                             rep(TRUE, n.manual.variables))
    attr(new.stacking.groups, "is.manually.stacked") <- is.manually.stacked

    new.stacking.groups
}

# Constructs the stacked data set as a data frame from the input data set and
# the stacking.groups matrix
stackedDataSet <- function(input.data.set, input.data.set.metadata,
                           stacking.groups, include.original.case.variable,
                           include.observation.variable,
                           common.labels.list)
{
    if (is.null(stacking.groups) || nrow(stacking.groups) == 0)
    {
        return(data.frame(lapply(input.data.set, function(v) {
            attr(v, "is.stacked") <- FALSE
            attr(v, "is.manually.stacked") <- NA
            v
        })))
    }

    input.v.names <- input.data.set.metadata$variable.names
    input.v.labels <- input.data.set.metadata$variable.labels
    retained.indices <- retainedIndices(stacking.groups,
                                        input.data.set.metadata$n.variables)
    stacked.indices <- stackedIndices(stacking.groups, retained.indices)

    stacked.data.set.v.names <- stackedDataSetVariableNames(stacking.groups,
                                                            input.v.names,
                                                            retained.indices,
                                                            stacked.indices)
    stacked.data.set.v.labels <- stackedDataSetVariableLabels(stacking.groups,
                                                              input.v.labels,
                                                              retained.indices,
                                                              stacked.indices,
                                                              stacked.data.set.v.names)

    checkStackedDataSetSize(input.data.set, stacking.groups, stacked.indices,
                            stacked.data.set.v.names)

    n.stacked <- ncol(stacking.groups)
    is.manually.stacked <- attr(stacking.groups, "is.manually.stacked")
    stacked.data.set <- data.frame(lapply(seq_along(stacked.data.set.v.names), function(i) {
        ind <- match(i, stacked.indices)
        if (!is.na(ind)) # Stacked variable
        {
            group.ind <- stacking.groups[ind, ]
            v <- unlist(lapply(group.ind, function(j) {
                if (!is.na(j))
                    input.data.set[[j]]
                else
                    rep(NA, nrow(input.data.set))
            }))
            v <- c(t(matrix(v, nrow = nrow(input.data.set))))
            attr(v, "is.stacked") <- TRUE
            attr(v, "is.manually.stacked") <- is.manually.stacked[ind]
            attr(v, "stacking.input.variable.names") <- input.v.names[group.ind]
            attr(v, "stacking.input.variable.labels") <- input.v.labels[group.ind]
            val.attr <- attr(input.data.set[[removeNA(group.ind)[1]]],
                             "labels", exact = TRUE)
        }
        else # Not stacked variable
        {
            input.var <- input.data.set[[stacked.data.set.v.names[i]]]
            v <- rep(input.var, each = n.stacked)
            attr(v, "is.stacked") <- FALSE
            attr(v, "is.manually.stacked") <- NA
            val.attr <- attr(input.var, "labels", exact = TRUE)
        }
        attr(v, "label") <- stacked.data.set.v.labels[i]
        if (!is.null(val.attr))
        {
            attr(v, "labels") <- val.attr
            class(v) <- c(class(v), "haven_labelled")
        }
        v
    }))

    names(stacked.data.set) <- c(stacked.data.set.v.names)

    if (include.original.case.variable)
    {
        original.case <- rep(seq_len(nrow(input.data.set)), each = n.stacked)
        attr(original.case, "label") <- "Original case number (pre stacking)"
        attr(original.case, "is.stacked") <- FALSE
        attr(original.case, "is.manually.stacked") <- NA
        attr(original.case, "is.original.case") <- TRUE
        stacked.data.set[[uniqueName("original_case",
                                     stacked.data.set.v.names)]] <- original.case
    }

    if (include.observation.variable)
    {
        observation <- rep(seq_len(n.stacked), nrow(input.data.set))
        attr(observation, "label") <- "Observation # (from stacking)"

        if (!is.null(common.labels.list))
        {
            common.labels <- common.labels.list[[1]]
            val.attr <- seq_len(n.stacked)
            names(val.attr) <- paste0("Observation ", seq_len(n.stacked))
            names(val.attr)[seq_along(common.labels)] <- common.labels
            attr(observation, "labels") <- val.attr
        }
        attr(observation, "is.stacked") <- FALSE
        attr(observation, "is.manually.stacked") <- NA
        attr(observation, "is.observation") <- TRUE

        stacked.data.set[[uniqueName("observation",
                                     stacked.data.set.v.names)]] <- observation
    }

    stacked.data.set
}

checkStackedDataSetSize <- function(input.data.set, stacking.groups,
                                    stacked.indices, stacked.data.set.v.names,
                                    common.labels)
{
    n.stacked <- ncol(stacking.groups)

    v.sizes <- vapply(seq_along(stacked.data.set.v.names), function(i) {
        ind <- match(i, stacked.indices)
        if (!is.na(ind)) # Stacked variable
        {
            j <- removeNA(stacking.groups[ind, ])[1]
            object.size(input.data.set[[j]]) * n.stacked
        }
        else # Not stacked variable
            object.size(input.data.set[[stacked.data.set.v.names[i]]]) * n.stacked
    }, numeric(1))

    # Set to 4GB as I found that memory issues start to occur around here.
    # May have to lower this if we find that users still get memory errors.
    if (sum(v.sizes) > 4 * 1e9)
    {
        msg <- paste0("The stacked data set is too large to create. Omit unnecessary ",
                      "variables to reduce its size.")
        if (!is.null(common.labels))
            msg <- paste0(msg, " Also ensure that the common labels are ",
                          "appropriate: ",
                          paste0(common.labels, collapse = ", "), ".")
        stop(msg)
    }
}

# Indices of input variables retained after stacking
retainedIndices <- function(stacking.groups, n.vars)
{
    ind.to.remove <- unlist(lapply(seq_len(nrow(stacking.groups)), function(i) {
        ind <- removeNA(stacking.groups[i, ])
        ind <- ind[-which.min(ind)]
    }))
    seq_len(n.vars)[-ind.to.remove]
}

# Indices of stacked variables in stacked data set
stackedIndices <- function(stacking.groups, retained.indices)
{
    first.ind <- apply(stacking.groups, 1, function(group.ind) {
        ind <- removeNA(group.ind)
        ind[which.min(ind)]
    })

    vapply(first.ind, match, integer(1), retained.indices)
}

stackedDataSetVariableNames <- function(stacking.groups, input.variable.names,
                                        retained.indices, stacked.indices)
{
    result <- input.variable.names[retained.indices]
    result[stacked.indices] <- apply(stacking.groups, 1, function(group.ind) {
        ind <- removeNA(group.ind)
        nm <- input.variable.names[ind]
        common.prefix <- trimws(getCommonPrefix(nm))
        common.suffix <- trimws(getCommonSuffix(nm))
        candidate <- trimws(paste0(common.prefix, common.suffix))
        if (candidate != "")
            candidate
        else
            "stacked_var"
    })

    dup <- which(duplicated(result))
    for (i in dup)
        result[i] <- uniqueName(result[i], result[-i], "_")

    result
}

stackedDataSetVariableLabels <- function(stacking.groups,
                                         input.variable.labels,
                                         retained.indices, stacked.indices,
                                         stacked.data.set.variable.names)
{
    result <- input.variable.labels[retained.indices]
    result[stacked.indices] <- apply(stacking.groups, 1, function(group.ind) {
        ind <- removeNA(group.ind)
        lbl <- input.variable.labels[ind]
        common.prefix <- trimws(getCommonPrefix(lbl))
        common.suffix <- trimws(getCommonSuffix(lbl))
        if (common.prefix == "" && common.suffix == "")
            NA_character_
        else if (common.prefix == common.suffix)
            common.prefix
        else
            trimws(paste(common.prefix, common.suffix))
    })
    require.label <- is.na(result)
    result[require.label] <- stacked.data.set.variable.names[require.label]
    result
}

# Creates a name from new.name that does not exist in existing.names by
# appending a numeric suffix if necessary
uniqueName <- function(new.name, existing.names, delimiter = "")
{
    if (!(new.name %in% existing.names))
        return (new.name)

    i <- 1
    repeat
    {
        candidate.name <- paste0(new.name, delimiter, i)
        if (!(candidate.name %in% existing.names))
            return(candidate.name)
        i <- i + 1
    }
}

# Common prefix from a character vector of names.
# If whole.words is TRUE, the prefix is truncated so that it does not end
# halfway into a word or number.
getCommonPrefix <- function(nms, whole.words = FALSE)
{
    common_prefix <- ""
    for (i in 1:min(nchar(nms)))
    {
        if (allIdentical(vapply(tolower(nms), substr, character(1), 1, i)))
            common_prefix <- substr(nms[1], 1, i)
        else
            break
    }

    if (whole.words)
    {
        ind <- gregexpr("[^[:alnum:]]", common_prefix)[[1]]
        if (setequal(ind, -1))
            ""
        else
            substr(common_prefix, 1, ind[length(ind)])
    }
    else
        common_prefix
}

# Faster than getCommonPrefix but only works on character vectors of length 2
getCommonPrefixTwoNames <- function(nms)
{
    n.chars <- nchar(nms)
    min.n.chars <- min(n.chars)
    split.nms <-  strsplit(nms, "")
    char.ind <- seq_len(min.n.chars)
    last.ind <- match(TRUE, split.nms[[1]][char.ind] != split.nms[[2]][char.ind]) - 1
    if (is.na(last.ind)) # one is a subset of the other
        nms[which.min(n.chars)]
    else
        paste0(split.nms[[1]][seq_len(last.ind)], collapse = "")
}

# Faster than getCommonSuffix but only works on character vectors of length 2
getCommonSuffixTwoNames <- function(nms)
{
    n.chars <- nchar(nms)
    min.n.chars <- min(n.chars)
    split.nms <- lapply(strsplit(nms, ""), rev)
    char.ind <- seq_len(min.n.chars)
    first.ind <- match(TRUE, split.nms[[1]][char.ind] != split.nms[[2]][char.ind]) - 1
    if (is.na(first.ind)) # one is a subset of the other
        nms[which.min(n.chars)]
    else
        paste0(rev(split.nms[[1]][seq_len(first.ind)]), collapse = "")
}

# Common suffix from a character vector of names.
# If whole.words is TRUE, the prefix is truncated so that it does not end
# halfway into a word or number.
getCommonSuffix <- function(nms, whole.words = FALSE)
{
    common_suffix <- ""
    for (i in 1:min(nchar(nms)))
    {
        suffixes <- vapply(tolower(nms), function(nm) {
            substr(nm, nchar(nm) - i + 1, nchar(nm))
        }, character(1))
        if (allIdentical(suffixes))
            common_suffix <- substr(nms[1], nchar(nms[1]) - i + 1, nchar(nms[1]))
        else
            break
    }

    if (whole.words)
    {
        ind <- gregexpr("[^[:alnum:]]", common_suffix)[[1]]
        if (setequal(ind, -1))
            ""
        else
            substr(common_suffix, ind[1], nchar(common_suffix))
    }
    else
        common_suffix
}

# Splits user-input text for variables to omit by commas
splitVariablesToOmitText <- function(variables.to.omit)
{
    if (is.null(variables.to.omit) || length(variables.to.omit) == 0 ||
        setequal(variables.to.omit, ""))
        return(character(0))

    unlist(lapply(variables.to.omit, splitByComma))
}

# Variable names from a data set that can be omitted, i.e., all variable names
# except original_case and observation variables
omittableNames <- function(data.set)
{
    is.omittable <- vapply(data.set, function(v) {
        is.original.case <- attr(v, "is.original.case")
        is.observation <- attr(v, "is.observation")
        (is.null(is.original.case) || !is.original.case) &&
            (is.null(is.observation) || !is.observation)
    }, logical(1))
    names(data.set)[is.omittable]
}

# Parses user-input variables to omit which is a character vector,
# i.e., the variables to omit can be specified over one or more strings.
parseVariablesToOmit <- function(variables.to.omit, variable.names,
                                 warning.if.not.found)
{
    if (length(variables.to.omit) == 0)
        return(character(0))

    if (length(warning.if.not.found) == 1)
        warning.if.not.found <- rep(warning.if.not.found, length(variables.to.omit))

    purpose <- "omitted variable"

    parse.status <- character(length(variables.to.omit))

    result <- character(0)
    for (i in seq_along(variables.to.omit))
    {
        t <- variables.to.omit[i]
        parsed <- if (grepl("-", t, fixed = TRUE)) # contains range
            parseRange(t, variable.names, purpose,
                       "The input range has been ignored.",
                       warning.if.not.found[i])
        else if (grepl("*", t, fixed = TRUE)) # contains wildcard
            parseWildcard(t, variable.names, purpose,
                          "This input has been ignored.",
                          warning.if.not.found[i])
        else
            parseVariableName(t, variable.names, purpose,
                              "This input has been ignored.",
                              warning.if.not.found[i])

        result <- c(result, parsed)
        if (length(parsed) > 0)
            parse.status[i] <- "parsed"
        else
        {
            is.not.found <- !is.null(attr(parsed, "is.not.found")) &&
                            attr(parsed, "is.not.found")
            if (is.not.found)
                parse.status[i] <- "variable(s) not found"
            else
                parse.status[i] <- "bad format"
        }
    }

    # Order omitted variables according to variable.names
    result <- result[order(vapply(result, match, integer(1), variable.names))]
    attr(result, "parse.status") <- parse.status
    result
}

parseVariablesToOmitAfterStacking <- function(variables.to.omit,
                                              omitted.variables,
                                              stacked.data.set)
{
    parse.status <- attr(omitted.variables, "parse.status")
    variables.to.omit <- variables.to.omit[parse.status != "bad format"]
    omittable.names <- omittableNames(stacked.data.set)
    warning.if.not.found <- parse.status[parse.status != "bad format"] ==
                            "variable(s) not found"

    c(omitted.variables,
      parseVariablesToOmit(variables.to.omit,
                           omittable.names,
                           warning.if.not.found))
}

# Parses a user-input variable range
parseRange <- function(range.text, variable.names, purpose, on.fail,
                       warning.if.not.found = TRUE)
{
    dash.ind <- match("-", strsplit(range.text, "")[[1]])
    start.var.text <- trimws(substr(range.text, 1, dash.ind - 1))
    end.var.text <- trimws(substr(range.text, dash.ind + 1, nchar(range.text)))
    if (grepl("*", start.var.text, fixed = TRUE))
    {
        warning("The start variable from the ", purpose, " input range '",
                range.text, "' contains the wildcard character '*' which is ",
                "not permitted in a range. ", on.fail)
        return(character(0))
    }
    if (grepl("*", end.var.text, fixed = TRUE))
    {
        warning("The end variable from the ", purpose, " input range '",
                range.text, "' contains the wildcard character '*' which is ",
                "not permitted in a range. ", on.fail)
        return(character(0))
    }

    start.ind <- if (start.var.text != "")
        match(start.var.text, variable.names)
    else
        1

    end.ind <- if (end.var.text != "")
        match(end.var.text, variable.names)
    else
        length(variable.names)

    if (is.na(start.ind))
    {
        if (warning.if.not.found)
        {
            warning("The start variable from the ", purpose, " input range '",
                    range.text, "' ", "could not be identified. ",on.fail,
                    " Ensure that the variable name is correctly specified.")
        }
        result <- character(0)
        attr(result, "is.not.found") <- TRUE
        return(result)
    }
    if (is.na(end.ind))
    {
        if (warning.if.not.found)
        {
            warning("The end variable from the ", purpose, " input range '",
                    range.text, "' ", "could not be identified. ",on.fail,
                    " Ensure that the variable name is correctly specified.")
        }
        result <- character(0)
        attr(result, "is.not.found") <- TRUE
        return(result)
    }
    if (start.ind > end.ind)
    {
        warning("The start variable from the ", purpose, "input range '",
                range.text, "' ", "appears after the end variable in the ",
                "data set. Ensure that the range has been correctly ",
                "specified. ", on.fail)
        return(character(0))
    }
    variable.names[start.ind:end.ind]
}

# Parses a user-input variable wildcard
#' @importFrom flipU EscapeRegexSymbols
parseWildcard <- function(wildcard.text, variable.names, purpose, on.fail,
                          warning.if.not.found = TRUE)
{
    ind.asterisk <- match("*", strsplit(wildcard.text, "")[[1]])
    start.var.text <- trimws(substr(wildcard.text, 1, ind.asterisk - 1))
    end.var.text <- trimws(substr(wildcard.text, ind.asterisk + 1,
                                  nchar(wildcard.text)))
    pattern <- paste0("^", EscapeRegexSymbols(start.var.text), ".*",
                      EscapeRegexSymbols(end.var.text), "$")
    is.match <- grepl(pattern, variable.names)
    if (!any(is.match))
    {
        if (warning.if.not.found)
            warning("No matches were found for the ", purpose,
                    " input wildcard name '",
                    wildcard.text, "'. Ensure that the wildcard variable name ",
                    "has been correctly specified. ", on.fail)
        result <- character(0)
        attr(result, "is.not.found") <- TRUE
        return(result)
    }
    variable.names[is.match]
}

# Parses a user-input variable name
parseVariableName <- function(variable.name.text, variable.names, purpose,
                              on.fail, warning.if.not.found = TRUE)
{
    if (variable.name.text %in% variable.names)
        variable.name.text
    else
    {
        if (warning.if.not.found)
            warning("The ", purpose, " input varible name '", variable.name.text,
                    "' could not be identified. ", on.fail)
        result <- character(0)
        attr(result, "is.not.found") <- TRUE
        return(result)
    }
}

# Returns the names of the variables which will be omitted
omitVariablesFromDataSet <- function(data.set, variables.to.omit)
{
    data.set[!(names(data.set) %in% variables.to.omit)]
}

# Returns the names of the stacked variables which will be omitted
omittedStackedVariables <- function(variables.to.omit, stacked.data.set)
{
    is.stacked <- vapply(stacked.data.set, attr, logical(1),
                         "is.stacked")[variables.to.omit]
    variables.to.omit[!is.na(is.stacked) & is.stacked]
}

# Cleans a user-input data set name or creates one from the input data set name
cleanStackedDataSetName <- function(stacked.data.set.name, input.data.set.name)
{
    if (is.null(stacked.data.set.name) || stacked.data.set.name == "")
    {
        if (grepl("\\.sav$", input.data.set.name))
        {
            n <- nchar(input.data.set.name)
            name.without.sav <- substr(input.data.set.name, 1, n - 4)
            return(paste0(name.without.sav, " stacked.sav"))
        }
        else
            return(paste0(input.data.set.name, " stacked.sav"))
    }
    else if (!grepl("\\.sav$", stacked.data.set.name))
        return(paste0(stacked.data.set.name, ".sav"))
    else
        return(stacked.data.set.name)
}

# Creates a list of metadata from a data set
metadataFromDataSet <- function(data.set, data.set.name)
{
    list(variable.names = names(data.set),
         variable.labels = vapply(data.set, function(v) {
             lbl <- attr(v, "label", exact = TRUE)
             ifelse(!is.null(lbl), lbl, "")
         }, character(1)),
         variable.types = vapply(data.set, variableType, character(1)),
         variable.value.attributes = lapply(data.set, attr, "labels",
                                      exact = TRUE),
         n.variables = length(data.set),
         data.set.name = dataSetNameWithoutPath(data.set.name))
}

# Creates a list of metadata from a stacked data set
metadataFromStackedDataSet <- function(stacked.data.set, stacked.data.set.name)
{
    result <- metadataFromDataSet(stacked.data.set, stacked.data.set.name)
    result$is.stacked.variable <- vapply(stacked.data.set, attr, logical(1),
                                         "is.stacked")
    result$is.manually.stacked.variable <- vapply(stacked.data.set, attr,
                                                  logical(1),
                                                  "is.manually.stacked")
    result$stacking.input.variable.names <- lapply(stacked.data.set, attr,
                                                   "stacking.input.variable.names")
    result$stacking.input.variable.labels <- lapply(stacked.data.set, attr,
                                                    "stacking.input.variable.labels")
    result
}

# Gets the variable type from a variable. The types are used internally by
# R code and not intended to be exposed to the user.
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

# Whether all elements in a vector are identical
allIdentical <- function(x)
{
    length(unique(x)) < 2
}

# Remove NA values from a vector
removeNA <- function(x)
{
    x[!is.na(x)]
}

# Split string by comma separators, removing whitespace and empty strings
splitByComma <- function(input.text)
{
    split.text <- trimws(strsplit(input.text, ",")[[1]])
    split.text[split.text != ""]
}

#' @importFrom flipFormat StackingWidget
#' @export
print.StackedData <- function(x, ...)
{
    StackingWidget(x$stacked.data.set.metadata,
                   x$unstackable.names,
                   x$omitted.variables,
                   x$omitted.stacked.variables,
                   x$common.labels,
                   x$is.saved.to.cloud)
}
