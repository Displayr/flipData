#' @title Stack data set
#' @description Stacks variables in a SPSS .sav data set that may be located
#'   locally or on the Displayr cloud drive (if run in Displayr). Stacking may
#'   be specified manually and/or by identifying common labels that appear in
#'   variable labels.
#'
#'   Manual stacking can be specified by variable or by observation. With the
#'   former, each group of variables to be stacked together is specified. With
#'   the latter, the variables in each stacked observation are specified (in
#'   order of the stacked variables). Any stacking can be performed with either
#'   option but often one is more convenient than the other depending on the
#'   structure and variable names of the required stacking.
#'
#'   Common label stacking occurs by stacking together groups of variables
#'   whose labels match the common labels after removing common prefixes and
#'   suffixes. Common labels can be specified to be generated automatically,
#'   inferred from a set of input reference variables or specified manually.
#'
#'   The stacked data set is saved as an SPSS .sav data set either locally or
#'   to the Displayr cloud drive (if run in Displayr).
#' @param input.data.set.name Name of data file to stack, either as a path to a
#'   local file (when running locally in R) or file in the Displayr Cloud Drive
#'   (when running in Displayr).
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
#'   variables to be stacked must contain these labels, and have the same prefix
#'   and suffix before and after these labels.
#' @param specify.by "Variable" or "Observation". See \code{manual.stacking}.
#' @param manual.stacking If \code{specify.by} is "Variable", this is a
#'   character vector where each string corresponds to the names of the
#'   variables to be stacked into a new variable. If \code{specify.by} is
#'   "Observation", this is a character vector where each string corresponds
#'   the names of the variables in an observation in the stacked variables.
#'   The strings in both cases can be a combination of comma-separated names,
#'   wildcards and ranges.
#'   See \code{reference.variables.to.stack} for more details on the format.
#' @param variables.to.include Character vector of comma-separated names of
#'   non-stacked variables to include. Each string can be a combination of
#'   comma-separated names, wildcards and ranges.
#'   See \code{reference.variables.to.stack} for more details on the format.
#' @param include.stacked.data.set.in.output Whether to include the stacked
#'   data set in the output object.
#' @param include.original.case.variable Whether to include the \code{original_case}
#'   variable in the stacked data set.
#' @param include.observation.variable Whether to include the \code{observation}
#'   variable in the stacked data set.
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{stacked.data.set.metadata} A list containing metadata on the
#'     the stacked data set such as variable names, labels etc.
#'   \item \code{unstackable.names} A list of character vectors containing
#'     names of the variables that could not be stacked using common labels due
#'     to mismatching types or value attributes.
#'   \item \code{common.labels.list} A list of character vectors containing
#'     the common labels used in the stacking. The source of these common
#'     labels depends on the parameter \code{stack.with.common.labels} and
#'     are either automatically generated, extracted from reference variables
#'     or manually supplied.
#'   \item \code{is.saved.to.cloud} Whether the stacked data set was saved to
#'     the Displayr cloud drive.
#'  }
#' @examples
#' path <- system.file("examples", "Cola.sav", package = "flipData")
#'
#' # Automatic common label stacking and manual stacking by variable
#' print(StackData(path,
#'                 specify.by = "Variable",
#'                 manual.stacking = c("Q6_*, NA", "Q9_A, Q9_B, Q9_C-Q9_F")))
#'
#' # Common labels from reference variables and included non-stacked variables
#' print(StackData(path,
#'                 stack.with.common.labels = "Using a set of variables to stack as reference",
#'                 reference.variables.to.stack = c("Q5_5_*", "Q6_A-Q6_F"),
#'                 variables.to.include = c("Q2")))
#'
#' # Manually specified common labels and manual stacking by observation
#' common.labels <- list(c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
#'                         "Diet Pepsi", "Pepsi Max", "None of these"))
#' print(StackData(path,
#'                 stack.with.common.labels = "Using manually input common labels",
#'                 manual.common.labels = common.labels,
#'                 specify.by = "Observation",
#'                 manual.stacking = c("Q6_A,Q9_A", "Q6_B,Q9_B", "Q6_C,Q9_C",
#'                                     "Q6_D,Q9_D", "Q6_E,Q9_E", "Q6_F,Q9_F")))
#' @export
StackData <- function(input.data.set.name,
                      stacked.data.set.name = NULL,
                      stack.with.common.labels = "Automatically",
                      reference.variables.to.stack = NULL,
                      manual.common.labels = NULL,
                      specify.by = "Variable",
                      manual.stacking = NULL,
                      variables.to.include = NULL,
                      include.stacked.data.set.in.output = FALSE,
                      include.original.case.variable = TRUE,
                      include.observation.variable = TRUE)
{
    # Load input data set as data frame
    input.data.set <- readDataSets(input.data.set.name, 1)[[1]]

    # Create an object containing metadata on the input data set such as
    # variable names and labels which can be easily passed into function calls
    input.data.set.metadata <- metadataFromDataSet(input.data.set,
                                                   input.data.set.name)

    # Get the list of common labels to be used in stacking, either
    # automatically, via reference variables, or manually
    common.labels.list <- commonLabels(manual.common.labels,
                                       stack.with.common.labels,
                                       input.data.set.metadata,
                                       reference.variables.to.stack)

    # Compute stacking groups from common labels and manual stacking groups,
    # and then merge them together. A stacking group is a matrix that specifies
    # variables to be stacked via their indices in the input data set. Each row
    # represents a group of variables to be stacked.
    common.label.stacking.groups <- stackWithCommonLabels(common.labels.list,
                                                          input.data.set.metadata)
    manual.stacking.groups <- stackManually(manual.stacking, specify.by,
                                            input.data.set.metadata)
    stacking.groups <- mergeCommonLabelAndManualStackingGroups(common.label.stacking.groups,
                                                               manual.stacking.groups)

    included.variable.names <- parseVariablesToInclude(variables.to.include,
                                                       input.data.set.metadata,
                                                       stacking.groups)

    # Create the stacked data set as a data frame from the stacking groups
    stacked.data.set <- stackedDataSet(input.data.set, input.data.set.metadata,
                                       stacking.groups,
                                       include.original.case.variable,
                                       include.observation.variable,
                                       common.labels.list,
                                       included.variable.names)

    stacked.data.set.name <- cleanStackedDataSetName(stacked.data.set.name,
                                                     input.data.set.name)

    write.stacked.data.set <- nrow(stacked.data.set) > 0
    is.saved.to.cloud <- write.stacked.data.set && IsDisplayrCloudDriveAvailable()
    if (write.stacked.data.set)
        writeDataSet(stacked.data.set, stacked.data.set.name, is.saved.to.cloud)

    # Create an object containing metadata on the stacked data set such as
    # variable names and labels
    stacked.data.set.metadata <- metadataFromStackedDataSet(stacked.data.set,
                                                            stacked.data.set.name)

    result <- list()
    result$input.data.set.metadata <- input.data.set.metadata
    result$stacked.data.set.metadata <- stacked.data.set.metadata
    result$unstackable.names <- attr(stacking.groups, "unstackable.names")
    result$common.labels.list <- common.labels.list
    result$is.saved.to.cloud <- is.saved.to.cloud

    if (include.stacked.data.set.in.output)
        result$stacked.data.set <- stacked.data.set

    class(result) <- "StackedData"
    result
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
            warning("Input common labels have been ignored because common labels are to be generated automatically.")
        return(automaticCommonLabels(input.data.set.metadata))
    }
    else if (stack.with.common.labels == "Using a set of variables to stack as reference")
    {
        if (!is.null(manual.common.labels) && length(manual.common.labels) > 0)
            warning("Input common labels have been ignored because common labels are to be obtained from a set of variables.")
        return(commonLabelsFromReferenceVars(reference.variables.to.stack,
                                             input.data.set.metadata))
    }
    else if (stack.with.common.labels == "Using manually input common labels")
        tidyManualCommonLabels(manual.common.labels)
    else if (stack.with.common.labels == "Disabled")
    {
        if (!is.null(manual.common.labels) && length(manual.common.labels) > 0)
            warning("Input common labels have been ignored as stacking with common labels has been disabled.")
        return(NULL)
    }
    else
        stop("Input for stack.with.common.labels not recognised: ",
             stack.with.common.labels)
}

# Finds a set of common labels from the variable labels. This works by
# 1. Finding all the complete-word prefixes between every pair of labels that
#    appear at least 3 times.
# 2. For each prefix, extract candidate common labels after removing the prefix
#    from labels.
# 3. For each set of candidate common labels, form stacking groups
# 4. Score stacking groups based on size, removing ones with many missing values
# 5. Choose common labels with highest score
# See unit test for automaticCommonLabels in test-stacking.R
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

    # Step 1
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
                prefixes <- c(prefixes, prefix)
                prefix.count <- c(prefix.count, 1)
            }
            else
                prefix.count[prefix.ind] <- prefix.count[prefix.ind] + 1
        }
    }
    prefixes <- prefixes[prefix.count > 2]

    # Step 2
    candidate.common.labels <- lapply(prefixes, commonLabelsByRemovingPrefix,
                                      v.labels)
    candidate.common.labels <- candidate.common.labels[!duplicated(candidate.common.labels)]

    # Step 3 and 4
    score <- vapply(candidate.common.labels, function(common.labels)
    {
        if (length(common.labels) == 1)
            return(-Inf)

        stacking.groups <- stackingGroupFromCommonLabels(common.labels,
                                                         v.names, v.labels)
        if (nrow(stacking.groups) == 0)
            return(-Inf)

        whole.column.missing <- apply(stacking.groups, 2,
                                      function(column) all(is.na(column)))
        stacking.groups <- stacking.groups[, !whole.column.missing, drop = FALSE]

        if (sum(!is.na(stacking.groups)) / length(stacking.groups) < 0.75)
            return(-Inf)

        sum(!is.na(stacking.groups))
    }, numeric(1))

    if (length(score) == 0 || max(score) == -Inf)
    {
        warning("No stacking was performed with common labels as they could not be found automatically. ",
                "To stack with common labels, specify them with reference variables or manually. ",
                "Otherwise specify stacking manually.")
        return(NULL)
    }

    # Step 5
    common.labels <- candidate.common.labels[[which.max(score)]]
    stacking.groups <- stackingGroupFromCommonLabels(common.labels,
                                                     v.names, v.labels)
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
# See unit tests for commonLabelsFromReferenceVars in test-stacking.R
commonLabelsFromReferenceVars <- function(reference.variables.to.stack,
                                          input.data.set.metadata)
{
    if (is.null(reference.variables.to.stack) ||
        length(reference.variables.to.stack) == 0)
    {
        warning("No reference variables to stack were supplied for common labels. ",
                "No stacking was conducted using common labels.")
        return(NULL)
    }
    common.labels.list <- list()
    for (i in seq_along(reference.variables.to.stack))
    {
        common.labels <- commonLabelsFromASetOfReferenceVars(reference.variables.to.stack[i],
                                                             input.data.set.metadata)
        if (!is.null(common.labels))
            common.labels.list <- c(common.labels.list, list(common.labels))
    }

    if (length(common.labels.list) > 0)
        common.labels.list
    else
    {
        warning("No common labels could be extracted from the input reference variables.")
        NULL
    }
}

# Extract common labels from a single set of reference variables.
# See unit tests for commonLabelsFromASetOfReferenceVars in test-stacking.R
commonLabelsFromASetOfReferenceVars <- function(ref.vars.to.stack.text,
                                                input.data.set.metadata)
{
    split.text <- splitByComma(ref.vars.to.stack.text)

    if (length(split.text) == 0)
        return(NULL)

    on.fail.msg <- paste0("Common labels could not be obtained from the input '",
                          ref.vars.to.stack.text, "'.")

    v.names <- input.data.set.metadata$variable.names
    v.labels <- input.data.set.metadata$variable.labels

    parsed.var.names <- character(0)
    for (t in split.text)
    {
        parsed <- if (grepl("-", t, fixed = TRUE)) # contains range
            parseVariableRange(t, v.names, "common labels", on.fail.msg)
        else if (grepl("*", t, fixed = TRUE)) # contains wildcard
            parseVariableWildcard(t, v.names, "common labels", on.fail.msg)
        else
            parseVariableName(t, v.names, "common labels",
                              on.fail.msg)

        if (length(parsed) == 0)
            return(NULL)

        parsed.var.names <- c(parsed.var.names, parsed)
    }

    if (length(parsed.var.names) == 1)
    {
        warning("Only one variable is present in the input '",
                ref.vars.to.stack.text,
                "' for extracting common labels. ",
                "It has been ignored as more than one variable is required.")
        return(NULL)
    }

    lbls.containing.common.lbls <- vapply(parsed.var.names, function(nm) {
        v.labels[match(nm, v.names)]
    }, character(1))

    nchar.prefix <- nchar(getCommonPrefix(lbls.containing.common.lbls, whole.words = TRUE))
    nchar.suffix <- nchar(getCommonSuffix(lbls.containing.common.lbls, whole.words = TRUE))

    vapply(lbls.containing.common.lbls, function(lbl) {
        substr(lbl, nchar.prefix + 1, nchar(lbl) - nchar.suffix)
    }, character(1), USE.NAMES = FALSE)
}

# Tidy up user-input common labels and check for issues
tidyManualCommonLabels <- function(manual.common.labels)
{
    if (is.null(manual.common.labels))
    {
        warning("No common labels were manually supplied. ",
                "No stacking was conducted using common labels.")
        return(NULL)
    }

    common.labels.list <- lapply(seq_along(manual.common.labels), function(i) {
        lbls <- manual.common.labels[[i]]
        lbls <- trimws(lbls)
        lbls <- lbls[lbls != ""]
        if (length(lbls) == 0)
        {
            warning("Set ", i,
                    " of the manually-entered common labels does not contain any labels. ",
                    "Ensure that the common labels are correctly entered.")
            return(NULL)
        }
        else if (length(lbls) == 1)
        {
            warning("Set ", i,
                    " of the manually-entered common labels contains only one label when more than one is required.")
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
# See unit test for stackWithCommonLabels in test-stacking.R
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
                                   v.names, v.labels)

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
                stacking.groups <- rbind(stacking.groups, new.row,
                                         deparse.level = 0)
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
                    stacking.groups <- rbind(stacking.groups, new.row,
                                             deparse.level = 0)
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
        warning("Some variables could not be stacked due to mismatching variable types or value attributes. ",
                "See Notes section in output for more details.")

    if (length(unstackable.ind) > 0)
        stacking.groups <- stacking.groups[-unstackable.ind, , drop = FALSE]
    attr(stacking.groups, "unstackable.names") <- unstackable.names
    stacking.groups
}

# Creates a stacking group matrix from common labels. This does the
# heavy-lifting for stackWithCommonLabels, i.e., determining which variables
# to stack together.
# This works by finding all the prefixes and suffixes from labels containing
# common labels and grouping together variables with common labels and the same
# prefix and suffix, also considering common prefixes and suffixes in variable
# names if necessary.
# See unit tests for stackingGroupFromCommonLabels in test-stacking.R
stackingGroupFromCommonLabels <- function(common.labels, v.names, v.labels)
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
            c(trimws(substr(lbl, 1, start.ind - 1)),
              trimws(substr(lbl, start.ind + nchar(common.lbl), nchar(lbl))))
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

# This function is needed by stacking with common labels because sometimes
# multiple variables have the same common label, label prefix and label suffix.
# In such a situation, it is not clear how to group variables just from the
# labels. We therefore fall back on variable names to finalize the grouping.

# The elements of ind.list correspond to a set of common labels and each
# element contains indices of names that have the same common label, label
# prefix and label suffix. The indices refer to the variables whose names are
# in nms.

# This function returns a matrix where each row contains the indices of
# a group of matched variables to be stacked together. A group is created by
# selecting one or zero variable indices from each element of ind.list such
# that the resulting variable names have common prefixes, suffixes and the
# remaining text is either numbers or letters but not both (since enumerations
# occur with letters or numbers but usually not both).

# See unit tests for matchIndicesBasedOnName in test-stacking.R
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
        result <- rbind(result, new.row, deparse.level = 0)

        if (all(vapply(ind.list, length, integer(1)) == 0))
            break
    }
    result
}

# Parse user-input manual stacking strings and append the variables to manually
# stack to stacking.groups. Most of the work is done by either
# stackingSpecifiedByVariable or stackingSpecifiedByObservation.
# See unit tests for stackManually in test-stacking.R
stackManually <- function(manual.stacking, specify.by, input.data.set.metadata)
{
    if (is.null(manual.stacking) || length(manual.stacking) == 0 ||
        setequal(manual.stacking, ""))
        return(NULL)

    manual.stacking.groups <- if (specify.by == "Variable")
        stackingSpecifiedByVariable(manual.stacking, input.data.set.metadata)
    else
        stackingSpecifiedByObservation(manual.stacking,
                                       input.data.set.metadata)
}

# See unit tests for permittedNA in test-stacking.R
permittedNA <- function(variable.names)
{
    na.ind <- match("NA", variable.names)
    has.na.variable <- !is.na(na.ind)
    if (has.na.variable)
    {
        warning("There is an input variable named 'NA'. ",
                "To avoid confusion, missing stacking variables need to be specified with an extra slash for this data set, i.e., N/A")
        "N/A"
    }
    else
        c("NA", "N/A")
}

# Parse user-input manual stacking strings for stacking specified by variable,
# i.e., each string in manual.stacking describes the variables to be stacked
# into one variable.
# See unit tests for stackingSpecifiedByVariable in test-stacking.R
stackingSpecifiedByVariable <- function(manual.stacking,
                                        input.data.set.metadata)
{
    v.names <- input.data.set.metadata$variable.names
    v.types <- input.data.set.metadata$variable.types
    v.val.attr <- input.data.set.metadata$variable.value.attributes

    permitted.na <- permittedNA(v.names)

    manual.stacking.groups.list<- list()
    manual.stacking.groups.text <- character(0)

    for (input.text in manual.stacking)
    {
        on.fail.msg <- paste0("The manual stacking input '", input.text,
                              "' has been ignored.")
        split.text <- splitByComma(input.text)

        group.names <- character(0)

        for (t in split.text)
        {
            parsed <- if (t %in% permitted.na)
                NA_character_
            else if (grepl("-", t, fixed = TRUE)) # contains range
                parseVariableRange(t, v.names, "manual stacking", on.fail.msg)
            else if (grepl("*", t, fixed = TRUE)) # contains wildcard
                parseVariableWildcard(t, v.names, "manual stacking", on.fail.msg)
            else
                parseVariableName(t, v.names, "manual stacking",
                                  on.fail.msg)
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
                    "' has been ignored as it contains variables with mismatching types or value attributes.")
            next
        }

        # Check for overlap with previous manual stacking inputs
        if (length(manual.stacking.groups.list) > 0)
        {
            group.ind.without.na <- removeNA(group.ind)
            overlap.ind <- which(vapply(manual.stacking.groups.list, function(manual.group) {
                any(group.ind.without.na %in% manual.group)
            }, logical(1)))
            if (length(overlap.ind) > 0)
            {
                warning("The manual stacking input '", input.text,
                        "' has been ignored as it contains variable(s) that overlap with another manual stacking input '",
                        manual.stacking.groups.text[overlap.ind[1]], "'.")
                next
            }
        }

        manual.stacking.groups.list <- c(manual.stacking.groups.list, list(group.ind))
        manual.stacking.groups.text <- c(manual.stacking.groups.text,
                                         input.text)
    }

    if (length(manual.stacking.groups.list) == 0)
        return(NULL)

    n.stacking <- max(vapply(manual.stacking.groups.list, length, integer(1)))
    do.call("rbind", lapply(manual.stacking.groups.list, function(group) {
        rw <- rep(NA_integer_, n.stacking)
        rw[seq_along(group)] <- group
        rw
    }))
}

# Parse user-input manual stacking strings for stacking specified by
# observation, i.e., each string in manual.stacking describes the variables in
# an observation to be used in stackings of variables.
# See unit tests for stackingSpecifiedByObservation in test-stacking.R
stackingSpecifiedByObservation <- function(manual.stacking,
                                           input.data.set.metadata)
{
    if (length(manual.stacking) < 2)
    {
        warning("No manual stacking was conducted as 2 or more manual stacking inputs (corresponding to obvservations) are required.")
        return(NULL)
    }

    v.names <- input.data.set.metadata$variable.names
    v.types <- input.data.set.metadata$variable.types
    v.val.attr <- input.data.set.metadata$variable.value.attributes

    permitted.na <- permittedNA(v.names)

    # obs.group = observation group, which is a group of variables be stacked
    # belonging to an observation, instead of a variable (in which case I just
    # use 'group').
    manual.stacking.obs.groups.list <- list()
    manual.stacking.obs.groups.text <- list()

    for (input.text in manual.stacking)
    {
        on.fail.msg <- paste0("No manual stacking was conducted.")
        split.text <- splitByComma(input.text)
        obs.group.names <- character(0)

        for (t in split.text)
        {
            parsed <- if (t %in% permitted.na)
                NA_character_
            else if (grepl("-", t, fixed = TRUE)) # contains range
                parseVariableRange(t, v.names, "manual stacking", on.fail.msg)
            else if (grepl("*", t, fixed = TRUE)) # contains wildcard
                parseVariableWildcard(t, v.names, "manual stacking", on.fail.msg)
            else
                parseVariableName(t, v.names, "manual stacking",
                                  on.fail.msg)
            if (length(parsed) == 0)
                return(NULL)
            else
                obs.group.names <- c(obs.group.names, parsed)
        }

        if (all(is.na(obs.group.names)))
        {
            warning("No manual stacking was conducted as the manual stacking input '",
                    input.text, "' does not contain any variables.")
            return(NULL)
        }

        # Check for duplicate variables
        dup <- duplicated(removeNA(obs.group.names))
        if (any(dup))
        {
            warning("No manual stacking was conducted as the manual stacking input '",
                    input.text, "' contains duplicate entries for ",
                    paste0("'", removeNA(obs.group.names)[dup], "'",
                           collapse = ", "), ".")
            return(NULL)
        }

        # Remove trailing NA
        ind <- which(!is.na(obs.group.names))
        obs.group.names <- obs.group.names[seq_len(ind[length(ind)])]

        obs.group.ind <- vapply(obs.group.names, match, integer(1),
                                v.names)

        # Check for overlap with previous manual stacking inputs
        if (length(manual.stacking.obs.groups.list) > 0)
        {
            obs.group.ind.without.na <- removeNA(obs.group.ind)
            overlap.ind <- which(vapply(manual.stacking.obs.groups.list, function(manual.obs.group) {
                any(obs.group.ind.without.na %in% manual.obs.group)
            }, logical(1)))
            if (length(overlap.ind) > 0)
            {
                warning("No manual stacking was conducted as the manual stacking input '",
                        input.text, "' contains variable(s) that overlap with another manual stacking input '",
                        manual.stacking.obs.groups.text[overlap.ind[1]], "'.")
                return(NULL)
            }
        }

        manual.stacking.obs.groups.list <- c(manual.stacking.obs.groups.list, list(obs.group.ind))
        manual.stacking.obs.groups.text <- c(manual.stacking.obs.groups.text,
                                             input.text)
    }

    if (length(manual.stacking.obs.groups.list) == 0)
        return(NULL)

    n.stacked.var <- max(vapply(manual.stacking.obs.groups.list, length, integer(1)))
    manual.stacking.groups <- do.call("cbind", lapply(manual.stacking.obs.groups.list, function(obs.group) {
        column <- rep(NA_integer_, n.stacked.var)
        column[seq_along(obs.group)] <- obs.group
        column
    }))

    # Check for mismatching variable types and value attributes
    for (i in seq_len(nrow(manual.stacking.groups)))
    {
        group.ind <- removeNA(manual.stacking.groups[i, ])
        if (!allIdentical(v.types[group.ind]) ||
            !allIdentical(v.val.attr[group.ind]))
        {
            warning("No manual stacking was conducted as the following variables to be stacked ",
                    "have mismatching types or value attributes: ",
                    paste0(v.names[group.ind], collapse = ", "), ".")
            return(NULL)
        }
    }
    manual.stacking.groups
}

# See unit tests for mergeCommonLabelAndManualStackingGroups in test-stacking.R
mergeCommonLabelAndManualStackingGroups <- function(common.label.stacking.groups,
                                                    manual.stacking.groups)
{
    if (is.null(common.label.stacking.groups))
    {
        if (is.null(manual.stacking.groups))
            return(NULL)

        stacking.groups <- manual.stacking.groups
        attr(stacking.groups, "is.manually.stacked") <- rep(TRUE, nrow(stacking.groups))
        return(stacking.groups)
    }

    if (is.null(manual.stacking.groups))
    {
        stacking.groups <- common.label.stacking.groups
        attr(stacking.groups, "is.manually.stacked") <- rep(FALSE, nrow(stacking.groups))
        return(stacking.groups)
    }

    # Extract attribute now as it will be lost if we modify
    # common.label.stacking.groups
    unstackable.names <- attr(common.label.stacking.groups,
                              "unstackable.names")

    # Remove rows in common.label.stacking.groups that contain overlap with
    # manual.stacking.groups
    for (manual.group in manual.stacking.groups)
    {
        manual.group.without.na <- removeNA(manual.group)
        is.overlapping <- apply(common.label.stacking.groups, 1, function(group) {
            any(manual.group.without.na %in% group)
        })
        common.label.stacking.groups <- common.label.stacking.groups[!is.overlapping, , drop = FALSE]
    }

    n.row.c <- nrow(common.label.stacking.groups)
    n.col.c <- ncol(common.label.stacking.groups)
    n.row.m <- nrow(manual.stacking.groups)
    n.col.m <- ncol(manual.stacking.groups)

    # We still merge the two groups even if the number of variables to stack
    # does not match. If this is not what the user intended, the output should
    # make this clear enough, so a warning isn't shown.
    n.stacking <- max(n.col.c, n.col.m)
    n.stacked.variables <- n.row.c + n.row.m
    stacking.groups <- matrix(NA_integer_,  nrow = n.stacked.variables,
                              ncol = n.stacking)
    stacking.groups[seq_len(n.row.c), seq_len(n.col.c)] <- common.label.stacking.groups
    stacking.groups[n.row.c + seq_len(n.row.m), seq_len(n.col.m)] <- manual.stacking.groups

    is.manually.stacked <- c(rep(FALSE, n.row.c), rep(TRUE, n.row.m))
    attr(stacking.groups, "is.manually.stacked") <- is.manually.stacked
    attr(stacking.groups, "unstackable.names") <- unstackable.names
    stacking.groups
}

parseVariablesToInclude <- function(variables.to.include,
                                    input.data.set.metadata,
                                    stacking.groups)
{
    if (length(variables.to.include) == 0 || setequal(variables.to.include, ""))
        return(character(0))

    v.names <- input.data.set.metadata$variable.names
    purpose <- "included variable"

    result <- character(0)
    for (i in seq_along(variables.to.include))
    {
        split.text <- trimws(strsplit(variables.to.include[i], ",")[[1]])
        split.text <- split.text[split.text != ""]

        for (t in split.text)
        {
            parsed <- if (grepl("-", t, fixed = TRUE)) # contains range
                parseVariableRange(t, v.names, purpose,
                                   "The input range has been ignored.")
            else if (grepl("*", t, fixed = TRUE)) # contains wildcard
                parseVariableWildcard(t, v.names, purpose,
                                      "This input has been ignored.")
            else
                parseVariableName(t, v.names, purpose,
                                  "This input has been ignored.")

            result <- c(result, parsed)
        }
    }

    stacked.variable.names <- v.names[removeNA(c(stacking.groups))]
    result[!(result %in% stacked.variable.names)]
}

# Constructs the stacked data set as a data frame from the input data set and
# the stacking.groups matrix
stackedDataSet <- function(input.data.set, input.data.set.metadata,
                           stacking.groups, include.original.case.variable,
                           include.observation.variable,
                           common.labels.list, included.variable.names)
{
    input.v.names <- input.data.set.metadata$variable.names
    input.v.labels <- input.data.set.metadata$variable.labels

    has.stacking <- !is.null(stacking.groups) && nrow(stacking.groups) > 0

    if (!has.stacking)
        return(data.frame())

    n.stacked <- ncol(stacking.groups)
    first.ind <- apply(stacking.groups, 1, function(rw) removeNA(rw)[1])
    is.manually.stacked <- attr(stacking.groups, "is.manually.stacked")

    stacked.data.set <- list()
    data.set.size <- 0
    for (i in seq_along(input.v.names))
    {
        ind <- match(i, first.ind)
        if (!is.na(ind))
        {
            group.ind <- stacking.groups[ind, ]
            v <- unlist(lapply(group.ind, function(j) {
                vals <- if (!is.na(j))
                    input.data.set[[j]]
                else
                    rep(NA, nrow(input.data.set))
                if (isIntegerValued(vals))
                    as.integer(vals)
                else
                    vals
            }))
            nm <- stackedVariableName(group.ind, input.v.names, names(stacked.data.set))
            v <- c(matrix(v, ncol = nrow(input.data.set), byrow = TRUE))
            attr(v, "is.stacked") <- TRUE
            attr(v, "is.manually.stacked") <- is.manually.stacked[ind]
            attr(v, "stacking.input.variable.names") <- input.v.names[group.ind]
            attr(v, "stacking.input.variable.labels") <- input.v.labels[group.ind]
            attr(v, "label") <- stackedVariableLabel(group.ind, input.v.labels, nm)
            val.attr <- attr(input.data.set[[removeNA(group.ind)[1]]],
                             "labels", exact = TRUE)
            if (!is.null(val.attr))
            {
                if (is.integer(v))
                {
                    nms <- names(val.attr)
                    val.attr <- as.integer(val.attr)
                    names(val.attr) <- nms
                }
                attr(v, "labels") <- val.attr
                class(v) <- c(class(v), "haven_labelled")
            }

            stacked.data.set[[nm]] <- v
            data.set.size <- data.set.size + object.size(v)
        }
        else if (input.v.names[i] %in% included.variable.names)
        {
            input.var <- input.data.set[[i]]
            val.attr <- attr(input.var, "labels", exact = TRUE)
            if (isIntegerValued(input.var))
                input.var <- as.integer(input.var)
            v <- rep(input.var, each = n.stacked)
            attr(v, "is.stacked") <- FALSE
            attr(v, "is.manually.stacked") <- NA
            attr(v, "label") <- input.v.labels[i]
            if (!is.null(val.attr))
            {
                if (is.integer(v))
                {
                    nms <- names(val.attr)
                    val.attr <- as.integer(val.attr)
                    names(val.attr) <- nms
                }
                attr(v, "labels") <- val.attr
                class(v) <- c(class(v), "haven_labelled")
            }
            nm <- uniqueName(input.v.names[i], names(stacked.data.set))
            stacked.data.set[[nm]] <- v
            data.set.size <- data.set.size + object.size(v)
        }

        if (data.set.size > DATA.SET.SIZE.LIMIT)
        {
            msg <- paste0("The stacked data set is too large to create. ",
                          "Consider reducing the number of variables in the stacked data set.")
            if (!is.null(common.labels.list))
                msg <- paste0(msg, " Also ensure that the common labels are ",
                              "appropriate: ",
                              paste0(unlist(common.labels.list), collapse = ", "), ".")
            stop(msg)
        }
    }

    if (include.original.case.variable && has.stacking)
    {
        original.case <- rep(seq_len(nrow(input.data.set)), each = n.stacked)
        attr(original.case, "label") <- "Original case number (pre-stacking)"
        attr(original.case, "is.stacked") <- FALSE
        attr(original.case, "is.manually.stacked") <- NA
        attr(original.case, "is.original.case") <- TRUE
        stacked.data.set[[uniqueName("original_case",
                                     names(stacked.data.set))]] <- original.case
    }

    if (include.observation.variable && has.stacking)
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
                                     names(stacked.data.set))]] <- observation
    }

    data.frame(stacked.data.set)
}

stackedVariableName <- function(group.ind, input.variable.names, taken.names)
{
    ind <- removeNA(group.ind)
    nm <- input.variable.names[ind]
    common.prefix <- trimws(getCommonPrefix(nm))
    common.suffix <- trimws(getCommonSuffix(nm))
    candidate <- trimws(paste0(common.prefix, common.suffix))
    if (candidate != "")
        candidate
    else
        "stacked_var"

    uniqueName(candidate, taken.names, "_")
}

stackedVariableLabel <- function(group.ind, input.variable.labels, stacked.variable.name)
{
    ind <- removeNA(group.ind)
    lbl <- input.variable.labels[ind]
    common.prefix <- trimws(getCommonPrefix(lbl, whole.words = TRUE))
    common.suffix <- trimws(getCommonSuffix(lbl, whole.words = TRUE))
    if (common.prefix == "" && common.suffix == "")
        lbl[1]
    else if (common.prefix == common.suffix)
        common.prefix
    else if (nchar(common.prefix) <= 1 && nchar(common.suffix) <= 1)
        lbl[1]
    else
        trimws(paste(common.prefix, common.suffix))
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

# Parses a user-input variable range
# See unit tests for parseVariableRange in test-stacking.R
parseVariableRange <- function(range.text, variable.names, purpose,
                               on.fail.msg)
{
    dash.ind <- match("-", strsplit(range.text, "")[[1]])
    start.var.text <- trimws(substr(range.text, 1, dash.ind - 1))
    end.var.text <- trimws(substr(range.text, dash.ind + 1, nchar(range.text)))
    if (grepl("*", start.var.text, fixed = TRUE))
    {
        warning("The start variable from the ", purpose, " input range '",
                range.text,
                "' contains the wildcard character '*' which is not permitted in a range. ",
                on.fail.msg)
        return(character(0))
    }
    if (grepl("*", end.var.text, fixed = TRUE))
    {
        warning("The end variable from the ", purpose, " input range '",
                range.text,
                "' contains the wildcard character '*' which is not permitted in a range. ",
                on.fail.msg)
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
        warning("The start variable from the ", purpose, " input range '",
                range.text, "' ", "could not be identified. ", on.fail.msg,
                " Ensure that the variable name is correctly specified.")
        result <- character(0)
        attr(result, "is.not.found") <- TRUE
        return(result)
    }
    if (is.na(end.ind))
    {
        warning("The end variable from the ", purpose, " input range '",
                range.text, "' ", "could not be identified. ", on.fail.msg,
                " Ensure that the variable name is correctly specified.")
        result <- character(0)
        attr(result, "is.not.found") <- TRUE
        return(result)
    }
    if (start.ind > end.ind)
    {
        warning("The start variable from the ", purpose, "input range '",
                range.text,
                "' appears after the end variable in the data set. ",
                "Ensure that the range has been correctly specified. ",
                on.fail.msg)
        return(character(0))
    }
    variable.names[start.ind:end.ind]
}

# Parses a user-input variable wildcard
# See unit tests for parseVariableWildcard in test-stacking.R
#' @importFrom flipU EscapeRegexSymbols
parseVariableWildcard <- function(wildcard.text, variable.names, purpose,
                                  on.fail.msg)
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
        warning("No matches were found for the ", purpose,
                " input wildcard name '", wildcard.text,
                "'. Ensure that the wildcard variable name has been correctly specified. ",
                on.fail.msg)
        result <- character(0)
        attr(result, "is.not.found") <- TRUE
        return(result)
    }
    variable.names[is.match]
}

# Parses a user-input variable name
# See unit tests for parseVariableName in test-stacking.R
parseVariableName <- function(variable.name.text, variable.names, purpose,
                              on.fail.msg)
{
    if (variable.name.text %in% variable.names)
        variable.name.text
    else
    {
        warning("The ", purpose, " input variable name '", variable.name.text,
                "' could not be identified. ", on.fail.msg)
        result <- character(0)
        attr(result, "is.not.found") <- TRUE
        return(result)
    }
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

#' @importFrom flipFormat StackingWidget
#' @export
print.StackedData <- function(x, ...)
{
    StackingWidget(x$input.data.set.metadata,
                   x$stacked.data.set.metadata,
                   x$unstackable.names,
                   x$common.labels,
                   x$is.saved.to.cloud)
}
