#' @title Stack data set
#' @description Stacks variables in a data set.
#' @param input.data.set.name Name of data file to stack, either as a path to a
#'   local file or file in the Displayr Cloud Drive
#' @param stacked.data.set.name Name of the stacked data file to be saved in
#'   the Displayr Cloud Drive (if run from Displayr) or saved locally.
#' @param common.labels A character vector of common labels to be used to
#'   identify variables to stack. To be identified, a set of variables to be
#'   stacked must contain these labels, have the same prefix and suffix before
#'   and after these labels and be contiguous.
#' @param specify.by "Variable" or "Observation".
#' @param manual.stacking To be filled
#' @param variables.to.omit String of comma-separated variable names to omit.
#'   Variable ranges can be specified by supplying the start and end variables
#'   separated by a dash '-'. If the start or end variables are left out, then
#'   the range is assumed to start from the first variable or end at the last
#'   variable respectively. Wildcards in variable names can be specified
#'   with an asterisk '*'.
#' @param write.data.set Whether to write the stacked data set.
#' @export
StackData <- function(input.data.set.name,
                      stacked.data.set.name = NULL,
                      automatic.common.labels = TRUE,
                      common.labels = NULL,
                      specify.by = NULL,
                      manual.stacking = NULL,
                      variables.to.omit = NULL,
                      write.data.set = TRUE)
{
    input.data.set <- readDataSets(input.data.set.name, 1)[[1]]
    input.data.set.metadata <- metadataFromDataSet(input.data.set,
                                                   input.data.set.name)
    n.vars <- input.data.set.metadata$n.variables

    if (automatic.common.labels)
    {
        if (!is.null(common.labels) && length(common.labels) > 0)
            warning("Input common labels have been ignored as automatic ",
                    "common labels are on.")
        common.labels <- automaticCommonLabels(input.data.set.metadata)
        if (!write.data.set)
        {
            return(common.labels)
        }
    }

    stacking.groups <- stackWithCommonLabels(common.labels,
                                             input.data.set.metadata)
    stacking.groups <- stackManually(stacking.groups, manual.stacking,
                                     specify.by, input.data.set.metadata)

    stacked.data.set <- stackedDataSet(input.data.set, input.data.set.metadata,
                                       stacking.groups, variables.to.omit)

    variables.to.omit <- parseVariablesToOmit(variables.to.omit,
                                              names(stacked.data.set))
    omitted.stacked.variables <-  omittedStackedVariables(variables.to.omit,
                                                          stacked.data.set)
    stacked.data.set <- omitVariablesFromDataSet(stacked.data.set,
                                                 variables.to.omit)

    stacked.data.set.name <- cleanStackedDataSetName(stacked.data.set.name,
                                                     input.data.set.name)
    if (write.data.set)
        writeDataSet(stacked.data.set, stacked.data.set.name)

    stacked.data.set.metadata <- metadataFromStackedDataSet(stacked.data.set,
                                                            stacked.data.set.name)


    result <- list()
    result$stacked.data.set.metadata <- stacked.data.set.metadata
    result$unstackable.names <- attr(stacking.groups, "unstackable.names")
    result$omitted.variables <- variables.to.omit
    result$omitted.stacked.variables <- omitted.stacked.variables
    class(result) <- "StackedData"
    result
}

# TODO: check for places where drop = FALSE is needed!

automaticCommonLabels <- function(input.data.set.metadata)
{
    lbls <- input.data.set.metadata$variable.labels

    split.lbls <- lapply(lbls, strsplit, "[^[:alnum:]]")

    prefixes <- character(0)

    for (i in 2:length(lbls))
    {
        lbl.1 <- lbls[i - 1]
        lbl.2 <- lbls[i]
        prefix <- getCommonPrefix(lbls[(i - 1):i], whole.words = TRUE)

        if (prefix != "" && !(prefix %in% prefixes))
            prefixes <- rbind(prefixes, prefix)
    }

    score <- vapply(seq_along(prefixes), function(i)
    {
        prefix <- prefixes[i]
        nchar.prefix <- nchar(prefix)

        common.labels <- character(0)

        for (lbl in lbls)
        {
            if (substr(lbl, 1, nchar.prefix) == prefix)
            {
                remaining.lbl <- substr(lbl, nchar.prefix + 1, nchar(lbl))
                common.labels <- c(common.labels, remaining.lbl)
            }
        }
        common.labels <- unique(common.labels)

        stacking.groups <- stackingGroupFromCommonLabels(common.labels, lbls)

        ratio <- (sum(!is.na(stacking.groups)) - ncol(stacking.groups)) / length(stacking.groups)
        if (ratio < 0.5)
            0
        else
            sum(!is.na(stacking.groups)) - ncol(stacking.groups)
    }, numeric(1))


    ind <- which.max(score)

    prefix <- prefixes[ind]
    nchar.prefix <- nchar(prefix)

    common.labels <- character(0)

    for (lbl in lbls)
    {
        if (substr(lbl, 1, nchar.prefix) == prefix)
        {
            remaining.lbl <- substr(lbl, nchar.prefix + 1, nchar(lbl))
            common.labels <- c(common.labels, remaining.lbl)
        }
    }

    common.labels <- unique(common.labels)

}

# Find contiguous groups of variables to stack given the common labels.
# A group of variables to stack would have labels that contain the common
# labels along with the same prefix and suffix.
stackWithCommonLabels <- function(common.labels, input.data.set.metadata)
{
    if (is.null(common.labels) || length(common.labels) == 0)
        return(NULL)

    v.names <- input.data.set.metadata$variable.names
    v.labels <- input.data.set.metadata$variable.labels
    v.types <- input.data.set.metadata$variable.types
    v.categories <- input.data.set.metadata$variable.categories
    nchar.labels <- nchar(common.labels)

    stacking.groups <- stackingGroupFromCommonLabels(common.labels, v.labels)

    # Remove groups with mismatching variable types and categories
    unstackable.ind <- which(apply(stacking.groups, 1, function(ind) {
        ind <- removeNA(ind)
        !allIdentical(v.types[ind]) || !allIdentical(v.categories[ind])
    }))
    unstackable.names <- lapply(unstackable.ind, function(ind) {
        v.names[removeNA(stacking.groups[ind, ])]
    })
    if (length(unstackable.names) > 0)
        warning("Variables could not be stacked due ",
                "to mismatching variable types or categories. ",
                "See Notes section in output for more details.")

    if (length(unstackable.ind) > 0)
        stacking.groups <- stacking.groups[-unstackable.ind, ]
    attr(stacking.groups, "unstackable.names") <- unstackable.names
    attr(stacking.groups, "is.manually.stacked") <- rep(FALSE,
                                                       nrow(stacking.groups))
    stacking.groups
}

stackingGroupFromCommonLabels <- function(common.labels, variable.labels)
{
    nchar.labels <- nchar(common.labels)

    stacking.groups <- matrix(nrow = 0, ncol = length(common.labels))

    current.group <- rep(NA_integer_, length(common.labels))
    current.prefix <- NA_character_
    current.suffix <- NA_character_
    for (i in seq_along(variable.labels))
    {
        lbl <- variable.labels[i]
        ind <- which(vapply(common.labels, grepl, logical(1), lbl, fixed = TRUE))
        ind <- ind[which.max(nchar.labels[ind])]

        if (length(ind) == 0)
        {
            if (any(!is.na(current.group)))
            {
                stacking.groups <- rbind(stacking.groups, current.group)
                current.group <- rep(NA_integer_, length(common.labels))
                current.prefix <- NA_character_
                current.suffix <- NA_character_
            }
            next
        }

        if (!is.na(current.group[ind]))
        {
            stacking.groups <- rbind(stacking.groups, current.group)
            current.group <- rep(NA_integer_, length(common.labels))
            current.prefix <- NA_character_
            current.suffix <- NA_character_
        }

        matched.common.label <- common.labels[ind]
        start.ind <- gregexpr(matched.common.label, lbl, fixed = TRUE)[[1]]
        prefix <- substr(lbl, 1, start.ind - 1)
        suffix <- substr(lbl, start.ind + nchar(matched.common.label), nchar(lbl))

        if (all(is.na(current.group)))
        {
            current.group[ind] <- i
            current.prefix <- prefix
            current.suffix <- suffix
        }

        if (prefix == current.prefix && suffix == current.suffix)
            current.group[ind] <- i
    }

    # Remove groups with only one element
    stacking.groups <- stacking.groups[rowSums(!is.na(stacking.groups)) > 1, ,
                                       drop = FALSE]

    stacking.groups
}

stackManually <- function(stacking.groups, manual.stacking,
                          specify.by, input.data.set.metadata)
{
    if (is.null(manual.stacking) || length(manual.stacking) == 0 ||
        manual.stacking == "")
        return(stacking.groups)

    variable.names <- input.data.set.metadata$variable.names

    na.ind <- match("NA", variable.names)
    has.na.variable <- !is.na(na.ind)
    if (has.na.variable)
        warning("There is an input variable named 'NA'. To avoid confusion, ",
                "missing stacking variables need to be specified with an ",
                "extra slash for this data set, i.e., N/A")

    stacking.groups <- if (specify.by == "Variable")
        stackingSpecifiedByVariable(stacking.groups, manual.stacking,
                                    variable.names, has.na.variable)
    else
        stackingSpecifiedByObservation(stacking.groups, manual.stacking,
                                       variable.names, has.na.variable)
}

stackingSpecifiedByVariable <- function(stacking.groups, manual.stacking,
                                        variable.names, has.na.variable)
{
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
            # deal with NA
            parsed <- if (t %in% permitted.na)
                NA_character_
            else if (grepl("-", t, fixed = TRUE)) # contains range
                parseRange(t, variable.names, "manual stacking", on.fail)
            else if (grepl("*", t, fixed = TRUE)) # contains wildcard
                parseWildcard(t, variable.names, "manual stacking", on.fail)
            else
                parseVariableName(t, variable.names, "manual stacking",
                                  on.fail)
            if (length(parsed) == 0)
            {
                group.names <- NULL
                break
            }
            else
                group.names <- c(group.names, parsed)
        }
        if (is.null(group.names))
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

        group.ind <- vapply(group.names, match, integer(1), variable.names)

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
        stacking.groups <- stacking.groups[!is.overlapping, ]
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

stackingSpecifiedByObservation <- function(stacking.groups, manual.stacking,
                                           variable.names, has.na.variable)
{
    if (length(manual.stacking) < 2)
    {
        warning("No manual stacking was conducted as 2 or more manual",
                "stacking inputs (corresponding to obvservations) are ",
                "required.")
        return(stacking.groups)
    }

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
            # deal with NA
            parsed <- if (t %in% permitted.na)
                NA_character_
            else if (grepl("-", t, fixed = TRUE)) # contains range
                parseRange(t, variable.names, "manual stacking", on.fail)
            else if (grepl("*", t, fixed = TRUE)) # contains wildcard
                parseWildcard(t, variable.names, "manual stacking", on.fail)
            else
                parseVariableName(t, variable.names, "manual stacking",
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
                                variable.names)

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

    if (is.null(stacking.groups))
        stacking.groups <- matrix(nrow = 0, ncol = 0)

    # Remove rows in stacking.groups that contain overlap with
    # manual.stacking.groups
    for (manual.obs.group in manual.stacking.obs.groups)
    {
        manual.obs.group.without.na <- removeNA(manual.obs.group)
        is.overlapping <- apply(stacking.groups, 1, function(obs.group) {
            any(manual.obs.group.without.na %in% obs.group)
        })
        stacking.groups <- stacking.groups[!is.overlapping, ]
    }

    # Increase number of columns in stacking.groups if necessary to fit
    # manual.stacking.groups
    n.rows <- nrow(stacking.groups)
    n.cols <- ncol(stacking.groups)
    max.stacking <- max(length(manual.stacking.obs.groups), n.cols)
    new.stacking.groups <- matrix(nrow = n.rows, ncol = max.stacking)
    new.stacking.groups[seq_len(n.rows), seq_len(n.cols)] <- stacking.groups
    stacking.groups <- new.stacking.groups

    n.manual.stacking <- max(vapply(manual.stacking.obs.groups, length,
                                    integer(1)))

    # Append manual.stacking.groups to stacking.groups
    for (i in seq_len(n.manual.stacking))
    {
        new.group <- rep(NA_integer_, max.stacking)
        ind <- seq_along(manual.stacking.obs.groups)
        new.group[ind] <- vapply(manual.stacking.obs.groups, `[`,
                                 integer(1), i)
        stacking.groups <- rbind(stacking.groups, new.group)
    }

    is.manually.stacked <- c(rep(FALSE, nrow(stacking.groups) - n.manual.stacking),
                             rep(TRUE, n.manual.stacking))
    attr(stacking.groups, "is.manually.stacked") <- is.manually.stacked

    stacking.groups
}

stackedDataSet <- function(input.data.set, input.data.set.metadata,
                           stacking.groups, variables.to.omit)
{
    if (is.null(stacking.groups))
    {
        return(data.frame(lapply(input.data.set, function(v) {
            attr(v, "is.stacked") <- FALSE
            attr(v, "is.manually.stacked") <- NA
            v
        })))
    }

    input.var.names <- input.data.set.metadata$variable.names
    input.var.labels <- input.data.set.metadata$variable.labels
    retained.indices <- retainedIndices(stacking.groups,
                                        input.data.set.metadata$n.variables)
    stacked.indices <- stackedIndices(stacking.groups, retained.indices)

    # TODO: refactor this, don't like the name "stackedVariableText"
    stacked.data.set.var.names <- stackedVariableText(stacking.groups,
                                                      input.var.names,
                                                      retained.indices,
                                                      stacked.indices, TRUE)
    stacked.data.set.var.labels <- stackedVariableText(stacking.groups,
                                                       input.var.labels,
                                                       retained.indices,
                                                       stacked.indices)

    n.stacked <- ncol(stacking.groups)
    is.manually.stacked <- attr(stacking.groups, "is.manually.stacked")
    stacked.data.set <- data.frame(lapply(seq_along(stacked.data.set.var.names), function(i) {
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
            attr(v, "is.stacked") <- TRUE
            attr(v, "is.manually.stacked") <- is.manually.stacked[ind]
            attr(v, "stacking.input.variable.names") <- input.var.names[group.ind]
            attr(v, "stacking.input.variable.labels") <- input.var.labels[group.ind]
            categories <- attr(input.data.set[[removeNA(group.ind)[1]]],
                               "labels", exact = TRUE)
        }
        else # Not stacked variable
        {
            input.var <- input.data.set[[stacked.data.set.var.names[i]]]
            v <- rep(input.var, n.stacked)
            attr(v, "is.stacked") <- FALSE
            attr(v, "is.manually.stacked") <- NA
            categories <- attr(input.var, "labels", exact = TRUE)
        }
        attr(v, "label") <- stacked.data.set.var.labels[i]
        if (!is.null(categories))
        {
            attr(v, "labels") <- categories
            class(v) <- c(class(v), "haven_labelled")
        }
        v
    }))
    names(stacked.data.set) <- stacked.data.set.var.names
    stacked.data.set
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
    # TODO: rename stacking.ind to avoid confusion with stacked.indices
    stacking.ind <- apply(stacking.groups, 1, function(group.ind) {
        ind <- removeNA(group.ind)
        ind[which.min(ind)]
    })

    vapply(stacking.ind, match, integer(1), retained.indices)
}

stackedVariableText <- function(stacking.groups, variable.text,
                                retained.indices, stacked.indices,
                                prevent.duplicates = FALSE)
{
    stacked.text <- apply(stacking.groups, 1, function(group.ind) {
        ind <- removeNA(group.ind)
        text <- variable.text[ind]
        common.prefix <- trimws(getCommonPrefix(text))
        if (common.prefix != "")
            common.prefix
        else
            "stacked_var"
    })

    result <- variable.text[retained.indices]
    result[stacked.indices] <- stacked.text

    if (!prevent.duplicates)
        return(result)

    dup <- which(duplicated(result))
    for (i in dup)
    {
        j <- 2
        repeat
        {
            candidate.name <- paste0(result[i], "_", j)
            if (!(candidate.name %in% result))
            {
                result[i] <- candidate.name
                break
            }
            j <- j + 1
        }
    }

    result
}

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
        substr(common_prefix, 1, ind[length(ind)] - 1)
    }
    else
        common_prefix
}

parseVariablesToOmit <- function(variables.to.omit, variable.names)
{
    if (is.null(variables.to.omit) || length(variables.to.omit) == 0 ||
        variables.to.omit == "")
        return(character(0))

    purpose <- "omitted variable"

    result <- character(0)
    for (input.text in variables.to.omit)
    {
        split.text <- splitByComma(input.text)
        for (t in split.text)
        {
            result <- if (grepl("-", t, fixed = TRUE)) # contains range
                c(result, parseRange(t, variable.names, purpose,
                                     "The input range has been ignored."))
            else if (grepl("*", t, fixed = TRUE)) # contains wildcard
                c(result, parseWildcard(t, variable.names, purpose,
                                        "This input has been ignored."))
            else
                c(result, parseVariableName(t, variable.names, purpose,
                                            "This input has been ignored."))
        }
    }

    # Order omitted variables according to variable.names
    result[order(vapply(result, match, integer(1), variable.names))]
}

parseRange <- function(range.text, variable.names, purpose, on.fail)
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
        warning("The start variable from the ", purpose, " input range '",
                range.text, "' ", "could not be identified. ",on.fail,
                " Ensure that the variable name is correctly specified.")
        return(character(0))
    }
    if (is.na(end.ind))
    {
        warning("The end variable from the ", purpose, " input range '",
                range.text, "' ", "could not be identified. ",on.fail,
                " Ensure that the variable name is correctly specified.")
        return(character(0))
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

#' @importFrom flipU EscapeRegexSymbols
parseWildcard <- function(wildcard.text, variable.names, purpose, on.fail)
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
                " input wildcard name '",
                wildcard.text, "'. Ensure that the wildcard variable name ",
                "has been correctly specified. ", on.fail)
        return(character(0))
    }
    variable.names[is.match]
}

parseVariableName <- function(variable.name.text, variable.names, purpose,
                              on.fail)
{
    if (variable.name.text %in% variable.names)
        variable.name.text
    else
    {
        warning("The ", purpose, " input varible name '", variable.name.text,
                "' could not be identified. ", on.fail)
        return(character(0))
    }
}

omittedStackedVariables <- function(variables.to.omit, stacked.data.set)
{
    is.stacked <- vapply(stacked.data.set, attr, logical(1), "is.stacked")
    names(which(is.stacked[variables.to.omit]))
}

omitVariablesFromDataSet <- function(data.set, variables.to.omit)
{
    data.set[!(names(data.set) %in% variables.to.omit)]
}

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

metadataFromStackedDataSet <- function(stacked.data.set, stacked.data.set.name)
{
    result <- metadataFromDataSet(stacked.data.set, stacked.data.set.name)
    result$is.stacked.variable <- vapply(stacked.data.set, attr, logical(1),
                                         "is.stacked")
    result$is.manually.stacked.variable <- vapply(stacked.data.set, attr, logical(1),
                                                  "is.manually.stacked")
    result$stacking.input.variable.names <- lapply(stacked.data.set, attr,
                                                   "stacking.input.variable.names")
    result$stacking.input.variable.labels <- lapply(stacked.data.set, attr,
                                                    "stacking.input.variable.labels")
    result
}

allIdentical <- function(x)
{
    length(unique(x)) < 2
}

removeNA <- function(x)
{
    x[!is.na(x)]
}

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
                   x$omitted.stacked.variables)
}
