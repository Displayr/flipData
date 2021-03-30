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
#' @param common.labels.variables A string of variables used to generate the
#'   common labels used for stacking. This can be a combination of
#'   comma-separated names, wildcards and ranges.
#' @param common.labels A character vector of common labels to be used to
#'   identify variables to stack. Only used when \code{stack.with.common.labels}
#'   is \code{"Using manually input common labels"}. To be identified, a set of
#'   variables to be stacked must contain these labels, have the same prefix
#'   and suffix before and after these labels.
#' @param specify.by "Variable" or "Observation".
#' @param manual.stacking To be filled
#' @param variables.to.omit String of comma-separated variable names to omit.
#'   Variable ranges can be specified by supplying the start and end variables
#'   separated by a dash '-'. If the start or end variables are left out, then
#'   the range is assumed to start from the first variable or end at the last
#'   variable respectively. Wildcards in variable names can be specified
#'   with an asterisk '*'.
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
#'     to mismatching types or categories.
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
                      common.labels.variables = NULL,
                      common.labels = NULL,
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
    parsed.variables.to.omit <- parseVariablesToOmit(variables.to.omit,
                                                     names(input.data.set),
                                                     FALSE)
    input.data.set <- omitVariablesFromDataSet(input.data.set,
                                               parsed.variables.to.omit)

    input.data.set.metadata <- metadataFromDataSet(input.data.set,
                                                   input.data.set.name)
    n.vars <- input.data.set.metadata$n.variables

    if (stack.with.common.labels == "Automatically")
    {
        if (!is.null(common.labels) && length(common.labels) > 0)
            warning("Input common labels have been ignored as common labels ",
                    "are to be generated automatically.")
        common.labels <- automaticCommonLabels(input.data.set.metadata)
    }
    else if (stack.with.common.labels == "Using a set of variables to stack as reference")
    {
        if (!is.null(common.labels) && length(common.labels) > 0)
            warning("Input common labels have been ignored as common labels ",
                    "are to be obtained from a set of variables.")
        common.labels <- commonLabelsFromVariables(common.labels.variables,
                                                   input.data.set.metadata)
    }
    else if (stack.with.common.labels == "Using manually input common labels")
    {
        if (is.null(common.labels) || length(common.labels) == 0)
            warning("No common labels were manually supplied. No stacking ",
                    "was done using common labels.")
    }

    stacking.groups <- if (stack.with.common.labels != "Disabled")
        stackWithCommonLabels(common.labels, input.data.set.metadata)
    else
        NULL

    stacking.groups <- stackManually(stacking.groups, manual.stacking,
                                     specify.by, input.data.set.metadata)

    stacked.data.set <- stackedDataSet(input.data.set, input.data.set.metadata,
                                       stacking.groups,
                                       include.original.case.variable,
                                       include.observation.variable)

    # Need to check for variables to omit a second time in case stacked
    # variables need to be omitted
    parse.status <- attr(parsed.variables.to.omit, "parse.status")
    variables.to.omit <- variables.to.omit[parse.status != "bad format"]
    omittable.names <- omittableNames(names(stacked.data.set),
                                      include.original.case.variable,
                                      include.observation.variable)
    warning.if.not.found <- parse.status[parse.status != "bad format"] ==
                                "variable(s) not found"

    parsed.variables.to.omit.2 <- parseVariablesToOmit(variables.to.omit,
                                                       omittable.names,
                                                       warning.if.not.found)
    omitted.stacked.variables <-  omittedStackedVariables(parsed.variables.to.omit.2,
                                                          stacked.data.set)
    stacked.data.set <- omitVariablesFromDataSet(stacked.data.set,
                                                 parsed.variables.to.omit.2)

    stacked.data.set.name <- cleanStackedDataSetName(stacked.data.set.name,
                                                     input.data.set.name)
    if (write.data.set)
        writeDataSet(stacked.data.set, stacked.data.set.name)

    stacked.data.set.metadata <- metadataFromStackedDataSet(stacked.data.set,
                                                            stacked.data.set.name)


    result <- list()
    result$stacked.data.set.metadata <- stacked.data.set.metadata
    result$unstackable.names <- attr(stacking.groups, "unstackable.names")
    result$omitted.variables <- c(parsed.variables.to.omit, parsed.variables.to.omit.2)
    result$omitted.stacked.variables <- omitted.stacked.variables
    result$common.labels <- common.labels
    result$is.saved.to.cloud <- write.data.set && canAccessDisplayrCloudDrive()

    if (include.stacked.data.set.in.output)
        result$stacked.data.set <- stacked.data.set

    class(result) <- "StackedData"
    result
}

# TODO: check for places where drop = FALSE is needed!

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
    # Remove extra attributes that are not needed for writing
    data.set <- data.frame(lapply(data.set, function(v) {
        attr(v, "input.category.values") <- NULL
        v
    }))

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

automaticCommonLabels <- function(input.data.set.metadata)
{
    lbls <- input.data.set.metadata$variable.labels

    first.words <- vapply(lapply(lbls, strsplit, " "),
                          function(splt) splt[[1]][1], character(1))

    prefixes <- character(0)

    for (i in 1:(length(lbls) - 1))
    {
        ind <- (i + 1):length(lbls)
        match.ind <- ind[which(first.words[i] == first.words[ind])]
        for (j in match.ind)
        {
            prefix <- getCommonPrefix(lbls[c(i, j)], whole.words = TRUE)
            if (prefix != "" && !(prefix %in% prefixes))
                prefixes <- rbind(prefixes, prefix)
        }
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
        {
            sum(!is.na(stacking.groups)) - ncol(stacking.groups)
        }
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

    # mention auto common labels in the notes?

    common.labels <- unique(common.labels)
}

commonLabelsFromVariables <- function(common.labels.variables,
                                      input.data.set.metadata)
{
    if (is.null(common.labels.variables) ||
        length(common.labels.variables) == 0)
    {
        warning("No variables were supplied for common labels.")
        return(NULL)
    }

    v.names <- input.data.set.metadata$variable.names
    v.labels <- input.data.set.metadata$variable.labels
    split.text <- splitByComma(common.labels.variables)

    if (length(split.text) == 0)
    {
        warning("No variables were supplied for common labels.")
        return(NULL)
    }

    on.fail <- "Common labels could not be obtained from the input variables."

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
            return(NULL)

        parsed.var.names <- c(parsed.var.names, parsed)
    }

    lbls.containing.common.lbls <- vapply(parsed.var.names, function(nm) {
        v.labels[match(nm, v.names)]
    }, character(1))

    nchar.prefix <- nchar(getCommonPrefix(lbls.containing.common.lbls, whole.words = TRUE))
    nchar.suffix <- nchar(getCommonSuffix(lbls.containing.common.lbls, whole.words = TRUE))

    vapply(lbls.containing.common.lbls, function(lbl) {
        substr(lbl, nchar.prefix + 1, nchar(lbl) - nchar.suffix)
    }, character(1))
}

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
    n.common.labels <- length(common.labels)
    common.label.prefixes.suffixes <- list()
    match.ind <- list()

    for (i in seq_len(n.common.labels))
    {
        common.lbl <- common.labels[i]
        matches <- gregexpr(common.lbl, variable.labels, fixed = TRUE)
        ind <- which(vapply(matches, function(m) m[[1]][1], integer(1)) != -1)
        common.label.prefixes.suffixes[[i]] <- t(vapply(ind, function(j) {
            lbl <- variable.labels[j]
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
        new.rows <- matrix(NA_integer_,
                           nrow = max(vapply(common.labels.ind, length, integer(1))),
                           ncol = n.common.labels)
        for (j in seq_len(n.common.labels))
            new.rows[seq_along(common.labels.ind[[j]]), j] <- common.labels.ind[[j]]

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

stackManually <- function(stacking.groups, manual.stacking,
                          specify.by, input.data.set.metadata)
{
    if (is.null(manual.stacking) || length(manual.stacking) == 0 ||
        setequal(manual.stacking, ""))
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
                                    input.data.set.metadata, has.na.variable)
    else
        stackingSpecifiedByObservation(stacking.groups, manual.stacking,
                                       input.data.set.metadata, has.na.variable)
}

stackingSpecifiedByVariable <- function(stacking.groups, manual.stacking,
                                        input.data.set.metadata, has.na.variable)
{
    v.names <- input.data.set.metadata$variable.names
    v.types <- input.data.set.metadata$variable.types
    v.categories <- input.data.set.metadata$variable.categories

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

        # Check for mismatching variable types and categories
        if (!allIdentical(v.types[removeNA(group.ind)]) ||
            !allIdentical(v.categories[removeNA(group.ind)]))
        {
            warning("The manual stacking input '", input.text,
                    "' has been ignored as it contains variables with ",
                    "mismatching types or categories.")
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
    v.categories <- input.data.set.metadata$variable.categories

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

    if (is.null(stacking.groups))
        stacking.groups <- matrix(nrow = 0, ncol = 0)

    n.manual.stacking <- max(vapply(manual.stacking.obs.groups, length,
                                    integer(1)))

    for (i in seq_len(n.manual.stacking))
    {
        ind <- removeNA(vapply(manual.stacking.obs.groups, `[`,
                        integer(1), i))
        if (!allIdentical(v.types[ind]) || !allIdentical(v.categories[ind]))
        {
            warning("No manual stacking was conducted as the manual ",
                    "stacking input '", input.text, "' would result in the ",
                    "stacking of variables with mismatching types or ",
                    "categories.")
            return(stacking.groups)
        }
    }

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

    # Append manual.stacking.groups to stacking.groups
    ind <- seq_along(manual.stacking.obs.groups)
    for (i in seq_len(n.manual.stacking))
    {
        new.group <- rep(NA_integer_, max.stacking)
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
                           stacking.groups, include.original.case.variable,
                           include.observation.variable)
{
    if (is.null(stacking.groups) || nrow(stacking.groups) == 0)
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
            v <- c(t(matrix(v, nrow = nrow(input.data.set))))
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
            v <- rep(input.var, each = n.stacked)
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

    names(stacked.data.set) <- c(stacked.data.set.var.names)

    if (include.original.case.variable)
    {
        original.case <- rep(seq_len(nrow(input.data.set)), each = n.stacked)
        attr(original.case, "label") <- "Original case number (pre stacking)"
        attr(original.case, "is.stacked") <- FALSE
        attr(original.case, "is.manually.stacked") <- NA
        stacked.data.set[[uniqueName("original_case",
                                     stacked.data.set.var.names)]] <- original.case
    }

    if (include.observation.variable)
    {
        observation <- rep(seq_len(n.stacked), nrow(input.data.set))
        attr(observation, "label") <- "Observation # (from stacking)"
        attr(observation, "is.stacked") <- FALSE
        attr(observation, "is.manually.stacked") <- NA
        stacked.data.set[[uniqueName("observation",
                                     stacked.data.set.var.names)]] <- observation
    }

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
        common.suffix <- trimws(getCommonSuffix(text))
        candidate <- trimws(paste(common.prefix, common.suffix))
        if (candidate != "")
            candidate
        else
            "stacked_var"
    })

    result <- variable.text[retained.indices]
    result[stacked.indices] <- stacked.text

    if (!prevent.duplicates)
        return(result)

    dup <- which(duplicated(result))
    for (i in dup)
        result[i] <- uniqueName(result[i], result[-i], "_")

    result
}

uniqueName <- function(new.name, existing.names, delimiter = "")
{
    if (!(new.name %in% existing.names))
        return (new.name)

    i <- 1
    repeat
    {
        candidate.name <- paste0(new.name, delimiter, i)
        if (!(new.name %in% existing.names))
            return(candidate.name)
        i <- i + 1
    }
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
        if (setequal(ind, -1))
            ""
        else
            substr(common_prefix, 1, ind[length(ind)])
    }
    else
        common_prefix
}

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

splitVariablesToOmitText <- function(variables.to.omit)
{
    if (is.null(variables.to.omit) || length(variables.to.omit) == 0 ||
        setequal(variables.to.omit, ""))
        return(character(0))

    unlist(lapply(variables.to.omit, splitByComma))
}

omittableNames <- function(variable.names, include.original.case.variable,
                           include.observation.variable)
{
    if (include.original.case.variable)
        variable.names <- variable.names[-length(variable.names)]
    if (include.observation.variable)
        variable.names <- variable.names[-length(variable.names)]
    variable.names
}

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

metadataFromDataSet <- function(data.set, data.set.name)
{
    list(variable.names = names(data.set),
         variable.labels = vapply(data.set, function(v) {
             lbl <- attr(v, "label", exact = TRUE)
             ifelse(!is.null(lbl), lbl, "")
         }, character(1)),
         variable.types = vapply(data.set, variableType, character(1)),
         variable.categories = lapply(data.set, attr, "labels",
                                      exact = TRUE),
         n.variables = length(data.set),
         data.set.name = dataSetNameWithoutPath(data.set.name))
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
                   x$omitted.stacked.variables,
                   x$common.labels,
                   x$is.saved.to.cloud)
}
