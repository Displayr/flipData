#' @export
StackData <- function(data.set.name,
                      specify.by = NULL,
                      common.labels = NULL,
                      variables.to.stack = NULL,
                      variables.to.omit = NULL)
{
    data.set <- readDataSets(data.set.name, 1)[[1]]
    variable.names <- names(data.set)
    variable.labels <- vapply(data.set, function(v) {
        lbl <- attr(v, "label", exact = TRUE)
        if (!is.null(lbl))
            lbl
        else
            ""
    }, character(1))
    n.vars <- length(variable.names)

    # variables.to.omit <- parseVariablesToOmit(variables.to.omit)

    # auto common.labels

    if (!is.null(common.labels))
    {
        stacking.groups <- stackByCommonLabels(common.labels, variable.names,
                                               variable.labels)
    }

    # stacked.data.set <- if (specify.by == "Variable")
    #     stackingSpecifiedByVariable(variable.names, variables.to.stack)
    # else # specify.by == "Case"
    #     stackingSpecifiedByCase(variable.names, variables.to.stack)
    retained.indices <- retainedIndices(stacking.groups, n.vars)
    stacked.indices <- stackedIndices(stacking.groups, retained.indices)
    stacked.variable.names <- stackedVariableText(stacking.groups,
                                                  variable.names,
                                                  retained.indices,
                                                  stacked.indices, TRUE)
    stacked.variable.labels <- stackedVariableText(stacking.groups,
                                                   variable.labels,
                                                   retained.indices,
                                                   stacked.indices)
    stacking.array <- stackingArray(stacking.groups, variable.names,
                                    variable.labels)
    # is.stacked <- isStacked(stacking.groups, n.vars)

    # Omit variables

    # Write stacked data set

    result <- list()
    result$stacked.variable.names <- stacked.variable.names
    result$stacked.variable.labels <- stacked.variable.labels
    result$stacking.array <- stacking.array
    class(result) <- "StackedData"
    result
}

stackByCommonLabels <- function(common.labels, variable.names,
                                variable.labels)
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
    stacking.groups <- stacking.groups[rowSums(!is.na(stacking.groups)) > 1, ]

    stacking.groups
}

retainedIndices <- function(stacking.groups, n.vars)
{
    ind.to.remove <- unlist(lapply(seq_len(nrow(stacking.groups)), function(i) {
        ind <- stacking.groups[i, ]
        ind <- ind[!is.na(ind)]
        ind <- ind[-which.min(ind)]
    }))
    seq_len(n.vars)[-ind.to.remove]
}

stackedIndices <- function(stacking.groups, retained.indices)
{
    stacking.ind <- vapply(seq_len(nrow(stacking.groups)), function(i) {
        ind <- stacking.groups[i, ]
        ind <- ind[!is.na(ind)]
        ind <- ind[which.min(ind)]
    }, integer(1))

    vapply(stacking.ind, match, integer(1), retained.indices)
}

stackedVariableText <- function(stacking.groups, variable.text,
                                retained.indices, stacked.indices,
                                prevent.duplicates = FALSE)
{
    stacked.text <- vapply(seq_len(nrow(stacking.groups)), function(i) {
        ind <- stacking.groups[i, ]
        ind <- ind[!is.na(ind)]
        text <- variable.text[ind]
        common.prefix <- getCommonPrefix(text)
        if (common.prefix != "")
            common.prefix
        else
            "stacked_var"
    }, character(1))

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
        }
    }

    result
}

# Source variable names and labels for the variables in the stacked data set
stackingArray <- function(stacking.groups, variable.names, variable.labels)
{
    n.stacked.vars <- nrow(stacking.groups)
    n.stacked <- ncol(stacking.groups)

    names.matrix <- t(vapply(seq_len(n.stacked.vars), function(i) {
        new.row <- rep(NA_character_, n.stacked)
        ind <- stacking.groups[i, ]
        new.row[!is.na(ind)] <- variable.names[ind[!is.na(ind)]]
        new.row
    }, character(n.stacked)))

    labels.matrix <- t(vapply(seq_len(n.stacked.vars), function(i) {
        new.row <- rep(NA_character_, n.stacked)
        ind <- stacking.groups[i, ]
        new.row[!is.na(ind)] <- variable.labels[ind[!is.na(ind)]]
        new.row
    }, character(n.stacked)))

    result <- array(dim = c(nrow(names.matrix), n.stacked, 2))
    result[, , 1] <- names.matrix
    result[, , 2] <- labels.matrix
    result
}

getCommonPrefix <- function(nms)
{
    common_prefix <- ""
    for (i in 1:min(nchar(nms)))
    {
        if (length(unique(vapply(tolower(nms), substr, character(1), 1, i))) == 1)
            common_prefix <- substr(nms[1], 1, i)
        else
            break
    }
    common_prefix
}

#' @importFrom flipFormat StackingWidget
#' @export
print.StackedData <- function(x, ...)
{
    StackingWidget(x$stacked.variable.names,
                   x$stacked.variable.labels,
                   x$stacking.array)
}
