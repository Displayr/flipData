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
#' @param specify.by To be filled
#' @param variables.to.stack To be filled
#' @param variables.to.omit To be filled
#' @export
StackData <- function(input.data.set.name,
                      stacked.data.set.name = NULL,
                      common.labels = NULL,
                      specify.by = NULL,
                      variables.to.manually.stack = NULL,
                      variables.to.omit = NULL)
{
    input.data.set <- readDataSets(input.data.set.name, 1)[[1]]
    input.data.set.metadata <- metadataFromDataSet(input.data.set)
    n.vars <- input.data.set.metadata$n.variables

    # variables.to.omit <- parseVariablesToOmit(variables.to.omit)

    # auto common.labels

    if (!is.null(common.labels))
    {
        stacking.groups <- stackByCommonLabels(common.labels,
                                               input.data.set.metadata)
    }

    # stacked.data.set <- if (specify.by == "Variable")
    #     stackingSpecifiedByVariable(variable.names, variables.to.stack)
    # else # specify.by == "Case"
    #     stackingSpecifiedByCase(variable.names, variables.to.stack)
    retained.indices <- retainedIndices(stacking.groups, n.vars)
    stacked.indices <- stackedIndices(stacking.groups, retained.indices)
    # is.stacked <- isStacked(stacking.groups, n.vars)

    # Omit variables


    stacked.data.set <- stackedDataSet(input.data.set, input.data.set.metadata, retained.indices,
                                       stacked.indices, stacking.groups)
    stacked.data.set.name <- cleanStackedDataSetName(stacked.data.set.name,
                                                     input.data.set.name)
    writeDataSet(stacked.data.set, stacked.data.set.name)

    stacked.data.set.metadata <- metadataFromDataSet(stacked.data.set)

    result <- list()
    result$input.data.set.metadata <- input.data.set.metadata
    result$stacked.data.set.metadata <- stacked.data.set.metadata
    result$stacking.groups <- stacking.groups
    result$stacked.indices <- stacked.indices
    result$stacked.data.set.name <- stacked.data.set.name
    class(result) <- "StackedData"
    result
}

# Creation and writing of stacked data set
# Variables to omit
# Manual stacking specified by variable
# Manual stacking specified by case
# Automatic common labels

stackByCommonLabels <- function(common.labels, variable.metadata)
{
    v.names <- variable.metadata$variable.names
    v.labels <- variable.metadata$variable.labels
    v.types <- variable.metadata$variable.types
    v.categories <- variable.metadata$variable.categories
    nchar.labels <- nchar(common.labels)

    stacking.groups <- matrix(nrow = 0, ncol = length(common.labels))

    current.group <- rep(NA_integer_, length(common.labels))
    current.prefix <- NA_character_
    current.suffix <- NA_character_
    for (i in seq_along(v.labels))
    {
        lbl <- v.labels[i]
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
    attr(stacking.groups, "unstackable.ind") <- unstackable.ind
    stacking.groups
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
    stacking.ind <- vapply(seq_len(nrow(stacking.groups)), function(i) {
        ind <- removeNA(stacking.groups[i, ])
        ind <- ind[which.min(ind)]
    }, integer(1))

    vapply(stacking.ind, match, integer(1), retained.indices)
}

stackedVariableText <- function(stacking.groups, variable.text,
                                retained.indices, stacked.indices,
                                prevent.duplicates = FALSE)
{
    stacked.text <- vapply(seq_len(nrow(stacking.groups)), function(i) {
        ind <- removeNA(stacking.groups[i, ])
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

getCommonPrefix <- function(nms)
{
    common_prefix <- ""
    for (i in 1:min(nchar(nms)))
    {
        if (allIdentical(vapply(tolower(nms), substr, character(1), 1, i)))
            common_prefix <- substr(nms[1], 1, i)
        else
            break
    }
    common_prefix
}

stackedDataSet <- function(input.data.set, variable.metadata, retained.indices,
                           stacked.indices, stacking.groups)
{
    stacked.variable.names <- stackedVariableText(stacking.groups,
                                                  variable.metadata$variable.names,
                                                  retained.indices,
                                                  stacked.indices, TRUE)
    stacked.variable.labels <- stackedVariableText(stacking.groups,
                                                   variable.metadata$variable.labels,
                                                   retained.indices,
                                                   stacked.indices)

    n.var <- length(stacked.variable.names)
    n.stacked <- ncol(stacking.groups)
    stacked.data.set <- data.frame(lapply(seq_len(n.var), function(i) {
        ind <- match(i, stacked.indices)
        if (!is.na(ind))
        {
            v <- unlist(lapply(stacking.groups[ind, ], function(j) {
                if (!is.na(j))
                    input.data.set[[j]]
                else
                    rep(NA, nrow(input.data.set))
            }))
            group.ind <- removeNA(stacking.groups[ind, ])
            categories <- attr(input.data.set[[group.ind[1]]],
                               "labels", exact = TRUE)
        }
        else
        {
            input.var <- input.data.set[[stacked.variable.names[i]]]
            v <- rep(input.var, n.stacked)
            categories <- attr(input.var, "labels", exact = TRUE)
        }
        attr(v, "label") <- stacked.variable.labels[i]
        if (!is.null(categories))
        {
            attr(v, "labels") <- categories
            class(v) <- c(class(v), "haven_labelled")
        }
        v
    }))

    names(stacked.data.set) <- stacked.variable.names
    stacked.data.set
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

allIdentical <- function(x)
{
    length(unique(x)) < 2
}

removeNA <- function(x)
{
    x[!is.na(x)]
}

#' @importFrom flipFormat StackingWidget
#' @export
print.StackedData <- function(x, ...)
{
    StackingWidget(x$input.data.set.metadata,
                   x$stacked.data.set.metadata,
                   x$stacking.groups,
                   x$stacked.indices,
                   x$stacked.data.set.name)
}
