

#' CombineVariableSetsAsBinary
#'
#' @description Combines a list of variable sets to binary variables, matching categories between them.
#' @param ... One or more variable sets which should be Nominal, Ordinal, Nominal/Ordinal - Multi,
#' Binary - Multi, or Binary - Multi (Compact)
#' @param compute.for.incomplete A boolean value. If \code{FALSE}, cases with any missing data
#' will have a missing vlaue. If \code{TRUE}, only cases whose data is entirely missing will
#' be assigned a missing value.
#' @param unmatched.pick.any.are.missing Boolean value. When one of the input variable sets
#' is binary (Pick Any variable set) and additonal columns need to be added, the new column is fillend 
#' entirely with missing values when a value of \code{TRUE} is supplied. If set to \code{FALSE}, 
#' missing values will only be assigned for cases where all existing columns are missing. Note that for 
#' mutually-exclusive input variables, new columns will be created such that only cases with entirely 
#' missing values are assigned a missing value. 
#' @importFrom verbs Count AnyOf SumEachRow
#' @importFrom flipTransformations AsNumeric
#' @export
CombineVariableSetsAsBinary <- function(..., compute.for.incomplete = TRUE, unmatched.pick.any.are.missing = TRUE) {

    variable.set.list <- list(...)

    # Check for duplicated labels which make life difficult when matching
    duplicated.labels = lapply(variable.set.list, function(x) {
        question.type <- attr(x, "questiontype")
        if (is.factor(x)) {
            question.type <- "PickOne" 
        }

        # Consider generalizing in future
        if (is.null(question.type)) {
            stop("This function should only be applied to variable sets in Displayr.")
        }


        if (question.type == "PickOneMulti") {
            x = x[[1]]
        }

        if (is.factor(x)) {
            levs = levels(x)
            return(levs[duplicated(levs)])
        }

        colnames(x)[duplicated(colnames(x))]
    })

    n.duplicates = vapply(duplicated.labels, FUN = length, FUN.VALUE = numeric(1))

    if (any(n.duplicates > 0)) {
        dup.qs = names(duplicated.labels)[n.duplicates > 0]
        dup.labels = duplicated.labels[n.duplicates > 0]
        stop("The input data contains duplicate labels and cannot be matched. Duplicated labels: " , dup.labels[[1]])
    }

    binary.versions <- lapply(variable.set.list, FUN = questionToBinary)

    final.binary.versions <- list()

    # Pick One - Multis will appear as a list.
    # Append thoses lists to the main list and delete the
    # corresponding original elements
    for (j in 1:length(binary.versions)) {
        if (!is.data.frame(binary.versions[[j]]) & is.list(binary.versions[[j]])) {
            final.binary.versions <- c(final.binary.versions, binary.versions[[j]])
        } else {
            final.binary.versions <- c(final.binary.versions, list(binary.versions[[j]]))
        }
    }

    binary.versions <- final.binary.versions

    n.cases <- vapply(binary.versions, FUN = NROW, FUN.VALUE = numeric(1))

    if (!all(n.cases == n.cases[1])) {
        stop("The number of cases is not the same in all of the input data.")
    }

    # If only one variable set then just return it
    if (length(binary.versions) == 1) {
        return(binary.versions[[1]] == 1)
    }

    # Check matching of column labels in binary data
    all.labels = lapply(binary.versions, FUN = colnames)
    unique.labels = unique(unlist(all.labels))
    common.labels = Reduce(intersect, all.labels)

    if (!setequal(unique.labels, common.labels)) {
        binary.versions <- lapply(binary.versions,
            FUN = fillInCategoriesWhenNotPresent,
            expected.columns = unique.labels,
            pick.any.all.missing = unmatched.pick.any.are.missing)
    }

    input.args = binary.versions
    input.args[["match.elements"]] <- "Yes"
    input.args[["elements.to.count"]] <- list(numeric = NA, categorical = NULL)
    input.args[["ignore.missing"]] <- TRUE

    # Count missing values for each case for each binary variable
    n.missing <- do.call(Count, input.args)

    # Combine the sets of binary variables using AnyOf
    input.args[["elements.to.count"]] <- list(numeric = 1, categorical = NULL)
    result <- do.call(AnyOf, input.args)

    # Handle missing values
    if (compute.for.incomplete) { # Only assign NA if all missing
        result[n.missing == length(binary.versions)] <- NA
    } else { # Assign NA if any missing
        result[n.missing > 0] <- NA
    }

    # Replace blank level labels
    c.names <- colnames(result)
    c.names[c.names == "<BLANK>"] <- ""
    colnames(result) <- c.names
    result
}


# Function to identify the default NET column
# in Pick Any data.
isDefaultNet <- function(codes, unique.codes) {
    all(unique.codes %in% codes) && length(codes) == length(unique.codes)
}


# Convert Displayr variable sets to binary.
# Factors are split out with one column per level
questionToBinary <- function(x) {
    question.type = attr(x, "questiontype")

    # Standalone factor variables can retain the "questiontype"
    # value of "PickOneMulti" inherited from their parent
    # question
    if (is.factor(x)) {
        question.type <- "PickOne"
        levs <- levels(x)
        levs[levs == ""] <- "<BLANK>" # Protecting against blank level labels. Will be put back later.
        levels(x) <- levs
    }

    # Consider generalizing in future
    if (is.null(question.type)) {
        stop("This function should only be applied to variable sets in Displayr.")
    }

    if (question.type %in% c("PickAny", "PickAnyCompact")) {
        # Identify and remove the NET column basedon the codeframe attribute
        cf <- attr(x, "codeframe")
        if (!is.null(cf)) {
            unique.codes = unique(unlist(cf))
            net.cols = vapply(cf, isDefaultNet, FUN.VALUE = logical(1), unique.codes = unique.codes)
            x <- x[, !net.cols]
        }
        attr(x, "originalquestiontype") <- "Pick Any"
        return(x)
    }

    # Each variable in a Pick One - Multi is split separately
    if (question.type == "PickOneMulti") {
        return(lapply(x, questionToBinary))
    }

    # Split factor into binary columns, retaining
    # levels as column names and replacing rows
    # with missing values where the original variable
    # had a missing value

    if (is.factor(x)) {
        # Remove the 'ordered' class so that AsNumeric
        # works as intended
        if (is.ordered(x))
            class(x) <- class(x)[class(x) != "ordered"]
        binary.version <- AsNumeric(x, binary = TRUE, name = levels(x))
        colnames(binary.version) <- levels(x)
        binary.version[is.na(x), ] <- NA
        attr(binary.version, "originalquestiontype") <- "Pick One"
        return(binary.version)
    }

    stop("Unsupported data type: ", question.type)
}

# Function to expand the number of columns in the binary data
# when there are fewer columns than expected. expected.columns
# should be a vector of column names.
fillInCategoriesWhenNotPresent <- function(binary.data, expected.columns, pick.any.all.missing = TRUE) {

    current.colnames <- colnames(binary.data)

    if (all(expected.columns %in% current.colnames))
        return(binary.data)

    new.colnames <- expected.columns[! expected.columns %in% current.colnames]
    new.data <- matrix(FALSE, nrow = nrow(binary.data), ncol = length(new.colnames))
    colnames(new.data) <- new.colnames


    # Missing data rule
    # For data which was originally mutually-exclusive,
    # cases are assigned missing values in the new columns
    # when the case has missing data in the existing columns.
    # In this case the row will always be entirely missing
    # or entirely non-missing.
    # For data which was already binary, new columns should be
    # entirely missing unless overridden by the argument.
    n.missing.per.case <- SumEachRow(is.na(binary.data))
    missing.in.new.data = rep(TRUE, nrow(binary.data))
    if (attr(binary.data, "originalquestiontype") == "Pick One" || !pick.any.all.missing) {
        missing.in.new.data <- n.missing.per.case == ncol(binary.data)
    }

    new.data[missing.in.new.data, ] <- NA

    binary.data <- cbind(binary.data, new.data)
    binary.data <- binary.data[, expected.columns]
    binary.data
}
