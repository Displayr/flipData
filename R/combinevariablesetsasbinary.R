

#' CombineVariableSetsAsBinary
#'
#' @description Combines a list of variable sets to binary variables, matching categories between them.
#' @param variable.set.list A list containing one or more variable sets which should be
#' Nominal, Ordinal, Nominal/Ordinal - Multi, Binary - Multi, or Binary - Multi (Compact)
#' @param compute.for.incomplete A boolean value. If \code{FALSE}, cases with any missing data
#' will have a missing vlaue. If \code{TRUE}, only cases whose data is entirely missing will
#' be assigned a missing value.
#' @importFrom verbs Count AnyOf
#' @importFrom flipTransformations AsNumeric
#' @export
CombineVariableSetsAsBinary <- function(variable.set.list, compute.for.incomplete = TRUE) {


    if (is.data.frame(variable.set.list) || !is.list(variable.set.list)) {
        stop("Input data should be supplied as a list.")
    }

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
        } else {
            return(colnames(x)[duplicated(colnames(x))])
        }
    })

    n.duplicates = vapply(duplicated.labels, FUN = length, FUN.VALUE = numeric(1))

    if (any(n.duplicates > 0)) {
        dup.qs = names(duplicated.labels)[n.duplicates > 0]
        dup.labels = duplicated.labels[n.duplicates > 0]
        stop(paste0("The input data contains duplicate labels and cannot be matched. Duplicated labels: " , dup.labels[[1]]))
    }

    
    binary.versions <- lapply(variable.set.list, FUN = questionToBinary)

    # Pick One - Multis will appear as a list.
    # Append thoses lists to the main list and delete the
    # corresponding original elements    
    for (j in 1:length(binary.versions)) {
        if (!is.data.frame(binary.versions[[j]]) & is.list(binary.versions[[j]])) {
            binary.versions <- c(binary.versions, binary.versions[[j]])
            binary.versions[j] <- NULL
        }
    }
    
    # If only one variable set then just return it
    if (length(binary.versions) == 1) {
        result <- binary.versions[[1]] == 1
    } else {

        # Check matching of column labels in binary data
        all.labels = lapply(binary.versions, FUN = colnames)
        unique.labels = unique(unlist(all.labels))
        common.labels = unique.labels
        for (j in seq_along(binary.versions)) {
            common.labels = intersect(common.labels, colnames(binary.versions[[j]]))
        }
        if (!setequal(unique.labels, common.labels)) {
            stop(paste0("Unable to match categories from the input data. The labels which do not appear in all inputs are: ", paste0(setdiff(unique.labels, common.labels), collapse = ",")))
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
    }
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
    }
    
    # Consider generalizing in future
    if (is.null(question.type)) {
        stop("This function should only be applied to variable sets in Displayr.")
    }
        
    if (question.type %in% c("PickAny", "PickAnyCompact")) {
        # Identify and remove the NET column basedon the codeframe attribute
        cf = attr(x, "codeframe")
        if (!is.null(cf)) {
            unique.codes = unique(unlist(cf))
            net.cols = vapply(cf, isDefaultNet, FUN.VALUE = logical(1), unique.codes = unique.codes)
            x[, !net.cols]    
        }
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
        return(binary.version)
    }
}
