#' @param data.set.names A character vector of names of data sets from the
#'  Displayr cloud drive to merge (if run from Displayr) or file paths of local
#'  data sets.
#' @param min.data.sets The minimum number of data sets required. An error is
#'  thrown if the number of data set names is below this number.
#' @return A list of data frames, with each representing a data set.
#' @noRd
#' @importFrom flipAPI IsDisplayrCloudDriveAvailable
readDataSets <- function(data.set.names, min.data.sets = 1)
{
    data.set.names <- vapply(data.set.names, trimws, character(1),
                             USE.NAMES = FALSE)

    if (length(data.set.names) < min.data.sets)
        stop("At least ", min.data.sets, " data set(s) are required.")

    if (!all(grepl('.+\\.sav$', data.set.names, ignore.case = TRUE))) {
        stop("An input data file was not an SPSS .sav data file. ",
             "Only SPSS .sav data files are accepted.")
    }

    if (IsDisplayrCloudDriveAvailable())
        readDataSetsFromDisplayrCloudDrive(data.set.names)
    else
        readLocalDataSets(data.set.names)
}

#' @param data.set.paths A character vector of paths to local data files.
#' @return A list of data frames, with each representing a data set.
#' @noRd
#' @importFrom haven read_sav
#' @importFrom flipU InterceptExceptions
readLocalDataSets <- function(data.set.paths, parser = read_sav)
{
    result <- lapply(data.set.paths, function(path) {
        handler = createReadErrorHandler(path)
        InterceptExceptions(parser(path), error.handler = handler)
    })
    names(result) <- basename(data.set.paths)
    result
}

#' @param data.set.names A character vector of data set names in the Displayr
#'  cloud drive.
#' @return A list of data frames, with each representing a data set.
#' @noRd
#' @importFrom flipAPI QLoadData
readDataSetsFromDisplayrCloudDrive <- function(data.set.names)
{
    result <- lapply(data.set.names, function(nm) {
        handler = createReadErrorHandler(nm)
        InterceptExceptions(QLoadData(nm), error.handler = handler)
    })
    names(result) <- data.set.names
    result
}

createReadErrorHandler <- function(data.set.name)
{
    function(e) {
        if (grepl("Invalid file, or file has unsupported features", e$message)) {
            stop(paste0("The data file '", data.set.name, "' could not be parsed. ",
                        "The data file may be fixed by inserting it in a Displayr document, ",
                        "exporting it as an SPSS file (.sav) via the Publish button, and then uploading it back to the cloud drive. "))
        } else {
            stop(e$message)
        }
    }
}

#' @param data.set A data frame containing the data set to write.
#' @param data.set.name A character scalar of data file name to write to.
#' @return Nothing.
#' @noRd
#' @importFrom haven write_sav
#' @importFrom flipAPI QSaveData IsDisplayrCloudDriveAvailable
writeDataSet <- function(data.set, data.set.name, is.saved.to.cloud)
{
    if (is.saved.to.cloud)
        QSaveData(data.set, data.set.name)
    else
        write_sav(data.set, data.set.name)
}

#' @description Creates a list of metadata for a data set
#' @param data.set A data frame containing a data set.
#' @param data.set.name A character scalar of the data set name.
#' @return A list with the following elements:
#'   \itemize{
#'     \item \code{variable.names} A character vector of names of variables in
#'       the data set.
#'     \item \code{variable.labels} A character vector of labels of variables in
#'       the data set.
#'     \item \code{variable.value.attributes} A list with elements corresponding
#'       to variables in the data set. If a variable is categorical, the element
#'       will be a named numeric vector representing the values and value
#'       labels. The element is NULL if the variable is not categorical.
#'     \item \code{variable.types} A character vector of variable types for
#'       variables in the data set (see function variableType).
#'     \item \code{n.variables} Integer scalar of the number of variables in the
#'       data set.
#'     \item \code{n.cases} Integer scalar of the number of cases in the data
#'       set.
#'     \item \code{data.set.name} Character scalar of the data set name.
#'   }
#' @noRd
metadataFromDataSet <- function(data.set, data.set.name)
{
    list(variable.names = names(data.set),
         variable.labels = vapply(data.set, function(v) {
             lbl <- attr(v, "label", exact = TRUE)
             ifelse(!is.null(lbl), lbl, "")
         }, character(1)),
         variable.value.attributes = lapply(data.set, attr, "labels",
                                            exact = TRUE),
         variable.types = vapply(data.set, variableType, character(1)),
         n.variables = length(data.set),
         n.cases = nrow(data.set),
         data.set.name = dataSetNameWithoutPath(data.set.name))
}

#' @description Creates a list of metadata for a list of data sets
#' @param data.sets A list of data frame, each representing a data set.
#' @return A list with the following elements:
#'   \itemize{
#'     \item \code{variable.names.list} A list whose elements correspond to the data
#'       sets. Each element is a character vector of names of variables in a
#'       data set.
#'     \item \code{variable.labels.list} A list whose elements correspond to the data
#'       sets. Each element is a character vector of labels of variables in a
#'       data set.
#'     \item \code{variable.value.attributes.list} A list whose elements correspond to the data
#'       sets. Each element is another list with elements corresponding to
#'       variables in a data set. If a variable is categorical, the element
#'       will be a named numeric vector representing the values and value
#'       labels. The element is NULL if the variable is not categorical.
#'     \item \code{variable.types.list} A list whose elements correspond to the data
#'       sets. Each element is a character vector of variable types for
#'       variables in a data set (see function variableType).
#'     \item \code{n.data.sets} Integer scalar of the number of data sets,
#'     \item \code{n.cases} Integer vector of the number of cases in each data
#'       set.
#'     \item \code{data.set.names} Character vector of the data set names.
#'   }
#' @noRd
metadataFromDataSets <- function(data.sets)
{
    list(variable.names.list = lapply(data.sets, names),
         variable.labels.list = lapply(data.sets, function(data.set) {
             vapply(data.set, function(v) {
                 lbl <- attr(v, "label", exact = TRUE)
                 if (!is.null(lbl))
                     lbl
                 else
                     ""
             }, character(1))
         }),
         variable.value.attributes.list = lapply(data.sets, function(data.set) {
             lapply(data.set, function(v) {
                 val.attr <- attr(v, "labels", exact = TRUE)
                 if (is.character(val.attr))
                     NULL
                 else
                     val.attr
             })
         }),
         variable.types.list = lapply(data.sets, function(data.set) {
             vapply(data.set, variableType, character(1))
         }),
         n.data.sets = length(data.sets),
         n.cases = vapply(data.sets, nrow, integer(1)),
         data.set.names = names(data.sets))
}

#' @description Gets the variable type from a variable. The types are used
#'  internally by R code and not intended to be exposed to the user.
#' @param variable A vector representing the variable.
#' @return A character scalar being one of "Numeric", "Text", "Categorical",
#'  "Date", "Date/Time", "Duration".
#' @noRd
variableType <- function(variable)
{
    if (is.null(variable))
        NA_character_
    else if (!is.null(attr(variable, "labels", exact = TRUE)))
    {
        if (is.numeric(attr(variable, "labels", exact = TRUE)))
            CATEGORICAL.VARIABLE.TYPE
        else
            TEXT.VARIABLE.TYPE
    }
    else if (is.numeric(variable))
        NUMERIC.VARIABLE.TYPE
    else if (is.character(variable))
        TEXT.VARIABLE.TYPE
    else if (inherits(variable, "Date"))
        DATE.VARIABLE.TYPE
    else if (inherits(variable, "POSIXct"))
        DATE.TIME.VARIABLE.TYPE
    else if (inherits(variable, "difftime"))
        DURATION.VARIABLE.TYPE
    else
        stop("Variable type not recognised")
}

NUMERIC.VARIABLE.TYPE = "Numeric";
TEXT.VARIABLE.TYPE = "Text";
CATEGORICAL.VARIABLE.TYPE = "Categorical";
DATE.VARIABLE.TYPE = "Date";
DATE.TIME.VARIABLE.TYPE = "Date/Time";
DURATION.VARIABLE.TYPE = "Duration";

#' @param var.types A character vector containing variable types (see function
#'  variableType).
#' @return A logical vector corresponding to the input var.types indicating if
#'  each type in var.types is Date or Date/Time.
#' @example
#' isDateType(c("Date", "Date/Time", "Numeric")) # c(TRUE, TRUE, FALSE)
#' @noRd
isDateType <- function(var.types)
{
    var.types %in% c("Date", "Date/Time")
}

#' @param x A vector or list.
#' @return A logical scalar indicating if all elements in x are identical.
#' @examples
#' allIdentical(1:3) # FALSE
#' allIdentical(c(1, 1, 1)) # TRUE
#' @noRd
allIdentical <- function(x)
{
    length(unique(x)) < 2
}

#' @param x A list of value attributes (named numeric vectors)
#' @return A logical scalar indicating if all value attributes are identical,
#'  even if the values are specified in a different order.
#' @examples
#' val.attr <- structure(1:3, .Names = c("A", "B", "C"))
#' allValueAttributesIdentical(list(val.attr, rev(val.attr))) # TRUE
#' @noRd
allValueAttributesIdentical <- function(x)
{
    allIdentical(lapply(x, sort))
}

#' @param x A vector.
#' @return A vector which is a subset of x with NA values removed.
#' @example
#' removeNA(c(NA, 1, 2, NA, 3)) # c(1, 2 ,3)
#' @noRd
removeNA <- function(x)
{
    x[!is.na(x)]
}

#' @description  Split string by comma separators, removing whitespace and
#'  empty strings.
#' @param input.text A character scalar of the text to be split.
#' @param ignore.commas.in.parentheses A logical scalar indicating whether
#'  commas inside parentheses should be ignored.
#' @return A character vector of the split text.
#' @examples
#' splitByComma("Q1,Q2, Q3") # c("Q1", "Q2", "Q3")
#' splitByComma("Q1,Q2(2,3), Q3", ignore.commas.in.parentheses = TRUE) # c("Q1", "Q2(2,3)", "Q3")
#' @noRd
splitByComma <- function(input.text, ignore.commas.in.parentheses = FALSE)
{
    if (!ignore.commas.in.parentheses)
    {
        split.text <- trimws(strsplit(input.text, ",")[[1]])
        return(split.text[split.text != ""])
    }
    else
    {
        split.char <- strsplit(input.text, "")[[1]]
        result <- c()
        start.ind <- NA_integer_
        in.parentheses <- FALSE
        for (i in seq_along(split.char))
        {
            if (is.na(start.ind))
            {
                if (split.char[i] != ",")
                    start.ind <- i
                else
                    next
            }

            if (!in.parentheses && split.char[i] == ",")
            {
                result <- c(result, paste0(split.char[start.ind:(i - 1)],
                                           collapse = ""))
                start.ind <- NA_integer_
            }
            else if (i == length(split.char))
                result <- c(result, paste0(split.char[start.ind:i],
                                           collapse = ""))
            else if (!in.parentheses && split.char[i] == "(")
                in.parentheses <- TRUE
            else if (in.parentheses && split.char[i] == ")")
                in.parentheses <- FALSE
        }
        result <- trimws(result)
        result <- result[result != ""]
        result
    }
}

#' @param x A vector.
#' @return A logical scalar indicating if all values in x are integers (in the
#'  mathematical sense, not type).
#' @examples
#' isIntegerValued(c(1, 2, 3)) # TRUE
#' isIntegerValued(c(1, 2.1, 3)) # FALSE
#' @noRd
isIntegerValued <- function(x)
{
    if (is.numeric(x))
    {
        x.without.na <- removeNA(x)
        all(floor(x.without.na) == x.without.na &
            !is.infinite(x.without.na))
    }
    else
        FALSE
}

#' @param data.set.name A character scalar of the user-input name for
#'  the data set. This may be NULL or empty.
#' @return A character scalar of a valid name for the merged data set.
#' @examples
#' correctDataSetName(NULL, "Merged data set.sav") # "Merged data set.sav"
#' correctDataSetName("", "Merged data set.sav") # "Merged data set.sav"
#' correctDataSetName(" merged ") # "merged.sav"
#' correctDataSetName("merged?") # "merged.sav"
#' @noRd
correctDataSetName <- function(data.set.name, default.data.set.name)
{
    if (is.null(data.set.name) || trimws(data.set.name) == "")
        default.data.set.name
    else
    {
        result <- data.set.name

        # Check for '<', '>', ':', '\"', '/', '\\', '|', '?', '*'
        if (grepl("[<>:\"/\\\\\\|\\?\\*]", result))
        {
            warning("The input data set name '", data.set.name
                    , "' contains invalid characters that have been removed.")
            result <- gsub("[<>:\"/\\\\\\|\\?\\*]", "", result)
        }

        result <- trimws(result)
        if (!grepl(".sav$", result))
            result <- paste0(result, ".sav")

        result
    }
}

#' @param data.set.name.or.path A character scalar of the data file name
#'  which could include the path.
#' @return A character scalar of the data file name with the path removed.
#' @example
#' dataSetNameWithoutPath("inst/testdata/Cola.sav") # "Cola.sav"
#' @noRd
dataSetNameWithoutPath <- function(data.set.name.or.path)
{
    if (IsDisplayrCloudDriveAvailable())
        data.set.name.or.path
    else
        basename(data.set.name.or.path)
}

#' @description Returns all variables in variable.names within the specified
#'  start and end variables.
#' @param variable.names A character vector of variable names.
#' @param range.start A character scalar of the name of the first variable in
#'  the range.
#' @param range.end A character scalar of the name of the last variable in the
#'  range.
#' @param data.set.index Integer scalar of the index of the data set from which
#'  variable.names originate.
#' @param input.text Character scalar of the input text containing the range.
#' @param error.if.not.found Logical scalar indicating whether to throw an
#'  error if the range could not be identified in variable.names.
#' @return A character vector of names of variables in the range. If the range
#'  could not be found and error.if.not.found is FALSE, NULL is returned.
#' @noRd
variablesFromRange <- function(variable.names, range.start, range.end,
                               data.set.index, input.text,
                               error.if.not.found = TRUE)
{
    start.ind <- ifelse(range.start != "", match(range.start, variable.names), 1)
    end.ind <- ifelse(range.end != "", match(range.end, variable.names),
                      length(variable.names))

    if (error.if.not.found)
    {
        if (is.na(start.ind))
            throwVariableNotFoundError(range.start, data.set.index)
        if (is.na(end.ind))
            throwVariableNotFoundError(range.end, data.set.index)
    }
    else
    {
        if (is.na(start.ind) || is.na(end.ind))
            return(NULL)
    }

    if (start.ind > end.ind)
        stop("The start variable '", range.start,
             "' appears after the end variable '", range.end,
             "' in the input data set ", data.set.index,
             " for the input range '", input.text, "'.")
    variable.names[start.ind:end.ind]
}

#' @param var.name Name of variable that was not found.
#' @param data.set.index Index of data set in which variable was not found.
#' @return Nothing.
#' @noRd
throwVariableNotFoundError <- function(var.name, data.set.index = NULL)
{
    data.set.text <- if (is.null(data.set.index))
        "any of the input data sets. "
    else
        paste0("input data set ", data.set.index, ". ")

    stop("The input variable '", var.name,
         "' could not be found in ", data.set.text,
         "Ensure that the variable has been correctly specified.")
}

#' @description Creates a name from new.name that does not exist in existing.names by
#'  appending a numeric suffix if necessary. Case is ignored when comparing names.
#' @param new.name Character scalar containing the candidate name that may need
#'  to be renamed to be different from the names in existing.names.
#' @param existing.names Character vector of existing names.
#' @param delimiter Character scalar to be placed between new.name and an
#'  integer suffix.
#' @return Character scalar of name that is not present in existing.names.
#' @examples
#' uniqueName("Q2", c("Q1", "Q2", "Q3")) # "Q21"
#' uniqueName("Q2", c("Q1", "Q2", "Q3"), delimiter = "_") # "Q2_1"
#' @noRd
uniqueName <- function(new.name, existing.names, delimiter = "")
{
    lower.case.new.name <- tolower(new.name)
    lower.case.existing.names <- tolower(existing.names)
    if (!(lower.case.new.name %in% lower.case.existing.names))
        return (new.name)

    i <- 1
    repeat
    {
        candidate.name <- addSuffixFittingByteLimit(new.name,
                                                    suffix = paste0(delimiter, i))
        if (!(tolower(candidate.name) %in% lower.case.existing.names))
            return(candidate.name)
        i <- i + 1
    }
}

#' @description Return variable name matches to wildcard.text. Throw error if no matches
#'  found and error.if.not.found == TRUE.
#' @param wildcard.text Character scalar of the wildcard pattern to match for.
#' @param variable.names Character vector of variable names to match against.
#' @param data.set.ind Integer scalar of the index of the data set from which
#'  the variable names originate.
#' @param error.if.not.found Logical scalar indicating whether to throw an
#'  error if no match is found.
#' @return Character vector containing the variable names that match the
#'  wildcard pattern.
#' @noRd
parseVariableWildcardForMerging <- function(wildcard.text, variable.names,
                                            data.set.ind, error.if.not.found)
{
    ind.asterisk <- match("*", strsplit(wildcard.text, "")[[1]])
    start.var.text <- trimws(substr(wildcard.text, 1, ind.asterisk - 1))
    end.var.text <- trimws(substr(wildcard.text, ind.asterisk + 1,
                                  nchar(wildcard.text)))
    pattern <- paste0("^", EscapeRegexSymbols(start.var.text), ".*",
                      EscapeRegexSymbols(end.var.text), "$")
    is.match <- grepl(pattern, variable.names)
    if (error.if.not.found && !any(is.match))
        stop("No variables were found in data set ", data.set.ind,
             " matching the wildcard input '", wildcard.text, "'.")
    variable.names[is.match]
}

# Set to 2GB as I found that memory issues start to occur beyond here
DATA.SET.SIZE.LIMIT <- 2 * 1e9

sanitizeSPSSVariableNames <- function(variable.names) {
    # Can't begin with or end with a period
    forbidden.period <- startsWith(variable.names, ".")
    if (any(forbidden.period)) {
        warning("Cannot save variables names which begin with '.'. Some variables have had the '.' removed from their names: ",
                 paste0(variable.names[forbidden.period], collapse = ", "))
        variable.names[forbidden.period] <- gsub("^\\.", "", variable.names[forbidden.period])
    }
    forbidden.period <- endsWith(variable.names, ".")
    if (any(forbidden.period)) {
        warning("Cannot save variables names which end with '.'. Some variables have had the '.' removed from their names: ",
                paste0(variable.names[forbidden.period], collapse = ", "))
        variable.names[forbidden.period] <- gsub("\\.$", "", variable.names[forbidden.period])
    }

    # SPSS variable names can't be reseved keywords
    reserved.keywords <- c("ALL", "AND", "BY", "EQ", "GE", "GT", "LE", "LT", "NE", "NOT", "OR", "TO", "WITH")
    forbidden.keywords <- variable.names %in% reserved.keywords
    if (any(forbidden.keywords)) {
        warning("Cannot save variables whose names are SPSS reserved keywords. The following variables have had '_r' added to their names:",
                paste0(variable.names[forbidden.keywords], collapse = ", "))
        variable.names[forbidden.keywords] <- paste0(variable.names[forbidden.keywords], "_r")
    }

    # SPSS variable names can't be longer than 64 bytes
    bad.length <- nchar(variable.names, type = "bytes") > 64
    if (any(bad.length)) {
        warning("Some variable names were too long and have been truncated: ",
                paste0(variable.names[bad.length], collapse = ", "))
        variable.names[bad.length] <- vapply(variable.names[bad.length],
                                             FUN = addSuffixFittingByteLimit,
                                             FUN.VALUE = character(1))
    }

    # SPSS variable names must be unique
    dupes <- duplicated(tolower(variable.names))
    if (any(dupes)) {
        dupe.ind <- which(dupes)
        for (i in dupe.ind) {
            variable.names[i] <- uniqueName(variable.names[i],
                                            existing.names = variable.names,
                                            delimiter = "_")
        }
    }

    variable.names
}


addSuffixFittingByteLimit <- function(string, suffix = "", byte.limit = 64) {
    new.string <- paste0(string, suffix)
    size <- nchar(new.string, type = "bytes")

    # Nothing to do here, return
    if (size <= byte.limit)
        return (new.string)

    # Easy encoding, just truncate and paste
    if (size == nchar(new.string))
        return(paste0(substr(string, 1, byte.limit - nchar(suffix)), suffix))

    # Approximately how many characters should we be?
    # Can't just count characters because could be a
    # different encoding.
    ratio <- byte.limit / size
    j <- min(floor(nchar(new.string) * ratio) - nchar(suffix), 2)

    # Grow the substring until we exceed the limit
    new.string <- paste0(substr(string, 1, j), suffix)
    while (nchar(new.string, type = "bytes") < byte.limit) {
        j <- j + 1
        new.string <- paste0(substr(string, 1, j), suffix)
    }

    # Reduce by one to ensure we are back under the limit
    j <- j - 1
    new.string = paste0(substr(string, 1, j), suffix)
    new.string

}
