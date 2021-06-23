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
    if (length(data.set.names) < min.data.sets)
        stop("At least ", min.data.sets, " data set(s) are required.")

    if (IsDisplayrCloudDriveAvailable())
        readDataSetsFromDisplayrCloudDrive(data.set.names)
    else
        readLocalDataSets(data.set.names)
}

#' @param data.set.paths A character vector of paths to local data files.
#' @return A list of data frames, with each representing a data set.
#' @noRd
#' @importFrom haven read_sav
readLocalDataSets <- function(data.set.paths)
{
    result <- lapply(data.set.paths, function(path) {
        read_sav(path)
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
    result <- lapply(data.set.names, QLoadData)
    names(result) <- data.set.names
    result
}

#' @param data.set A data frame containing the data set to write.
#' @param data.set.name A character scalar of data file name to write to.
#' @return Nothing.
#' @noRd
#' @importFrom haven write_sav
#' @importFrom flipAPI QSaveData IsDisplayrCloudDriveAvailable
writeDataSet <- function(data.set, data.set.name)
{
    if (IsDisplayrCloudDriveAvailable())
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
#'     \item \code{variable.names} A list whose elements correspond to the data
#'       sets. Each element is a character vector of names of variables in a
#'       data set.
#'     \item \code{variable.labels} A list whose elements correspond to the data
#'       sets. Each element is a character vector of labels of variables in a
#'       data set.
#'     \item \code{variable.value.attributes} A list whose elements correspond to the data
#'       sets. Each element is another list with elements corresponding to
#'       variables in a data set. If a variable is categorical, the element
#'       will be a named numeric vector representing the values and value
#'       labels. The element is NULL if the variable is not categorical.
#'     \item \code{variable.types} A list whose elements correspond to the data
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
    list(variable.names = lapply(data.sets, names),
         variable.labels = lapply(data.sets, function(data.set) {
             vapply(data.set, function(v) {
                 lbl <- attr(v, "label", exact = TRUE)
                 if (!is.null(lbl))
                     lbl
                 else
                     ""
             }, character(1))
         }),
         variable.value.attributes = lapply(data.sets, function(data.set) {
             lapply(data.set, function(v) {
                 val.attr <- attr(v, "labels", exact = TRUE)
                 if (is.character(val.attr))
                     NULL
                 else
                     val.attr
             })
         }),
         variable.types = lapply(data.sets, function(data.set) {
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
            "Categorical"
        else
            "Text"
    }
    else if (is.numeric(variable))
        "Numeric"
    else if (is.character(variable))
        "Text"
    else if (inherits(variable, "Date"))
        "Date"
    else if (inherits(variable, "POSIXct"))
        "Date/Time"
    else if (inherits(variable, "difftime"))
        "Duration"
    else
        stop("Variable type not recognised")
}

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
        all(floor(x.without.na) == x.without.na)
    }
    else
        FALSE
}

#' @param merged.data.set.name A character scalar of the candidate name of the
#'  merged data set. This may be NULL or empty.
#' @return A character scalar of a valid name for the merged data set.
#' @examples
#' cleanMergedDataSetName(NULL) # "Merged data set.sav"
#' cleanMergedDataSetName("") # "Merged data set.sav"
#' cleanMergedDataSetName(" merged ") # "merged.sav"
#' cleanMergedDataSetName("merged?") # error due to question mark in name
#' @noRd
cleanMergedDataSetName <- function(merged.data.set.name)
{
    if (is.null(merged.data.set.name) || trimws(merged.data.set.name) == "")
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

#' @description Throws an error if the file name contains invalid characters.
#' @param file.name A character scalar of the file name.
#' @return Nothing
#' @examples
#' checkFileNameCharacters("Merged?.sav") # error due to question mark in name
#' checkFileNameCharacters("Merged.sav") # no error
#' @noRd
checkFileNameCharacters <- function(file.name)
{
    if (grepl("[<>:\"/\\\\\\|\\?\\*]", file.name))
        stop("The file name '", file.name,
             "' is invalid as file names cannot contain the characters '>', ':', '\"', '/', '\\', '|', '?', '*'.")
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
            variableNotFoundError(range.start, data.set.index)
        if (is.na(end.ind))
            variableNotFoundError(range.end, data.set.index)
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
        paste0("the input data set ", data.set.index, ". ")

    stop("The input variable '", var.name,
         "' could not be found in ", data.set.text,
         "Ensure that the variable has been correctly specified.")
}

# Creates a name from new.name that does not exist in existing.names by
# appending a numeric suffix if necessary
#' @param new.name Character scalar containing the candidate name that may need
#'  to be renamed to be different from the names in existing.names.
#' @param existing.names Character vector of existing names.
#' @param delimiter Character scalar to be placed between new.name and an
#'  integer suffix.
#' @return
#' @examples
#' uniqueName("Q2", c("Q1", "Q2", "Q3")) # "Q21"
#' uniqueName("Q2", c("Q1", "Q2", "Q3"), delimiter = "_") # "Q2_1"
#' @noRd
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

# Set to 2GB as I found that memory issues start to occur beyond here
DATA.SET.SIZE.LIMIT <- 2 * 1e9
