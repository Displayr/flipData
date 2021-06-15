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

#' @importFrom haven read_sav
readLocalDataSets <- function(data.set.paths)
{
    result <- lapply(data.set.paths, function(path) {
        read_sav(path)
    })
    names(result) <- basename(data.set.paths)
    result
}

#' @importFrom flipAPI QLoadData
readDataSetsFromDisplayrCloudDrive <- function(data.set.names)
{
    result <- lapply(data.set.names, QLoadData)
    names(result) <- data.set.names
    result
}

#' @importFrom haven write_sav
#' @importFrom flipAPI QSaveData IsDisplayrCloudDriveAvailable
writeDataSet <- function(data.set, data.set.name)
{
    if (IsDisplayrCloudDriveAvailable())
        QSaveData(data.set, data.set.name)
    else
        write_sav(data.set, data.set.name)
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
         n.cases = nrow(data.set),
         data.set.name = dataSetNameWithoutPath(data.set.name))
}

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
         data.set.names = names(data.sets),
         n.data.sets = length(data.sets))
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

isDateType <- function(var.types)
{
    var.types %in% c("Date", "Date/Time")
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

# Set to 2GB as I found that memory issues start to occur beyond here
DATA.SET.SIZE.LIMIT <- 2 * 1e9
