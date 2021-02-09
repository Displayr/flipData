#' Automatically coerces a vector into an appropriate type.
#'
#' Converts it to \code{\link{character}}, and then uses pattern
#' matching to work out the most appropriate type.
#'
#' @param x A \code{vector}.
#' @param stringsAsFactors logical: should character vectors be
#'     converted to factors?
#' @param max.value.labels logical: only variables with value labels
#' and at most this many unique values will be converted to factors.
#' Ignored if \code{stringsAsFactors} is \code{FALSE}.
#' @importFrom lubridate is.Date is.POSIXt is.timepoint
#' @importFrom flipTime AsDateTime
#' @importFrom stats as.formula
#' @importFrom verbs Sum
#' @export

AutoCoerceClass <- function(x, stringsAsFactors = TRUE, max.value.labels = 12)
{
    # Do not coerce types of already OK vectors
    if (is.numeric(x) | is.logical(x) | is.Date(x) | is.POSIXt(x) | is.timepoint(x))
        return(x)
    n <- length(x)

    # Missing values
    missing <- IsMissing(x)
    if (all(missing))
        return(out)
    # Converting everything else to character
    x.original <- x
    x <- as.character(x)
    x.not.missing <- x[!missing]
    x.lower = tolower(x)

    # Logical
    if (all(tolower(x.not.missing) %in% c("true", "false", "t", "f")))
    {
        out = x.lower %in% c("true", "t")
        out[missing] <- NA
        return(out)
    }
    out <- rep(NA, n)

    # Numeric
    if (suppressWarnings(Sum(is.na(num <- as.numeric(x.not.missing)), remove.missing = FALSE) == 0))
    {
        out[!missing] <- num
        return(out)
    }

    # Dates and times
    ## This first bit will be deprecated soon via DS-1992
    if (all(stringr::str_count(x.not.missing, "-") == 2 & stringr::str_count(x.not.missing, ":") == 2 & stringr::str_count(x.not.missing, "T") == 1))        {
        dts <- suppressWarnings(AsDateTime(gsub(".000Z", "", x.not.missing), on.parse.failure = "warn"))
        out[!missing] <- dts
        class(out) <- class(dts)
        return(out)
    }
    if (suppressWarnings(Sum(is.na((dat <- AsDateTime(x, on.parse.failure = "warn"))[!missing]), remove.missing = FALSE) == 0))
        return(dat)

    # Text versus factors
    if (stringsAsFactors)
    {
        if (length(unique(x.not.missing)) > max.value.labels)
            return(x)
        if (!is.factor(x.original))
            return(as.factor(x))
        return(x.original)
    }
    if (is.factor(x.original))
        return(x)
    x.original
}
