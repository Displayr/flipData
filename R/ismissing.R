#' Identifies missing values in character objects
#'
#' Looks for common ways of expressing values, such as \code{NA},
#' \code{"NA"}, and \code{""}.
#'
#' @param x A \code{vector}.
#' @export

IsMissing <- function(x)
{
    return(is.na(x) | grepl("^[[:blank:]]*(|-|\\.|n/a|na|nan|null|missing|[I|i]nvalid)?[[:blank:]]*$", tolower(x)))
}
