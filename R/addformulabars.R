#' Add random effects terms to a model formula
#'
#' Replaces factor variables in a \code{\link{formula}} with their
#' representation as a random effect (e.g. a factor \code{f} becomes
#' \code{(1|f)}) as done for formulas in e.g. the \code{lme4} and
#' \code{nlme} packages.
#' @param frml A model \code{\link{formula}}.
#' @param data A \code{data.frame} containing the variables in
#'     \code{frml}.
#' @details A formula with one factor \code{f} and one continuous
#'     variable \code{c} in the formula
#' @importFrom flipU AllVariablesNames
#' @importFrom stats as.formula
#' @export
#' @examples
#' dat <- data.frame(c1 = 1:3, c2 = rnorm(3), f1 = factor(1:3), f2 = ordered(1:3))
#' frml <- ~c1+c2+f1+f2
#' AddFormulaBars(frml, dat)
AddFormulaBars <- function(frml, data)
{
    vars <- flipU::AllVariablesNames(frml, data)
    is.fact <- with(data, vapply(vars, function(v) is.factor(get(v)),
                                             logical(1L)))

    frml.chr <- paste0("~", paste(vars[!is.fact], collapse = "+"))
    if (any(is.fact))
        frml.chr <- paste0(frml.chr, if (any(!is.fact)) "+",
                           paste(paste0("(1|", vars[is.fact], ")"),
                                 collapse = "+"))

    return(as.formula(frml.chr))
}
