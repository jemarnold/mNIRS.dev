#' Replace Missing Values
#'
#' Detect missing values in vector data and replace via methods from the
#' [zoo][zoo::na.approx()] package.
#'
#' @param x A numeric vector.
#' @param method Indicates how to replace missing data.
#'  \describe{
#'      \item{`"locf"`}{Stands for 'last observation carried forward'. Replaces each
#'      `NA` with the most recent non-`NA` value prior to it via [zoo::na.locf()].}
#'      \item{`"linear"`}{Replaces each `NA` via linear interpolation via
#'      [zoo::na.approx()].}
#'      \item{`"spline"`}{Replaces each `NA` with cubic spline interpolation via
#'      [zoo::na.spline()].}
#'      \item{`"omit"`}{Removes `NA` via [stats::na.omit()].}
#'  }
#' @param na.rm A logical. If the result of the interpolation still results in
#' leading and/or trailing `NA`s, should these be removed (using na.trim)?
#' @param maxgap A numeric scalar for the maximum number of consecutive `NA`s
#' to fill. Any longer gaps will be left unchanged.
#'
#' @details
#' This function will also replace `NaN` and `Inf` to `NA`.
#' \describe{
#'  \item{`method = "locf"`}{if there are no earlier non-`NA`s, then the `NA` is
#'      either omitted (if `na.rm = TRUE`) or it is not replaced (if
#'      `na.rm = FALSE`).}
#'  \item{`method = "linear"`}{`na.rm = TRUE` will extrapolate over leading and
#'      trailing `NA`s by applying `rule = 2` (see [stats::approx()]).
#'      `na.rm = FALSE` will return leading/trailing `NA`s by applying `rule = 1`.}
#'  \item{`method = "spline"`}{*TODO*}
#'  \item{`method = "omit"`}{the returned vector `y` will be a named vector with
#'      the original indices of each value as names. This allows for preserving and
#'      re-inserting the omitted `NA`s back into the final dataset, if needed.}
#' }
#'
#' @seealso [zoo::na.locf()], [zoo::na.approx()], [zoo::na.spline()],
#'  [stats::na.omit()]
#'
#' @examples
#' (x <- c(1, 2, NA, Inf, 5, 6, NA))
#' replace_missing(x, method = "omit")
#' replace_missing(x, method = "locf")
#' replace_missing(x, method = "linear", na.rm = FALSE)
#' replace_missing(x, method = "linear", na.rm = TRUE)
#'
#' @return A numeric vector of filtered data or a named numeric vector
#'     with names from indices of the original input vector.
#'
#' @export
replace_missing <- function(
        x,
        method = c("linear", "locf", "spline", "omit"),
        na.rm = FALSE,
        maxgap = Inf
) {

    method <- match.arg(method)

    ## validation: `x` must be a numeric vector
    if (!is.numeric(x)) {
        cli::cli_abort("{.arg x} must be a {.cls numeric} vector.")
    }

    ## replace NaN & Inf to NA
    if (is.numeric(x)) {
        x[is.nan(x) | is.infinite(x)] <- NA
    }

    if (method == "linear") {

        rule <- ifelse(na.rm, 2, 1)
        y <- zoo::na.approx(x, rule = rule, na.rm = na.rm, maxgap = maxgap)

    } else if (method == "locf") {

        y <- zoo::na.locf(x, na.rm = na.rm, maxgap = maxgap)

    } else if (method == "spline") {

        y <- zoo::na.spline(x, na.rm = na.rm, maxgap = maxgap)

    } else if (method == "omit") {

        x.all <- stats::setNames(x, seq_along(x))
        y <- stats::na.omit(x.all)[names(na.omit(x.all))]

    }

    return(y)
}
