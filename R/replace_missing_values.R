#' Replace Missing Values
#'
#' Detect missing values within mNIRS vector data and replace via methods from
#' the `{zoo}` package.
#'
#' @param x A numeric vector of mNIRS data.
#' @param method Indicates how to replace missing data.
#' - *"locf"* Stands for 'last observation carried forward'. Replaces each `NA`
#' with the most recent non-`NA` value prior to it.
#' - *"linear"* Replaces each `NA` via linear interpolation.
#' - *"spline"* Replaces each `NA` with cubic spline interpolation.
#' @param rule An integer of length 1 or 2 describing how interpolation is to
#' take place outside the interval [`min(x), max(x)`]. If `rule = 1` then `NA`s
#' are returned for such points and if `rule = 2`, the value at the closest
#' data extreme is used. Use, e.g., `rule = 2:1`, if the left and right side
#' extrapolation should differ.
#' @param na.rm A logical. If the result of the interpolation still results in
#' leading and/or trailing `NA`s, should these be removed (using na.trim)?
#' @param maxgap A numeric scalar for the maximum number of consecutive `NA`s
#' to fill. Any longer gaps will be left unchanged.
#' @param ... Additional arguments.
#'
#' @details
#' For *"locf"*, if there are no earlier non-`NA`s, then the `NA` is either
#' omitted (if `na.rm = TRUE`) or it is not replaced (if `na.rm = FALSE`).
#'
#' ...
#'
#' @return A list `L` with `L$y` the corrected time series and
#' `L$idx` the indices of the values replaced.
#'
#' @export
replace_missing_data <- function(
        x,
        method = c("locf", "linear", "spline"),
        rule = 2,
        na.rm = FALSE,
        maxgap = Inf,
        ...
) {

    method <- match.arg(method)

    if (method == "locf") {
        y <- zoo::na.locf(x, rule = rule, na.rm = na.rm, maxgap = maxgap)
    } else if (method == "linear") {
        y <- zoo::na.approx(x, rule = rule, na.rm = na.rm, maxgap = maxgap)
    } else if (method == "spline") {
        y <- zoo::na.spline(x, na.rm = na.rm, maxgap = maxgap)
    }

    idx <- which(is.na(x))

    return(list(y = y, idx = idx))
}
# (x <- c(2,NA,1,4,5,2, NA))
# zoo::na.locf(x)
# replace_missing_data(x, "locf", rule = 1, na.rm = FALSE)
