#' Replace Missing Values
#'
#' Detect missing values within mNIRS vector data and replace via methods from
#' the `{zoo}` package.
#'
#' @param x A numeric vector of mNIRS data.
#' @param method Indicates how to replace missing data.
#' - *"locf"* Stands for 'last observation carried forward'. Replaces each `NA`
#' with the most recent non-`NA` value prior to it via [zoo::na.locf()].
#' - *"linear"* Replaces each `NA` via linear interpolation via
#' [zoo::na.approx()].
#' - *"spline"* Replaces each `NA` with cubic spline interpolation via
#' [zoo::na.spline()].
#' - *"omit"* Removes `NA` via [stats::na.omit()].
#' @param na.rm A logical. If the result of the interpolation still results in
#' leading and/or trailing `NA`s, should these be removed (using na.trim)?
#' @param maxgap A numeric scalar for the maximum number of consecutive `NA`s
#' to fill. Any longer gaps will be left unchanged.
#' @param ... Additional arguments.
#'
#' @details
#' For `method = locf`, if there are no earlier non-`NA`s, then the `NA` is either
#' omitted (if `na.rm = TRUE`) or it is not replaced (if `na.rm = FALSE`).
#'
#' For `method = linear`, `na.rm = TRUE` will extrapolate over leading and trailing
#' `NA`s by applying `rule = 2` (see [stats::approx()]). `na.rm = FALSE` will
#' return leading/trailing `NA`s by applying `rule = 1`.
#'
#' For `method = spline` *TODO*.
#'
#' For `method = omit`, the returned vector `y` will be a named vector with the
#' original indices of each value as names. This allows for preserving and
#' re-inserting the omitted `NA`s back into the final dataset.
#'
#' @return A list `L` with `L$y` the corrected time series and
#' `L$idx` the indices of the values replaced.
#'
#' @export
replace_missing_values <- function(
        x,
        method = c("locf", "linear", "spline", "omit"),
        na.rm = FALSE,
        maxgap = Inf,
        ...
) {

    method <- match.arg(method)

    if (method == "locf") {

        y <- zoo::na.locf(x, na.rm = na.rm, maxgap = maxgap)

    } else if (method == "linear") {

        rule <- ifelse(na.rm, 2, 1)
        y <- zoo::na.approx(x, rule = rule, na.rm = na.rm, maxgap = maxgap)

    } else if (method == "spline") {

        y <- zoo::na.spline(x, na.rm = na.rm, maxgap = maxgap)

    } else if (method == "omit") {

        x.all <- setNames(x, seq_along(x))
        y <- na.omit(x.all)[names(na.omit(x.all))]

    }

    idx <- which(is.na(x))

    return(list(y = y, idx = idx))
}
# (x <- c(2,NA,1,4,5,2, NA))
# zoo::na.locf(x)
# zoo::na.spline()
# replace_missing_data(x, "linear", na.rm = TRUE)
