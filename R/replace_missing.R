#' Replace Missing Values
#'
#' Detect missing values in vector data and replace via methods from the
#' [zoo][zoo::na.approx()] package.
#'
#' @param x A numeric vector.
#' @param method Indicates how to replace missing data.
#'  \describe{
#'      \item{`"linear"`}{Replaces each `NA` with linear interpolation via
#'      [zoo::na.approx()] (the *default*).}
#'      \item{`"locf"`}{Stands for 'last observation carried forward'. Replaces each
#'      `NA` with the most recent non-`NA` value prior to it, or following it for
#'      leading `NA`s.}
#'      \item{`"omit"`}{`<under development>`}
#'  }
#' @param na.rm A logical indicating whether leading and/or trailing `NA`s should
#'  be extrapolated/filled (`TRUE`, the *default*) or ignored (`FALSE`).
#' @param maxgap A numeric scalar for the maximum number of consecutive `NA`s
#'  to fill. Any longer gaps will be left unchanged. *Default* is `Inf`.
#'
#' @details This function will also replace `c(NaN, Inf, -Inf)`.
#'
#' \describe{
#'  \item{`method = "linear"`}{`na.rm = TRUE` will extrapolate over leading and
#'      trailing `NA`s by applying `rule = 2` (see [stats::approx()]).
#'      `na.rm = FALSE` will return leading/trailing `NA`s by applying `rule = 1`.}
#'  \item{`method = "locf"`}{`na.rm = TRUE` will fill NOCB 'next observation
#'      carried backward over leading `NA`s. `na.rm = FALSE` will return leading
#'      `NA`s.}
# #'  \item{`method = "omit"`}{`<under development>.` The returned vector `y` will
# #'      be a named vector with the original indices of each value as names. This
# #'      allows for preserving and re-inserting the omitted `NA`s back into the
# #'      final dataset, if needed.}
#' }
#'
#' @seealso [zoo::na.locf()], [zoo::na.approx()]
#'
#' @examples
#' (x <- c(1, 2, NA, Inf, 5, 6, NA))
#' replace_missing(x, method = "linear", na.rm = FALSE)
#' replace_missing(x, method = "linear", na.rm = TRUE)
#' replace_missing(x, method = "locf")
#'
#' @return A numeric vector of filtered data.
#'
#' @export
replace_missing <- function(
        x,
        method = c("linear", "locf"),
        na.rm = TRUE,
        maxgap = Inf
) {
    method <- match.arg(method)

    ## validation: `x` must be a numeric vector
    if (!is.numeric(x)) {
        cli_abort("{.arg x} must be a {.cls numeric} vector.")
    }

    ## replace NaN & Inf to NA
    x[is.nan(x) | is.infinite(x)] <- NA

    if (method == "linear") {
        rule <- ifelse(na.rm, 2, 1)
        y <- zoo::na.approx(x, na.rm = na.rm, maxgap = maxgap, rule = rule)
    } else if (method == "locf") {
        # ## zoo::na.loct.default https://stackoverflow.com/a/19839474
        # na_locf <- function(x) {
        #     L <- !is.na(x)
        #     c(x[L][1], x[L])[cumsum(L)+1]
        # }
        ## helper functions to add leading locf
        if (na.rm) {
            first_valid <- x[which(!is.na(x))[1]]
            x[seq_len(which(!is.na(x))[1] - 1)] <- first_valid
        }
        y <- zoo::na.locf(x, na.rm = FALSE, maxgap = maxgap) ## na.rm omits leading NAs
    } #else if (method == "omit") {
    #     x.all <- setNames(x, seq_along(x))
    #     y <- x.all[!is.na(x.all)]
    # }

    return(y)
}
