#' Replace Fixed Values
#'
#' Detect specified values such as `c(0, 100)` within mNIRS vector data and
#' replaces with the local median value.
#'
#' @param x A numeric vector of mNIRS data.
#' @param fixed_values A numeric vector of values to be replaced,
#' e.g. `fixed_values = c(0, 100)`.
#' @param k A numeric scalar for the window length of `(2 · k + 1)` samples.
#' @param return Indicates whether outliers should be replaced with the
#' local *"median"* value *(default)*, or returned as `NA`.
#' @param ... Additional arguments.
#'
#' @details
#' ...
#'
#' @return A list `L` with `L$y` the corrected time series and
#' `L$idx` the indices of the values replaced.
#'
#' @export
replace_fixed_values <- function(
        x,
        fixed_values, ## numeric vector
        k = 20, ## numeric scalar
        return = c("median", "NA")
) {

    return <- match.arg(return)

    ## validation: `k` must be a numeric scalar
    if (!rlang::is_double(k) | length(k) > 1) {
        cli::cli_abort(paste("{.arg k} must be a {.cls numeric} scalar."))
    }

    ## validation: `k` must be shorter than x
    if (k >= ceiling(length(x)/2)) {
        cli::cli_abort(paste("{.arg k} must be 1/2 the length of {.arg x}."))
    }

    ## validation: `fixed_values` must be a numeric vector
    if (!rlang::is_double(fixed_values)) {
        cli::cli_abort(paste(
            "{.arg fixed_values} must be a {.cls numeric} vector."))
    }

    n <- length(x)
    y <- x
    idx <- c()
    for (i in (k + 1):(n - k)) {
        x0 <- median(x[(i - k):(i + k)])
        if (x[i] %in% fixed_values) {
            y[i] <- if (return == "median") {x0} else {NA_real_}
            idx <- c(idx, i)
        }
    }

    return(list(y = y, idx = idx))
}
#
# set.seed(11)
# (x <- sample.int(20, 40, replace = TRUE))
# (y <- replace_fixed_values(x, fixed_values=c(8), k = 10)$y)
# y == x
