#' Replace Local Outliers
#'
#' Detect local outliers in vector data with a Hampel filter using median
#' absolute deviation (MAD), and replaces with the local median value or `NA`.
#'
#' @param x A numeric vector.
# #' @param span A numeric scalar defining the locally centred window length in
# #'  units of `x` `(span = x ± floor(span/2))`.
#' @param width A numeric scalar for the window length of `(2 × width + 1)` samples.
#' @param t0 A numeric scalar for the outlier threshold, default is 3
#'  (Pearson's rule).
#' @param na.rm A logical indicating whether missing values should be ignored
#'  before the filter is applied (see *Details*).
#' @param return Indicates whether outliers should be replaced with the
#'  local *"median"* value (the *default*), or returned as `NA`.
#'
#' @details
#' The *"median absolute deviation"* computation is done in the `[-width...width]`
#' vicinity of each point at least `width` steps away from the end points of the
#' interval. At the lower and upper end the time series values are preserved.
#'
#' A high threshold makes the filter more forgiving, a low one will declare
#' more points to be outliers. `t0 = 3` (the *default*) corresponds to Pearson's
#' 3 sigma edit rule, `t0 = 0` to Tukey's median filter.
#'
#' `NA` values in the numeric vector will cause an error unless `na.rm = TRUE`.
#' Then `NA` values are skipped for outlier detection, and preserved in the
#' returned vector.
#'
#' The default `return = "median"` will replace outliers with the local
#' median value, as in [pracma::hampel()]. Otherwise, outliers will be
#' returned as `NA` (both returns are equally computationally expensive).
#'
#' @seealso [pracma::hampel()]
#'
#' @examples
#' set.seed(8421)
#' x <- numeric(1024)
#' z <- rnorm(1024)
#' x[1] <- z[1]
#' for (i in 2:1024) {
#'     x[i] <- 0.4*x[i-1] + 0.8*x[i-1]*z[i-1] + z[i]
#' }
#' x[150:200] <- NA ## generate NA values
#' # replace_outliers(x, width = 20, na.rm = FALSE) ## returns error
#' y <- replace_outliers(x, width = 20, na.rm = TRUE)
#' ind <- which(x != y) ## identify outlier indices
#' outliers <- x[ind] ## identify outlier values
#'
#' \dontrun{
#' plot(1:1024, x, type = "l")
#' points(ind, outliers, pch = 21, col = "darkred")
#' lines(y, col = "blue")
#' }
#'
#' @return A numeric vector of filtered data.
#'
#' @export
replace_outliers <- function(
        x,
        width,
        t0 = 3,
        na.rm = FALSE,
        return = c("median", "NA")
) {
    return <- match.arg(return)

    ## validation: `x` must be a numeric vector
    if (!is.numeric(x)) {
        cli::cli_abort("{.arg x} must be a {cli::col_blue('numeric')} vector.")
    }
    ## validation: `width` must be a numeric scalar
    if (!is.numeric(width) || length(width) > 1) {
        cli::cli_abort("{.arg width} must be a {cli::col_blue('numeric')} scalar.")
    }
    ## validation: `width` must be shorter than x
    if (width >= ceiling(length(x)/2)) {
        cli::cli_abort("{.arg width} must be half the length of {.arg x}.")
    }
    ## validation: `t0` must be a numeric scalar
    if (!is.numeric(t0) || length(t0) > 1) {
        cli::cli_abort("{.arg t0} must be a {cli::col_blue('numeric')} scalar.")
    }

    na_idx <- is.na(x)
    x_clean <- if (na.rm) {x[!na_idx]} else {x} ## na.omitted vector
    y_clean <- x_clean ## eventual na.omitted vector with outliers corrected
    n <- length(x_clean)
    L <- 1.4826
    for (i in seq_len(n)) {
        # Calculate the window bounds, ensuring they stay within vector limits
        start_idx <- max(1, i - width)
        end_idx <- min(n, i + width)
        x0 <- median(x_clean[start_idx:end_idx])
        S0 <- L * median(abs(x_clean[start_idx:end_idx] - x0))
        if (abs(x_clean[i] - x0) > t0 * S0) {
            y_clean[i] <- if (return == "median") {x0} else {NA_real_}
        }
    }

    ## return y_clean to the original x with NAs
    x[!na_idx] <- y_clean
    return(x)
}


