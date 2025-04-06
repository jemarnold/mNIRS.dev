#' Replace Local Outliers
#'
#' Hampel filter with median absolute deviation (MAD) to detect outliers in
#' mNIRS vector data. Modified from [pracma::hampel()] to skip `NA`s.
#'
#' @param x A numeric vector of mNIRS data.
#' @param k A numeric scalar for the window length of `(2 × k + 1)` samples.
#' @param t0 A numeric scalar for the outlier threshold, default is 3
#' (Pearson's rule), see below.
#' @param na.rm A logical indicating whether missing values should be ignored
#' before the filter is applied.
#' @param return Indicates whether outliers should be replaced with the
#' local *"median"* value *(default)*, or returned as `NA`.
#'
#' @details
#' The *"median absolute deviation"* computation is done in the `[-k...k]`
#' vicinity of each point at least `k` steps away from the end points of the
#' interval. At the lower and upper end the time series values are preserved.
#'
#' A high threshold makes the filter more forgiving, a low one will declare
#' more points to be outliers. `t0 = 3` *(default)* corresponds to Ron
#' Pearson's 3 sigma edit rule, `t0 = 0` to John Tukey's median filter.
#'
#' `NA` values in the numeric vector will cause an error unless
#' `na.rm` is set to `TRUE`. Then `NA` values are skipped and preserved in the
#' returned vector.
#'
#' The default `return = "median"` will replace outliers with the local
#' median value, as in [pracma::hampel()]. Otherwise, outliers will be
#' returned as `NA`.
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
#' x[100:200] <- NA
#' omad <- replace_outliers(x, k = 20, na.rm = TRUE)
#'
#' ## Not run:
#' plot(1:1024, x, type="l")
#' points(omad$idx, x[omad$idx], pch=21, col="darkred")
#' grid()
#' ## End(Not run)
#'
#' @return A list `L` with `L$y` the corrected time series and
#' `L$idx` the indices of the values replaced.
#'
#' @export
replace_outliers <- function(
        x,
        k,
        t0 = 3,
        na.rm = FALSE,
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

    ## validation: `t0` must be a numeric scalar
    if (!rlang::is_double(t0) | length(t0) > 1) {
        cli::cli_abort(paste("{.arg t0} must be a {.cls numeric} scalar."))
    }

    x.all <- setNames(x, seq_along(x))
    x <- if (na.rm) {na.omit(x.all)} else {x.all}

    n <- length(x)
    y <- x
    idx <- c()
    L <- 1.4826
    for (i in (k + 1):(n - k)) {
        x0 <- median(x[(i - k):(i + k)])
        S0 <- L * median(abs(x[(i - k):(i + k)] - x0))
        if (abs(x[i] - x0) > t0 * S0) {
            y[i] <- if (return == "median") {x0} else {NA_real_}
            idx <- c(idx, i)
        }
    }

    y.idx <- as.numeric(names(y[idx]))
    x.all[y.idx] <- y[idx]

    return(list(y = x.all, idx = y.idx))
}
