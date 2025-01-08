#' Hampel Filter
#'
#' Median absolute deviation (MAD) outlier in time series which skips `NA`
#' (modified from [pracma::hampel()])
#'
#' @param x Numeric vector representing a time series.
#' @param k Window length `2*k+1` in indices.
#' @param t0 Threshold, default is 3 (Pearson's rule), see below.
#'
#' @details
#' The *"median absolute deviation"* computation is done in the `[-k...k]`
#' vicinity of each point at least `k` steps away from the end points of the
#' interval. At the lower and upper end the time series values are preserved.
#'
#' A high threshold makes the filter more forgiving, a low one will declare
#' more points to be outliers. `t0 <- 3` (the default) corresponds to Ron
#' Pearson's 3 sigma edit rule, `t0 <- 0` to John Tukey's median filter.
#'
#' This modified Hampel filter records the index of `NA` values, omits `NA`,
#' applies the filter, then re-inserts those `NA` back to their original
#' indices, preserving the original input data indices and replacing outliers
#' as intended.
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
#' x[c(3, 8)] <- NA
#' omad <- hampel(x, k=20)
#'
#' ## Not run:
#' plot(1:1024, x, type="l")
#' points(omad$ind, x[omad$ind], pch=21, col="darkred")
#' grid()
#' ## End(Not run)
#'
#' @return A list `L` with `L$y` the corrected time series and
#' `L$ind` the indices of outliers in the *"median absolute deviation"* sense.
#'
#' @export
hampel <- function(x, k, t0 = 3) {

    x.all <- setNames(x, seq_along(x))
    x <- na.omit(x.all)

    n <- length(x)
    y <- x
    ind <- c()
    L <- 1.4826
    for (i in (k + 1):(n - k)) {
        x0 <- median(x[(i - k):(i + k)])
        S0 <- L * median(abs(x[(i - k):(i + k)] - x0))
        if (abs(x[i] - x0) > t0 * S0) {
            y[i] <- x0
            ind <- c(ind, i)
        }
    }

    y.ind <- as.numeric(names(y[ind]))
    x.all[y.ind] <- y[ind]

    list(y = x.all, ind = y.ind)
}
