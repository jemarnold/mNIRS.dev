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
#'  (`TRUE`) before the filter is applied. Otherwise (`FALSE`, the *default*) will
#'  throw an error (see *Details*).
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
#' Missing values (`NA`) in the numeric vector will cause an error unless
#' `na.rm = TRUE`. Then `NA` values are removed for processing, and restored in
#' the returned vector.
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
        cli_abort("{.arg x} must be a {col_blue('numeric')} vector.")
    }
    ## validation: `width` must be a numeric scalar
    if (!is.numeric(width) || length(width) > 1) {
        cli_abort("{.arg width} must be a {col_blue('numeric')} scalar.")
    }
    ## validation: `width` must be shorter than x
    if (width >= ceiling(length(x)/2)) {
        cli_abort("{.arg width} must be half the length of {.arg x}.")
    }
    ## validation: `t0` must be a numeric scalar
    if (!is.numeric(t0) || length(t0) > 1) {
        cli_abort("{.arg t0} must be a {col_blue('numeric')} scalar.")
    }

    ## logical whether to handle NAs
    handle_na <- na.rm & any(is.na(x))
    if (handle_na) {
        na_info <- preserve_na(x)
        x <- na_info$x_clean
    }

    n <- length(x)
    y <- x
    L <- 1.4826

    ## vectorised window bounds
    start_idx <- pmax(1, seq_len(n) - width)
    end_idx <- pmin(n, seq_len(n) + width)

    ## vectorised median & MAD
    x0 <- vapply(seq_len(n), \(i) {
        median(x[start_idx[i]:end_idx[i]])}, numeric(1))
    S0 <- L * vapply(seq_len(n), \(i) {
        median(abs(x[start_idx[i]:end_idx[i]] - x0[i]))}, numeric(1))

    ## logical outlier positions
    is_outlier <- abs(x - x0) > t0 * S0
    ## fill outliers with median or NA
    y[is_outlier] <- if (return == "median") {x0[is_outlier]} else {NA_real_}
    ## return y to original x length with NAs if handled
    result <- if (handle_na) {restore_na(y, na_info)} else {y}

    return(result)
}






#' Preserve NA Information Within Processing Functions
#'
#' Stores `NA` vector positions and extracts non-`NA` values for later restoration.
#'
#' @param x A numeric vector which contains missing `NA` values.
#'
#' @return A list with components:
#' \describe{
#'   \item{na_idx}{A logical vector indicating `NA` positions.}
#'   \item{x_clean}{A numeric vector with `NA` values removed.}
#'   \item{n_orig}{A numeric scalar of the original input vector length.}
#' }
#'
#' @examples
#' x <- c(1, NA, 3, NA, 5)
#' na_info <- preserve_na(x)
#' # Process na_info$x_clean, then use restore_na()
#'
#' @seealso \code{\link{restore_na}}
#'
#' @keywords internal
#' @export
preserve_na <- function(x) {
    na_info <- list(na_idx = is.na(x),
                    x_clean = x[!is.na(x)],
                    n_orig = length(x))
    return(na_info)
}

#' Restore NA Information Within Processed Results
#'
#' Restores `NA` values to their original vector positions after processing
#' non-`NA` values.
#'
#' @param y A numeric vector of non-`NA` output values within a function.
#' @param na_info A list returned by \code{\link{preserve_na}}.
#'
#' @return A numeric vector of the original input vector length with `NA` values
#'  restored to their original positions.
#'
#' @examples
#' x <- c(1, NA, 3, NA, 5)
#' na_info <- preserve_na(x)
#' processed <- na_info$x_clean * 2  # Process clean values
#' result <- restore_na(processed, na_info)
#' # Returns: c(2, NA, 6, NA, 10)
#'
#' @seealso \code{\link{preserve_na}}
#'
#' @keywords internal
#' @export
restore_na <- function(y, na_info) {
    if (all(!na_info$na_idx)) return(y)
    ## fill original length of NAs
    result <- rep(NA_real_, na_info$n_orig)
    if (all(na_info$na_idx)) return(result)
    ## replace non-NA with processed output values
    result[!na_info$na_idx] <- y
    return(result)
}
