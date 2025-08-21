#' Replace Invalid Values
#'
#' Detect specific values such as `c(0, 100, 102.3)` in vector data and replaces
#' with `NA` or the local median value.
#'
#' @param x A numeric vector
#' @param values A numeric vector of invalid values to be replaced, e.g.
#'  `values = c(0, 100, 102.3)`.
#' @param width A numeric scalar for the window length of `(2 · width + 1)` samples.
#' @param return Indicates whether invalid values should be replaced with `NA`
#'  (the *default*) or the local *"median"* value.
#'
#' @details Useful to overwrite known invalid/nonsense values, such as `0`,
#'  `100`, or `102.3`.
#'
#' *TODO: allow for overwriting all values greater or less than known values.*
#'
#' @seealso [pracma::hampel()]
#'
#' @examples
#' set.seed(13)
#' (x <- sample.int(10, 20, replace = TRUE))
#' (y <- replace_invalid(x, values = c(1, 10), width = 5))
#'
#' @return A numeric vector of filtered data.
#'
#' @export
replace_invalid <- function(
        x,
        values,
        width,
        return = c("NA", "median")
) {
    return <- match.arg(return)
    return <- return == "median" ## into logical

    ## validation: `x` must be a numeric vector
    if (!is.numeric(x)) {
        cli::cli_abort("{.arg x} must be a {cli::col_blue('numeric')} vector.")
    }
    ## validation: `values` must be a numeric vector
    if (!is.numeric(values)) {
        cli::cli_abort("{.arg values} must be a {cli::col_blue('numeric')} vector.")
    }
    ## validation: `width` must be a numeric scalar
    if (return && (!is.numeric(width) || length(width) > 1)) {
        cli::cli_abort("{.arg width} must be a {cli::col_blue('numeric')} scalar.")
    }
    ## validation: `width` must be shorter than x
    if (return && width >= ceiling(length(x)/2)) {
        cli::cli_abort("{.arg width} must be half the length of {.arg x}.")
    }

    n <- length(x)
    y <- x

    if (return) {
        ## vectorised window bounds
        start_idx <- pmax(1, seq_len(n) - width)
        end_idx <- pmin(n, seq_len(n) + width)
        ## vectorised median & MAD
        x0 <- vapply(seq_len(n), \(i) {
            median(x[start_idx[i]:end_idx[i]], na.rm = TRUE)}, numeric(1))
    }

    ## fill invalid with median or NA
    y[y %in% values] <- if (return) {x0[y %in% values]} else {NA_real_}

    return(y)
}
