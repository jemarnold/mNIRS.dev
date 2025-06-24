#' Replace Fixed Values
#'
#' Detect specified values such as `c(0, 100)` in vector data and replaces
#' with `NA` or the local median value.
#'
#' @param x A numeric vector
#' @param fixed_values A numeric vector of values to be replaced, e.g.
#' `fixed_values = c(0, 100)`.
#' @param width A numeric scalar for the window length of `(2 · width + 1)` samples.
#' @param return Indicates whether outliers should be replaced with `NA`
#' *(default)* or the local *"median"* value.
#' @param ... Additional arguments (*currently not used*).
#'
#' @details
#' Useful to overwrite known nonsense values, such as `0`, `100`, or `102.3`.
#'
#' TODO: allow for overwriting all values greater or less than known values.
#'
#' @seealso [pracma::hampel()]
#'
#' @examples
#' set.seed(13)
#' (x <- sample.int(10, 20, replace = TRUE))
#' (y <- replace_fixed_values(x, fixed_values = c(1, 10), width = 5))
#'
#' @return The corrected vector `y`.
#'
#' @export
replace_fixed_values <- function(
        x,
        fixed_values, ## numeric vector
        width, ## numeric scalar
        return = c("NA", "median"),
        ...
) {

    return <- match.arg(return)

    ## validation: `width` must be a numeric scalar
    if (!is.numeric(width) | length(width) > 1) {
        cli::cli_abort(paste("{.arg width} must be a {.cls numeric} scalar."))
    }

    ## validation: `width` must be shorter than half length(x)
    if (width >= ceiling(length(x)/2)) {
        cli::cli_abort(paste(
            "{.arg width} must be less than half the length of {.arg x}."))
    }

    ## validation: `fixed_values` must be a numeric vector
    if (!is.numeric(fixed_values)) {
        cli::cli_abort(paste(
            "{.arg fixed_values} must be a {.cls numeric} vector."))
    }

    y <- x
    n <- length(x)
    for (i in 1:n) {
        # Calculate the window bounds, ensuring they stay within vector limits
        start_idx <- max(1, i - width)
        end_idx <- min(n, i + width)
        x0 <- median(x[start_idx:end_idx])
        if (x[i] %in% fixed_values) {
            y[i] <- if (return == "median") {x0} else {NA_real_}
        }
    }

    return(y)
}
