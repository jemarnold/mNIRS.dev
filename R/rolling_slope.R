#' Calculate Rolling Slope
#'
#' Computes rolling first derivative (slope) of `y` over `x` using least squares
#' regression within a moving window.
#'
#' @param y A numeric vector of response values.
#' @param x A numeric vector of predictor values. If `NULL`, uses `seq_along(y)`.
#' @param width An integer scalar defining the window width for rolling calculations.
#' @param align Specifies the window alignment of `width` as *"center"*
#'  (the default), *"left"*, or *"right"*.
#' @param na.rm A logical indicating whether to exclude NA values from slope
#'  calculations.
#'
#' @details
#' The function uses the least squares formula:
#' `slope = sum((x - x_mean) * (y - y_mean)) / sum((x - x_mean)^2)`.
#'
#' When `na.rm = TRUE`, `NA` values are excluded from calculations. If fewer than 2
#' valid points remain in a window, `NA` will be returned for that position.
#'
#' @seealso [zoo::rollapply()]
#'
#' @return A numeric vector of rolling local slopes of the same length as `y`.
#'
#' @examples
#' ## basic usage with default centred alignment
#' y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
#' (slopes <- rolling_slope(y, width = 3))
#'
#' ## with custom x values and left alignment
#' x <- seq(0, 1, length.out = 10)
#' (slopes <- rolling_slope(y, x, width = 3, align = "left"))
#'
#' ## handling missing values with `na.rm = TRUE`
#' y_na <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, 14)
#' (slopes <- rolling_slope(y_na, width = 3, na.rm = FALSE))
#' (slopes <- rolling_slope(y_na, width = 3, na.rm = TRUE))
#'
#' @export
rolling_slope <- function(
        y,
        x = NULL,
        width,
        align = c("center", "left", "right"),
        na.rm = FALSE
) {
    align <- match.arg(align)

    ## where `x` is not defined
    if (is.null(x)) {x <- seq_along(y)}

    n <- length(y)
    slopes <- numeric(n)

    for (i in 1:n) {
        ## calculate window boundaries based on `align`
        if (align == "center") {
            start_idx <- max(1, i - floor((width - 1) / 2))
            end_idx <- min(n, i + floor(width / 2))
        } else if (align == "left") {
            start_idx <- i
            end_idx <- min(n, i + width - 1)
        } else if (align == "right") {
            start_idx <- max(1, i - width + 1)
            end_idx <- i
        }

        ## extract data within window
        x_window <- x[start_idx:end_idx]
        y_window <- y[start_idx:end_idx]

        ## handle `NA`
        if (na.rm) {
            valid_idx <- !is.na(x_window) & !is.na(y_window)
            x_window <- x_window[valid_idx]
            y_window <- y_window[valid_idx]
        }

        ## check for sufficient non-NA observations
        if (length(x_window) < 2 || any(is.na(x_window)) || any(is.na(y_window))) {
            slopes[i] <- NA
            next
        }

        ## calculate slopes using least squares
        x_mean <- mean(x_window)
        y_mean <- mean(y_window)

        ## covariance between x & y (+ve when they move in same direction)
        numerator <- sum((x_window - x_mean) * (y_window - y_mean))
        ## variance of x (spread of x around mean of x)
        denominator <- sum((x_window - x_mean)^2)
        ## best-fit line gradient faster than calling `lm()`
        slopes[i] <- if (denominator == 0) {0} else {numerator / denominator}
    }

    return(slopes)
}
