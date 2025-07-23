#' Determine the Slope of a Numeric Vector
#'
#' Computes the rate of change for a numeric response variable `y`.
#'
#' @param y A numeric vector of response values.
#' @param x A numeric vector of predictor values. If `NULL`, uses `seq_along(y)`.
#' @param na.rm A logical indicating whether to exclude NA values from rolling slope
#'  calculations.
#'
#' @return A numeric slope value.
#'
#' @keywords internal
slope <- function(
        y,
        x = NULL,
        na.rm = FALSE
) {
    ## where `x` is not defined
    if (is.null(x)) {x <- seq_along(y)}

    ## handle `NA`
    ## `na.rm = FALSE` will return peak value of the remaining non-`NA` slopes
    ## `na.rm = TRUE` will return peak value of `NA`-excluded slopes
    if (na.rm) {
        valid_idx <- !is.na(x) & !is.na(y)
        x <- x[valid_idx]
        y <- y[valid_idx]
    }

    ## check for sufficient non-NA observations
    if (length(x) < 2) {
        overall_slope <- NA
        return(overall_slope)
    }

    ## determine overall trend using direct least squares calculation
    x_mean <- mean(x)
    y_mean <- mean(y)

    ## covariance between x & y (+ve when they move in same direction)
    numerator <- sum((x - x_mean) * (y - y_mean), na.rm = na.rm)
    ## variance of x (spread of x around mean of x)
    denominator <- sum((x - x_mean)^2, na.rm = na.rm)
    ## best-fit line gradient faster than calling `lm()`
    overall_slope <- if (denominator == 0) {0} else {numerator / denominator}

    return(overall_slope)
}



#' Calculate Rolling Slope
#'
#' Computes rolling first derivative (slope) of `y` over `x` using least squares
#' regression within a moving window.
#'
#' @param y A numeric vector of response values.
#' @param x A numeric vector of predictor values. If `NULL`, uses `seq_along(y)`.
#' @param width An integer scalar defining the window width (in units of `x`)
#'  for rolling calculations.
#' @param align Specifies the window alignment of `width` as *"center"*
#'  (the default), *"left"*, or *"right"*. Where *"left"* is *forward looking*, and
#'  *"right"* is *backward looking* by the window `width` from the current
#'  observation.
#' @param na.rm A logical indicating whether to exclude NA values from rolling slope
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
#' rolling_slope(y, width = 3)
#'
#' ## with custom x values and left alignment
#' x <- seq(0, 1, length.out = 10)
#' rolling_slope(y, x, width = 3, align = "left")
#'
#' ## handling missing values with `na.rm = TRUE`
#' y_na <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, 14)
#' rolling_slope(y_na, width = 3, na.rm = FALSE)
#' rolling_slope(y_na, width = 3, na.rm = TRUE)
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

    ## validation length of y
    if (length(y) < 2) {
        cli::cli_abort("{.arg y} should be of length 2 or greater.")
    }

    ## validation y must contain >= 2 non-NA values
    if (length(na.omit(y)) < 2) {
        cli::cli_abort(paste(
            "{.arg y} should contain at least 2 or more non-NA values."))
    }

    ## validation length of width
    if (width < 2) {
        cli::cli_abort("{.arg width} should be equal to 2 or greater.")
    }

    for (i in 1:n) {
        ## find indices for x values between width (in units of x) based on align
        if (align == "center") {
            start_idx <- head(which(x >= (x[i] - width/2)), 1)
            end_idx <- tail(which(x <= (x[i] + width/2)), 1)
        } else if (align == "left") {
            ## align left is FORWARD looking
            ## current observation is at leftmost position of window
            ## window starts at current x value, extends width units forward
            start_idx <- i
            end_idx <- min(1, i + width - 1)
        } else if (align == "right") {
            ## align right is BACKWARD looking
            ## current observation is at rightmost position of window
            ## window ends at current x value, extends width units backward
            start_idx <- max(1, i - width + 1)
            end_idx <- i
        }

        ## extract data within window
        x_window <- x[start_idx:end_idx]
        y_window <- y[start_idx:end_idx]

        slopes[i] <- slope(y = y_window, x = x_window, na.rm)
    }

    return(slopes)
}







#' Calculate Peak Positive or Negative Slope
#'
#' Computes the peak positive or negative slope for upward or downward trending
#' data, respectively. Using [rolling slope][rolling_slope()] analysis.
#'
#' @param y A numeric vector of response values.
#' @param x A numeric vector of predictor values. If `NULL`, uses `seq_along(y)`.
#' @param width An integer scalar defining the window width (in units of `x`)
#'  for rolling calculations.
#' @param align Specifies the window alignment of `width` as *"center"*
#'  (the default), *"left"*, or *"right"*.
#' @param na.rm A logical indicating whether to exclude NA values from rolling slope
#'  calculations.
#'
#' @details
#' The function first calculates rolling slopes using [rolling_slope()], then
#'  determines overall trend direction using least squares regression. For upward
#'  trends (positive overall slope), returns the greatest positive rolling slope.
#'  For downward trends (negative overall slope), returns the greatest negative
#'  rolling slope.
#'
#' @return A list containing `L$y`, a numeric scalar of the peak slope value,
#'  and `L$idx`, the position index of the peak slope value.
#'
#' @examples
#' # Upward trending data returns peak positive slope
#' y_up <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
#' peak_directional_slope(y_up, width = 3)
#'
#' # Downward trending data returns peak negative slope
#' y_down <- c(14, 11, 12, 9, 7, 8, 5, 2, 3, 1)
#' peak_directional_slope(y_down, width = 3)
#'
#' @export
peak_directional_slope <- function(
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

    ## return overall slope
    ## na.rm hard-coded TRUE to always return overall_slope for direction test
    overall_slope <- slope(y, x, na.rm = TRUE)

    ## return local rolling slopes
    slopes <- rolling_slope(y, x, width, align, na.rm)

    ## return peak slope sample based on trend direction
    if (overall_slope >= 0) {
        peak_idx <- which.max(slopes)
    } else {
        peak_idx <- which.min(slopes)
    }

    ## return peak slope from peak slope sample
    peak_slope <- slopes[peak_idx]

    ## return window around peak slope sample
    ## find indices for x values between width (in units of x) based on align
    if (align == "center") {
        start_idx <- head(which(x >= (x[peak_idx] - width/2)), 1)
        end_idx <- tail(which(x <= (x[peak_idx] + width/2)), 1)
    } else if (align == "left") {
        ## align left is FORWARD looking
        ## current observation is at leftmost position of window
        ## window starts at current x value, extends width units forward
        start_idx <- peak_idx
        end_idx <- min(1, peak_idx + width - 1)
    } else if (align == "right") {
        ## align right is BACKWARD looking
        ## current observation is at rightmost position of window
        ## window ends at current x value, extends width units backward
        start_idx <- max(1, peak_idx - width + 1)
        end_idx <- peak_idx
    }

    ## extract peak window data
    x_window <- x[start_idx:end_idx]
    y_window <- y[start_idx:end_idx]

    ## calculate fitted values using peak slope
    if (length(x_window) >= 2) {
        x_window_mean <- mean(x_window, na.rm = TRUE)
        y_window_mean <- mean(y_window, na.rm = TRUE)
        fitted <- y_window_mean + peak_slope * (x_window - x_window_mean)
    } else {
        fitted <- NA
    }

    return(list(slope = peak_slope, x = x[peak_idx],
                y_fitted = fitted, x_fitted = x_window))
}
