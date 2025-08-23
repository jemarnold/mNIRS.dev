#' Calculate the Slope of a Numeric Vector
#'
#' Computes the linear regression slope for a numeric response variable `y`.
#'
#' @param y A numeric vector of the response variable.
#' @param x A numeric vector of the predictor variable, defaults to using the
#'  index of `x = seq_along(y)`.
#'
#' @details Uses the least squares formula on complete case analysis. A single
#'  valid observation will return `NA`.
#'
#' @return A numeric slope value of `y/x`.
#'
#' @examples
#' \dontrun{
#' y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
#' slope(y)
#'
#' y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, NA)
#' slope(y)
#' }
#'
## TODO 2025-08-20 do I want this internal? #' @keywords internal
#' @export
slope <- function(y, x = seq_along(y)) {
    complete_cases <- !is.na(y) & !is.na(x)
    if (sum(complete_cases) < 2) return(NA_real_)

    x <- round(x, 10); y <- round(y, 10) ## avoid floating point precision issues

    x_valid <- x[!is.na(y)]
    y_valid <- y[!is.na(y)]
    n <- length(y_valid)

    ## covariance between x & y (+ve when they move in same direction)
    numerator <- n * sum(x_valid * y_valid) - sum(x_valid) * sum(y_valid)
    ## variance of x scaled by n (spread of x)
    denominator <- n * sum(x_valid^2) - sum(x_valid)^2

    if (denominator == 0) {return(0)} else {return(numerator / denominator)}
}

# #' @param na.rm A logical indicating how to handle missing data: `FALSE` (the
# #'  *default*) allows for only complete case analysis and will return `NA` if any
# #'  of `y` are missing. `TRUE` will permit calculation across interior missing
# #'  values but will return `NA` if the first or last values of `y` are missing,
# #'  to avoid extrapolation.
# slope <- function(y, x = seq_along(y), na.rm = FALSE) {
#     ## handle NA: either complete case or soft avoid extrapolation for NA at boundaries
#     if (!na.rm & any(is.na(y))) {
#         return(NA_real_)
#     } else if (na.rm & any(is.na(y[c(1, length(y))]))) {
#         return(NA_real_)
#     }
#
#     x <- round(x, 10); y <- round(y, 10) ## avoid floating point precision issues
#
#     x_valid <- x[!is.na(y)]
#     y_valid <- y[!is.na(y)]
#     n <- length(y_valid)
#
#     ## covariance between x & y (+ve when they move in same direction)
#     numerator <- n * sum(x_valid * y_valid) - sum(x_valid) * sum(y_valid)
#     ## variance of x scaled by n (spread of x)
#     denominator <- n * sum(x_valid^2) - sum(x_valid)^2
#     slope <- if (denominator == 0) {0} else {numerator / denominator}
#
#     return(round(slope, 10))
# }



# #' If all samples within a window are invalid, will return `NA`. If only one
# #'  sample within a window is valid, will return `0`.
# #' ## where only one valid value within `width`, will return `0`
# #' ## where no valid values within `width`, will return `NA`
# #' y_na <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
# #' rolling_slope(y_na, width = 3)




#' Calculate Rolling Slope
#'
#' Computes rolling first derivative (linear regression slope) for a numeric
#' response variable `y` within a moving window.
#'
#' @param y A numeric vector of the response variable.
#' @param x A numeric vector of the predictor variable, defaults to using the
#'  index of `x = seq_along(y)`.
#' @param width A numeric scalar defining the window width in units of `x`
#'  for rolling calculations.
#' @param align Specifies the window alignment of `width` as *"center"*
#'  (the *default*), *"left"*, or *"right"*. Where *"left"* is *forward looking*,
#'  and *"right"* is *backward looking* by the window `width` from the current
#'  sample.
#' @param na.rm A logical indicating how missing data will be handled. `FALSE`
#'  (the *default*) will perform complete case analysis and return the rolling
#'  slopes where all local `y` samples are valid. `TRUE` will return the rolling
#'  slopes where the local target `y` sample is valid.
#'
#' @details Uses the least squares formula on complete case analysis to calculate
#'  local slopes within a rolling window along `x` specified by `width` in units
#'  of `x`. i.e. `x = 10` would include samples within a 10-second window for
#'  time series data.
#'
#' @seealso [zoo::rollapply()]
#'
#' @return A numeric vector of rolling local slopes in units of `y/x` of the
#'  same length as `y`.
#'
#' @examples
#' y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
#' rolling_slope(y, width = 3)
#' rolling_slope(y, width = 3, align = "left")
#'
#' y_na <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
#' rolling_slope(y_na, width = 3)
#'
#' @export
rolling_slope <- function(
        y,
        x = seq_along(y),
        width,
        align = c("center", "left", "right"),
        na.rm = FALSE
) {
    ## check for sufficient non-NA observations
    if (sum(!is.na(y)) < 1) {return(NA_real_)}

    align <- match.arg(align)
    x <- round(x, 10); y <- round(y, 10) ## avoid floating point precision issues
    n <- length(y)

    ## find indices for x values between width (in units of x) based on align
    if (align == "center") {
        start_x <- pmax(x[1], x - width/2)
        end_x <- pmin(x[n], x + width/2)
    } else if (align == "left") {
        ## align left is FORWARD looking
        ## current observation is at leftmost position of window
        ## window starts at current x value, extends width units forward
        start_x <- x
        end_x <- pmin(x[n], x + width)
    } else if (align == "right") {
        ## align right is BACKWARD looking
        ## current observation is at rightmost position of window
        ## window ends at current x value, extends width units backward
        start_x <- pmax(x[1], x - width)
        end_x <- x
    }

    # Vectorised window detection and slope calculation
    slopes <- sapply(1:n, \(.x) {
        ## find indices for x within width
        window_idx <- which(x >= start_x[.x] & x <= end_x[.x])

        ## handle local missing data
        if ((!na.rm & any(is.na(y[window_idx]))) || (na.rm & is.na(y[.x]))) {
            NA_real_
        } else if (length(window_idx) < 2) {
            0
        } else {
            ## calculate slope within window
            slope(y[window_idx], x[window_idx])
        }
    })

    return(slopes)
}

# y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
# x <- seq_along(y)
# width <- 3
# align = c("center")

# `NA` if any of local `y` values are missing.



#' Calculate Peak Positive or Negative Slope
#'
#' Computes the peak directional linear regression slope for upward or downward
#' trending response variable `y`, respectively using [rolling_slope()] within
#' a moving window.
#'
#' @param y A numeric vector of the response variable.
#' @param x A numeric vector of the predictor variable, defaults to using the
#'  index of `x = seq_along(y)`.
#' @param width A numeric scalar defining the window width in units of `x`
#'  for rolling calculations.
#' @param align Specifies the window alignment of `width` as *"center"*
#'  (the *default*), *"left"*, or *"right"*. Where *"left"* is *forward looking*,
#'  and *"right"* is *backward looking* by the window `width` from the current
#'  sample.
#' @param na.rm A logical indicating how missing data will be handled. `FALSE`
#'  (the *default*) will perform rolling slopes with complete case analysis and
#'  return the peak of rolling slopes where all local `y` samples are valid.
#'  `TRUE` will return the peak of rolling slopes where the local target `y`
#'  sample is valid.
#'
#' @details Uses the least squares formula on complete case analysis to calculate
#'  local slopes within a rolling window along `x` specified by `width` in units
#'  of `x`. i.e. `x = 10` would include samples within a 10-second window for
#'  time series data.
#'
#' The direction of `y` (upward or downward) will determine whether a positive
#'  or negative peak slope is returned. Peak positive slope will be returned for
#'  an overall slope >= 0.
#'
#' @return A list of class `mNIRS.kinetics` with components `L$...`:
#'      \item{`x`}{The `x` position of the local peak slope.}
#'      \item{`y`}{The `y` value predicted from linear regression at the local
#'          peak slope.}
#'      \item{`slope`}{The slope value in units of `y/x`.}
#'      \item{`x_fitted`}{The range of `x` values given by `width` corresponding
#'          to the local peak slope.}
#'      \item{`y_fitted`}{The range of `y` values given by `width` predicted
#'          from linear regression, corresponding to the local peak slope.}
#'
#' @examples
#' y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
#' peak_slope(y, width = 3)
#'
#' y <- c(14, 11, 12, 9, 7, 8, 5, 2, 3, 1)
#' peak_slope(y, width = 3)
#'
#' @export
peak_slope <- function(
        y,
        x = seq_along(y),
        width,
        align = c("center", "left", "right"),
        na.rm = FALSE
) {
    ## check for sufficient non-NA observations
    if (sum(!is.na(y)) < 2) {
        cli_abort("{.arg y} contains insufficient valid data.")
    }

    align <- match.arg(align)
    x <- round(x, 10); y <- round(y, 10) ## avoid floating point precision issues
    n <- length(y)

    ## return local rolling slopes
    slopes <- rolling_slope(y, x, width, align, na.rm)
    valid_slopes <- ifelse(is.na(y), NA_real_, slopes)

    ## return peak slope sample based on global trend direction
    if (slope(y, x) >= 0) {
        peak_idx <- which.max(valid_slopes) ## positive
    } else {
        peak_idx <- which.min(valid_slopes) ## negative
    }

    ## peak slope value
    peak_slope <- valid_slopes[peak_idx]
    ## x value at peak slope
    current_x <- x[peak_idx]
    current_y <- y[peak_idx]

    ## find indices for x values between width (in units of x) based on align
    if (align == "center") {
        start_x <- max(x[1], current_x - width / 2)
        end_x <- min(x[n], current_x + width / 2)
    } else if (align == "left") {
        ## align left is FORWARD looking
        ## current observation is at leftmost position of window
        ## window starts at current x value, extends width units forward
        start_x <- current_x
        end_x <- min(x[n], current_x + width)
    } else if (align == "right") {
        ## align right is BACKWARD looking
        ## current observation is at rightmost position of window
        ## window ends at current x value, extends width units backward
        start_x <- max(x[1], current_x - width)
        end_x <- current_x
    }

    ## find indices for x within width
    window_idx <- which(x >= start_x & x <= end_x)
    ## extract data within window
    x_window <- x[window_idx]
    y_window <- y[window_idx]

    ## calculate fitted values using peak slope
    if (length(x_window) >= 2) {
        x_mean <- mean(x_window, na.rm = TRUE)
        y_mean <- ifelse(!is.na(mean(y_window)), mean(y_window), current_y)
        fitted <- y_mean + peak_slope * (x_window - x_mean)
        fitted_y <- y_window[which(x_window %in% current_x)]
    } else {fitted <- NA_real_}

    return(list(x = current_x,
                y = fitted_y,
                slope = peak_slope,
                x_fitted = x_window,
                y_fitted = fitted))
}

