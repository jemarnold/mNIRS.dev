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
        cli::cli_abort("{.arg width} must be equal to 2 or greater.")
    }

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
        if (
            length(x_window) < 2 || any(is.na(x_window)) || any(is.na(y_window))
        ) {
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




#' Calculate Peak Positive or Negative Slope
#'
#' Computes the peak positive or negative slope for upward or downward trending
#' data, respectively. Using [rolling slope][rolling_slope()] analysis.
#'
#' @param y A numeric vector of response values.
#' @param x A numeric vector of predictor values. If `NULL`, uses `seq_along(y)`.
#' @param width An integer scalar defining the window width for rolling calculations.
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
#' peak_slope(y_up, width = 3)
#'
#' # Downward trending data returns peak negative slope
#' y_down <- c(14, 11, 12, 9, 7, 8, 5, 2, 3, 1)
#' peak_slope(y_down, width = 3)
#'
#' @export
peak_directional_slope <- function(
        y,
        x = NULL,
        width,
        align = c("center", "left", "right"),
        na.rm = FALSE
) {
    ## where `x` is not defined
    if (is.null(x)) {x <- seq_along(y)}

    ## return local rolling slopes
    slopes <- rolling_slope(y, x, width, align, na.rm)

    ## handle `NA`
    ## `na.rm = FALSE` will return peak value of the remaining non-`NA` slopes
    ## `na.rm = TRUE` will return peak value of `NA`-excluded slopes
    if (na.rm) {
        valid_idx <- !is.na(x) & !is.na(y)
        x_clean <- x[valid_idx]
        y_clean <- y[valid_idx]
    } else {
        x_clean <- x
        y_clean <- y
    }

    ## determine overall trend using direct least squares calculation
    x_mean <- mean(x_clean)
    y_mean <- mean(y_clean)

    ## covariance between x & y (+ve when they move in same direction)
    numerator <- sum((x_clean - x_mean) * (y_clean - y_mean), na.rm = TRUE)
    ## variance of x (spread of x around mean of x)
    denominator <- sum((x_clean - x_mean)^2, na.rm = TRUE)
    ## best-fit line gradient faster than calling `lm()`
    overall_slope <- if (denominator == 0) {0} else {numerator / denominator}

    ## return peak slope based on trend direction
    if (overall_slope >= 0) {
        peak_idx <- which.max(slopes)
        peak_value <- slopes[peak_idx]
    } else {
        peak_idx <- which.min(slopes)
        peak_value <- slopes[peak_idx]
    }

    ## TODO I think this is redundant. Needs to be unit tested
    # if (na.rm && is.na(peak_value)) {
    #     valid_slopes <- !is.na(slopes)
    #     if (any(valid_slopes)) {
    #         valid_indices <- which(valid_slopes)
    #         if (overall_slope >= 0) {
    #             max_idx <- which.max(slopes[valid_indices])
    #             peak_idx <- valid_indices[max_idx]
    #         } else {
    #             min_idx <- which.min(slopes[valid_indices])
    #             peak_idx <- valid_indices[min_idx]
    #         }
    #         peak_value <- slopes[peak_idx]
    #     }
    # }

    return(list(value = peak_value, idx = peak_idx))
}
