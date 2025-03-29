#' Normalise Signal Range
#'
#' Shift the signal values while preserving the dynamic range (e.g. shift to
#' positive values), or normalise the signal to within a new dynamic range (e.g.
#' normalise range to `[0, 100]`).
#'
#' @param ... Additional arguments.
#'
#' @details
#' ...
#'
#' @return A numeric vector of filtered data.
#'
#' @export
normalise_data <- function(
        x,
        shift_to = 0,
        normalise_range = c(0, 100),
        shift_by = NULL, ## numeric scalar
        position = c("minimum", "first", "maximum"),
        mean_samples, ## c(1, 30, 1)
        ...
) {

    ## shift minimum to zero
    ## shift maximum to zero (not common)
    ## shift starting value (mean X samples?) to zero
    ## TODO shift certain time range mean to zero?


    position <- match.arg(position)
    ## pass through optional arguments
    args <- list(...)


    ## shift range ========================================

    if (is.null(shift_to) & is.null(shift_by)) {

        y <- x

    } else if (position == "minimum") {

        y <- x - ifelse(
            !is.null(shift_by),
            shift_by,
            min(x, na.rm = TRUE)) + shift_to

    } else if (position == "maximum") {

        y <- x - ifelse(
            !is.null(shift_by),
            shift_by,
            max(x, na.rm = TRUE)) + shift_to

    } else if (position == "first") {

        y <- x - ifelse(
            !is.null(shift_by),
            shift_by,
            dplyr::first(x, na_rm = TRUE))

    }

    ## normalise range ==================================

    if (is.null(normalise_range)) {

        z <- y

    } else {

        z <- (y - min(y, na.rm = TRUE)) /
            diff(range(y, na.rm = TRUE)) * diff(normalise_range) +
            min(normalise_range)

    }

    return(z)
}
#
#
# test <- function(A = 0, B) {
#     missing(B)
# }
# test()
# x <- c(-2, -1, 0, -3, 1, 2, NA)
# normalise_data(x, normalise_range = c(0, 1))
