#' Butterworth Filter with Better Edge Detection
#'
#' Apply a Butterworth digital filter to vector data with [signal::butter()] and
#' [signal::filtfilt()] which handles 'edges' better at the start and end of
#' the data.
#'
#' @param x A numeric vector.
#' @param n An integer scalar defining the filter order number.
#' @param W A numeric scalar or two-element vector defining the fractional
#'  critical frequency of the filter (see *Details*).
#' @param type Digital filter type (see *Details*).
#'  \describe{
#'      \item{`"low"`}{For a *low-pass* filter (*default*).}
#'      \item{`"high"`}{For a *high-pass* filter.}
#'      \item{`"stop"`}{For a *stop-band* (band-reject) filter.}
#'      \item{`"pass"`}{For a *pass-band* filter.}
#'  }
#' @param edges Indicates how to pad `x`.
#'  \describe{
#'      \item{`"rev"`}{Will pad `x` with the preceding 10%
#'      data in reverse sequence (*default*).}
#'      \item{`"rep1"`}{Will pad `x` with the last preceding value.}
#'      \item{`"none"`}{Will return the default [signal::filtfilt()] output.}
#'  }
#'
#' @details
#' Applies a centred (two-pass symmetrical) Butterworth digital filter from
#' [signal::butter()] and [signal::filtfilt()].
#'
#' The filter order is defined by `n`, an integer scalar typically in the
#' range `n = [1, 10]`.
#'
#' The critical (cutoff) frequency is defined by `W`, a numeric scalar for
#' *low-pass* and *high-pass* filters, or a two-element vector `c(low, high)`
#' defining the lower and upper bands for *stop-band* and *pass-band* filters.
#'
#' *Low-pass* and *high-pass* filters allow only frequencies *lower* or *higher*
#' than the critical frequency `W` to be passed through as the output signal,
#' respectively. *Stop-band* defines a critical range of frequencies which are
#' rejected from the output signal. *Pass-band* defines a critical range of
#' frequencies which are passed through as the output signal.
#'
#' @examples
#' set.seed(13)
#' sin <- sin(2 * pi * 1:150 / 50) * 20 + 40
#' noise <- rnorm(150, mean = 0, sd = 6)
#' noisy_sin <- sin + noise
#' filt_without_edge <- filtfilt_edges(x = noisy_sin, n = 2, W = 0.1, edges = "none")
#' filt_with_edge <- filtfilt_edges(x = noisy_sin, n = 2, W = 0.1, edges = "rep1")
#' plot(noisy_sin, type = "l")
#' lines(filt_without_edge, col = "red", lwd = 4)
#' lines(filt_with_edge, col = "blue", lwd = 4)
#'
#' @return A numeric vector of the same length as `x`.
#'
#' @export
filtfilt_edges <- function (
        x,
        n = 1,
        W,
        type = c("low", "high", "stop", "pass"),
        edges = c("rev", "rep1", "none")
) {
    # if (!requireNamespace("signal", quietly = TRUE)) {
    #     cli::cli_abort(paste(
    #         "Package {.pkg signal} is required for low-pass filtering.",
    #         "Please install it."))
    # }
    rlang::check_installed("signal", reason = "to use Butterworth digital filter")

    type = match.arg(type)
    edges = match.arg(edges)

    ## argument validation
    if (!is.numeric(x)) {
        cli::cli_abort("{.arg x} must be a numeric vector.")
    }
    if (!rlang::is_integerish(n) | n == 0) {
        cli::cli_abort("{.arg n} must be an integer scalar of 1 or greater.")
    }
    if (!is.numeric(W) | W == 0 | W == 1) {
        cli::cli_abort(paste(
            "{.arg W} must be a numeric scalar or two-element vector",
            "`c(low, high)` between 0 and 1."))
    }

    switch(edges,
           ## pads x with the first and last 10% of the vector length
           "rev" = pad_edges <- c(
               rev(head(x, length(x)/10)),
               x,
               rev(tail(x, length(x)/10))),
           ## pads x with repeating first / last value
           "rep1" = pad_edges <- c(
               rep(head(x, 1), length(x)/10),
               x,
               rep(tail(x, 1), length(x)/10)),
           "none" = pad_edges <- x
    )

    ## butterworth filter order (n) and relative cutoff frequency (W)
    x_filt <- signal::filtfilt(
        filt = signal::butter(n = n, W = W, type = type),
        x = pad_edges)

    ## returns the original vector length of x with padding omitted
    switch(edges,
           "rev" = x_filt[(length(x)/10 + 1):(length(x)/10 + length(x))],
           "rep1" = x_filt[(length(x)/10 + 1):(length(x)/10 + length(x))],
           "none" = x_filt)
}
