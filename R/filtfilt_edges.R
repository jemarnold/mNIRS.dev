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
#'
#' \dontrun{
#' plot(noisy_sin, type = "l")
#' lines(filt_without_edge, col = "red", lwd = 4)
#' lines(filt_with_edge, col = "blue", lwd = 4)
#' }
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
    rlang::check_installed("signal", reason = "to use Butterworth digital filter")

    type = match.arg(type)
    edges = match.arg(edges)

    ## validation: `x` must be a numeric vector
    if (!is.numeric(x)) {
        cli::cli_abort("{.arg x} must be a {cli::col_blue('numeric')} vector.")
    }
    if (!rlang::is_integerish(n) || n == 0) {
        cli::cli_abort(paste(
            "{.arg n} must be an {cli::col_blue('integer')} scalar",
            "of 1 or greater."))
    }
    if (!is.numeric(W) || W == 0 || W == 1) {
        cli::cli_abort(paste(
            "{.arg W} must be a {cli::col_blue('numeric')} scalar or",
            "two-element vector `c(low, high)` between 0 and 1."))
    }

    pad_length <- length(x) %/% 20
    pad_edges <- switch(
        edges,
        "rev" = c(rev(head(x, pad_length)), x, rev(tail(x, pad_length))),
        "rep1" = c(rep(x[1], pad_length), x, rep(x[length(x)], pad_length)),
        "none" = x
    )

    ## butterworth filter order (n) and relative cutoff frequency (W)
    x_filt <- signal::filtfilt(filt = signal::butter(n = n, W = W, type = type),
                               x = pad_edges)

    ## returns the original vector length of x with padding omitted
    if (edges == "none") {
        return(x_filt)
    } else {
        return(x_filt[(pad_length + 1):(pad_length + length(x))])
    }
}
