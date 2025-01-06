#' Apply a Butterworth filter with better edge detection
#'
#' Custom Butterworth filtering function from [signal::butter()] and
#' [signal::filtfilt()] which handles data 'edges' better at the start and
#' end of data.
#'
#' @param x A numeric vector.
#' @param n An integer scalar specifying the filter order number. Passed
#' through to [signal::butter()].
#' @param W A numeric scalar between 0 and 1 specifying the relative cutoff
#' frequency, where 1 is the Nyquist frequency (half of the sample frequency
#' in Hz).
#' @param edges Indicates how to pad `x`.
#' - *"rev"* (the default) will pad `x` with the preceding 10% data in reverse
#' sequence.
#' - *"rep1"* will pad `x` with the last preceding value.
#' - *"none"* will return the default [signal::filtfilt()] output.
#'
#' @return A numeric vector of filtered data.
#'
#' @export
#'
#' @examples
#' set.seed(13)
#' sin <- sin(2 * pi * 1:500 / 50) * 20 + 40
#' noise <- rnorm(500, mean = 0, sd = 6)
#' noisy_sin <- sin + noise
#' filt_without_edge <- filtfilt_edges(x = noisy_sin, n = 2, W = 0.1, edges = "none")
#' filt_with_edge <- filtfilt_edges(x = noisy_sin, n = 2, W = 0.1, edges = "rep1")
#' plot(noisy_sin, type = "l")
#' lines(filt_without_edge, col = "red", lwd = 4)
#' lines(filt_with_edge, col = "blue", lwd = 4)
filtfilt_edges <- function (
        x,
        n = 1,
        W = 0.1,
        edges = c("rev", "rep1", "none")
) {
    edges = match.arg(edges)

    ## argument validation
    if (!rlang::is_double(x)) {
        cli::cli_abort("{.arg x} must be a numeric vector.")
    }
    if (!rlang::is_integerish(n)) {
        cli::cli_abort("{.arg n} must be an integer scalar of 1 or greater.")
    }
    if (!rlang::is_scalar_double(W)) {
        cli::cli_abort("{.arg W} must be a numeric scalar between 0 and 1.")
    }

    switch(edges,
           ## pads x with the first and last 10% of the vector length
           "rev" = pad_edges <- c(
               rev(utils::head(x, length(x)/10)),
               x,
               rev(utils::tail(x, length(x)/10))),
           ## pads x with repeating first / last value
           "rep1" = pad_edges <- c(
               rep(utils::head(x, 1), length(x)/10),
               x,
               rep(utils::tail(x, 1), length(x)/10)),
           "none" = pad_edges <- x
    )

    x_filt <- signal::filtfilt(
        ## butterworth filter order (n) and relative cutoff frequency (W)
        filt = signal::butter(n, W),
        x = pad_edges,
    )

    ## returns the original vector length of x with padding omitted
    switch(edges,
           "rev" = x_filt[(length(x)/10 + 1):(length(x)/10 + length(x))],
           "rep1" = x_filt[(length(x)/10 + 1):(length(x)/10 + length(x))],
           "none" = x_filt)
}









