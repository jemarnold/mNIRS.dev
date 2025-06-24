#' Filter Data
#'
#' Applies digital filtering with either: 1. A cubic smoothing spline.
#' 2. A Butterworth low-pass filter. 3. A simple moving average.
#'
#' @param x A numeric vector.
#' @param method Indicates how to digitally filter the data (see *Details*).
#' \describe{
#'   \item{`"smooth-spline"`}{fits a cubic smoothing spline.}
#'   \item{`"low-pass"`}{uses a centred Butterworth low-pass filter.}
#'   \item{`"moving-average"`}{uses a centred moving average filter.}
#' }
#' @param spar A numeric scalar defining the smoothing parameter for
#' `method = "smooth-spline"`.
#' @param n An integer scalar defining the order of a low-pass filter for
#' `method = "low-pass"`.
#' @param W A numeric scalar defining the fractional critical frequency of a
#' low-pass filter for `method = "low-pass"`.
#' @param critical_frequency A numeric scalar defining the critical frequency of
#' a low-pass filter for `method = "low-pass"`.
#' @param sample_rate A numeric scalar for the sample rate in Hz for
#' `method = "low-pass"`.
#' @param width A numeric scalar defining the window length of samples for
#' `method = "moving-average"`.
#'
#' @details
#' \describe{
#'   \item{`method = "smooth-spline"`}{applies a non-parametric cubic
#'   smoothing spline from [stats::smooth.spline()]. Smoothing is defined
#'   by the parameter `spar`, which can be left blank and automatically determined
#'   via penalised log liklihood. This usually works well for smoothing responses
#'   occurring on the order of minutes or longer. Or `spar` can be defined
#'   explicitly, typically (but not necessarily) in the range `spar = [0, 1]`.}
#'   \item{`method = "low-pass"`}{applies a centred (two-pass symmetrical)
#'   Butterworth low-pass digital filter from [signal::butter()] and
#'   [signal::filtfilt()]. The filter order is defined by `n`, typically in the
#'   range `n = [1, 10]`. Higher filter orders tend to capture rapid changes in
#'   amplitude better, but also cause more distortion artefacts in the signal.
#'   General advice is to use the lowest order which sufficiently captures rapid
#'   step-changes in the data.
#'
#'   The low frequencies to be passed through the filter are defined by a
#'   critical frequency. This can be defined by either `critical_frequency`
#'   representing the desired critical frequency in Hz, and `sample_rate`
#'   reflecting the sample rate of the data in Hz together. Or by `W`
#'   representing the desired fractional critical frequency in the range
#'   `W = [0, 1]`, where `1` is the Nyquist frequency, i.e., half the sample
#'   rate of the data in Hz.
#'
#'   Defining both `critical_frequency` and `sample_rate` explicitly will
#'   overwrite `W`.}
#'   \item{`method = "moving-average"`}{applies a centred (two-way symmetrical)
#'   moving average filter from [zoo::rollapply()]. The moving-average is
#'   calculated over a window of width `width` specifying
#'   the number of samples between `[i - floor(width/2), i + floor(width/2)]`.
#'   A partial moving-average will be calculated at the edges of the existing
#'   data.}
#' }
#'
#' @examples
#' set.seed(13)
#' n <- 500
#' x <- seq(0, 2*pi, len = n)
#' y_clean <- 2*sin(x)
#' noise <- rnorm(n, mean = 0, sd = 0.5)
#' y <- y_clean + noise
#'
#' y.spline <- filter_data(y, method = "smooth-spline")
#' y.LP <- filter_data(y, method = "low-pass", n = 2, W = 0.05)
#' y.MA <- filter_data(y, method = "moving-average", width = 30)
#'
#' plot(x, y)
#' lines(x, y.spline, lwd = 2, col = "blue")
#' lines(x, y.LP, lwd = 2, col = "red")
#' lines(x, y.MA, lwd = 2, col = "green4")
#'
#' @return A numeric vector of filtered data.
#'
#' @export
filter_data <- function(
        x,
        method = c("smooth-spline", "low-pass", "moving-average"),
        spar = NULL,
        n,
        W,
        critical_frequency,
        sample_rate,
        width
) {

    method <- match.arg(method)

    ## filter parameters should be manually defined, therefore left blank in
    ## function definition, but `NULL` makes conditional detection easier
    if (missing(n)) {n <- NULL}
    if (missing(W)) {W <- NULL}
    if (missing(critical_frequency)) {critical_frequency <- NULL}
    if (missing(sample_rate)) {sample_rate <- NULL}
    if (missing(width)) {width <- NULL}

    if (method == "smooth-spline") {

        if (is.null(spar)) {
            y <- stats::smooth.spline(x = x)$y
        } else if (is.numeric(spar)) {
            y <- stats::smooth.spline(x = x, spar = spar)$y
        }

    } else if (method == "low-pass") {

        if (!is.null(critical_frequency) & !is.null(sample_rate)) {
            if (is.numeric(critical_frequency) & is.numeric(sample_rate)) {
                y <- mNIRS::filtfilt_edges(
                    x = x, n = n, W = critical_frequency / (sample_rate/2))
            } else {
                cli::cli_abort(paste(
                    "{.arg critical_frequency} and {.arg sample_rate}",
                    "must each be {.val numeric} values for the low-pass",
                    "Butterworth filter."))
            }

        } else if (!is.null(W)) {
            if (is.numeric(W)) {
                y <- mNIRS::filtfilt_edges(x = x, n = n, W = W)
            } else {
                cli::cli_abort(paste(
                    "{.arg W} must be a {.val numeric} value for the low-pass",
                    "Butterworth filter."))
            }

        } else {
            cli::cli_abort(paste(
                "Either {.arg W} alone, or {.arg critical_frequency} and",
                "{.arg sample_rate} together must be specified for the",
                "Butterworth low-pass filter."))
        }

    } else if (method == "moving-average") {

        if (is.numeric(width)) {
            y <- zoo::rollapply(
                x, width = width, FUN = mean,
                align = "center", partial = TRUE, na.rm = TRUE)
        } else {
            cli::cli_abort(paste(
                "{.arg width} must be a {.val numeric} value for the",
                "moving-average filter."))
        }
    }

    return(y)
}
#
# df <- mNIRS::read_data(
#     file_path = r"(C:\OneDrive - UBC\EPL\Matt Fliss\matt pitshark.csv)",
#     nirs_columns = c(smo2_673 = "SmO2_673", smo2_702 = "SmO2_702"),
#     sample_column = c(time = "Timestamp (seconds passed)")
# ) |>
#     dplyr::mutate(
#         # smo2_filt = filter_data(smo2_673, method = "smooth-spline", spar = 0.8),
#         # smo2_filt = filter_data(smo2_673, method = "low-pass", n = 1, W = 0.1),
#         # smo2_filt = filter_data(smo2_673, method = "low-pass", n = 2, critical_frequency = 0.5, sample_rate = 10),
#         smo2_filt = filter_data(smo2_673, method = "moving-average", width = 50),
#     ) |>
#     print()
# #
# ggplot2::ggplot(df, ggplot2::aes(x = time)) +
#     {list( ## Settings
#         # JAPackage::theme_JA(),
#         ggplot2::geom_line(ggplot2::aes(y = smo2_673)),
#         ggplot2::geom_line(ggplot2::aes(y = smo2_filt, colour = "filt")),
#         NULL)} ## Data
