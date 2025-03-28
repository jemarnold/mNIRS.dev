#' Filter Data
#'
#' Applies digital filtering with either:
#' 1. A cubic smoothing spline.
#' 2. A Butterworth low-pass filter.
#' 3. A simple moving average.
#'
#' @param x A numeric vector of mNIRS data.
#' @param method Indicates how to digitally filter the data.
#' - *"smooth-spline"* fits a cubic smoothing spline.
#' - *"low-pass"* uses a centred Butterworth low-pass filter.
#' - *"moving-average"* uses a centred moving average filter.
#' @param ... Additional arguments.
#'
#' @details
#' `method = "smooth-spline"` applies a non-parametric cubic smoothing spline
#' from [stats::smooth.spline()]. Smoothing is defined by the parameter
#' `spar`, a numeric scalar which can be left blank and automatically
#' determined via penalised log liklihood. This usually works well for
#' smoothing responses occuring on the order of minutes or longer. Or `spar`
#' can be defined explicitly, typically (but not necessarily) `spar = [0, 1]`.
#'
#' `method = "low-pass"` applies a centred (two-pass symmetrical) Butterworth
#' low-pass digital filter from [signal::butter()] and [signal::filtfilt()].
#' The filter order is defined by `n`, an integer scalar typically `n = [1, 10]`.
#' Higher filter orders tend to capture rapid changes in amplitude better, but
#' also cause more distortion artefacts in the signal. General advice is to use
#' the lowest order that sufficiently captures rapid step-changes in the data.
#'
#' The low frequencies to be passed through the filter are defined by a
#' critical frequency. This can be defined by either `fc`, a numeric scalar
#' representing the desired critical frequency in Hz, and `sr`, a numeric
#' scalar reflecting the sample rate of the data in Hz together. Or by `W`,
#' a numeric scalar representing the desired fractional critical frequency
#' between `[0, 1]`, where `1` is the Nyquist frequency, i.e., half the sample
#' rate of the data in Hz.
#'
#' Defining both `fc` and `sr` explicitly will overwrite `W`.
#'
#' `method = "moving-average"` applies a centred (two-way symmetrical) moving
#' average filter from [zoo::rollapply()]. The moving-average is calculated
#' over a window of width `k`, an integer scalar specifying the number of
#' samples between `[i - floor(k/2), i + floor(k/2)]`. A partial moving-average
#' will be calculated at the edges of the existing data.
#'
#' @return A numeric vector of filtered data.
#'
#' @export
filter_data <- function(
        x,
        method = c("smooth-spline", "low-pass", "moving-average"),
        spar = NULL,
        n = 4,
        W = 0.1,
        fc,
        sr,
        k = 15,
        ...
) {

    method <- match.arg(method)
    W <- W
    ## pass through optional arguments
    args <- list(...)

    if (method == "smooth-spline") {

        y <- stats::smooth.spline(x = x, spar = spar)$y
        cli::cli_alert_info("Cubic smooth-spline: spar = {.val {spar}}.")

    } else if (method == "low-pass") {

        if (!missing(fc) & !missing(sr)) {

            y <- mNIRS::filtfilt_edges(x = x, n = n, W = fc / (sr/2))
            cli::cli_alert_info(paste(
                "Butterworth low-pass filter: n = {.val {n}},",
                "fc = {.val {fc}}, sr = {.val {sr}}."))

        } else if ((missing(fc) | missing(sr)) & !(missing(W) | is.null(W))) {

            y <- mNIRS::filtfilt_edges(x = x, n = n, W = W)
            cli::cli_alert_info(paste(
                "Butterworth low-pass filter: n = {.val {n}},",
                "W = {.val {W}}."))

        } else {
            cli::cli_abort(paste(
                "Either {.arg W}, or {.arg fc} and {.arg sr}",
                "must be specified for the Butterworth low-pass filter."))
        }

    } else if (method == "moving-average") {

        y <- zoo::rollapply(
            x, width = k, FUN = mean,
            align = "center", partial = TRUE, na.rm = TRUE)
        cli::cli_alert_info("Moving-average: k = {.val {k}}.")

    }

    return(y)
}
#
# library(tidyverse)
# library(JAPackage)
# library(mNIRS)
# df <- mNIRS::read_data(
#     file_path = r"(C:\OneDrive - UBC\Group Projects\EPL\Matt Fliss\matt pitshark.csv)",
#     nirs_columns = c("smo2_673" = "SmO2_673", "smo2_702" = "SmO2_702"),
#     sample_column = c("time" = "Timestamp (seconds passed)")
# ) |>
#     dplyr::mutate(
#         # smo2_filt = filter_data(
#         #     smo2_673, method = "smooth-spline", spar = 0.6),
#         # smo2_filt = filter_data(
#         #     smo2_673, method = "low-pass", n = 2, W = 0.01),
#         smo2_filt = filter_data(
#             smo2_673, method = "low-pass", fc = 0.5),
#         # smo2_filt = filter_data(
#         #     smo2_673, method = "moving-average", k = 50),
#     ) |>
#     print()
#
# ggplot(df) +
#     {list( ## Settings
#         aes(x = index),
#         theme_JA(),
#         geom_line(aes(y = smo2_673)),
#         geom_line(aes(y = smo2_filt, colour = "filt")),
#         NULL)} ## Data
#
#
# test <- function(W=0.1, fc, sr) {
#     W <- W
#     print(missing(fc))
#     print(missing(sr))
#
#     if (!missing(fc) & !missing(sr)) {
#
#         print("fc & sr EXIST")
#
#     } else if ((missing(fc) | missing(sr)) & !(missing(W) | is.null(W))) {
#
#         print("either fc OR sr are missing")
#
#     } else {
#
#         print("fc & sr are MISSING")
#     }
# }
# test(W = NULL, fc = 1)
