#' Filter Data
#'
#' Apply signal filtering to vector data with either: 1. A cubic smoothing spline.
#' 2. A Butterworth digital filter. 3. A simple moving average.
#'
#' @param x A numeric vector.
#' @param method Indicates how to filter the data (see *Details*).
#'  \describe{
#'      \item{`"smooth-spline"`}{fits a cubic smoothing spline.}
#'      \item{`"butterworth"`}{uses a centred Butterworth digital filter. `type` should
#'      be defined (see *Details*).}
#'      \item{`"moving-average"`}{uses a centred moving average filter.}
#'  }
#' @param type Specify the filter type. *Only relevant for `method = "butterworth"`*
#'  (see *Details*).
#'  \describe{
#'      \item{`"low"`}{For a *low-pass* filter (*default*).}
#'      \item{`"high"`}{For a *high-pass* filter.}
#'      \item{`"stop"`}{For a *stop-band* (band-reject) filter.}
#'      \item{`"pass"`}{For a *pass-band* filter.}
#'  }
#' @param spar A numeric scalar defining the smoothing parameter for
#'  `method = "smooth-spline"`.
#' @param n An integer scalar defining the order of a Butterworth filter for
#'  `method = "butterworth"`.
#' @param W A numeric scalar or two-element vector defining the fractional
#'  critical frequency(ies) of a Butterworth filter for `method = "butterworth"`.
#' @param fc A numeric scalar or two-element vector defining the critical
#'  frequency(ies) of a Butterworth filter for `method = "butterworth"`.
#' @param sample_rate A numeric scalar for the sample rate in Hz for
#'  `method = "butterworth"`.
#' @param width A numeric scalar defining the window length of samples for
#'  `method = "moving-average"`.
#' @param na.rm A logical indicating whether missing values should be ignored
#'  (`TRUE`) before the filter is applied. Otherwise (`FALSE`, the *default*) will
#'  throw an error (see *Details*).
#' @param verbose A logical. `TRUE` (the *default*) will return warnings and
#'  messages which can be used for troubleshooting. `FALSE` will silence these
#'  messages. Errors will always be returned.
#'
#' @details
#' \describe{
#'  \item{`method = "smooth-spline"`}{applies a non-parametric cubic
#'  smoothing spline from [stats::smooth.spline()]. Smoothing is defined
#'  by the parameter `spar`, which can be left blank and automatically
#'  determined via penalised log liklihood. This usually works well for
#'  smoothing responses occurring on the order of minutes or longer. Or `spar`
#'  can be defined explicitly, typically (but not necessarily) in the range
#'  `spar = [0, 1]`.}
#'  \item{`method = "butterworth"`}{applies a centred (two-pass symmetrical)
#'  Butterworth digital filter from [signal::butter()] and [signal::filtfilt()].
#'  The filter order is defined by `n`, typically in the range `n = [1, 10]`.
#'  Higher filter orders tend to better capture rapid changes in amplitude,
#'  but also cause more distortion artefacts in the signal. General advice is
#'  to use the lowest order which sufficiently captures any rapid step-changes
#'  in the data.
#'
#'  Filter `type` defines how the desired signal frequencies are either passed
#'  through or rejected from the output signal. *Low-pass* and *high-pass*
#'  filters allow only frequencies *lower* or *higher* than the critical
#'  frequency `W` to be passed through as the output signal, respectively.
#'  *Stop-band* defines a critical range of frequencies which are rejected
#'  from the output signal. *Pass-band* defines a critical range of frequencies
#'  which are passed through as the output signal.
#'
#'  The critical (cutoff) frequency is defined by `W`, a numeric scalar for
#'  *low-pass* and *high-pass* filters, or a two-element vector `c(low, high)`
#'  defining the lower and upper bands for *stop-band* and *pass-band* filters.
#'  `W` represents the desired fractional critical frequency in the range
#'  `W = [0, 1]`, where `1` is the Nyquist frequency, i.e., half the sample
#'  rate of the data in Hz.
#'
#'  Alternatively, the critical frequency can be defined by `fc`
#'  and `sample_rate` together. `fc` represents the desired
#'  critical frequency in Hz, and `sample_rate` is the sample rate of the
#'  recorded data in Hz. `W = fc / (sample_rate/2)`.
#'
#'  Defining both `fc` and `sample_rate` explicitly will
#'  overwrite `W`.}
#'  \item{`method = "moving-average"`}{applies a centred (two-way symmetrical)
#'  moving average filter from [zoo::rollapply()]. The moving-average is
#'  calculated over a window of width `width` defining the number of samples
#'  between `[i - floor(width/2), i + floor(width/2)]`. A partial moving-
#'  average will be calculated at the edges of the existing data.}
#'  }
#'
#' Missing values (`NA`) in the numeric vector will cause an error unless
#' `na.rm = TRUE`. Then `NA` values are removed for processing, and restored in
#' the returned vector.
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
#' y.LP <- filter_data(y, method = "butterworth", n = 2, W = 0.05)
#' y.MA <- filter_data(y, method = "moving-average", width = 30)
#'
#' \dontrun{
#' plot(x, y)
#' lines(x, y.spline, lwd = 2, col = "blue")
#' lines(x, y.LP, lwd = 2, col = "red")
#' lines(x, y.MA, lwd = 2, col = "green4")
#' }
#'
#' @return A numeric vector of filtered data.
#'
#' @export
filter_data <- function(
        x,
        method = c("smooth-spline", "butterworth", "moving-average"),
        type = c("low", "high", "stop", "pass"),
        spar = NULL,
        n = 1,
        W,
        fc,
        sample_rate,
        width,
        na.rm = FALSE,
        verbose = TRUE,
        ...
) {
    method <- match.arg(method)
    type = match.arg(type)
    args <- list(...)
    edges <- args$edges %||% "rev" ## default filtfilt_edges(edges)

    ## validation: `x` must be a numeric vector
    if (!is.numeric(x)) {
        cli::cli_abort("{.arg x} must be a {cli::col_blue('numeric')} vector.")
    }

    validate_numeric <- function(arg) {
        name <- deparse(substitute(arg))
        if (!is.numeric(arg)) {
            cli::cli_abort("{.arg {name}} must be {cli::col_blue('numeric')}.")
        }
    }

    ## filter parameters should be manually defined, therefore left blank in
    ## function definition, but `NULL` makes conditional detection easier
    if (missing(spar) || spar == 0) {spar <- NULL}
    if (missing(W) || W == 0) {W <- NULL}
    if (missing(fc) || fc == 0) {fc <- NULL}
    if (missing(sample_rate) || sample_rate == 0) {sample_rate <- NULL}
    if (missing(width) || width == 0) {width <- NULL}

    ## logical whether to handle NAs
    handle_na <- na.rm & any(is.na(x))
    if (handle_na) {
        na_info <- preserve_na(x)
        x <- na_info$x_clean
    }

    if (method == "smooth-spline") {

        if (is.null(spar)) {
            spline_model <- stats::smooth.spline(x = x)
            y <- spline_model$y
            spar <- round(spline_model$spar, 3)

            if (verbose) {
                cli::cli_alert_info(
                    "{.fn smooth.spline} {.arg spar} set to {.val {spar}}")
            }
        } else {
            validate_numeric(spar)
            y <- stats::smooth.spline(x = x, spar = spar)$y
        }

    } else if (method == "butterworth") {

        if (is.null(W) & !is.null(fc) & !is.null(sample_rate)) {
            validate_numeric(fc)
            validate_numeric(sample_rate)
            y <- filtfilt_edges(x = x, n = n, W = fc / (sample_rate/2),
                                type = type, edges = edges)
        } else if (!is.null(W) && (is.null(fc) & is.null(sample_rate))) {
            y <- filtfilt_edges(x = x, n = n, W = W, type = type)
        } else {
            cli::cli_abort(paste(
                "Either {.arg W} alone, or {.arg fc} and {.arg sample_rate}",
                "together must be {cli::col_blue('numeric')}",
                "for a Butterworth filter."))
        }

    } else if (method == "moving-average") {
        validate_numeric(width)
        y <- zoo::rollapply(x, width = width, FUN = mean, partial = TRUE, na.rm = TRUE)
    }

    ## return y to original x length with NAs if handled
    result <- if (handle_na) {restore_na(y, na_info)} else {y}

    return(result)
}
