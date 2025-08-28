#' Re-sample Dataframe
#'
#' Increase or decrease the number of observations in a dataframe using linear
#' interpolation for up- and down-sampling.
#'
#' @param data A dataframe.
#' @param sample_channel A character scalar indicating the time or sample data
#'  column. Must match `data` column names exactly. Will be taken from metadata
#'  if not defined explicitly.
#' @param sample_rate A numeric scalar for the sample rate in Hz. Will be taken
#'  from metadata or estimated from `sample_channel` if not defined explicitly.
#' @param resample_rate An *optional* numeric scalar indicating the desired
#'  output sample rate (in Hz) to convert the dataframe. Setting
#'  `resample_rate = sample_rate` will interpolate over missing and repeated
#'  samples within the bounds of the existing data. If both `resample_rate = NULL`
#'  and `resample_time = NULL` (the *default*), the original dataframe will be
#'  passed through.
#' @param resample_time An *optional* numeric scalar indicating the desired
#'  sample time (in seconds) to convert the dataframe.
#' @param na.rm A logical indicating whether `NA`s should be interpolated across
#'  or passed through to the re-sampled dataframe.
#'  \describe{
#'      \item{`na.rm = FALSE`}{(The *default*) will pass through `NA`s from the
#'      original data, and any new samples will be `NA`.}
#'      \item{`na.rm = TRUE`}{Will use [stats::approx()] `method = "linear"` to
#'      interpolate across `NA`s and any new samples. Leading and trailing `NA`s
#'      will be filled by applying `rule = 2`.}
#'  }
#' @param verbose A logical. `TRUE` (the *default*) will return warnings and
#' messages which can be used for data error checking. `FALSE` will silence these
#' messages. Errors will always be returned.
#'
#' @details
#' `sample_channel` and `sample_rate` will be taken from metadata for a dataframe
#' of class `"mnirs.data` which has been processed with `{mnirs}`, if not specified
#' explicitly.
#'
#' Otherwise, `sample_rate` will be estimated from the values in `sample_channel`.
#' However, this may return unexpected values, and it is safer to define
#' `sample_rate` explicitly.
#'
#' The default setting `resample_rate = sample_rate` will interpolate over missing
#' and repeated samples within the bounds of the existing data. Setting both
#' `resample_rate = NULL` and `resample_time = NULL` (the *default*) will pass
#' through the original dataframe.
#'
#' @return A [tibble][tibble::tibble-package] of class `mnirs.data` with
#'  metadata available with `attributes()`.
#'
#' @export
resample_data <- function(
        data,
        sample_channel = NULL,
        sample_rate = NULL,
        resample_rate = NULL,
        resample_time = NULL,
        na.rm = FALSE,
        verbose = TRUE
) {
    ## pass through =============================

    ## validation if both `resample_rate` and `resample_time` are not defined
    ## then pass through original dataframe
    if ((is.null(resample_rate) & is.null(resample_time)) ||
        any(c(resample_rate, resample_time) <= 0)) {
        return(data)
    }

    ## metadata ================================
    metadata <- attributes(data)

    ## define `sample_channel`. priority is manually defined
    if (is.null(sample_channel) & !is.null(metadata$sample_channel)) {
        sample_channel <- metadata$sample_channel
    } else if (is.null(sample_channel) | !any(sample_channel %in% names(data))) {
        cli_abort(
            "{.arg sample_channel} not found. Make sure column names match exactly.")
    } else if (!is.numeric(data[[sample_channel]])) {
        cli_abort(paste("{.arg sample_channel} = {.val {sample_channel}} must be",
                        "a {.cls numeric} vector."))
    }

    ## check sample_channel length
    if (nrow(data[!is.na(data[[sample_channel]]), ]) < 2) {
        cli_abort(paste("{.arg sample_channel} = {.val {sample_channel}} needs",
                        "at least two non-NA values to interpolate across."))
    }

    ## check for non-NULL, not applicable `sample_rate`
    if (!is.null(sample_rate) & (!is.numeric(sample_rate) || sample_rate <= 0)) {
        sample_rate <- NULL

        if (verbose) {
            cli_alert_info(paste(
                "{.arg sample_rate} should be defined explicitly",
                "as a numeric value > {.val {0}} Hz."))
        }
    }

    ## estimate sample_rate in samples per second
    sample_vector <- round(as.numeric(data[[sample_channel]]), 10)
    estimated_sample_rate <- diff(sample_vector)[1:100] |>
        mean(na.rm = TRUE) |>
        (\(.x) round((1/.x)/0.5)*0.5)()

    ## define `sample_rate`. priority is manually defined
    if (is.null(sample_rate) & !is.null(metadata$sample_rate)) {
        ## take sample_rate from metadata
        sample_rate <- metadata$sample_rate
        sample_info <- paste("{.arg sample_rate} = {.val {sample_rate}} Hz.")

    } else if (is.null(sample_rate)) {
        ## estimate sample_rate from data
        sample_rate <- estimated_sample_rate
        sample_info <- paste(
            "Estimated {.arg sample_rate} = {.val {sample_rate}} Hz.",
            "Overwrite this with {.arg sample_rate} = {.cls numeric}.")

    } else {
        sample_info <- paste("{.arg sample_rate} = {.val {sample_rate}} Hz.")
    }

    ## explicitly share definitions between `resample_rate` and `resample_time`
    if (!is.null(resample_rate) & is.null(resample_time)) {
        resample_time <- 1 / resample_rate
    } else if (!is.null(resample_time) & is.null(resample_rate)) {
        resample_rate <- 1 / resample_time
    } else {
        cli_warn(paste("Either {.arg resample_rate} or {.arg resample_time}",
                       "should be defined, not both. Defaulting to",
                       "{.arg resample_rate} = {.val {resample_rate}}"))
    }
    #
    ## Processing ===================================

    ## TODO 2025-08-28 - na.rm = FALSE should pass through NAs and not interpolate
    ## over non-existing samples when up-sampling. na.rm = TRUE should interpolate
    ## accross existing NA and across new up-samples.

    ## calculate resampling parameters
    sample_range <- floor(range(sample_vector, na.rm = TRUE) * sample_rate) / sample_rate
    new_times <- seq(sample_range[1], sample_range[2], by = resample_time)
    result <- data.frame(setNames(list(new_times), sample_channel))

    ## interpolate numeric columns
    numeric_cols <- sapply(data, \(.x) is.numeric(.x))
    numeric_cols[sample_channel] <- FALSE
    non_numeric_cols <- names(data)[!numeric_cols & names(data) != sample_channel]

    if (any(numeric_cols)) {
        numeric_data <- data[numeric_cols]

        interpolated <- lapply(numeric_data, \(.x) {
            if (na.rm) {
                ## if na.rm, interpolate across NA and new up-samples
                approx(x = sample_vector, y = .x, xout = new_times,
                       method = "linear", rule = 2, ties = list("ordered", mean),
                       na.rm = TRUE)$y
            } else {
                ## if !na.rm, pass through NA and leave new up-samples as NA
                ## TODO 2025-08-28 what happens with down-samples?
                # y <- rep(NA_real_, length(new_times))
                # match_idx <- match(new_times, sample_vector)
                # valid_matches <- !is.na(match_idx)
                # y[valid_matches] <- .x[match_idx[valid_matches]]
                # y
                .x[match(new_times, sample_vector)]
            }
        })

        result[names(interpolated)] <- interpolated
    }

    ## vectorized forward fill for non-numeric columns
    if (length(non_numeric_cols) > 0) {
        indices <- findInterval(new_times, sample_vector, rightmost.closed = FALSE)
        indices[indices == 0] <- 1
        result[non_numeric_cols] <- lapply(data[non_numeric_cols], \(.x) .x[indices])
    }

    if (verbose) {
        cli_alert_info(sample_info)
        cli_alert_info("Output is resampled at {.val {resample_rate}} Hz.")
    }
    #
    ## Metadata =================================
    metadata$sample_channel <- unlist(sample_channel)
    metadata$sample_rate <- resample_rate

    return(create_mnirs_data(result, metadata))
}
