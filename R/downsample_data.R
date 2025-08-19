#' Downsample Data
#'
#' Condense the number of samples of a dataframe using time-weighted averaging.
#'
#' @param data A dataframe.
#' @param sample_column An *optional* character scalar indicating the name of
#'  the time or sample data column. Must match exactly.
#' @param sample_rate An *optional* numeric scalar for the sample rate in Hz.
#' @param downsample_rate An *optional* numeric scalar indicating the desired
#'  output sample rate (in Hz) to convert the dataframe.
#' @param downsample_time An *optional* numeric scalar indicating the desired
#'  sample time (in seconds) to convert the dataframe.
#' @param verbose A logical. `TRUE` (the *default*) will return warnings and
#' messages which can be used for data error checking. `FALSE` will silence these
#' messages. Errors will always be returned.
#'
#' @details
#' `sample_column` and `sample_rate` will be taken from metadata for an
#' mNIRS dataframe, if not defined explicitly.
#'
#' If not present in metadata, `sample_column` must be defined explicitly.
#' `sample_rate` will be estimated based on the mean difference between values
#' in the `sample_column`. If `sample_column` contains integer sample numbers,
#' then `sample_rate` will be incorrectly estimated  to be 1 Hz, and should be
#' defined explicitly.
#'
#' *TODO include upsample interpolation?*
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#'  metadata available with `attributes()`.
#'
#' @export
downsample_data <- function(
        data,
        sample_column = NULL,
        sample_rate = NULL,
        downsample_rate = NULL, ## 10 Hz
        downsample_time = NULL, ## 0.01 s
        verbose = TRUE
) {
    ## pass through =============================

    ## validation if both `downsample_rate` and `downsample_time` are not defined
    ## then pass through original dataframe
    if ((is.null(downsample_rate) & is.null(downsample_time)) |
        any(c(downsample_rate, downsample_time) == 0)) {
        return(data)
    }

    ## metadata ================================
    metadata <- attributes(data)

    ## define `sample_column`. priority is manually defined
    if (
        (missing(sample_column) | is.null(sample_column)) &
        !is.null(metadata$sample_column)
    ) {
        sample_column <- metadata$sample_column
    }

    ## validation: `sample_column` must match expected dataframe names
    if (!all(unlist(sample_column) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg sample_column} not found. Make sure column names",
            "match exactly."))
    }

    ## check for non-NULL, not applicable `sample_rate`
    if (!is.null(sample_rate) && (!is.numeric(sample_rate) || sample_rate <= 0)) {
        sample_rate <- NULL

        if (verbose) {
            cli::cli_alert_info(paste(
                "{.arg sample_rate} should be defined explicitly",
                "as a numeric value > 0 Hz."))
        }
    }

    sample_vector <- as.numeric(data[[sample_column]])

    ## estimate sample_rate in samples per second
    estimated_sample_rate <- head(diff(sample_vector), 100) |>
        mean(na.rm = TRUE) |>
        (\(.x) round((1/.x)/0.5)*0.5)()

    ## define `sample_rate`. priority is manually defined
    if (
        (missing(sample_rate) | is.null(sample_rate)) &
        !is.null(metadata$sample_rate)
    ) {
        ## take sample_rate from metadata
        sample_rate <- metadata$sample_rate

        sample_info <- paste("Sample rate = {.val {sample_rate}} Hz.")

    } else if (missing(sample_rate) | is.null(sample_rate)) {
        ## estimate sample_rate from data
        sample_rate <- estimated_sample_rate

        sample_info <- paste(
            "Estimated sample rate = {.val {sample_rate}} Hz.",
            "Overwrite this with {.arg sample_rate = X}.")

    } else {
        sample_info <- paste("Sample rate = {.val {sample_rate}} Hz.")
    }
    #
    ## Processing ===================================
    ## explicitly define `downsample_rate` from `downsample_time`
    downsample_rate <- if (!is.null(downsample_rate) & is.null(downsample_time)) {
        downsample_rate
    } else if (!is.null(downsample_time) & is.null(downsample_rate)) {
        1 / downsample_time
    } else {
        cli::cli_abort(paste(
            "Either {.arg sample_rate} or {.arg sample_time}",
            "should be defined, not both."))
    }

    ## validation: downsample_rate must be less than sample_rate
    if (downsample_rate > sample_rate) {
        if (verbose) {
            cli::cli_alert_info(paste(
                "{.arg downsample_rate} should be less than {.arg sample_rate}.",
                "Returning original dataframe"))
        }
        return(data)
    }

    if (verbose) {
        cli::cli_alert_info(paste(
            "i" = sample_info,
            "i" = "Output is downsampled at {.val {downsample_rate}} Hz."
        ))
    }

    y <- data |>
        dplyr::mutate(
            ## calculate time difference for weighting
            dplyr::across(
                dplyr::any_of(sample_column),
                \(.x) c(diff(.x), tail(diff(.x), 1)),
                .names = "delta_sample"),
            ## Round to nearest downsample rate
            dplyr::across(
                dplyr::any_of(sample_column),
                \(.x) if (estimated_sample_rate == sample_rate) {
                    ## if `sample_column` is time values and `sample_rate` is
                    ## estimated correctly, should output correct time values
                    floor(.x * downsample_rate) / downsample_rate
                } else {
                    ## if `sample_column` is integers, average across samples
                    ## and output binned sample numbers
                    floor(.x * downsample_rate / sample_rate) /
                        downsample_rate * sample_rate
                }),
        ) |>
        dplyr::summarise(
            .by = dplyr::any_of(sample_column),
            ## weighted mean value for numeric columns
            dplyr::across(
                dplyr::where(is.numeric),
                \(.x) stats::weighted.mean(.x, delta_sample, na.rm = TRUE)),
            ## take the first non-na value from other columns
            ## TODO 2025-06-23 is this robust enough if multiple event strings
            ## are present within one downsample bin?
            dplyr::across(
                !dplyr::where(is.numeric),
                \(.x) dplyr::first(na.omit(.x))),
        ) |>
        dplyr::select(-delta_sample)
    #
    ## Metadata =================================
    metadata$sample_column <- unlist(sample_column)
    metadata$sample_rate <- downsample_rate

    y <- create_mNIRS_data(y, metadata)

    return(y)
}
#
## troubleshooting =================================
# (data <- read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\Eva-pilot-Oxy-2025-05-27.xlsx)",
#     nirs_columns = c(VL_O2Hb = 5, VL_HHb = 6),
#     sample_column = c(sample = 1),
#     # event_column = c(event = "...11"),
#     .keep_all = FALSE))
# (data <- read_data(
#     file_path = r"(C:\R-Projects\mNIRS\inst\moxy_ramp_example.xlsx)",
#     nirs_columns = c(smo2_left = "smo2_left_VL",
#                      smo2_right = "smo2_right_VL"),
#     sample_column = c(time = "Time"),
#     # event_column = c(event = "...11"),
#     .keep_all = FALSE))
# #
# (sample_column <- attributes(data)$sample_column)
# (sample_rate <- attributes(data)$sample_rate)
# (downsample_rate <- 0.1)
# # #
# (y <- downsample_data(
#     data = data,
#     # sample_column = "time",
#     sample_rate = 50,
#     downsample_rate = 10,
#     # downsample_time = NULL
# ))
# attributes(y)
#
# library(ggplot2)
# library(JAPackage)
#
# ggplot(data) +
#     {list( ## Settings
#         aes(x = sample, y = VL_O2Hb),
#         # coord_cartesian(
#         #     xlim = c(NA, 20),
#         #     ylim = c(0, 40)),
#         theme_JA(),
#         NULL)} + ## Settings
#     {list( ## Data
#         geom_line(),
#         # geom_point(),
#         NULL)} ## Data


# set.seed(13) ; tibble::tibble(
#     A = seq(1, by = 0.5, len = 100),
#     B = rnorm(100, mean = 10)
# ) |>
#     mutate(
#         dplyr::across(
#             A,
#             \(.x) c(diff(.x), tail(diff(.x), 1)),
#             .names = "delta_sample"),
#         dplyr::across(
#             A,
#             \(.x) floor(.x * downsample_rate / 2) / downsample_rate),
#     ) |>
#     dplyr::summarise(
#         .by = A,
#         ## weighted mean value for numeric columns
#         dplyr::across(
#             dplyr::where(is.numeric),
#             \(.x) weighted.mean(.x, delta_sample, na.rm = TRUE)),
#         ## take the first non-na value from other columns
#         ## TODO 2025-06-23 is this robust enough if multiple event strings
#         ## are present within one downsample?
#         dplyr::across(
#             !dplyr::where(is.numeric),
#             \(.x) dplyr::first(na.omit(.x))),
#     ) |>
#     dplyr::select(-delta_sample)
