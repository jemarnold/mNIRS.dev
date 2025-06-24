#' Resample Data
#'
#' Resamples dataframe using time-weighted interpolation.
#'
#' @param data A dataframe containing mNIRS data.
#' @param sample_column *(optional)* A character scalar indicating the name of
#' the time or sample data column. Must match exactly.
#' @param sample_rate *(optional)* An integer scalar for the sample rate in Hz.
#' @param resample_rate *(optional)* An integer scalar indicating the desired
#' output sample rate (in Hz) to convert the dataframe.
#' @param resample_time *(optional)* A numeric scalar indicating the desired
#' sample time (in seconds) to convert the dataframe.
#' @param .verbose A logical. `TRUE` *(default)* will return warnings and
#' messages which can be used for data error checking. `FALSE` will silence these
#' messages. Errors will always be returned.
#' @param ... Additional arguments (*currently not used*).
#'
#' @details
#' *CURRENTLY ONLY IMPLEMENTED FOR DOWNSAMPLING*
#'
#' `sample_column` and `sample_rate` will be taken from metadata for an
#' mNIRS dataframe, if not defined explicitly.
#'
#' If not present in metadata, `sample_column` must be defined explicitly.
#' `sample_rate` will be estimated based on the mean difference between values
#' in the `sample_column`. If `sample_column` contains integer sample numbers,
#' then `sample_rate` will be incorrectly estimated  to be 1 Hz, and should be
#' defined explicitly.
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#' metadata available with `attributes()`.
#'
#' @export
resample_dataframe <- function(
        data,
        sample_column = NULL,
        sample_rate = NULL,
        resample_rate = NULL, ## 10 Hz
        resample_time = NULL, ## 0.01 s
        .verbose = TRUE,
        ...
) {
    ## pass through =============================

    ## validation if both `resample_rate` and `resample_time` are not defined
    ## then pass through original dataframe
    if ((is.null(resample_rate) & is.null(resample_time)) |
        any(c(resample_rate, resample_time) == 0)) {
        return(data)
    }

    ## metadata ================================
    metadata <- attributes(data)
    args <- list(...)

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

    sample_vector <- as.numeric(data[[sample_column]])
    ## estimate sample_rate in samples per second
    empirical_sample_rate <- head(diff(sample_vector), 100) |>
        mean(na.rm = TRUE) |>
        (\(.x) round((1/.x)/0.5)*0.5)()

    ## define `sample_rate`. priority is manually defined
    if (
        (missing(sample_rate) | is.null(sample_rate)) &
        !is.null(metadata$sample_rate)
    ) {
        ## take sample_rate from metadata
        sample_rate <- metadata$sample_rate
    } else if (missing(sample_rate) | is.null(sample_rate)) {
        ## estimate sample_rate from data
        sample_rate <- empirical_sample_rate
    }
    #
    ## Processing ===================================
    ## explicitly define `resample_rate` from `resample_time`
    resample_rate <- if (!is.null(resample_rate) & is.null(resample_time)) {
        resample_rate
    } else if (!is.null(resample_time) & is.null(resample_rate)) {
        1 / resample_time
    } else {
        cli::cli_abort(paste(
            "Either {.arg sample_rate} or {.arg sample_time}",
            "should be defined, not both."))
    }

    y <- data |>
        dplyr::mutate(
            ## calculate time difference for weighting
            dplyr::across(
                dplyr::any_of(sample_column),
                \(.x) c(diff(.x), tail(diff(.x), 1)),
                .names = "delta_sample"),
            ## Round to nearest resample rate
            dplyr::across(
                dplyr::any_of(sample_column),
                \(.x) if (empirical_sample_rate == sample_rate) {
                    ## if `sample_column` is time values and `sample_rate` is
                    ## estimated correctly, should output correct time values
                    floor(.x * resample_rate) / resample_rate
                } else {
                    ## if `sample_column` is integers, average across samples
                    ## and output binned sample numbers
                    floor(.x * resample_rate / sample_rate) /
                        resample_rate * sample_rate
                }),
        ) |>
        dplyr::summarise(
            .by = dplyr::any_of(sample_column),
            ## weighted mean value for numeric columns
            dplyr::across(
                dplyr::where(is.numeric),
                \(.x) weighted.mean(.x, delta_sample, na.rm = TRUE)),
            ## take the first non-na value from other columns
            ## TODO 2025-06-23 is this robust enough if multiple event strings
            ## are present within one resample?
            dplyr::across(
                !dplyr::where(is.numeric),
                \(.x) dplyr::first(na.omit(.x))),
        ) |>
        dplyr::select(-delta_sample)
    #
    ## Metadata =================================
    metadata$sample_column <- unlist(sample_column)
    metadata$sample_rate <- resample_rate

    y <- create_mnirs_data(y, metadata)

    if (.verbose) {
        cli::cli_alert_info(paste(
            "Estimated sample rate is {.val {sample_rate}} Hz.",
            "Output is resampled at {.val {resample_rate}} Hz."
        ))
    }

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
# (resample_rate <- 0.1)
# # #
# (y <- resample_dataframe(
#     data = data,
#     # sample_column = "time",
#     sample_rate = 50,
#     resample_rate = 10,
#     # resample_time = NULL
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
#             \(.x) floor(.x * resample_rate / 2) / resample_rate),
#     ) |>
#     dplyr::summarise(
#         .by = A,
#         ## weighted mean value for numeric columns
#         dplyr::across(
#             dplyr::where(is.numeric),
#             \(.x) weighted.mean(.x, delta_sample, na.rm = TRUE)),
#         ## take the first non-na value from other columns
#         ## TODO 2025-06-23 is this robust enough if multiple event strings
#         ## are present within one resample?
#         dplyr::across(
#             !dplyr::where(is.numeric),
#             \(.x) dplyr::first(na.omit(.x))),
#     ) |>
#     dplyr::select(-delta_sample)
