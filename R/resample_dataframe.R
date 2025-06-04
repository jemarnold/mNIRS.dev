#' Resample Data
#'
#' Resamples mNIRS data using time-weighted interpolation.
#'
#' @param data A dataframe containing mNIRS data.
#' @param sample_column A character scalar indicating the name of
#' the time or sample data column. Must match exactly.
#' @param sample_rate An integer scalar for the sample rate in Hz of the
#' dataframe.
#' @param resample_rate An integer scalar indicating the desired output sample
#' rate (in Hz) to convert the file to.
#' @param resample_time A numeric scalar indicating the desired sample time
#' (in seconds) to convert the file to.
#' @param ... Additional arguments (currently not used).
#'
#' @details
#' `sample_column` will be taken from metadata for an mNIRS dataframe.
#'
#' If not defined explicitly, `sample_rate` will be estimated based
#' on the mean difference between values in the `sample_column`. If
#' `sample_column` contains integer sample numbers rather than time values,
#' then `sample_rate` will be incorrectly estimated to be 1 Hz, and should
#' be defined explicitly.
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
        ...
) {
    ## pass through =============================

    if ((is.null(resample_rate) & is.null(resample_time)) |
        any(c(resample_rate, resample_time) == 0)) {
        return(data)
    }

    ## Validation =================================
    metadata <- attributes(data)
    args <- list(...)

    ## define `sample_column`
    ## priority is manually defined
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

    ## define `sample_rate`
    ## priority is manually defined
    if (
        (missing(sample_rate) | is.null(sample_rate)) &
        !is.null(metadata$sample_rate)
    ) {
        sample_rate <- metadata$sample_rate
    } else if (missing(sample_rate) | is.null(sample_rate)) {

        ## samples per second
        sample_rate <- head(diff(sample_vector), 100) |>
            mean(na.rm = TRUE) |>
            (\(.x) round((1/.x)/0.5)*0.5)()
    }

    cli::cli_alert_info(paste(
        "Estimated sample rate is {.val {sample_rate}} Hz.",
        "Overwrite this by re-running with {.arg sample_rate = X}"
    ))

    #
    ## Processing ===================================

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
            dplyr::across(
                dplyr::any_of(sample_column),
                \(.x) c(diff(.x), NA),
                .names = "delta_sample"),
            dplyr::across(
                dplyr::any_of(sample_column),
                \(.x) round(.x * sample_rate)/resample_rate)
        ) |>
        dplyr::summarise(
            .by = dplyr::any_of(sample_column),
            ## weighted mean value for numeric columns
            dplyr::across(
                dplyr::where(is.numeric),
                \(.x) weighted.mean(.x, delta_sample, na.rm = TRUE)),
            ## take the first non-na value from other columns
            dplyr::across(
                !dplyr::where(is.numeric),
                \(.x) dplyr::first(na.omit(.x))),
        ) |>
        dplyr::select(-delta_sample) |>
        ## remove last row with all NA
        dplyr::filter(
            dplyr::if_all(
                !dplyr::any_of(sample_column) & dplyr::where(is.numeric),
            \(.x) !is.na(.x))
        )

    #
    ## Metadata =================================
    metadata$sample_column <- unlist(sample_column)
    metadata$sample_rate <- resample_rate

    y <- create_mnirs_data(y, metadata)

    return(y)
}
#
# (data <- mNIRS::read_data(
#     file_path = "C:/OneDrive - UBC/Body Position Study/Raw Data/BP01-Train.Red-2025-04-01.csv",
#     nirs_columns = c("smo2_left" = "SmO2 unfiltered",
#                      "HHb_left" = "HHb unfiltered",
#                      "O2Hb_left" = "O2HB unfiltered",
#                      "smo2_right" = "SmO2 unfiltered"),
#     sample_column = c("time" = "Timestamp (seconds passed)"),
#     event_column = c("Lap" = "Lap/Event")))
# (data <- mNIRS::read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\Eva-pilot-Oxy-2025-05-27.xlsx)",
#     nirs_columns = c(VL_O2Hb = 5, VL_HHb = 6),
#     sample_column = c(sample = 1),
#     # event_column = c(event = "...11"),
#     .keep_all = FALSE))
# attributes(data)
#
# (y <- mNIRS::resample_dataframe(
#     data = data,
#     # sample_column = "time",
#     # sample_rate = 10,
#     resample_rate = 10, ## 10 Hz
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
