#' Resample Data
#'
#' Resamples mNIRS data using either time-weighted or linear
#' interpolation
#'
#' @param data A dataframe containing mNIRS data.
#' @param ... Additional arguments.
#'
#' @details
#' ...
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#' metadata available with `attributes()`.
#'
#' @export
resample_dataframe <- function(
        data,
        sample_column,
        sample_rate = NULL,
        resample_rate = NULL, ## 10 Hz
        resample_time = NULL, ## 0.01 s
        ...
) {
    ## pass through =============================

    if (is.null(resample_rate) & is.null(resample_time)) {
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

    y <- data |>
        dplyr::mutate(
            dplyr::across(
                dplyr::any_of(sample_column),
                \(.x) c(diff(.x), NA),
                .names = "delta_sample"),
            dplyr::across(
                dplyr::any_of(sample_column),
                \(.x) if (!is.null(resample_rate) & is.null(resample_time)) {

                    floor(.x * resample_rate) / resample_rate

                } else if (!is.null(resample_time) & is.null(resample_rate)) {

                    floor(.x / resample_time) * resample_time

                } else {

                    cli::cli_abort(paste(
                        "Either {.arg sample_rate} or {.arg sample_time} should",
                        "be defined, not both."))
                })
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
        dplyr::full_join(
            tibble::tibble(
                !!sample_column := seq(
                    floor(min(sample_vector, na.rm = TRUE)
                          * sample_rate) / sample_rate,
                    ceiling(max(sample_vector, na.rm = TRUE)
                            * sample_rate) / sample_rate,
                    by = if (!is.null(resample_rate) &
                             is.null(resample_time)) {
                        1 / resample_rate
                    } else if (!is.null(resample_time) &
                               is.null(resample_rate)) {
                        resample_time
                    })
            ),
            by = sample_column
        ) |>
        dplyr::arrange(dplyr::pick(dplyr::any_of(sample_column))) |>
        dplyr::mutate(
            dplyr::across(
                where(is.numeric),
                \(.x) mNIRS::replace_missing_values(
                    .x, method = "linear", na.rm = TRUE)$y),
        )
    ## TODO CONTINUE
    #
    ## Metadata =================================
    metadata$sample_column <- unlist(sample_column)

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
#
# (y <- resample_dataframe(
#     data = data,
#     resample_rate = 10, ## 10 Hz
#     resample_time = NULL,
#     nirs_columns,
#     sample_columns,
# ))
# attributes(y)
#
# library(tidyverse)
# library(JAPackage)
#
# ggplot(y) +
#     {list( ## Settings
#         aes(x = time, y = smo2_left),
#         coord_cartesian(
#             xlim = c(NA, 20),
#             ylim = c(0, 40)),
#         theme_JA(),
#         NULL)} + ## Settings
#     {list( ## Data
#         geom_line(),
#         geom_point(),
#         NULL)} ## Data
