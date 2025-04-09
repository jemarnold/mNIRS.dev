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
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
resample_dataframe <- function(
        data,
        sample_rate,
        resample_rate=NULL, ## 10 Hz
        resample_time=NULL, ## 0.01 s
        ...
) {
    ## pass through =============================

    if (is.null(resample_rate) & is.null(resample_time)) {
        return(data)
    }


    ## Validation =================================
    metadata <- attributes(data)
    args <- list(...)

    ## define `nirs_columns`
    if ("nirs_columns" %in% names(args)) {
        ## priority is manually defined columns
        nirs_columns <- args$nirs_columns

    } else if (!is.null(metadata$nirs_columns) &
               !"nirs_columns" %in% names(args)) {
        ## otherwise take existing metadata columns
        nirs_columns <- names(metadata$nirs_columns)

    }

    ## validation: `nirs_columns` must match expected dataframe names
    if (!all(unlist(nirs_columns) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg nirs_columns} must be a list of names.",
            "Make sure column names match exactly."))
    }

    ## define `sample_column`
    if ("sample_column" %in% names(args)) {
        ## priority is manually defined columns
        sample_column <- args$sample_column

    } else if (!is.null(metadata$sample_column) &
               !"sample_column" %in% names(args)) {
        ## otherwise take existing metadata columns
        sample_column <- names(metadata$sample_column)

    }

    ## validation: `sample_column` must match expected dataframe names
    if (!all(unlist(sample_column) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg sample_column} not found. Make sure column names",
            "match exactly."))
    }

    sample_vector <- as.numeric(data[[sample_column]])

    ## define `sample_rate`
    if (!is.null(metadata$sample_rate)) {

        sample_rate <- metadata$sample_rate

    } else {

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
        dplyr::select(-dplyr::matches("index")) |>
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
            ## weighted mean value for specified `nirs_columns`
            dplyr::across(
                dplyr::any_of(nirs_columns),
                \(.x) weighted.mean(.x, delta_sample, na.rm = TRUE)),
            ## take the first non-na value from other columns
            dplyr::across(
                !dplyr::any_of(nirs_columns),
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
                dplyr::any_of(nirs_columns),
                \(.x) mNIRS::replace_missing_values(
                    .x, method = "linear", na.rm = TRUE)$y),
            ## take the first non-na value from other columns
            dplyr::across(
                !dplyr::any_of(nirs_columns),
                \(.x) mNIRS::replace_missing_values(
                    .x, method = "locf", na.rm = FALSE)$y),
        )

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
#     resample_time = NULL
# ))
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
