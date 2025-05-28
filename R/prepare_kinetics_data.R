#' Prepare mNIRS Data for Kinetics Analysis
#'
#' Processes a dataframe for kinetics analysis
#'
#' @param data A dataframe containing mNIRS data.
#' @param event_index (*optional*) A numeric vector indicating the start
#' indices of kinetics events.
#'
#' - Any of `event_label`, `event_index`, or `event_sample` will be passed
#' along for further analysis.
#' @param event_sample (*optional*) A vector in the same class as
#' `sample_column` indicating the start of kinetic events.
#' @param event_label (*optional*) A character vector specified in
#' `event_column` indicating the start of kinetics events.
#' @param fit_baseline_window A numeric scalar specifying the number of
#' samples preceding to the kinetics events to include as baseline.
#' @param fit_kinetics_window A numeric scalar specifying the number of
#' samples following to the kinetics events to include for kinetics modelling.
#' @param display_baseline_window A numeric scalar specifying the number of
#' samples preceding to the kinetics events to include for display.
#' @param display_kinetics_window A numeric scalar specifying the number of
#' samples following to the kinetics events to include for display.
#' @param group_kinetics_events Indicates whether kinetics events should
#' be analysed separately or together.
#'
#' - `"distinct"` (*the default*) will prepare a list of unique dataframes for
#' each kinetics event.
#' - `"ensemble"`` will prepare one dataframe with the ensemble-averaged mNIRS
#' data from all kinetics events (similar to pulmonary VO2 kinetics).
#' - Alternatively, a list of numeric vectors can be specified to
#' ensemble-average groups of kinetics events together. Such as
#' `group_kinetics_events = list(c(1, 2), c(3, 4))` for two sets of
#' repeated bouts. Any bout numbers not listed explicitly will return
#' un-grouped dataframes.
#' @param ... Additional arguments.
#'
#' @details
#' ...
#'
#' @return A list of [tibbles][tibble::tibble-package] of class `mNIRS.data`
#' with metadata available with `attributes()`.
#'
#' @export
prepare_kinetics_data <- function(
        data,
        event_index = NULL, ## 59
        event_sample = NULL, ## c(3, 4)
        event_label = NULL, ## "end stage"
        fit_baseline_window = 30,
        fit_kinetics_window = 180,
        display_baseline_window = NULL,
        display_kinetics_window = NULL,
        group_kinetics_events = list("distinct", "ensemble"),
        ...
) {
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
        nirs_columns <- metadata$nirs_columns

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
        sample_column <- metadata$sample_column

    } else {sample_column <- NULL}

    ## validation: `sample_column` must match expected dataframe names
    if (!all(unlist(sample_column) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg sample_column} not found. Make sure column names",
            "match exactly."))
    }

    ## define `event_column`
    if ("event_column" %in% names(args)) {
        ## priority is manually defined columns
        event_column <- args$event_column

    } else if (!is.null(metadata$event_column) &
               !"event_column" %in% names(args)) {
        ## otherwise take existing metadata columns
        event_column <- metadata$event_column

    } else {event_column <- NULL}

    ## validation: `event_column` must match expected dataframe names
    if (!all(unlist(event_column) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg event_column} not found. Make sure column names",
            "match exactly."))
    }

    ## fit windows ========================================
    ## validation: `fit_windows` must be numeric scalar
    purrr::map(
        c(fit_baseline_window, fit_kinetics_window),
        \(.x) if (!is.numeric(.x) | !length(.x) == 1) {

            cli::cli_abort(paste(
                "{.arg fit_windows} must each be a single",
                "{.cls numeric} scalar."))

        })

    ## define & validation: `display_baseline_window`
    if (is.null(display_baseline_window)) {

        display_baseline_window <- fit_baseline_window

    } else if (!is.numeric(display_baseline_window) |
               !length(display_baseline_window) == 1) {

        cli::cli_abort(paste(
            "{.arg display_baseline_window} must be a single",
            "{.cls numeric} scalar."))

    }

    ## define & validation: `display_kinetics_window`
    if (is.null(display_kinetics_window)) {

        display_kinetics_window <- fit_kinetics_window

    } else if (!is.numeric(display_kinetics_window) |
               !length(display_kinetics_window) == 1) {

        cli::cli_abort(paste(
            "{.arg display_kinetics_window} must be a single",
            "{.cls numeric} scalar."))

    }
    #
    ## Event Indices ===================================

    if (!is.null(event_index)) {

        event_index_sample <- data |>
            dplyr::mutate(index = dplyr::row_number()) |>
            dplyr::filter(index %in% event_index) |>
            dplyr::pull(dplyr::any_of(sample_column))

    } else {event_index_sample <- NULL}

    if (!is.null(event_label)) {

        event_label_sample <- data |>
            dplyr::filter(dplyr::if_any(
                dplyr::any_of(event_column),
                \(.x) grepl(paste(event_label, collapse = "|"),
                            .x, ignore.case = TRUE)
            )) |>
            dplyr::pull(dplyr::any_of(sample_column))

    } else {event_label_sample <- NULL}

    event_sample_list <- sort(
        c(event_index_sample, event_sample, event_label_sample))

    fit_baseline_window <- -max(abs(fit_baseline_window))
    display_baseline_window <- -max(abs(display_baseline_window))
    start_sample <- min(c(fit_baseline_window, display_baseline_window))
    end_sample <- max(c(fit_kinetics_window, display_kinetics_window))

    display_column <- paste0("display_", sample_column)
    fit_column <- paste0("fit_", sample_column)

    ## Metadata =================================
    metadata$nirs_columns <- unique(
        c(metadata$nirs_columns, nirs_columns))
    metadata$sample_column <- sample_column
    metadata$event_column <- event_column
    metadata$event_sample_list <- event_sample_list
    metadata$fit_window <- c(fit_baseline_window,
                             fit_kinetics_window)
    metadata$display_window <- c(display_baseline_window,
                                 display_kinetics_window)

    ## data list =================================
    data_list <- purrr::map(
        event_sample_list,
        \(.x) data |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(sample_column),
                    \(.xx) .xx - .x,
                    .names = display_column)
            ) |>
            dplyr::filter(
                dplyr::if_any(
                    dplyr::any_of(display_column),
                    \(.xx) dplyr::between(.xx, start_sample, end_sample))
            ) |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(display_column),
                    \(.xx) dplyr::if_else(
                        dplyr::between(
                            .xx,
                            fit_baseline_window,
                            fit_kinetics_window),
                        .xx, NA),
                    .names = fit_column),
            ) |>
            dplyr::relocate(dplyr::any_of(c(display_column, fit_column)))
    )

    if (head(unlist(group_kinetics_events), 1) == "distinct") {

        kinetics_data_list <- purrr::map(
            data_list,
            \(.df) {
                kinetics_data <- .df |>
                    dplyr::relocate(dplyr::any_of(c(
                        display_column, fit_column,
                        sample_column, event_column, nirs_columns)))

                kinetics_data <- create_mnirs_data(kinetics_data, metadata)

                return(kinetics_data)
            })

    } else if (head(unlist(group_kinetics_events), 1) == "ensemble") {

        kinetics_data <- data_list |>
            dplyr::bind_rows() |>
            dplyr::select(
                -dplyr::any_of(c(sample_column, event_column))
            ) |>
            dplyr::summarise(
                .by = dplyr::any_of(display_column),
                dplyr::across(
                    dplyr::where(is.numeric),
                    \(.x) mean(.x, na.rm = TRUE)),
                dplyr::across(
                    !dplyr::where(is.numeric),
                    \(.x) dplyr::first(na.omit(.x))),
            ) |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::where(is.numeric),
                    \(.x) ifelse(.x %in% c(Inf, -Inf, NaN), NA_real_, .x)),
            ) |>
            dplyr::relocate(dplyr::any_of(c(display_column, fit_column)))

        kinetics_data_list <- list(create_mnirs_data(kinetics_data, metadata))

    } else if (is.numeric(unlist(group_kinetics_events))) {

        ungrouped_events <- setdiff(
            1:length(data_list), unlist(group_kinetics_events))

        for (i in seq_along(ungrouped_events)) {
            group_kinetics_events[[length(group_kinetics_events)+1]] <-
                ungrouped_events[i]
        }

        kinetics_data_list <- purrr::map(
            if (is.list(group_kinetics_events)) {
                group_kinetics_events
            } else {list(group_kinetics_events)},
            \(.x) {
                kinetics_data <- data_list[.x] |>
                    dplyr::bind_rows() |>
                    dplyr::select(
                        -dplyr::any_of(c(sample_column, event_column))
                    ) |>
                    dplyr::summarise(
                        .by = dplyr::any_of(display_column),
                        dplyr::across(
                            dplyr::where(is.numeric),
                            \(.x) mean(.x, na.rm = TRUE)),
                        dplyr::across(
                            !dplyr::where(is.numeric),
                            \(.x) dplyr::first(na.omit(.x))),
                    ) |>
                    dplyr::mutate(
                        dplyr::across(
                            dplyr::where(is.numeric),
                            \(.x) ifelse(.x %in% c(Inf, -Inf, NaN), NA_real_, .x)),
                    ) |>
                    dplyr::relocate(dplyr::any_of(c(display_column, fit_column)))

                kinetics_data <- create_mnirs_data(kinetics_data, metadata)

                return(kinetics_data)
            })
    }

    return(kinetics_data_list)
}
#
# (data <- mNIRS::read_data(
#     file_path = "C:/OneDrive - UBC/Body Position Study/Raw Data/BP01-oxysoft-2025-04-01.xlsx",
#     nirs_columns = c("PS_O2Hb" = "2",
#                      "PS_HHb" = "3",
#                      "VL_O2Hb" = "5",
#                      "VL_HHb" = "6"),
#     sample_column = c("sample" = "1"),
#     event_column = c("label" = "...11"),
#     .keep_all = FALSE))
# # # attributes(data)
# # # # # # #
# (data_list <- mNIRS::prepare_kinetics_data(
#     data,
#     nirs_columns = c("PS_HHb", "VL_HHb"),
#     sample_column = "sample",
#     event_column = "label",
#     event_index = NULL,
#     event_sample = NULL,
#     event_label = c("end RP", "end UP", "end stage"),
#     fit_baseline_window = 30,
#     fit_kinetics_window = 180,
#     display_baseline_window = 40,
#     display_kinetics_window = 240,
#     group_kinetics_events = "ensemble"
#     # group_kinetics_events = list(c(1, 3, 5), c(2, 4)) #"ensemble"
# ))
# attributes(data_list[[1]])
