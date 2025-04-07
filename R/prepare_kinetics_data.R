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
#' @param end_kinetics_window A numeric scalar specifying the number of
#' samples in which to look for a peak or nadir value indicating the kinetics
#' plateau.
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
#' repeated bouts.
#' @param ... Additional arguments.
#'
#' @details
#' ...
#'
#' @return A [tibble][tibble::tibble-package].
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
        end_kinetics_window = NULL,
        group_kinetics_events = c("distinct", "ensemble"),
        ...
) {

    ## functionality
    ## TODO
    ## define nirs_columns (or metadata)
    ## define sample column (or metadata)
    ## define event column (or metadata)
    ## define kinetics event
    ## - by sample number, index, or event label
    ## split into list of dataframes
    ## - or ensemble average

    ## if group_kinetics_events == "distinct", output is a list of dataframes
    ## if == "ensemble", output is a single dataframe with all ensembled
    ## if == list(c(1, 2), c(3, 4)), output is a list of ensembled dataframes

    ## Validation =================================
    metadata <- attributes(data)
    args <- list(...)

    ## define `nirs_columns`
    if ("nirs_columns" %in% names(args)) {
        ## priority is manually defined columns
        nirs_columns <- args$nirs_columns

    } else if (!is.null(attributes(data)$nirs_columns) &
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

    } else if (!is.null(attributes(data)$sample_column) &
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

    ## define `event_column`
    if ("event_column" %in% names(args)) {
        ## priority is manually defined columns
        event_column <- args$event_column

    } else if (!is.null(attributes(data)$event_column) &
               !"event_column" %in% names(args)) {
        ## otherwise take existing metadata columns
        event_column <- names(metadata$event_column)

    }

    ## validation: `event_column` must match expected dataframe names
    if (!all(unlist(event_column) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg event_column} not found. Make sure column names",
            "match exactly."))
    }

    ## validation: `fit_windows` must be numeric scalar
    purrr::map(
        c(fit_baseline_window, fit_kinetics_window),
        \(.x) if (!rlang::is_double(.x) | !length(.x) == 1) {

            cli::cli_abort(paste(
                "{.arg fit_windows} must each be a single",
                "{.cls numeric} scalar."))

        })

    ## define & validation: `display_baseline_window`
    if (is.null(display_baseline_window)) {

        display_baseline_window <- fit_baseline_window

    } else if (!rlang::is_double(display_baseline_window) |
               !length(display_baseline_window) == 1) {

        cli::cli_abort(paste(
            "{.arg display_baseline_window} must be a single",
            "{.cls numeric} scalar."))

    }

    ## define & validation: `display_kinetics_window`
    if (is.null(display_kinetics_window)) {

        display_kinetics_window <- fit_kinetics_window

    } else if (!rlang::is_double(display_kinetics_window) |
               !length(display_kinetics_window) == 1) {

        cli::cli_abort(paste(
            "{.arg display_kinetics_window} must be a single",
            "{.cls numeric} scalar."))

    }

    ## define & validation: inform `end_kinetics_window`
    if (is.null(end_kinetics_window)) {

        end_kinetics_window <-
            pmin(pmax(round(fit_kinetics_window * 0.15/15)*15, 15), 30)

        cli::cli_alert_info(paste(
            "{.arg end_kinetics_window} set to {.val {end_kinetics_window}}",
            "samples"
        ))
    } else if (!rlang::is_double(end_kinetics_window) |
               !length(end_kinetics_window) == 1) {

        cli::cli_abort(paste(
            "{.arg end_kinetics_window} must be a single",
            "{.cls numeric} scalar."))

    }
    #
    ## Event Indices ===================================

    ## re-define `index`
    data <- dplyr::mutate(data, index = dplyr::row_number())

    if (!is.null(event_sample)) {

        event_sample_index <- dplyr::filter(
            data,
            dplyr::if_any(
                dplyr::any_of(sample_column),
                \(.x) .x %in% event_sample
            )) |>
            dplyr::pull(index)

    } else {event_sample_index <- NULL}

    if (!is.null(event_label)) {

        event_label_index <- dplyr::filter(
            data,
            dplyr::if_any(
                dplyr::any_of(event_column),
                \(.x) grepl(paste(event_label, collapse = "|"),
                            .x, ignore.case = TRUE)
            )) |>
            dplyr::pull(index)

    } else {event_label_index <- NULL}

    event_index_list <- sort(
        c(event_index, event_sample_index, event_label_index))

    start_index <- -min(c(fit_baseline_window, display_baseline_window))
    end_index <- min(c(fit_kinetics_window, display_kinetics_window))

    data_list <- purrr::map(
        event_index_list,
        \(.x) {
            indices <- pmin(
                pmax(.x + start_index:end_index, 1),
                nrow(data)) |> unique()

            dplyr::slice(data, indices) |>
                dplyr::mutate(
                    display_index = index - .x
                ) |>
                dplyr::relocate(display_index)
        }
    )

    if (head(group_kinetics_events, 1) == "distinct") {

        y <- data_list

    } else if (head(group_kinetics_events, 1) == "ensemble") {

        y <- dplyr::bind_rows(data_list) |>
            dplyr::select(
                -c(index, dplyr::any_of(c(sample_column, event_column)))
            ) |>
            dplyr::arrange(display_index) |>
            dplyr::summarise(
                .by = display_index,
                dplyr::across(
                    dplyr::where(is.numeric),
                    \(.x) mean(.x, na.rm = TRUE))
            )

    } else if (rlang::is_double(unlist(group_kinetics_events))) {

        ungrouped_events <- setdiff(
            1:length(data_list), unlist(group_kinetics_events))


        for (i in seq_along(ungrouped_events)) {
            group_kinetics_events[[length(group_kinetics_events)+1]] <-
                ungrouped_events[i]
        }

        y <- purrr::map(
            if (is.list(group_kinetics_events)) {
                group_kinetics_events
            } else {list(group_kinetics_events)},
            \(.x) data_list[.x] |>
                dplyr::bind_rows() |>
                dplyr::select(
                    -c(index, dplyr::any_of(c(sample_column, event_column)))
                ) |>
                dplyr::arrange(display_index) |>
                dplyr::summarise(
                    .by = display_index,
                    dplyr::across(
                        dplyr::where(is.numeric),
                        \(.x) mean(.x, na.rm = TRUE))
                )
        )
    }


    return(y)
}
#
# data <- mNIRS::read_data(
#     file_path = "C:/OneDrive - UBC/Body Position Study/Raw Data/BP01-oxysoft-2025-04-01.xlsx",
#     nirs_columns = c("PS_O2Hb" = "2",
#                      "PS_HHb" = "3",
#                      "VL_O2Hb" = "5",
#                      "VL_HHb" = "6"),
#     sample_column = c("sample" = "1"),
#     event_column = c("event" = "10", "label" = "...11"),
#     .keep_all = FALSE)
#
# prepare_kinetics_data(
#     data,
#     # nirs_columns = c("PS_O2Hb"),
#     # event_column = "event",
#     # event_index = c(10, 20, 30),
#     # event_sample = c(1, 2, 3),
#     event_label = c("end RP", "end UP", "end stage"),
#     group_kinetics_events = list(c(1, 2), c(3, 4)) #"distinct"
# )
