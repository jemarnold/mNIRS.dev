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
#' repeated bouts. Any bout numbers not listed explicity will return un-grouped
#' dataframes.
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
        event_column <- names(metadata$event_column)

    } else {event_column <- NULL}

    ## validation: `event_column` must match expected dataframe names
    if (!all(unlist(event_column) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg event_column} not found. Make sure column names",
            "match exactly."))
    }

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

    ## re-define `index`
    data_intake <- data |>
        dplyr::select(
            dplyr::any_of(c(sample_column, event_column, nirs_columns))) |>
        dplyr::mutate(index = dplyr::row_number())

    if (!is.null(event_sample)) {

        event_sample_index <- dplyr::filter(
            data_intake,
            dplyr::if_any(
                dplyr::any_of(sample_column),
                \(.x) .x %in% event_sample
            )) |>
            dplyr::pull(index)

    } else {event_sample_index <- NULL}

    if (!is.null(event_label)) {

        event_label_index <- dplyr::filter(
            data_intake,
            dplyr::if_any(
                dplyr::any_of(event_column),
                \(.x) grepl(paste(event_label, collapse = "|"),
                            .x, ignore.case = TRUE)
            )) |>
            dplyr::pull(index)

    } else {event_label_index <- NULL}

    event_index_list <- sort(
        c(event_index, event_sample_index, event_label_index))

    fit_baseline_window <- -max(abs(fit_baseline_window))
    display_baseline_window <- -max(abs(display_baseline_window))
    start_index <- min(c(fit_baseline_window, display_baseline_window))
    end_index <- max(c(fit_kinetics_window, display_kinetics_window))

    data_list <- purrr::map(
        event_index_list,
        \(.x) {
            indices <- pmin(
                pmax(.x + start_index:end_index, 1),
                nrow(data_intake)) |> unique()

            data_intake |>
                dplyr::select(-index) |>
                dplyr::mutate(
                    display_index = dplyr::row_number() - .x,
                ) |>
                dplyr::slice(indices) |>
                dplyr::mutate(
                    fit_index = dplyr::if_else(
                        dplyr::between(
                            display_index,
                            fit_baseline_window,
                            fit_kinetics_window),
                        display_index, NA),
                ) |>
                dplyr::relocate(display_index, fit_index)
        }
    )

    if (head(group_kinetics_events, 1) == "distinct") {

        y <- purrr::map(
            data_list,
            \(.df)
            .df |>
                dplyr::relocate(
                    display_index, fit_index,
                    dplyr::any_of(
                        c(sample_column, event_column, nirs_columns)))
        )

    } else if (head(group_kinetics_events, 1) == "ensemble") {

        y <- data_list |>
            dplyr::bind_rows() |>
            dplyr::summarise(
                .by = display_index,
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
            dplyr::relocate(
                display_index, fit_index,
                dplyr::any_of(c(sample_column, event_column, nirs_columns)))

    } else if (is.numeric(unlist(group_kinetics_events))) {

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
                dplyr::summarise(
                    .by = display_index,
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
                dplyr::relocate(
                    display_index, fit_index,
                    dplyr::any_of(c(sample_column, event_column, nirs_columns)))
        )
    }
    #
    ## metadata =======================================
    # attributes(y[[1]])
    # # metadata$nirs_columns <-
    # # metadata$sample_column <-
    # # metadata$event_column <-
    # metadata$event_index <- event_index_list
    # metadata$fit_window <- c(
    #     -abs(fit_baseline_window), fit_kinetics_window)
    # metadata$display_window <- c(
    #     -abs(display_baseline_window), display_kinetics_window)
    # #
    # processed_data <- create_mnirs_data(y, metadata)

    return(y)
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
# attributes(data)
# # #
# (data_list <- mNIRS::prepare_kinetics_data(
#     data,
#     nirs_columns = c("PS_HHb", "VL_HHb"),
#     # sample_column = NULL,
#     # event_column = "label",
#     # event_index = c(1000),
#     event_label = c("end RP", "end UP", "end stage"),
#     fit_baseline_window = 30,
#     fit_kinetics_window = 180,
#     display_baseline_window = 40,
#     display_kinetics_window = 240,
#     group_kinetics_events = list(c(1, 3, 5), c(2, 4)) #"ensemble"
# ))
