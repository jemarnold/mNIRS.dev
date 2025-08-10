#' Prepare mNIRS Data for Kinetics Analysis
#'
#' Processes a dataframe of class `"mNIRS.data"` for kinetics analysis.
#'
#' @param data A dataframe of class `"mNIRS.data"`.
#' @param event_index (*Optional*). A numeric vector indicating the starting row
#'  indices of kinetics events. i.e., to identify the start of kinetic events by
#'  row number.
#' @param event_sample (*Optional*). A numeric vector corresponding to values of
#'  `sample_column` indicating the start of kinetic events. i.e., to identify
#'  the start of kinetic events by time value or sample number.
#' @param event_label (*Optional*). A character vector corresponding to values of
#'  `event_column` indicating the start of kinetics events. i.e., to identify
#'  the start of kinetic events by an event string such as *"end work"*.
#' @param fit_window A two-element numeric vector in the form `c(before, after)`
#'  in units of the `sample_column`, defining the window before and after the
#'  kinetics events to include in the model fitting process
#'  (*default = c(30, 180)*).
#' @param display_window (*Optional*). A two-element numeric vector in the form
#'  `c(before, after)` in units of the `sample_column`, defining the window before
#'  and after the kinetics events to include for display, but not for model
#'  fitting.
#' @param group_events Indicates how kinetics events should be analysed. Typically
#'  either *"distinct"* (*the default*) or *"ensemble"*, but can be manually
#'  specified (see *Details*).
#' @param ... Additional arguments.
#'
#' @details
#' `group_events` indicates how kinetics events should be analysed, either
#' separately, or grouped and ensemble averaged similar to oxygen uptake kinetics.
#'
#'  \describe{
#'      \item{`group_events = "distinct"`}{Will prepare a list of unique dataframes
#'      for each kinetics event (*default*).}
#'      \item{`group_events = "ensemble"`}{Will prepare one dataframe with the
#'      ensemble-averaged data from all mNIRS kinetics events.}
#'      \item{`group_events = list(c(1, 2), c(3, 4))`}{Will group kinetic events
#'      together in sequence of appearance, and prepare a list of ensemble-averaged
#'      dataframes for each group. Any kinetic events detected in the data but
#'      not explicitly defined here will return as a distinct dataframe.}
#'  }
#'
#' @return A list of [tibbles][tibble::tibble-package] of class `mNIRS.data`
#'  with metadata available with `attributes()`.
#'
#' @export
prepare_kinetics_data <- function(
        data,
        event_index = NULL, ## 59
        event_sample = NULL, ## c(3, 4)
        event_label = NULL, ## "end stage"
        fit_window = c(30, 180),
        display_window = NULL,
        group_events = list("distinct", "ensemble"),
        ...
) {
    ## Validation =================================
    metadata <- attributes(data)
    args <- list(...)

    ## define `nirs_columns`
    if ("nirs_columns" %in% names(args)) {
        ## priority is manually defined columns
        nirs_columns <- args$nirs_columns

    } else if (!is.null(metadata$nirs_columns)) {
        ## otherwise take existing metadata columns
        nirs_columns <- metadata$nirs_columns

    }

    ## validation: `nirs_columns` must match expected dataframe names
    if (!all(unlist(nirs_columns) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg nirs_columns} must be a list of names.",
            "Make sure column names match dataframe exactly."))
    }

    ## define `sample_column`
    if ("sample_column" %in% names(args)) {
        ## priority is manually defined columns
        sample_column <- args$sample_column

    } else if (!is.null(metadata$sample_column)) {
        ## otherwise take existing metadata columns
        sample_column <- metadata$sample_column

    } else {sample_column <- NULL}

    ## validation: `sample_column` must match expected dataframe names
    if (!all(unlist(sample_column) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg sample_column} not found. Make sure column names",
            "match dataframe exactly."))
    }

    ## define `event_column`
    if ("event_column" %in% names(args)) {
        ## priority is manually defined columns
        event_column <- args$event_column

    } else if (!is.null(metadata$event_column)) {
        ## otherwise take existing metadata columns
        event_column <- metadata$event_column

    } else {event_column <- NULL}

    ## validation: `event_column` must match expected dataframe names
    if (!all(unlist(event_column) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg event_column} not found. Make sure column names",
            "match dataframe exactly."))
    }

    ## fit windows ========================================
    ## validation: `fit_windows` must be numeric scalar
    if (!is.numeric(fit_window) | !length(fit_window) == 2) {
        cli::cli_abort(paste(
            "{.arg fit_window} must be a two-element {.cls numeric} vector",
            "{.val c(before, after)}."))
    }

    ## define & validation: `display_window`
    if (is.null(display_window)) {

        display_window <- fit_window

    } else if (!is.numeric(display_window) | !length(display_window) == 2) {

        cli::cli_abort(paste(
            "{.arg display_window} must be a two-element {.cls numeric} vector",
            "{.val c(before, after)}."))

    }
    #
    ## Event Indices ===================================

    if (!is.null(event_index)) {

        event_index_sample <- data |>
            dplyr::mutate(index = dplyr::row_number()) |>
            dplyr::filter(index %in% event_index) |>
            dplyr::pull(tidyselect::any_of(sample_column))

    } else {event_index_sample <- NULL}

    if (!is.null(event_label)) {

        event_label_sample <- data |>
            dplyr::filter(
                grepl(paste(event_label, collapse = "|"),
                      .data[[event_column]], ignore.case = TRUE)
            ) |>
            dplyr::pull(tidyselect::any_of(sample_column))

    } else {event_label_sample <- NULL}

    event_sample_list <- sort(unique(c(
        event_index_sample, event_sample, event_label_sample)))

    fit_window[1] <- -abs(fit_window[1])
    display_window[1] <- -abs(display_window[1])
    display_start <- min(c(fit_window[1], display_window[1]))
    display_end <- max(c(fit_window[2], display_window[2]))

    # display_column <- paste0("display_", sample_column)
    fit_column <- paste0("fit_", sample_column)

    ## Metadata =================================
    metadata$nirs_columns <- unique(
        c(metadata$nirs_columns, nirs_columns))
    metadata$sample_column <- sample_column
    metadata$event_column <- event_column
    metadata$event_sample_list <- event_sample_list
    metadata$fit_window <- fit_window
    metadata$display_window <- display_window

    ## data list =================================
    data_list <- lapply(
        event_sample_list,
        \(.x)
        data |>
            dplyr::mutate(
                display_sample = signif(.data[[sample_column]] - .x, 5)
            ) |>
            ## include the dataframe only within the display_window bounds
            dplyr::filter(
                dplyr::between(display_sample, display_start, display_end)
            ) |>
            ## create a new column with x values for model fitting, and NA
            ## where outside of fit_window bounds
            dplyr::mutate(
                "{fit_column}" := dplyr::if_else(
                    dplyr::between(display_sample, fit_window[1], fit_window[2]),
                    display_sample, NA_real_),
            ) |>
            dplyr::select(-c(display_sample)) |>
            dplyr::relocate(tidyselect::all_of(fit_column))
    )

    ensemble_data <- function(data) {
        data |>
            dplyr::bind_rows() |>
            dplyr::select(-tidyselect::any_of(event_column)) |>
            dplyr::summarise(
                .by = tidyselect::any_of(fit_column),
                dplyr::across(
                    tidyselect::where(is.numeric),
                    \(.x) mean(.x, na.rm = TRUE)),
                dplyr::across(
                    !tidyselect::where(is.numeric),
                    \(.x) dplyr::first(na.omit(.x))),
            ) |>
            dplyr::mutate(
                dplyr::across(
                    tidyselect::where(is.numeric),
                    \(.x) ifelse(.x %in% c(Inf, -Inf, NaN), NA_real_, .x)),
            )
    }

    if (head(unlist(group_events), 1) == "distinct") {

        kinetics_data_list <- lapply(
            data_list,
            \(.df) {
                kinetics_data <- .df |>
                    dplyr::relocate(tidyselect::any_of(c(
                        fit_column, sample_column, event_column, nirs_columns)))

                return(create_mNIRS.data(kinetics_data, metadata))
            }) |>
            setNames(paste0(sample_column, "_", event_sample_list))
        ## TODO 2025-07-19 set names based on events or sample or index number

    } else if (head(unlist(group_events), 1) == "ensemble") {

        kinetics_data_list <- list(
            create_mNIRS.data(ensemble_data(data_list), metadata))

    } else if (is.numeric(unlist(group_events))) {

        ungrouped_events <- setdiff(1:length(data_list), unlist(group_events))

        for (i in seq_along(ungrouped_events)) {
            group_events[[length(group_events) + 1]] <- ungrouped_events[i]
        }

        kinetics_data_list <- lapply(
            if (is.list(group_events)) {group_events} else {list(group_events)},
            \(.x)
            create_mNIRS.data(ensemble_data(data_list[.x]), metadata)
        ) |>
            setNames(paste0(sample_column, "_", event_sample_list))
    }

    ## TODO 2025-07-18 keep this as list() of one?
    # if (length(kinetics_data_list) == 1) {
    #     return(kinetics_data_list[[1]])
    # } else {
    return(kinetics_data_list)
    # }
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
#     group_events = "ensemble"
#     # group_events = list(c(1, 3, 5), c(2, 4)) #"ensemble"
# ))
# attributes(data_list[[1]])
