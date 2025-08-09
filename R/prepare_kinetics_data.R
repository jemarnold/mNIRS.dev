#' Prepare mNIRS Data for Kinetics Analysis
#'
#' Processes a dataframe for kinetics analysis.
#'
#' @param data A dataframe.
#' @param event_index (*Optional*). A numeric vector indicating the start
#'  indices of kinetics events. Any of `event_label`, `event_index`, or
#'  `event_sample` will be passed along for further analysis.
#' @param event_sample (*Optional*). A vector in the same class as
#'  `sample_column` indicating the start of kinetic events.
#' @param event_label (*Optional*). A character vector specified in
#'  `event_column` indicating the start of kinetics events.
#' @param fit_window A two-element numeric vector defining the number of
#'  samples around the kinetics events to include in the fitting process (see
#'  *Details*).
#' @param display_window (*Optional*). A two-element numeric vector defining
#'  the number of samples around the kinetics events to include for display (see
#'  *Details*).
#' @param group_events Indicates how kinetics events should be analysed
#'  (see *Details*).
#' @param ... Additional arguments.
#'
#' @details
#' `fit_window` is a two-element vector `c(before, after)` specifying the number of
#' samples before and after the kinetics event to include in the kinetics fitting
#' process.
#'
#' `display_window` is a two-element vector `c(before, after)` specifying the
#' number of samples before and after the kinetics event to include in the prepared
#' kinetics dataframes, to allow for plotting of data beyond the bounds of the
#' fitting process.
#'
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
#'      not explicitly defined here will return as un-grouped dataframes.}
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
            "{.arg fit_window} must be a two-element {.cls numeric} vector",
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
            dplyr::filter(dplyr::if_any(
                tidyselect::any_of(event_column),
                \(.x) grepl(paste(event_label, collapse = "|"),
                            .x, ignore.case = TRUE)
            )) |>
            dplyr::pull(tidyselect::any_of(sample_column))

    } else {event_label_sample <- NULL}

    event_sample_list <- sort(c(event_index_sample,
                                event_sample,
                                event_label_sample))

    fit_window[1] <- -abs(fit_window[1])
    display_window[1] <- -abs(display_window[1])
    start_sample <- min(c(fit_window[1], display_window[1]))
    end_sample <- max(c(fit_window[2], display_window[2]))

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
        \(.x) {
            data |>
                ## TODO 2025-07-18 verify dplyr methods
                # dplyr::mutate(
                #     "{sample_column}" := signif(.data[[sample_column]] - .x, 5)
                # ) |>
                # dplyr::filter(
                #     dplyr::between(.data[[sample_column]], start_sample, end_sample)
                # ) |>
                dplyr::mutate(
                    ## overwrite sample_column to start locally from 0 at
                    ## kinetics event
                    dplyr::across(
                        tidyselect::any_of(sample_column),
                        \(.xx) signif(.xx - .x, 5),
                        .names = "display_sample")
                ) |>
                dplyr::filter(
                    dplyr::between(display_sample, start_sample, end_sample)
                ) |>
                dplyr::mutate(
                    dplyr::across(
                        display_sample,
                        \(.xx) dplyr::if_else(
                            dplyr::between(.xx, fit_window[1], fit_window[2]),
                            .xx, NA),
                        .names = fit_column),
                ) |>
                dplyr::select(-display_sample) |>
                dplyr::relocate(tidyselect::any_of(fit_column))
        })

    if (head(unlist(group_events), 1) == "distinct") {

        kinetics_data_list <- lapply(
            data_list,
            \(.df) {
                kinetics_data <- .df |>
                    dplyr::relocate(tidyselect::any_of(c(
                        fit_column, sample_column, event_column, nirs_columns)))

                kinetics_data <- create_mNIRS.data(kinetics_data, metadata)

                return(kinetics_data)
            }) |>
            setNames(paste0(sample_column, "_", event_sample_list))
        ## TODO 2025-07-19 set names based on events or sample or index number

    } else if (head(unlist(group_events), 1) == "ensemble") {

        kinetics_data <- data_list |>
            dplyr::bind_rows() |>
            dplyr::select(-tidyselect::any_of(c(display_sample, event_column))) |>
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
            ) |>
            dplyr::relocate(tidyselect::any_of(fit_column))

        kinetics_data_list <- list(create_mNIRS.data(kinetics_data, metadata))

    } else if (is.numeric(unlist(group_events))) {

        ungrouped_events <- setdiff(
            1:length(data_list), unlist(group_events))

        for (i in seq_along(ungrouped_events)) {
            group_events[[length(group_events)+1]] <- ungrouped_events[i]
        }

        kinetics_data_list <- lapply(
            if (is.list(group_events)) {
                group_events
            } else {list(group_events)},
            \(.x) {
                kinetics_data <- data_list[.x] |>
                    dplyr::bind_rows() |>
                    dplyr::select(
                        -tidyselect::any_of(c(display_sample, event_column))
                    ) |>
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
                    ) |>
                    dplyr::relocate(
                        tidyselect::any_of(fit_column))

                kinetics_data <- create_mNIRS.data(kinetics_data, metadata)

                return(kinetics_data)
            }) |>
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
