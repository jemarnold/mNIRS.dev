#' Prepare mNIRS Data for Kinetics Analysis
#'
#' Processes a list of one or more dataframes of class `"mNIRS.data"` representing
#' distinct or ensembled kinetics events for further analysis.
#'
#' @param data A dataframe of class `"mNIRS.data"`.
#' @param event_sample An *optional* numeric vector corresponding to values of
#'  `sample_column` indicating the start of kinetic events. i.e., by time value
#'  or sample number.
#' @param event_label An *optional* character vector corresponding to values of
#'  `event_column` indicating the start of kinetics events. i.e., by an event
#'  label such as *"end work"*.
#' @param event_index An *optional* numeric vector indicating the starting row
#'  indices of kinetics events. i.e., to identify the start of kinetic events by
#'  row number.
#' @param fit_window A two-element numeric vector in the form `c(before, after)`
#'  in units of `sample_column`, defining the window around the kinetics events
#'  to include in the model fitting process (*default = c(30, 180)*).
#' @param display_window (*Not currently implemented*)
#'  An *optional* two-element numeric vector in the form
#'  `c(before, after)` in units of `sample_column`, defining the window around
#'  the kinetics events to include for display, but not for model fitting.
#' @param peak_window A numeric scalar indicating the local window in units of
#'  `sample_column` after the kinetics peak (trough) value to look for subsequent
#'  higher (lower) values. The kinetics model will be fit to the data up to the
#'  first local peak (trough) value with no subsequent higher (lower) values
#'  within the lesser of either the `peak_window` or the limits of `fit_window`.
#' @param group_events Indicates how kinetics events should be analysed. Typically
#'  either *"distinct"* (*the default*) or *"ensemble"*, but can be manually
#'  specified (see *Details*).
#' @param nirs_columns An *optional* character vector indicating the mNIRS data
#'  columns to be processed from your dataframe. Must match exactly. Defining
#'  column names explicitly will overwrite metadata.
#' @param sample_column An *optional* character scalar indicating the name of
#'  a time or sample data column. Must match exactly. Defining
#'  column names explicitly will overwrite metadata.
#' @param event_column An *optional* character scalar indicating the name of
#'  an event or lap data column. Must match exactly. Defining
#'  column names explicitly will overwrite metadata.
#' @param ... Additional arguments.
#'
#' @details
#' `display_window` defines the widest range of data before and after the kinetics
#'  event which will be passed on in the dataframe, but not necessarily included
#'  in the modelling process. `fit_window` defines the widest extent of data
#'  before and after the kinetics event which will be included in the modelling
#'  process. `peak_window` limits the extent of data included to be modelled after
#'  the kinetics event to the first local peak (trough) value of the response
#'  variable (i.e. `y`), with no higher (lower) values within that window.
#'
#' `group_events` indicates how kinetics events should be analysed, either
#'  separately, or grouped and ensemble averaged similar to oxygen uptake kinetics.
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
        event_sample = NULL,
        event_label = NULL,
        event_index = NULL,
        fit_window = c(30, 180),
        display_window = NULL,
        peak_window = 30,
        group_events = list("distinct", "ensemble"),
        nirs_columns = NULL,
        sample_column = NULL,
        event_column = NULL,
        ...
) {
    ## Validation =================================
    metadata <- attributes(data)
    args <- list(...)

    ## define `nirs_columns` manually overrides metadata
    if (is.null(nirs_columns) & is.null(metadata$nirs_columns)) {
        cli::cli_abort(paste(
            "{.arg nirs_columns} not found in metadata. Please check your data",
            "attributes or define {.arg nirs_columns} explicitly."))
    } else if (is.null(nirs_columns) & !is.null(metadata$nirs_columns)) {
        nirs_columns <- metadata$nirs_columns
    } else if (!is.character(nirs_columns) || !all(nirs_columns %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg nirs_columns} must be a vector of column names within",
            "{.arg data}. Make sure column names match exactly."))
    }

    ## define `sample_column` manually overrides metadata
    if (is.null(sample_column) & is.null(metadata$sample_column)) {
        cli::cli_abort(paste(
            "{.arg sample_column} not found in metadata. Please check your data",
            "attributes or define {.arg sample_column} explicitly."))
    } else if (is.null(sample_column) & !is.null(metadata$sample_column)) {
        sample_column <- metadata$sample_column
    } else if (!is.character(sample_column) || !sample_column %in% names(data)) {
        cli::cli_abort(paste(
            "{.arg sample_column} must be a column name within your {.arg data}.",
            "Make sure column names match exactly."))
    }

    ## define `event_column` manually overrides metadata
    if (is.null(event_label)) {
        ## this is probably a bad way to write this
    } else if (is.null(event_column) & is.null(metadata$event_column)) {
        cli::cli_abort(paste(
            "You have defined {.arg event_label} but {.arg event_column}",
            "not found in metadata. Please check your data attributes or define",
            "{.arg event_column} explicitly."))
    } else if (is.null(event_column) & !is.null(metadata$event_column)) {
        event_column <- metadata$event_column
    } else if (!is.character(event_column) || !event_column %in% names(data)) {
        cli::cli_abort(paste(
            "{.arg event_column} must be a column name within your {.arg data}.",
            "Make sure column names match exactly."))
    }

    ## validation: `fit_windows` must be numeric scalar
    if (!is.numeric(fit_window) || !length(fit_window) == 2) {
        cli::cli_abort(paste(
            "{.arg fit_window} must be a two-element {.cls numeric} vector",
            "{.val c(before, after)}."))
    }

    ## define & validation: `display_window`
    ## TODO 2025-08-12 NOT CURRENTLY IMPLEMENTED
    # if (is.null(display_window)) {
    display_window <- fit_window
    # } else if (!is.numeric(display_window) || !length(display_window) == 2) {
    #     cli::cli_abort(paste(
    #         "{.arg display_window} must be a two-element {.cls numeric} vector",
    #         "{.val c(before, after)}."))
    # }
    #
    ## Event Indices ===================================

    if (!is.null(event_index)) {

        event_index_sample <- data[[sample_column]][event_index]

    } else {event_index_sample <- NULL}

    if (!is.null(event_label)) {

        pattern <- paste(event_label, collapse = "|")
        matches <- grepl(pattern, data[[event_column]], ignore.case = TRUE)
        event_label_sample <- data[[sample_column]][matches]

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
    ## TODO 2025-08-12 NOT CURRENTLY IMPLEMENTED
    # metadata$display_window <- display_window

    ## data list =================================
    data_list <- lapply(
        event_sample_list,
        \(.x) {
            ## sample vector zeroed to the kinetics event
            ## TODO 2025-08-12 display_sample currently not implemented in
            ## outgoing data_list. Only being used to define fit_sample
            ## round to avoid floating point error
            display_sample <- round(data[[sample_column]] - .x, 8)
            keep_idx <- display_sample >= display_start & display_sample <= display_end

            ## filter data & display_sample vector within the kinetics event window
            event_data <- data[keep_idx, ]
            display_sample <- display_sample[keep_idx]

            ## create new fit_sample column named from `fit_column`
            ## as a sample vector zeroed to the kinetics event
            ## filtered within the `fit_window`
            event_data[[fit_column]] <- ifelse(
                display_sample >= fit_window[1] & display_sample <= fit_window[2],
                display_sample, NA_real_)

            ## relocates `fit_column` as the first col, includes remaining cols
            event_data[c(fit_column, setdiff(names(event_data), fit_column))]
        })

    ensemble_data <- function(data) {
        data |>
            dplyr::bind_rows() |>
            dplyr::select(-tidyselect::any_of(c(sample_column, event_column))) |>
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
                    \(.x) ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x)),
            )
    }

    if (length(data_list) == 1 || head(unlist(group_events), 1) == "distinct") {

        kinetics_data_list <- lapply(
            data_list,
            \(.df) {
                cols <- c(fit_column, sample_column, event_column, nirs_columns)
                kinetics_data <- .df[c(cols, setdiff(names(.df), cols))]

                return(create_mNIRS_data(kinetics_data, metadata))
            }) |>
            setNames(paste0(sample_column, "_", event_sample_list))
        ## TODO 2025-07-19 set names based on events or sample or index number

    } else if (head(unlist(group_events), 1) == "ensemble") {

        kinetics_data_list <- list(
            create_mNIRS_data(ensemble_data(data_list), metadata)) |>
            setNames("ensemble")

    } else if (is.numeric(unlist(group_events))) {

        ungrouped_events <- setdiff(seq_along(data_list), unlist(group_events))

        for (i in seq_along(ungrouped_events)) {
            group_events[[length(group_events) + 1]] <- ungrouped_events[i]
        }

        kinetics_data_list <- lapply(
            if (is.list(group_events)) {group_events} else {list(group_events)},
            \(.x)
            create_mNIRS_data(ensemble_data(data_list[.x]), metadata)
        ) |>
            setNames(lapply(group_events, paste, collapse = "_"))
    }

    ## TODO 2025-07-18 keep this as list() of one?
    # if (length(kinetics_data_list) == 1) {
    #     return(kinetics_data_list[[1]])
    # } else {
    return(kinetics_data_list)
    # }
}
