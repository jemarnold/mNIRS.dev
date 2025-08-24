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
#' @param fit_span A two-element numeric vector in the form `c(before, after)`
#'  in units of `sample_column`, defining the window around the kinetics events
#'  to include in the model fitting process (*default* `fit_span = c(30, 180)`).
#' @param display_span *`<under development>`*.
# #'  An *optional* two-element numeric vector in the form
# #'  `c(before, after)` in units of `sample_column`, defining the window around
# #'  the kinetics events to include for display, but not for model fitting.
#' @param group_events Indicates how kinetics events should be analysed. Typically
#'  either *"distinct"* (the *default*) or *"ensemble"*, but can be manually
#'  specified (see *Details*).
#' @param nirs_columns A character vector indicating the mNIRS data columns to
#'  be processed from your dataframe. Must match `data` column names exactly.
#'  Will be taken from metadata if not defined explicitly.
#' @param sample_column A character scalar indicating the time or sample data
#'  column. Must match `data` column names exactly. Will be taken from metadata
#'  if not defined explicitly.
#' @param event_column An *optional* character scalar indicating an event or
#'  lap data column. Must match `data` column names exactly. Will be taken from
#'  metadata if not defined explicitly.
#' @param sample_rate A numeric scalar for the sample rate in Hz. Will be taken
#'  from metadata if not defined explicitly.
#' @param ... Additional arguments.
#'
#' @details
#' `display_span` defines the widest range of data before and after the kinetics
#'  event which will be passed on in the dataframe, but not included in the
#'  modelling process. `fit_span` defines the widest extent of data before and
#'  after the kinetics event which may be included in the modelling process.
#'
#' `group_events` indicates how kinetics events should be analysed, either
#'  separately, or grouped and ensemble averaged similar to oxygen uptake kinetics.
#'  \describe{
#'      \item{`group_events = `*`"distinct"`*}{Will prepare a list of unique dataframes
#'      for each kinetics event (*default*).}
#'      \item{`group_events = `*`"ensemble"`*}{Will prepare one dataframe with the
#'      ensemble-averaged data from all mNIRS kinetics events.}
#'      \item{`group_events = list(c(1, 2), c(3, 4))`}{Will group kinetic events
#'      together in sequence of appearance, and prepare a list of ensemble-averaged
#'      dataframes for each group. Any kinetic events detected in the data but
#'      not explicitly defined here will return as a distinct dataframe.}
#'  }
#'
#' @return A list of [tibbles][tibble::tibble-package] of class `"mNIRS.data"`
#'  with metadata available with `attributes()`.
#'
#' @export
prepare_kinetics_data <- function(
        data,
        event_sample = NULL,
        event_label = NULL,
        event_index = NULL,
        fit_span = c(30, 180),
        display_span = NULL,
        group_events = list("distinct", "ensemble"),
        nirs_columns = NULL,
        sample_column = NULL,
        event_column = NULL,
        sample_rate = NULL,
        ...
) {
    ## Validation =================================
    metadata <- attributes(data)
    args <- list(...)

    ## define `nirs_columns` manually overrides metadata
    if (is.null(nirs_columns) & is.null(metadata$nirs_columns)) {
        cli_abort(paste(
            "{.arg nirs_columns} not found in metadata. Please check your data",
            "attributes or define {.arg nirs_columns} explicitly."))
    } else if (is.null(nirs_columns) & !is.null(metadata$nirs_columns)) {
        nirs_columns <- metadata$nirs_columns
    } else if (!is.character(nirs_columns) || !all(nirs_columns %in% names(data))) {
        cli_abort(paste(
            "{.arg nirs_columns} must be a vector of column names within",
            "{.arg data}. Make sure column names match exactly."))
    }

    ## define `sample_column` manually overrides metadata
    if (is.null(sample_column) & is.null(metadata$sample_column)) {
        cli_abort(paste(
            "{.arg sample_column} not found in metadata. Please check your data",
            "attributes or define {.arg sample_column} explicitly."))
    } else if (is.null(sample_column) & !is.null(metadata$sample_column)) {
        sample_column <- metadata$sample_column
    } else if (!is.character(sample_column) || !sample_column %in% names(data)) {
        cli_abort(paste(
            "{.arg sample_column} must be a column name within your {.arg data}.",
            "Make sure column names match exactly."))
    }

    ## define `event_column` manually overrides metadata
    if (is.null(event_label)) {
        ## this is probably a bad way to write this
    } else if (is.null(event_column) & is.null(metadata$event_column)) {
        cli_abort(paste(
            "You have defined {.arg event_label} but {.arg event_column}",
            "not found in metadata. Please check your data attributes or define",
            "{.arg event_column} explicitly."))
    } else if (is.null(event_column) & !is.null(metadata$event_column)) {
        event_column <- metadata$event_column
    } else if (!is.character(event_column) || !event_column %in% names(data)) {
        cli_abort(paste(
            "{.arg event_column} must be a column name within your {.arg data}.",
            "Make sure column names match exactly."))
    }

    ## define `sample_rate` manually overrides metadata
    if (is.null(sample_rate) & is.null(metadata$sample_rate)) {
        cli_abort(paste(
            "{.arg sample_rate} not found in metadata. Please check your data",
            "attributes or define {.arg sample_rate} explicitly."))
    } else if (is.null(sample_rate) & !is.null(metadata$sample_rate)) {
        sample_rate <- metadata$sample_rate
    }

    ## validation: `fit_spans` must be numeric scalar
    if (!is.numeric(fit_span) || !length(fit_span) == 2) {
        cli_abort(paste(
            "{.arg fit_span} must be a two-element {.cls numeric} vector",
            "{.val c(before, after)}."))
    }

    ## define & validation: `display_span`
    ## TODO 2025-08-12 NOT CURRENTLY IMPLEMENTED
    # if (is.null(display_span)) {
    display_span <- fit_span
    # } else if (!is.numeric(display_span) || !length(display_span) == 2) {
    #     cli_abort(paste(
    #         "{.arg display_span} must be a two-element {.cls numeric} vector",
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

    fit_span[1] <- -abs(fit_span[1])
    display_span[1] <- -abs(display_span[1])
    display_start <- min(c(fit_span[1], display_span[1]))
    display_end <- max(c(fit_span[2], display_span[2]))

    # display_column <- paste0("display_", sample_column)
    fit_column <- paste0("fit_", sample_column)

    ## Metadata =================================
    metadata$nirs_columns <- unique(
        c(metadata$nirs_columns, nirs_columns))
    metadata$sample_column <- sample_column
    metadata$event_column <- event_column
    metadata$event_sample_list <- event_sample_list
    metadata$fit_span <- fit_span
    ## TODO 2025-08-12 NOT CURRENTLY IMPLEMENTED
    # metadata$display_span <- display_span

    ## data list =================================
    data_list <- lapply(
        event_sample_list,
        \(.x) {
            ## sample vector zeroed to the kinetics event
            ## TODO 2025-08-12 display_sample currently not implemented in
            ## outgoing data_list. Only being used to define fit_sample
            ## round to avoid floating point error
            display_sample <- round((data[[sample_column]] - .x) * sample_rate) / sample_rate
            keep_idx <- display_sample >= display_start & display_sample <= display_end

            ## filter data & display_sample vector within the kinetics event window
            event_data <- data[keep_idx, ]
            display_sample <- display_sample[keep_idx]

            ## create new fit_sample column named from `fit_column`
            ## as a sample vector zeroed to the kinetics event
            ## filtered within the `fit_span`
            event_data[[fit_column]] <- ifelse(
                display_sample >= fit_span[1] & display_sample <= fit_span[2],
                display_sample, NA_real_)

            ## relocates `fit_column` as the first col, includes remaining cols
            event_data[c(fit_column, setdiff(names(event_data), fit_column))]
        })

    ensemble_data <- function(data_list) {
        ## zoo::na.loct.default https://stackoverflow.com/a/19839474
        na_locf <- function(x) {
            L <- !is.na(x)
            c(x[L][1], x[L])[cumsum(L)+1]
        }

        x_all <- sort(unique(unlist(lapply(data_list, `[[`, fit_column))))

        filled_data_list <- lapply(data_list, \(.df) {
            ## df with all x values
            df_x_all <- tibble(!!fit_column := x_all)
            ## remove redundant columns
            .df[c(sample_column, event_column)] <- NULL
            ## redundant step keep only numeric
            .df <- .df[sapply(.df, is.numeric)]
            ## merge each data_list with df_x_all
            merged <- merge(df_x_all, .df, by = fit_column, all.x = TRUE)
            ## average duplicate x values
            merged <- aggregate(
                merged[, -1, drop = FALSE],
                by = setNames(list(merged[[fit_column]]), fit_column),
                FUN = mean, na.rm = TRUE)
            ## sort
            merged <- merged[order(merged[[fit_column]]),]
            ## apply locf to missing data
            merged[names(merged)[-1]] <- lapply(merged[names(merged)[-1]], na_locf)

            return(tibble(merged))
        })

        result <- filled_data_list[[1]][, fit_column, drop = FALSE]

        ## extract all nirs_columns
        all_nirs_cols <- do.call(
            cbind, lapply(filled_data_list, \(.df) .df[nirs_columns]))

        ## calculate rowwise means for all nirs_columns
        result[nirs_columns] <- lapply(seq_along(nirs_columns), \(.col) {
            col_indices <- seq(.col, ncol(all_nirs_cols), by = length(nirs_columns))
            rowMeans(all_nirs_cols[, col_indices, drop = FALSE], na.rm = TRUE)
        })

        return(result)
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
