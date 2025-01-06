#' Prepare Dataframe for mNIRS Kinetics Analysis
#'
#' This function retrieves processed data from [process_data()] and allows for manual specification
#' of kinetics events by index, sample, or event label. Parameters are defined to extract data
#' to display and fit the kinetics events. Each kinetics event can be analysed separately, or
#' ensemble-averaged and analysed together.
#'
#' @param .data The data retrived [process_data()].
#' @param event_label A character vector specified in `event_column` indicating the start
#' of kinetics events to pass along for further analysis.
#' @param event_index A numeric vector indicating the start indices of kinetics events
#' to pass along for further analysis.
#' @param event_sample A vector in the class of `sample_column` indicating the start
#' of kinetic events to pass along. Any of `event_label`, `event_index`, or `event_sample`
#' will be passed along for further analysis.
#' @param baseline_fit_window TODO.
#' @param kinetics_fit_window TODO.
#' @param baseline_display_window TODO.
#' @param kinetics_display_window TODO.
#' @param end_kinetics_window TODO.
#' @param multiple_kinetics_events Indicates whether each *"distinct"* kinetics events should be
#' analysed separately (the default). Or whether multiple kinetics events should be *"ensemble"*
#' averaged and analysed together (similar to pulmonary VO2 kinetics).
#' @param ... Additional arguments.
#'
#' @details
#' TODO
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
prepare_kinetics_data <- function(
        .data,
        event_label = NULL, ## "end stage"
        event_index = NULL, ## 59
        event_sample = NULL, ## c("00:00:02", "00:00:03")
        baseline_fit_window = 25,
        kinetics_fit_window = 60,
        baseline_display_window = 30,
        kinetics_display_window = 70,
        end_kinetics_window = NULL,
        multiple_kinetics_events = c("distinct", "ensemble"),
        ...
) {
    ## validations
    ## one of either event_label or event_index should exist
    ## if event column doesn't exist, event_index must exist
    ## event_label can be string or numeric, convert to string, search for as string
    ## event_label should exist
    ## event_index should be numeric-ish/integer-ish, search for in index_column
    ## event_index should exist
    ## _windows should be integer, in seconds
    ## sum of _windows should not be longer than existing data
    ## TODO how to soft-check _windows against existing data?
    ## sample rate should be numeric-ish/integer-ish
    ## TODO how to soft-check?

    ## functionality
    ## TODO
    ## ...
    ## split into a list of dataframes if multiple_kinetics_events == "distinct"


    multiple_kinetics_events <- match.arg(multiple_kinetics_events)


    ## validation:: `event_column` should be defined if `event_sample` is specified
    if ((is.null(event_column) || is.na(event_column)) & !is.null(event_label)) {
        cli::cli_alert_warning(paste(
            "{.arg event_column} has not been specified, but",
            "{.arg event_label} is set to {.val {event_label}}.",
            if (is.numeric(event_label)) {"Did you mean to set {.arg event_index}?"},
            "These events are currently ignored."))
    }

    ## validation: `event_index` must be numeric data
    if (!(is.null(event_index) | rlang::is_double(event_index))) {
        cli::cli_abort(paste(
            "{.arg event_index} must be a {.cls numeric} vector",
            "or {.cls NULL}, not {.cls {class(event_index)}}.",
            if (is.character(event_index)) {"Enter {.cls character} in {.arg event_label}"}
        ))
    }

    ## validation: `event_sample` should be the same type as `sample_column`
    ## TODO POSIXct might be a bit tricky?
    if (!is.null(event_sample)) {
        if (all(typeof(event_sample) != typeof(.raw_data[[sample_column]]))) {
            cli::cli_abort(paste(
                "{.arg event_sample} must be a {.cls {class(.raw_data[[sample_column]])}}",
                "or {.cls NULL}, not {.cls {class(event_sample)}}.",
                if (is.character(event_sample)) {"Enter {.cls character} in {.arg event_label}"},
                if (is.numeric(event_sample)) {"Enter {.cls numeric} in {.arg event_index}"}
            ))
        }
    }


    ## validation: `event_label` must be character string
    if (!(is.null(event_label) | rlang::is_character(event_label))) {
        cli::cli_abort(paste(
            "{.arg event_label} must be a {.cls character} vector",
            "or {.cls NULL}, not {.cls {class(event_label)}}.",
            "Enter numeric values in {.arg event_index}"))
    }

    ## validation: `event_index` must be numeric data
    if (!(is.null(event_index) | rlang::is_double(event_index))) {
        cli::cli_abort(paste(
            "{.arg event_index} must be a {.cls numeric} vector",
            "or {.cls NULL}, not {.cls {class(event_index)}}.",
            "Enter character strings in {.arg event_label}"))
    }

    ## validation: inform message when end_kinetics_window set to default
    if (is.null(end_kinetics_window)) {
        end_kinetics_window <- min(round(kinetics_fit_window * 0.15/5)*5, 30)
        cli::cli_alert_info(c(
            "{.arg end_kinetics_window} set to {.val {end_kinetics_window}} samples"
        ))
    }

    ## detect kinetics events ============================
    ## TODO
    event_label
    event_index
    event_sample


    data_normalised |>
        dplyr::mutate(
            dplyr::across(
                any_of(sample_column),
                ~ as.character(.)
            )
        ) |>
        dplyr::filter(
            index %in% event_index |
                dplyr::if_any(
                    any_of(sample_column),
                    ~ grepl(paste(event_sample, collapse = "|"), .)) |
                dplyr::if_any(
                    any_of(event_column),
                    ~ grepl(paste(event_label, collapse = "|"), .))
        ) |>
        dplyr::pull(index)

    ## prepare prepared_kinetics_data ===================================
    prepared_kinetics_data <-
        .data |>
        dplyr::mutate(
            display_index = index - event_index,
            nirs_fit_window = dplyr::if_else(
                dplyr::between(display_index, 1, kinetics_fit_window),
                nirs, NA_real_),
            nirs_fit_window = signif(nirs_fit_window, 3),
        ) |>
        dplyr::filter(
            dplyr::between(
                display_index,
                -baseline_display_window,
                kinetics_display_window)
        ) |>
        dplyr::mutate(
            ## TROUBLESHOOTING FUNCTION
            # nirs_fit_window = 2 * (nirs_fit_window/nirs_fit_window), ## only within nirs_fit_window,
            ## local peak values with no greater value within 30 samples in either direction
            ## TODO stress test centred rolling max window
            max = zoo::rollapply(
                nirs_fit_window,
                width = 2 * end_kinetics_window + 1,
                FUN = max,
                align = "center",
                partial = TRUE,
                na.rm = TRUE) *
                (nirs_fit_window/nirs_fit_window), ## only within nirs_fit_window
            # ## highest value preserved forward (top to bottom)
            # max_right = zoo::rollapplyr(
            #     nirs_fit_window,
            #     width = end_kinetics_window,
            #     FUN = max,
            #     partial = TRUE,
            #     na.rm = TRUE),
            # ## highest value preserved backward (bottom to top)
            # max_left = zoo::rollapply(
            #     nirs_fit_window,
            #     width = end_kinetics_window,
            #     FUN = max,
            #     align = "left",
            #     partial = TRUE,
            #     na.rm = TRUE),
            ## ensure the peak must be above the mean data within kinetics_fit_window
            ## I think to avoid early peaks?
            mean = signif(mean(nirs_fit_window, na.rm = TRUE), 3),
            peak = tidyr::replace_na(nirs_fit_window == max & nirs_fit_window > mean, 0),
            ## where no local peak exists, use the last kinetics_fit_window value
            peak2 = (all(is.na(peak) | !peak) & display_index > 0 &
                         is.na(dplyr::lead(nirs_fit_window))) + peak,
            ## take the first peak value if multiple exist
            first_peak = cumsum(peak2) - ((peak2 == 0) * cumsum(peak2)) == 1,

            ## convert Inf or NaN values to NA
            dplyr::across(dplyr::where(is.numeric),
                          ~ dplyr::if_else(is.infinite(.) | is.nan(.), NA_real_, .)),
        ) |>
        suppressWarnings()

    metadata <- list(
        event_index  = event_index,
        event_sample = event_sample,
        event_label  = event_label,
        NULL)

    return(prepared_kinetics_data)

}
