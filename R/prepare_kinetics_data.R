#' Prepare Dataframes for mNIRS Kinetics Analysis
#'
#' This function retrieves processed data from [process_data()] with manually
#' identified kinetics events. Kinetics parameters are defined to extract
#' data excerpts to fit those kinetics events. Each kinetics event can be
#' analysed separately, or ensemble-averaged and analysed together.
#'
#' @param .data The data retrived [process_data()].
#' @param multiple_kinetics_events Indicates whether kinetics events should
#' be analysed separately or together.
#' - *"distinct"* (the default) will prepare a list of unique dataframes for
#' each kinetics event.
#' - *"ensemble"* will prepare one dataframe with the ensemble-averaged mNIRS
#' data from all kinetics events (similar to pulmonary VO2 kinetics).
#' - Alternatively, a list of numeric vectors can be specified to
#' ensemble-average groups of kinetics events together. Such as
#' `multiple_kinetics_events = list(c(1, 2), c(3, 4))` for two sets of
#' repeated bouts.
#' @param baseline_fit_window A numeric scalar specifying the number of
#' samples preceding to the kinetics events to include as baseline.
#' @param kinetics_fit_window A numeric scalar specifying the number of
#' samples following to the kinetics events to include for kinetics modelling.
#' @param baseline_display_window A numeric scalar specifying the number of
#' samples preceding to the kinetics events to include for display (will not
#' necessarily be modelled).
#' @param kinetics_display_window A numeric scalar specifying the number of
#' samples following to the kinetics events to include for display (will not
#' necessarily be modelled).
#' @param end_kinetics_window A numeric scalar specifying the number of
#' samples in which to look for a peak or nadir value indicating the kinetics
#' plateau.
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
        multiple_kinetics_events = c("distinct", "ensemble"),
        baseline_fit_window = 30,
        kinetics_fit_window = 180,
        baseline_display_window = 30,
        kinetics_display_window = 180,
        end_kinetics_window = NULL,
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
    ## take in processed_data with event_indices metadata
    ## split into a list of dataframes using display_window
    ## prepare kinetics_data mapping over each dataframe and each nirs_column
    ## prepare metadataf or each nirs_column
    ## ... how?
    ## if multiple_kinetics_events == "distinct", output is a list of dataframes
    ## if == "ensemble", output is a single dataframe with all ensembled
    ## if == list(c(1, 2), c(3, 4)), output is a list of ensembled dataframes



    ## validation: check for metadata to ensure `process_data()` has been run
    if (is.null(attributes(processed_data)$event_indices)) {
        cli::cli_abort(paste(
            "Metadata has no kinetics events identified.",
            "Indicate kinetics events in {.fn process_data} first."
        ))
    }

    metadata <- attributes(.data)
    nirs_columns <- metadata$nirs_columns
    sample_column <- metadata$sample_column
    event_column <- metadata$event_column
    event_indices <- metadata$event_indices

    ## validation: inform message when end_kinetics_window set to default
    if (is.null(end_kinetics_window)) {
        end_kinetics_window <- min(round(kinetics_fit_window * 0.15/5)*5, 30)
        cli::cli_alert_info(paste(
            "{.arg end_kinetics_window} set to {.val {end_kinetics_window}}",
            "samples"
        ))
    }


    ## split dataframes ==================================
    start_index <- min(c(baseline_fit_window,
                         baseline_display_window))
    end_index <- min(c(kinetics_fit_window,
                       kinetics_display_window))

    df_list <- purrr::pmap(
        list(
            start_idx = event_indices - start_index,
            end_idx = event_indices + end_index
        ),
        \(start_idx, end_idx) {
            .data |>
                dplyr::filter(
                    dplyr::between(index, start_idx, end_idx)
                )
        })


    ## TODO pmap over expanded grid for prepared_kinetics_data
    ## insert metadata individually
    ## output list of dataframes with prepared kinetics metadata
    purrr::pmap(
        tidyr::expand_grid(
            df = df_list,
            nirs = names(nirs_columns),
        ),
        \(df, nirs) {
            df |>
                dplyr::summarise(
                    dplyr::across(
                        dplyr::any_of(nirs),
                        \(.x) mean(.x, na.rm = TRUE)),
                )
        })


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
            # nirs_fit_window = 2 * (nirs_fit_window/nirs_fit_window),
            ## local peak values with no greater value within 30 samples in
            ## either direction
            ## TODO stress test centred rolling max window
            max = zoo::rollapply(
                nirs_fit_window,
                width = 2 * end_kinetics_window + 1,
                FUN = max,
                align = "center",
                partial = TRUE,
                na.rm = TRUE) *
                (nirs_fit_window/nirs_fit_window),
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
            ## ensure the peak must be above the mean data within
            ## kinetics_fit_window. I think to avoid early peaks?
            mean = signif(mean(nirs_fit_window, na.rm = TRUE), 3),
            peak = tidyr::replace_na(
                nirs_fit_window == max & nirs_fit_window > mean, 0),
            ## where no local peak exists, use the last kinetics_fit_window
            ## value
            peak2 = (all(is.na(peak) | !peak) & display_index > 0 &
                         is.na(dplyr::lead(nirs_fit_window))) + peak,
            ## take the first peak value if multiple exist
            first_peak = cumsum(peak2) - ((peak2 == 0) * cumsum(peak2)) == 1,

            ## convert Inf or NaN values to NA
            dplyr::across(
                dplyr::where(is.numeric),
                \(.x) dplyr::if_else(
                    is.infinite(.x) | is.nan(.x), NA_real_, .x)),
        ) |>
        suppressWarnings()

    metadata

    return(prepared_kinetics_data)

}


## Troubleshooting =======================================
# library(mNIRS)
# (raw_data <- read_data(
#     file_path = r"(C:\OneDrive - UBC\5-1 Assessments\Processed Data\03-2_2021-08-10-data.xlsx)",
#     nirs_columns = c("smo2_left_VL", "smo2_right_VL"),
#     sample_column = "Time",
#     event_column = "Event"))
# attributes(raw_data)
# (processed_data <- process_data(
#     raw_data,
#     sample_rate = 1,
#     event_label = "end stage"
# ))
# attributes(processed_data)
# .data = processed_data
# processed_data |>
#     dplyr::filter(!is.na(Event))
