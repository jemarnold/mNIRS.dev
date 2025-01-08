#' Process Data for mNIRS Analysis
#'
#' This function retrieves raw data from [read_data()] and applies processing
#' methods.
#'
#' @param .data The data retrieved from [read_data()].
#' @param sample_rate An integer scalar indicating the sample rate at which the
#' raw data was recorded (in Hz; samples per second).
#' @param event_index (optional) A numeric vector indicating the start indices
#' of kinetics events.
#' - Any of `event_label`, `event_index`, or `event_sample`
#' will be passed along for further analysis.
#' @param event_sample (optional) A vector in the same class as `sample_column`
#' indicating the start of kinetic events.
#' @param event_label (optional) A character vector specified in `event_column`
#' indicating the start of kinetics events.
#' @param remove_fixed_values (optional) Either `<NULL>` (the default) or a
#' numeric vector of values to remove from the data.
#' e.g. `remove_fixed_values = c(0, 100)`.
#' @param remove_outliers A boolean indicating whether local outliers should
#' be removed using a Hampel filter.
#' @param handle_missing_values Indicates how to handle missing data.
#' - *"linear interpolate"* (the default) will interpolate between existing
#' values.
#' - *"none"* will pass through missing data. This may introduce errors
#' further along in processing.
#' @param filter_method Indicates the digital filter method used to smooth data.
#' - *"none"* (the default) will pass through raw data without any digital
#' filtering.
#' - *"smooth-spline"* fits a cubic smoothing spline to the supplied data with
#' and automatically determined smoothing parameter.
#' - *"low-pass"* uses a symmetrical (centred) butterworth low-pass filter.
#' - *"moving-average"* uses a symmetrical (centred) moving average filter.
#' @param filter_parameters (optional) Either `<NULL<` or a named numeric
#' vector of parameters to pass through to the digital filter defined in
#' `filter_method`.
#' - For `filter_method = "low-pass"` this should be `c(n, fc)` or `c(n, W)`
#' where `n` is an integer specifying the filter order number; `fc` is a
#' numeric value specifying the cutoff frequency in Hz; `W` is a numeric
#' scalar between 0 and 1 specifying the relative cutoff frequency, where 1
#' is the Nyquist frequency (half of the sample frequency in Hz).
#' - For `filter_method = "moving-average"` this should be `c(k)` where `k`
#' is an odd integer specifying the window width (number of samples) centred
#' around the index sample (`x +- k/2-1`).
#' - For `filter_method = "smooth-spline"` the default spar value is often good
#' enough. Higher values within the range 0 to 1 will return more smoothing.
#' Lower values will return less smoothing.
#' - Reasonable default values are set to `c(n = 1, fc = 0.05, k = 5)`.
#' @param shift_range_positive A boolean indicating whether the range of mNIRS
#' data should be shifted to return only positive data. For example, to be
#' used when mNIRS values are arbitrarily scaled from the starting value
#' (e.g., Artinis Oxysoft). This does not change the scale of the data.
#' @param normalise_range Indicates whether and how the range of observed mNIRS
#' data should be re-scaled to between 0 and 100. For example, to be used when
#' calibrating the data to an ischaemic occlusion (physiological range), or to
#' the existing observed data (functional range).
#' - *"none"* (the default) will not normalise any data.
#' - *"column-wise"* will re-scale each NIRS column to 0-100 within it's own
#' range.
#' - *"globally"* will re-scale all NIRS columns to a common 0-100 range, from
#' the minimum to the maximum values across all columns.
#' - Alternatively, a list of character vectors of NIRS column names can be
#' specified to re-scale across selected columns in groups, e.g. for left and
#' right limbs separately, such as `c("smo2_left", "smo2_right")`.
#' @param ... Additional arguments.
#'
#' @details
#' Processing methods:
#' 1) Removes custom fixed values such as c(0, 100), and remove outliers with
#' a modified version of [pracma::hampel()] which skips `NA`.
#' 2) Handles missing data by linear interpolation.
#' 3) Applies digital filtering with either a Butterworth low-pass filter with
#' [signal::butter()] and [filtfilt_edges()], a simple moving average with
#' [zoo::rollapply], or a smoothing spline with [stats::smooth.spline()].
#' Default or custom parameters can be used.
#' 4) Transform the data by either shifting values to be positive, and/or
#' re-scale (normalise) data to the observed range, either column-wise (where
#' each column is scaled individually), dataframe-wise (all columns are
#' res-scaled to the same scale), or with manually defined groups.
#' 5) Kinetics events can be manually specified by index, sample, or
#' event label.
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
process_data <- function(
        .data,
        sample_rate,
        event_index = NULL, ## 59
        event_sample = NULL, ## c(3, 4)
        event_label = NULL, ## "end stage"
        remove_fixed_values = NULL, #c(0, 100),
        remove_outliers = FALSE,
        handle_missing_values = c("linear interpolate", "none"),
        filter_method = c("none", "smooth-spline", "low-pass", "moving-average"),
        filter_parameters = NULL, #c(n = 2, fc = 0.03), #c(k = 5)
        shift_range_positive = FALSE,
        normalise_range = c("none", "column-wise", "globally"),
        ...
) {
    handle_missing_values <- match.arg(handle_missing_values)
    filter_method <- match.arg(filter_method)


    ## validation: check for metadata to ensure `read_data()` has been run
    if (is.null(attributes(.data)$nirs_columns)) {
        cli::cli_abort(paste(
            "Data should be extracted with {.fn read_data} before processing."
        ))
    }

    metadata <- attributes(.data)
    nirs_columns <- metadata$nirs_columns
    sample_column <- metadata$sample_column
    event_column <- metadata$event_column


    ## validation `event_column` should be defined if `event_sample` is
    ## specified
    if ((is.null(event_column) | is.na(event_column)) &
        !is.null(event_label)) {
        cli::cli_alert_warning(paste(
            "{.arg event_column} has not been specified, but",
            "{.arg event_label} is set to {.val {event_label}}.",
            if (is.numeric(event_label)) {
                "Did you mean to set {.arg event_index}?"
            },
            "These events are currently ignored."))
    }

    ## validation: `event_index` must be numeric data
    if (!(is.null(event_index) | rlang::is_double(event_index))) {
        cli::cli_abort(paste(
            "{.arg event_index} must be a {.cls numeric} vector",
            "or {.cls NULL}, not {.cls {class(event_index)}}.",
            if (is.character(event_index)) {
                "Enter {.cls character} in {.arg event_label}"}
        ))
    }

    ## validation: `event_sample` should be the same type as `sample_column`
    ## TODO POSIXct might be a bit tricky?
    if (!is.null(event_sample)) {
        if (all(typeof(event_sample) != typeof(.data[[sample_column]]))) {
            cli::cli_abort(paste(
                "{.arg event_sample} must be a",
                "{.cls {class(.data[[sample_column]])}}",
                "or {.cls NULL}, not {.cls {class(event_sample)}}.",
                if (is.character(event_sample)) {
                    "Enter {.cls character} in {.arg event_label}"
                },
                if (is.numeric(event_sample)) {
                    "Enter {.cls numeric} in {.arg event_index}"
                }))
        }
    }

    ## validation: `event_label` must be character string
    if (!(is.null(event_label) | rlang::is_character(event_label))) {
        cli::cli_abort(paste(
            "{.arg event_label} must be a {.cls character} vector",
            "or {.cls NULL}, not {.cls {class(event_label)}}.",
            "Enter numeric values in {.arg event_index}"))
    }


    ## validation: `remove_fixed_values` must be numeric data
    if (!(is.null(remove_fixed_values) |
          rlang::is_double(remove_fixed_values))) {
        cli::cli_abort(paste(
            "{.arg remove_fixed_values} must be a {.cls numeric} vector",
            "or {.cls NULL}, not {.cls {class(remove_fixed_values)}}."))
    }

    ## validation: `remove_outliers` must be boolean
    if (!isTRUE(remove_outliers) & !isFALSE(remove_outliers)) {
        cli::cli_abort(paste(
            "{.arg remove_outliers} must be either {.val {TRUE}}",
            "or {.val {FALSE}}, not {.val {remove_outliers}}."))
    }

    ## validation: `filter_parameters` must include `n` and `fc` or `W`, or `k`
    if (!is.null(filter_parameters)) {
        if (filter_method == "moving-average" &
            !any(grepl("k", names(filter_parameters), ignore.case = TRUE))) {
            cli::cli_abort(paste(
                "{.arg filter_parameters} should be a named {.cls numeric}",
                "vector which includes {.arg k}. Where {.arg k} specifies the",
                "moving-average window width in samples, centred around the",
                "index sample (x +- k/2-1)."))
        } else if (
            filter_method == "low-pass" &
            !any(grepl("n", names(filter_parameters), ignore.case = TRUE) |
                 grepl("fc|W", names(filter_parameters), ignore.case = TRUE))
        ) {
            cli::cli_abort(paste(
                "{.arg filter_parameters} should be a named {.cls numeric}",
                "vector which includes {.arg n} and one of either {.arg fc}",
                "or {.arg W}. Where {.arg n} specifies the low-pass filter",
                "order and {.arg fc} specifies the cutoff frequency in Hz,",
                "or {.arg W} specifies the relative cutoff frequency as a",
                "scalar between {.val 0} and {.val 1}, where {.val 1} is the",
                "Nyquist frequency (half of the sample frequency in Hz)."))
        } else if (
            filter_method == "smooth-spline" &
            !any(grepl("spar", names(filter_parameters), ignore.case = TRUE))
        ) {
            cli::cli_abort(paste(
                "{.arg filter_parameters} should be a named {.cls numeric}",
                "vector which includes {.arg spar}. Where {.arg spar} is a",
                "numeric scalar between {.val -1.5} and {.val 1.5} and",
                "specifies the smoothing parameter of the",
                "{.fn smooth.spline}."))
        }
    }

    ## validation: shift_range_positive must be boolean
    if (!isTRUE(shift_range_positive) & !isFALSE(shift_range_positive)) {
        cli::cli_abort(paste(
            "{.arg shift_range_positive} must be either {.val {TRUE}}",
            "or {.val {FALSE}}, not {.val {shift_range_positive}}."))
    } else if (shift_range_positive &
               all(.data[names(nirs_columns)] >= 0, na.rm = TRUE)) {
        ## validation: warns that shift_range_positive is unnecessary if
        ## no negative or 0 values
        cli::cli_alert_warning(paste(
            "{.arg shift_range_positive} is set to {.val {TRUE}},",
            "but all values of {.var nirs_columns} are already positive.",
            "Did you mean to set {.arg normalise_range}?"))
        cli::cli_alert_warning(
            "Data range will be shifted to a floor at zero.")
    }


    ## validation: `normalise_range` must be boolean
    if (!all(unlist(normalise_range) %in%
             c("none", "column-wise", "globally",
               names(nirs_columns), nirs_columns))
    ) {
        cli::cli_abort(paste(
            "{.arg normalise_range} must be either {.val none},",
            "{.val column-wise}, {.val globally}, or a list of names from",
            "{.arg nirs_columns}. Make sure column names match exactly"))
    }


    ## remove fixed values ================================

    if (!is.null(remove_fixed_values)) {

        data_nofixed <- .data |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    ~ dplyr::if_else(. %in% remove_fixed_values, NA_real_, .)
                ))

        cli::cli_alert_info(paste(
            "Values {.val {remove_fixed_values}} will be removed from",
            "{.arg nirs_columns} and set to {.val {NA}}."
        ))

    } else if (is.null(remove_fixed_values)) {
        data_nofixed <- .data
    }


    ## remove outliers ================================
    if (remove_outliers) {
        data_nooutliers <- data_nofixed |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    ~ hampel(., k = 20 * sample_rate)$y
                ))
    } else {
        data_nooutliers <- data_nofixed
    }


    ## handle missing values ================================
    if (handle_missing_values == "linear interpolate") {
        data_nomissing <- data_nooutliers |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    ~ zoo::na.approx(., na.rm = FALSE, rule = 2)), ## maxgap?
            )
        ## omit method not implemented. Redundant?
        # } else if (handle_missing_values == "omit") {
        #     missing_indices <- data_nooutliers |>
        #         dplyr::filter(
        #             dplyr::if_any(
        #                 dplyr::any_of(names(nirs_columns)),
        #                 ~ is.na(.))) |>
        #         dplyr::pull(index)
        #
        #     data_nomissing <- data_nooutliers |>
        #         tidyr::drop_na(dplyr::any_of(names(nirs_columns)))
    } else if (handle_missing_values == "none") {
        data_nomissing <- data_nooutliers
    }


    ## apply digital filter method ================================
    ## pass through arguments for either LPF or MA filter methods
    ## or set to defaults
    if (is.null(filter_parameters) & filter_method == "low-pass") {
        n <- 1 ## TODO set defaults based on sample_rate?
        fc <- 0.05

        cli::cli_alert_info(
            "Low-pass filter order {.arg n} set to {.val {n}}.")
        cli::cli_alert_info(paste(
            "Low-pass filter order cutoff frequency {.arg fc} set to",
            "{.val {fc}} Hz."))
    } else if (is.null(filter_parameters) &
               filter_method == "moving-average") {
        k <- 5

        cli::cli_alert_info(
            "Moving-average window {.arg k} set to {.val {k}} samples")
    } else if (is.null(filter_parameters) &
               filter_method == "smooth-spline") {
        ## TODO add custom spar
        spar <- purrr::map(
            names(nirs_columns),
            \(nirs) {
                stats::smooth.spline(
                    x = data_nomissing$index,
                    y = data_nomissing[[nirs]]
                )$spar |>
                    signif_trailing(x = _, 2) |>
                    stats::setNames(nirs)
            }) |>
            unlist()

        spar_string <- paste(
            paste(names(spar), spar, sep = " = "),
            collapse = ", ")

        cli::cli_alert_info(paste(
            "smooth-spline {.arg spar}{cli::qty(names(spar))} parameter{?s}",
            "set: {.val {spar_string}}"))

    } else if (filter_method != "none") {
        list2env(as.list(filter_parameters), envir = .GlobalEnv)
    }


    if (filter_method == "low-pass") {

        data_filtered <- data_nomissing |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    ~ filtfilt_edges(., n = n, W = fc / (sample_rate/2))),
            )

    } else if (filter_method == "moving-average") {

        data_filtered <- data_nomissing |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    ~ zoo::rollapply(., width = k, FUN = mean,
                                     partial = TRUE, na.rm = TRUE)),
            )

    } else if (filter_method == "smooth-spline") {

        data_filtered <- data_nomissing |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    ~ stats::smooth.spline(x = index, y = .)$y),
            )

    } else if (filter_method == "none") {
        data_filtered <- data_nomissing
    }


    ## shift range positive ================================
    if (shift_range_positive) {
        data_shifted <- data_filtered |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    ~ . - min(., na.rm = TRUE) + 0.1
                ))
    } else {
        data_shifted <- data_filtered
    }


    ## normalise range ================================
    if (all(normalise_range == "column-wise")) {

        data_normalised <- data_shifted |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    ~ (. - min(., na.rm = TRUE)) /
                        diff(range(., na.rm = TRUE))*100),
            )
    } else if (all(normalise_range == "globally")) {

        data_normalised <- data_shifted |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    ~ (. - min(
                        dplyr::pick(dplyr::any_of(names(nirs_columns))),
                        na.rm = TRUE)) /
                        diff(range(
                            dplyr::pick(dplyr::any_of(names(nirs_columns))),
                            na.rm = TRUE))*100),
            )
    } else if(all(normalise_range == "none")) {
        data_normalised <- data_shifted
    } else { ## take custom normalising groups

        data_normalised <-
            purrr::map(
                if (is.list(normalise_range)) {
                    normalise_range
                } else {list(normalise_range)},
                \(norm_range)
                data_shifted |>
                    dplyr::select(dplyr::any_of(norm_range)) |>
                    dplyr::mutate(
                        dplyr::across(
                            dplyr::any_of(norm_range),
                            ~ (. - min(
                                dplyr::pick(dplyr::any_of(norm_range)),
                                na.rm = TRUE)) /
                                diff(range(
                                    dplyr::pick(dplyr::any_of(norm_range)),
                                    na.rm = TRUE))*100),
                    )
            ) |>
            dplyr::bind_cols(
                dplyr::select(
                    data_shifted,
                    -dplyr::any_of(unlist(normalise_range)))
            ) |>
            dplyr::relocate(names(data_shifted))

    }

    ## detect kinetics events ============================
    event_indices <- data_normalised |>
        dplyr::mutate(
            dplyr::across(
                any_of(sample_column),
                ~ as.character(.)
            )
        ) |>
        dplyr::filter(
            index %in% event_index |
                dplyr::if_any(
                    any_of(names(sample_column)),
                    ~ . %in% as.character(event_sample)) |
                dplyr::if_any(
                    any_of(names(event_column)),
                    ~ grepl(paste(event_label, collapse = "|"), .))
        ) |>
        dplyr::pull(index)

    ## TODO what happens when a file is run through process_data()
    ## multiple times?
    # metadata <- modifyList(
    #     list(
    #         sample_rate = sample_rate,
    #         event_indices = event_indices,
    #         missing_index = data_normalised$index[
    #             which(is.na(data_normalised[names(nirs_columns)]))],
    #         filtered = filter_method,
    #         shifted_positive = shift_range_positive,
    #         normalised = normalise_range
    #     ),
    #     metadata,
    # )
    metadata$sample_rate <- sample_rate
    metadata$event_indices <- event_indices
    metadata$outliers_removed <- remove_outliers
    metadata$missing_index <- data_normalised$index[
        which(is.na(data_normalised[names(nirs_columns)]))]
    metadata$filtered <- filter_method
    metadata$shifted_positive <- shift_range_positive
    metadata$normalised <- normalise_range

    processed_data <- create_mnirs_data(data_normalised, metadata)
    # attributes(processed_data)

    return(processed_data)

}


## troubleshooting =====================================
# library(tidyverse)
# library(JAPackage)
# library(mNIRS)
#
# raw_data <- read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_columns = c("ICG_VL" = "9", "ICG_SCM" = "10"),
#     sample_column = c("Sample" = "1"),
#     # event_column = c("Event" = "11"),
# )
# processed_data <- process_data(
#     .data = raw_data,
#     sample_rate = 50,
#     normalise_range = "column-wise",
#     filter_method = "smooth-spline"
# )
# attributes(processed_data)
# ## Moxy
# # (raw_data <-
# #         read_data(
# #             file_path = r"(C:\OneDrive - UBC\5-1 Assessments\Processed Data\03-2_2021-08-10-data.xlsx)",
# #             nirs_columns = c("smo2_left_VL", "smo2_right_VL"),
# #             sample_column = "Time",
# #             event_column = "Event") |>
# #         # dplyr::filter(index >= 2340, index <= 2590) |>
# #         # dplyr::filter(index >= 2950, index <= 3100) |> ## reoxy
# #         # dplyr::mutate(index = index - dplyr::first(index)+1) |>
# #         dplyr::mutate(
# #             smo2_left_VL = dplyr::case_when(
# #                 index %in% sample.int(max(index), 30) ~ NA,
# #                 index %in% 1300:1329 ~ NA,
# #                 # index %in% 222 ~ -nirs,
# #                 TRUE ~ smo2_left_VL)
# #         ))
# ## PerfPro
# (raw_data <- read_data(
#     file_path = r"(C:\OneDrive - UBC\5-1 Assessments\Raw Data\03-2_2021-08-10-16-44-33.xlsx)",
#     nirs_columns = c("smo2_something" = "SmO2 (3623)",
#                      "smo2_left_VL" = "SmO2 (3724)",
#                      "smo2_somethingelse" = "SmO2 (1617)"),
#     sample_column = "Time",
#     event_column = NULL,
#     .keep_all = FALSE) |>
#         dplyr::mutate(
#             smo2_somethingelse = dplyr::if_else(
#                 index %in% c(4:10), NA, smo2_somethingelse))
# )
# # attributes(raw_data)
# (processed_data <-
#     process_data(.data = raw_data,
#                  remove_fixed_values = 0,
#                  filter_method = "low-pass",
#                  filter_parameters = c(n = 2, fc = 0.02, k = 3),
#                  normalise_range = list(c("smo2_something", "smo2_somethingelse")))
#     )
#
#
# ggplot(processed_data) + ## data_shifted data_normalised
#     {list( ## Settings
#         aes(x = index),
#         coord_cartesian(
#         # xlim = c(500, 800),
#         # xlim = c(8600, 9100),
#         ylim = c(0, 100)),
#         theme_JA(),
#         NULL)} + ## Settings
#     {list( ## Data
#         # geom_hline(yintercept = c(0, 100)),
#         # geom_line(data = data_nomissing, colour = "red", alpha = 0.2),
#         geom_line(aes(y = ICG_VL), colour = "red"),
#         geom_line(aes(y = ICG_SCM), colour = "blue"),
#         # geom_line(aes(y = smo2_somethingelse), colour = "green3"),
#
#         # geom_point(),
#         NULL)} ## Data
