#' Apply filter methods to mNIRS data
#'
#' Applies digital filtering with either:
#' 1. A smoothing spline with [stats::smooth.spline()]
#' 2. A Butterworth low-pass filter with [signal::butter()] and
#' [filtfilt_edges()]
#' 3. A simple moving average with [zoo::rollapply()].
#'
#' @param .data A dataframe with mNIRS metadata.
#' @param handle_missing_values Indicates how to handle missing data globally
#' before filtering can be applied.
#' - *"none"* (the default) will pass through data as-is. May cause errors.
#' - *"interpolate"* will interpolate between existing values.
#' - *"omit"* will remove `NA`s before analysis and re-insert them back to
#' their original indices.
#' @param filter_method A list of character vectors indicating the digital
#' filter method used to smooth each mNIRS data column, either column-wise
#' if the list is named by mNIRS columns, or globally if not named. See details.
#' - *"none"* (the default) will pass through data as-is without any digital
#' filtering.
#' - *"smooth-spline"* fits a cubic smoothing spline to the supplied data with
#' and automatically determined smoothing parameter `spar`.
#' - *"low-pass"* uses a symmetrical (centred) butterworth low-pass filter.
#' - *"moving-average"* uses a symmetrical (centred) moving average filter.
#' @param filter_parameters (optional) Either `NULL` or a list of named
#' numeric vectors of parameters to pass through to the digital filter(s)
#' defined in `filter_method`. Either column-wise if the list is named by
#' mNIRS columns, or globally if not named. See details.
#' @param sample_rate An integer scalar indicating the sample rate at which the
#' raw data was recorded (in Hz; samples per second).
#' @param ... Additional arguments.
#'
#' @details
#' `handle_missing_values` imported from [handle_missing_data()].
#'
#' `handle_missing_values` must be defined before filtering can be performed.
#' - *"none"* will pass through data as-is, but will cause *"low-pass"*
#' or *"smooth-spline"* filtering to fail if there are nay missing or
#' non-finite data.
#' - *"interpolate"* uses [zoo::na.approx()] to linearly interpolate across
#' missing data. Missing values on the edges of the dataframe will be
#' extrapolated from the first or last present value (see *"rule = 2"* in
#' [stats::approx()]). Additional arguments include *"maxgap"* to
#' define a limit of consecutive `NA`s to fill. Longer gaps will be left
#' unchanged and may cause subsequent errors (see *"maxgap"* in
#' [zoo::na.approx()]).
#' - *"omit"* CURRENTLY NOT IMPLEMENTED. will record the indices of `NA`s and
#' omit them before applying the digital filter. Then `NA`s are re-inserted
#' back to their original indices when the filtered dataframe is returned. #'
#'
#' `filter_method` may be a named list for column-wise filtering of individual
#' mNIRS columns. Or a non-named list or vector with one filtering method to
#' apply to all mNIRS columns. Currently will only work if either none or all
#' mNIRS columns are named. Such as:
#' - `filter_method = c("smooth-spline")`.
#' - `filter_method = list(SmO2 = "moving-average", HHb = "smooth-spline")`.
#'
#' `filter_parameters` will be chosen by default to correspond to
#' `filter_method`. If `filter_method` is a named list, then
#' `filter_parameters` must be either unnamed for global parameters, or must
#' have all of the same mNIRS names as `filter_method`.
#' - For `filter_method = "smooth-spline"` the default spar value is often good
#' enough. Higher values within the range `0` to `1` will apply more smoothing.
#' Lower values will apply less smoothing. CURRENTLY DEFAULT ONLY IMPLEMENTED.
#' - For `filter_method = "low-pass"`, `filter_parameters = list(c(n, fc))`,
#' where `n` is an integer scalar specifying the filter order number and
#' `fc` is a numeric scalar specifying the cutoff frequency in Hz.
#' - For `filter_method = "moving-average"`, `filter_parameters = list(c(k))`,
#' where `k` is an odd integer scalar specifying the window width (number of
#' samples) centred around the index sample (`x +- k/2-1`).
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
filter_dataframe <- function(
        .data,
        handle_missing_values = c("none", "interpolate", "omit"),
        filter_method = "none",
        filter_parameters = NULL, ## TODO convert this to ... arguments?
        sample_rate = NULL,
        ...
) {
    ## TODO
    ## convert filter_parameters to ... arguments?
    ## implement `handle_missing_data = "omit"`
    ## make default `filter_parameters` adjust to sample rate
    ## auto-detect sample rate or pass through from previous metadata?

    ## validation: check for metadata to ensure `read_data()` has been run
    if (is.null(attributes(.data)$nirs_columns)) {
        cli::cli_abort(paste(
            "Data should be extracted with {.fn read_data} before processing."
        ))
    }

    ## pass through optional arguments
    args <- list(...)

    metadata <- attributes(.data)
    nirs_columns <- metadata$nirs_columns

    ## ensure sample_rate is defined in metadata or manually
    if (is.null(sample_rate) & is.na(metadata$sample_rate)) {
        cli::cli_abort(paste(
            "Argument {.arg sample_rate} is missing, with no default."))

    } else if (is.null(sample_rate) & !is.na(metadata$sample_rate)) {
        sample_rate <- metadata$sample_rate
    }

    ## validation: `filter_method` must be a character vector or list
    if (any(!unlist(filter_method) %in% c(
        "none", "smooth-spline", "low-pass", "moving-average"))) {
        cli::cli_abort(paste(
            "{.arg filter_method} not recognised. Please see `?filter_data()`",
            "for options"))
    }

    ## handle missing values ================================
    data_nomissing <- handle_missing_data(
        .data = .data,
        handle_missing_values = handle_missing_values)

    ## set digital filter parameters ===================================
    ## Filter methods can be defined column-wise or globally
    ## a vector will be converted to a list, a list will be
    ## converted to a named list. The named list will be mapped over
    ## to filter each defined nirs_column

    filter_method_list <- assign_to_named_list(
        filter_method,
        names(nirs_columns))

    default_parameters <- lapply(
        filter_method_list,
        \(x)
        list("smooth-spline" = c(spar = "AUTO"),
             "low-pass" = c(n = 2, fc = 0.05),
             "moving-average" = c(k = 5),
             "none" = NULL)[[x]]
    )

    filter_parameters_list <- assign_to_named_list(
        filter_parameters,
        names(nirs_columns))

    filter_parameters_list <- lapply(
        names(filter_method_list),
        \(nirs) {
            if (is.null(filter_parameters_list[[nirs]])) {
                default_parameters[[nirs]]
            } else if (
                length(intersect(
                    names(filter_parameters_list[[nirs]]),
                    names(default_parameters[[nirs]]))) > 0
            ) {
                param_replace_list <- intersect(
                    names(filter_parameters_list[[nirs]]),
                    names(default_parameters[[nirs]]))

                default_parameters[[nirs]][param_replace_list] <-
                    filter_parameters_list[[nirs]]

                default_parameters[[nirs]]
            } else {
                default_parameters[[nirs]]
            }
        }) |>
        setNames(names(nirs_columns))

    ## Apply digital filter ============================================

    data_filtered <- purrr::imap(
        filter_method_list,
        \(method, nirs) {

            if (method != "none") {
                list2env(
                    as.list(filter_parameters_list[[nirs]]),
                    envir = .GlobalEnv)

                cli::cli_alert_info(paste(
                    "{.arg {nirs}}: {.val {method}} =",
                    "{.val {filter_parameters_list[nirs]}}."
                ))

                data_nomissing |>
                    dplyr::select(index, dplyr::any_of(nirs)) |>
                    dplyr::mutate(
                        dplyr::across(
                            dplyr::any_of(nirs),
                            \(.x) if (method == "smooth-spline") {
                                stats::smooth.spline(x = index, y = .x)$y
                            } else if (method == "low-pass") {
                                filtfilt_edges(.x, n = n, W = fc / (sample_rate/2))
                            } else if (method == "moving-average") {
                                zoo::rollapply(
                                    .x, width = k, FUN = mean,
                                    partial = TRUE, na.rm = TRUE)
                            }  else if (method == "none") {.x}
                        )) |>
                    dplyr::select(-index)
            } else if (method == "none") {
                data_nomissing |>
                    dplyr::select(dplyr::any_of(nirs))
            }
        }) |>
        dplyr::bind_cols(
            dplyr::select(
                .data,
                -dplyr::any_of(names(filter_method_list)))
        ) |>
        dplyr::relocate(names(.data)) ## data_filtered

    ## metadata ==================================================
    metadata$filter_parameters <- filter_parameters_list
    metadata$missing_indices <- attributes(data_nomissing)$missing_indices
    create_mnirs_data(data_filtered, metadata)
    # attributes(data_filtered)

}
#
## Troubleshooting ========================
# library(mNIRS)
# (raw_data <- read_data(
#     file_path = r"(C:\OneDrive - UBC\5-1 Assessments\Processed Data\03-2_2021-08-10-data.xlsx)",
#     nirs_columns = c("smo2_left_VL", "smo2_right_VL"),
#     sample_column = "Time",
#     event_column = "Event"))
# # attributes(raw_data)
# raw_data <- raw_data |>
#     dplyr::mutate(
#         dplyr::across(dplyr::matches("smo2_"), ~ round(., 1))
#     )
#
# (filtered_data <- filter_data(
#     .data = raw_data,
#     sample_rate = 1,
#     filter_method = "low-pass",
#     filter_parameters = list(smo2_left_VL = c(k = 15),
#                              smo2_right_VL = c(n = 4, fc=0.01))
# ))
#
# library(ggplot2)
# ggplot(filtered_data) +
#     aes(x = index) +
#     theme_minimal() +
#     geom_line(aes(y = smo2_left_VL)) +
#     geom_line(aes(y = smo2_right_VL))
#
#

#
# filter_method = "none"
# filter_method = "moving-average"
# filter_method = list("moving-average")
# filter_method = list(smo2_left_VL = "low-pass",
#                      smo2_right_VL = "moving-average")
# filter_parameters = NULL
# filter_parameters = c(k = 11)
# filter_parameters = list(c(fc = 0.01))
# # filter_parameters = list(smo2_left_VL = c(n = 2, fc = 0.01))
# filter_parameters = list(smo2_left_VL = NULL,#c(n = 2, fc = 0.01),
#                          smo2_right_VL = c(k = 15))
# #
# #
# #
# (filter_method_list <-
#     assign_to_named_list(filter_method, names(nirs_columns)))
#
# (default_parameters <- lapply(
#     filter_method_list,
#     \(x)
#     list("smooth-spline" = c(spar = 0),
#          "low-pass" = c(n = 2, fc = 0.05),
#          "moving-average" = c(k = 5),
#          "none" = NULL)[[x]]
# ))
#
# (filter_parameters_list <-
#     assign_to_named_list(filter_parameters, names(filter_method_list)))
#
# lapply(
#     names(filter_method_list),
#     \(nirs) {
#         if (is.null(filter_parameters_list[[nirs]])) {
#             default_parameters[[nirs]]
#         } else if (
#             length(intersect(
#                 names(filter_parameters_list[[nirs]]),
#                 names(default_parameters[[nirs]]))) > 0
#         ) {
#             param_replace_list <- intersect(
#                 names(filter_parameters_list[[nirs]]),
#                 names(default_parameters[[nirs]]))
#
#             default_parameters[[nirs]][param_replace_list] <-
#                 filter_parameters_list[[nirs]]
#
#             default_parameters[[nirs]]
#         } else {
#             default_parameters[[nirs]]
#         }
#     }) |>
#     setNames(names(nirs_columns))
