#' Normalise mNIRS data range
#'
#' Shifts the data range to be positive values, and normalise the dynamic
#' range of data to 0-100.
#'
#' @param shift_minimum Indicates whether and how the range of observed mNIRS
#' data should be shifted, to return only positive data or to a specified
#' starting baseline value. For example, can be used when mNIRS values are
#' arbitrarily scaled from the starting value (e.g., Artinis Oxysoft).
#' This does not change the scale of the data.
#' - *"none"* (the default) will not shift any data.
#' - *"column"* will shift each NIRS column to it's own new baseline value.
#' - *"global"* will shift all NIRS columns to a common baseline value, from
#' the minimum value across all columns. Will preserve the relationship across
#' NIRS columns.
#' - Alternatively, a list of character vectors of NIRS column names can be
#' specified to shift across selected columns in groups, e.g. for left and
#' right quadriceps and deltoid sites separately, such as
#' `list(c("smo2_left", "smo2_right"), "smo2_delt")`.
#' @param shift_value A numeric scalar to which the minimum value from the
#' specified NIRS data columns will be shifted to.
#' @param normalise Indicates whether and how the range of observed mNIRS
#' data should be re-scaled to between 0 and 100. For example, to be used when
#' calibrating the data to an ischaemic occlusion (physiological range), or to
#' the existing observed data (functional range).
#' - *"none"* (the default) will not normalise any data.
#' - *"column"* will re-scale each NIRS column to 0-100 within it's own
#' range.
#' - *"global"* will re-scale all NIRS columns to a common 0-100 range, from
#' the minimum to the maximum values across all columns. Will preserve the
#' relationship across NIRS columns.
#' - Alternatively, a list of character vectors of NIRS column names can be
#' specified to re-scale across selected columns in groups, e.g. for left and
#' right quadriceps and deltoid sites separately, such as
#' `list(c("smo2_left", "smo2_right"), "smo2_delt")`.
#' @param normalise_range A numeric vector in the form `c(min, max)`,
#' indicating the minimum and maximum values to which the specified NIRS
#' data columns will be  re-scaled.
#' @param ... Additional arguments.
#'
#' @details
#' Additional arguments will accept ``
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
normalise_dataframe <- function(
        .data,
        shift_minimum = c("none", "column", "global"),
        shift_value = 0,
        normalise = c("none", "column", "global"),
        normalise_range = c(0, 100),
        ...
) {
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

    ## validation: `shift_minimum` must match expected arguments
    if (!all(unlist(shift_minimum) %in%
             c("none", "column", "global",
               names(nirs_columns), nirs_columns))
    ) {
        cli::cli_abort(paste(
            "{.arg shift_minimum} must be either {.val none},",
            "{.val column}, {.val global}, or a list of names from",
            "{.arg nirs_columns}. Make sure column names match exactly"))
    }

    ## validation: `shift_value` must be numeric data
    if (!rlang::is_double(shift_value) | length(shift_value) != 1) {
        cli::cli_abort(paste(
            "{.arg shift_value} must be a single {.cls numeric} scalar"))
    }

    ## validation: `normalise` must match expected arguments
    if (!all(unlist(normalise) %in%
             c("none", "column", "global",
               names(nirs_columns), nirs_columns))
    ) {
        cli::cli_abort(paste(
            "{.arg normalise} must be either {.val none},",
            "{.val column}, {.val global}, or a list of names from",
            "{.arg nirs_columns}. Make sure column names match exactly"))
    }

    ## validation: `shift_value` must be numeric data
    if (!rlang::is_double(normalise_range) | length(normalise_range) != 2) {
        cli::cli_abort(paste(
            "{.arg normalise_range} must be a {.cls numeric} vector",
            "{.val c(min, max)}, with the minimum and maximum values to",
            "which data will be re-scaled"))
    }

    ## shift range positive ================================
    if (all(shift_minimum == "column")) {

        data_shifted <- .data |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    \(.x) .x - min(.x, na.rm = TRUE) + shift_value),
            )

        cli::cli_alert_info(paste(
            "Data shifted column-wise to a minimum of {.val {shift_value}}"))
    } else if (all(shift_minimum == "global")) {

        data_shifted <- .data |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    \(.x) .x - min(
                        dplyr::pick(dplyr::any_of(names(nirs_columns))),
                        na.rm = TRUE) + shift_value),
            )

        cli::cli_alert_info(paste(
            "Data shifted globally to a minimum of {.val {shift_value}}"))
    } else if(all(shift_minimum == "none")) {

        data_shifted <- .data
    } else { ## take custom groups

        data_shifted <- purrr::map(
            if (is.list(shift_minimum)) {
                shift_minimum
            } else {list(shift_minimum)},
            \(norm_range)
            .data |>
                dplyr::select(dplyr::any_of(norm_range)) |>
                dplyr::mutate(
                    dplyr::across(
                        dplyr::any_of(norm_range),
                        \(.x) .x - min(dplyr::pick(dplyr::any_of(norm_range)),
                                        na.rm = TRUE) + shift_value),
                )
        ) |>
            dplyr::bind_cols(
                dplyr::select(
                    .data,
                    -dplyr::any_of(unlist(shift_minimum)))
            ) |>
            dplyr::relocate(names(.data))

        cli::cli_alert_info(paste(
            "Data shifted per specified groups to a minimum of",
            "{.val {shift_value}}"))
    }

    ## normalise range ================================
    if (all(normalise == "column")) {

        data_normalised <- data_shifted |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    \(.x) (.x - min(.x, na.rm = TRUE)) /
                        diff(range(.x, na.rm = TRUE)) *
                        diff(normalise_range) + min(normalise_range)),
            )

        cli::cli_alert_info(paste(
            "Data re-scaled column-wise to a range of {.val {normalise_range}}"))
    } else if (all(normalise == "global")) {

        data_normalised <- data_shifted |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    \(.x) (.x - min(
                        dplyr::pick(dplyr::any_of(names(nirs_columns))),
                        na.rm = TRUE)) /
                        diff(range(
                            dplyr::pick(dplyr::any_of(names(nirs_columns))),
                            na.rm = TRUE)) *
                        diff(normalise_range) + min(normalise_range)),
            )

        cli::cli_alert_info(paste(
            "Data re-scaled globally to a range of {.val {normalise_range}}"))
    } else if(all(normalise == "none")) {

        data_normalised <- data_shifted
    } else { ## take custom normalising groups

        data_normalised <- purrr::map(
            if (is.list(normalise)) {
                normalise
            } else {list(normalise)},
            \(norm_range)
            data_shifted |>
                dplyr::select(dplyr::any_of(norm_range)) |>
                dplyr::mutate(
                    dplyr::across(
                        dplyr::any_of(norm_range),
                        \(.x) (.x - min(dplyr::pick(dplyr::any_of(norm_range)),
                                        na.rm = TRUE)) /
                            diff(range(dplyr::pick(dplyr::any_of(norm_range)),
                                       na.rm = TRUE)) *
                            diff(normalise_range) + min(normalise_range)),
                )
        ) |>
            dplyr::bind_cols(
                dplyr::select(
                    data_shifted,
                    -dplyr::any_of(unlist(normalise)))
            ) |>
            dplyr::relocate(names(data_shifted))

        cli::cli_alert_info(paste(
            "Data re-scaled per specified groups to a range of",
            "{.val {normalise_range}}"))
    }

    ## metadata ==================================================
    metadata$shifted <- shift_minimum
    metadata$normalised <- normalise
    create_mnirs_data(data_normalised, metadata)

}
#
## troubleshooting ===================================
# library(tidyverse)
# library(JAPackage)
# library(mNIRS)
#
# (raw_data <- read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_columns = c("ICG_VL" = "9", "ICG_SCM" = "10", "smo2" = "7"),
#     sample_column = c("Sample" = "1"),
#     # event_column = c("Event" = "11"),
# ) |> slice(-1))
#
# normalise_data(
#     .data = raw_data,
#     shift_minimum = list(c("smo2"))
#     normalise = list(c("ICG_VL", "smo2"), "ICG_SCM")
# )
#
# ggplot(data_shifted) +
#     {list( ## Settings
#         aes(x = index),
#         coord_cartesian(
#             xlim = c(NA, NA),
#             ylim = c(NA, NA)),
#         theme_JA(legend.position = "top"),
#         scale_x_continuous(
#             name = "index",
#         ),
#         scale_y_continuous(
#             name = "ICG_VL",
#             breaks = scales::breaks_pretty(),
#         ),
#         NULL)} + ## Settings
#     {list( ## Data
#         geom_hline(yintercept = c(0), linetype = "dotted"),
#         geom_line(aes(y = ICG_VL, colour = "VL")),
#         geom_line(aes(y = ICG_SCM, colour = "SCM")),
#         geom_line(aes(y = smo2, colour = "smo2")),
#         NULL)} ## Data
