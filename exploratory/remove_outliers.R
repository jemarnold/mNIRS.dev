#' Remove Fixed Values and Outliers from mNIRS Data
#'
#' Remove manually specified fixed values such as `c(0, 100)`, and remove
#' local outliers with [hampel()].
#'
#' @param .data A dataframe with mNIRS metadata.
#' @param remove_fixed_values (optional) Either `NULL` (the default) or a
#' list of numeric vectors of values to remove from mNIRS data, either
#' column-wise if a named list, or globally if not named.
#'
#' e.g.: `remove_fixed_values = list(SmO2 = c(0, 100), HHb = c(-5, 5))`.
#' @param remove_outliers A logical indicating whether local outliers should
#' be removed globally using a Hampel filter [hampel()].
#' @param sample_rate An integer scalar indicating the sample rate at which the
#' raw data was recorded (in Hz; samples per second).
#' @param ... Additional arguments.
#'
#' @details
#' Additional arguments will accept `k, t0, na.rm, return` for [hampel()].
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
remove_outliers <- function(
        .data,
        remove_fixed_values = NULL,
        remove_outliers = FALSE,
        sample_rate = NULL,
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

    ## ensure sample_rate is defined in metadata or manually
    if (is.null(sample_rate) & is.na(metadata$sample_rate)) {
        cli::cli_abort(paste(
            "Argument {.arg sample_rate} is missing, with no default."))

    } else if (is.null(sample_rate) & !is.na(metadata$sample_rate)) {
        sample_rate <- metadata$sample_rate
    }

    ## validation: `remove_fixed_values` must be NULL, numeric, or a list
    if (!(
        is.null(remove_fixed_values) |
        rlang::is_list(remove_fixed_values) |
        rlang::is_double(remove_fixed_values)
    )) {
        cli::cli_abort(paste(
            "{.arg remove_fixed_values} must be a {.cls numeric} vector",
            "or {.cls NULL}, not {.cls {class(remove_fixed_values)}}."))
    }

    ## validation: `remove_outliers` must be logical
    if (!isTRUE(remove_outliers) & !isFALSE(remove_outliers)) {
        cli::cli_abort(paste(
            "{.arg remove_outliers} must be either {.val {TRUE}}",
            "or {.val {FALSE}}, not {.val {remove_outliers}}."))
    }

    ## remove fixed values ================================
    ## fixed values can be defined column-wise or globally
    ## a vector will be converted to a list, a list will be
    ## converted to a named list. The named list will be mapped over
    ## to remove specified values for each defined nirs_column
    if (!is.null(remove_fixed_values)) {
        removed_fixed_values_list <- assign_to_named_list(
            remove_fixed_values,
            names(nirs_columns))

        data_nofixed <- purrr::imap(
            removed_fixed_values_list,
            \(fixed_vals, nirs) {
                .data |>
                    dplyr::select(dplyr::any_of(nirs)) |>
                    dplyr::mutate(
                        dplyr::across(
                            dplyr::any_of(nirs),
                            \(.x) dplyr::if_else(
                                .x %in% fixed_vals, NA_real_, .x))
                    )
            }) |>
            dplyr::bind_cols(
                dplyr::select(
                    .data,
                    -dplyr::any_of(names(removed_fixed_values_list)))
            ) |>
            dplyr::relocate(names(.data))

        removed_fixed_values_string <- paste(
            names(removed_fixed_values_list),
            removed_fixed_values_list,
            sep = " = ")

        cli::cli_alert_info(paste(
            "Values from {.val {removed_fixed_values_string}} set to",
            "{.val {NA}}."
        ))

    } else if (is.null(remove_fixed_values)) {
        data_nofixed <- .data
    }


    ## remove outliers ================================
    ## a hampel filter will be applied with default or custom parameters
    ## globally to all nirs_columns
    ## TODO can I pull out indices of outliers to metadata?
    if (remove_outliers) {

        if ("k" %in% names(args)) {
            k <- args$k
            cli::cli_alert_info("`hampel(k = {.val {k}})`")
        } else {k <- 20} ## 20 * sample_rate

        if ("t0" %in% names(args)) {
            t0 <- args$t0
            cli::cli_alert_info("`hampel(t0 = {.val {t0}})`")
        } else {t0 <- 3}

        if ("na.rm" %in% names(args)) {
            na.rm <- args$na.rm
            cli::cli_alert_info("`hampel(na.rm = {.val {na.rm}})`")
        } else {na.rm <- TRUE}

        if ("return" %in% names(args)) {
            return <- args$return
            cli::cli_alert_info("`hampel(return = {.val {return}})`")
        } else {return <- "median"}

        data_nooutliers <- data_nofixed |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    \(.x) hampel(.x, k = k, t0 = t0,
                                 na.rm = na.rm, return = return)$y
                ))

        cli::cli_alert_info("Local outliers set to the local median.")
    } else {
        data_nooutliers <- data_nofixed
    }

    ## metadata ==================================================
    metadata$fixed_values_removed <- if (!is.null(remove_fixed_values)) {
        removed_fixed_values_list
    } else {NA}
    metadata$outliers_removed <- remove_outliers

    create_mnirs_data(data_nooutliers, metadata)
    # attributes(data_nooutliers)
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
# (processed_data <- remove_outliers(
#     .data = raw_data,
#     sample_rate = 1,
#     remove_fixed_values = c(66.4, 70.9),
#     remove_outliers = TRUE,
#     k = 30, t0 = 3
# ))
# attributes(processed_data)

# remove_fixed_values = list(smo2_left_VL = c(66.4, 66.8),
#                            smo2_right_VL = c(70.9, 70.6))
# remove_fixed_values <- list(c(66.4, 70.9))
# remove_fixed_values <- c(66.4, 70.9)
#
# tst <- function(...) {
#
#     args <- list(...)
#
#
#     if (!"k" %in% names(args)) k <- 2 else k <- args$k
#     k + 3
# }
#
# tst(k = 5)
