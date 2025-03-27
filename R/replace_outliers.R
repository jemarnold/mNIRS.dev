#' Replace Fixed Values and Outliers from mNIRS Data
#'
#' Detect specified values such as `c(0, 100)`, and local outliers with
#' [hampel()] within mNIRS vector data, and replaces with the local median value.
#'
#' @param .x A numeric vector of mNIRS data.
#' @param fixed_values (optional) Either `NULL` (the default) or a
#' numeric vector of values to be overwritten. e.g. `fixed_values = c(0, 100)`.
#' @param outliers A logical indicating whether local outliers should
#' be replaced using a Hampel filter [hampel()].
#' @param k A numeric scalar for the window length of `(2 · k + 1)` indices,
#' for local outlier detection and the median value replacement.
#' @param ... Additional arguments.
#'
#' @details
#' Additional arguments will accept `t0, na.rm, return` for [hampel()].
#'
#' @return A numeric vector.
#'
#' @export
replace_outliers <- function(
        .x,
        fixed_values = NULL, ## numeric vector
        outliers = FALSE, ## logical
        k = 20, ## numeric scalar
        ...
) {

    ## pass through optional arguments
    args <- list(...)

    if (!rlang::is_double(k) | length(k) > 1) {
        cli::cli_abort(paste("{.arg k} must be a {.cls numeric} scalar."))
    }

    ## replace fixed values ================================
    ## TODO replace fixed values with local median
    ## validation: `fixed_values` must be NULL or numeric vector
    if (!(
        is.null(fixed_values) | rlang::is_double(fixed_values)
    )) {

        cli::cli_abort(paste(
            "{.arg fixed_values} must be a {.cls numeric} vector",
            "or {.cls NULL}, not {.cls {class(fixed_values)}}."))
    } else if (rlang::is_double(fixed_values)) {

        # fixed_removed <- .x[!.x %in% fixed_values]

        ## TODO replace fixed values with local median
        fixed_indices <- which(.x %in% fixed_values)

    } else if (is.null(fixed_values)) {
        fixed_removed <- .x
    }


    ## replace outliers ================================
    ## validation: `outliers` must be logical
    if (!isTRUE(outliers) & !isFALSE(outliers)) {

        cli::cli_abort(paste(
            "{.arg outliers} must be either {.val {TRUE}}",
            "or {.val {FALSE}}, not {.val {outliers}}."))

    } else if (outliers) {

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

        outliers_removed <- mNIRS::hampel(
            fixed_removed,
            k = k,
            t0 = t0,
            na.rm = na.rm,
            return = return)$y

    } else if (!outliers) {
        outliers_removed <- fixed_removed
    }

    ## return ====================================
    return(outliers_removed)
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
# (processed_data <- outliers(
#     .data = raw_data,
#     sample_rate = 1,
#     fixed_values = c(66.4, 70.9),
#     outliers = TRUE,
#     k = 30, t0 = 3
# ))
# attributes(processed_data)

# fixed_values = list(smo2_left_VL = c(66.4, 66.8),
#                            smo2_right_VL = c(70.9, 70.6))
# fixed_values <- list(c(66.4, 70.9))
# fixed_values <- c(66.4, 70.9)
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
