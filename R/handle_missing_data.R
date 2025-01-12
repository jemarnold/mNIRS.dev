#' Handle missing data in mNIRS Data
#'
#' Linearly interpolate across missing mNIRS data.
#'
#' @param .data A dataframe with mNIRS metadata.
#' @param handle_missing_values Indicates how to handle missing data.
#' - *"interpolate"* (the default) will interpolate between existing
#' values.
#' - *"none"* will pass through missing data. This may introduce errors
#' further along in processing.
#'
#' @details
#' Additional details...
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
handle_missing_data <- function(
        .data,
        handle_missing_values = c("interpolate", "none"),
        ...
) {
    ## validation: check for metadata to ensure `read_data()` has been run
    if (is.null(attributes(.data)$nirs_columns)) {
        cli::cli_abort(paste(
            "Data should be extracted with {.fn read_data} before processing."
        ))
    }

    ## pass through optional arguments
    handle_missing_values <- match.arg(handle_missing_values)
    args <- list(...)

    metadata <- attributes(.data)
    nirs_columns <- metadata$nirs_columns

    ## handle missing values ================================
    if (handle_missing_values == "interpolate") {

        if ("maxgap" %in% names(args)) {
            maxgap <- args$maxgap
            cli::cli_alert_info("`na.approx(maxgap = {.val {maxgap}})`")
        } else {maxgap <- Inf}

        data_nomissing <- .data |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(names(nirs_columns)),
                    ~ zoo::na.approx(
                        ., na.rm = FALSE, rule = 2, maxgap = maxgap)),
            )
        ## omit method not implemented. Redundant?
        # } else if (handle_missing_values == "omit") {
        #     missing_indices <- .data |>
        #         dplyr::filter(
        #             dplyr::if_any(
        #                 dplyr::any_of(names(nirs_columns)),
        #                 ~ is.na(.))) |>
        #         dplyr::pull(index)
        #
        #     data_nomissing <- .data |>
        #         tidyr::drop_na(dplyr::any_of(names(nirs_columns)))
    } else if (handle_missing_values == "none") {
        data_nomissing <- .data
    }

    ## metadata ==================================================
    metadata$missing_index <- data_nomissing$index[
        which(is.na(data_nomissing[names(nirs_columns)]))]

    create_mnirs_data(data_nomissing, metadata)
    # attributes(data_nomissing)
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
# (df <- remove_outliers(
#     .data = raw_data,
#     sample_rate = 1,
#     remove_fixed_values = c(66.4, 70.9),
#     remove_outliers = TRUE,
#     k = 30, t0 = 3
# ))
# (df <- handle_missing_data(
#     .data = df,
#     handle_missing_values = "interpolate"#,
#     # maxgap = 30
# ))
# dplyr::filter(df, is.na(smo2_left_VL))
# attributes(df)
