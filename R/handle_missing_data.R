#' Handle missing data in mNIRS Data
#'
#' Linearly interpolate across missing mNIRS data.
#'
#' @param .data A dataframe with mNIRS metadata.
#' @param handle_missing_values Indicates how to handle missing data globally
#' before filtering can be applied.
#' - *"none"* (the default) will pass through data as-is. May cause errors.
#' - *"interpolate"* will interpolate between existing values.
#' - *"omit"* will remove `NA`s before analysis and re-insert them back to
#' their original indices.
#' @param ... Additional arguments.
#'
#' @details
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
#' back to their original indices when the filtered dataframe is returned.
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
handle_missing_data <- function(
        .data,
        handle_missing_values = c("none", "interpolate", "omit"),
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

    ## report of missing values ===================================
    missing_indices <- .data |>
        dplyr::filter(
            dplyr::if_any(
                dplyr::any_of(names(nirs_columns)),
                \(.x) is.na(.x))
        ) |>
        dplyr::reframe(
            dplyr::across(
                dplyr::any_of(names(nirs_columns)),
                \(.x) list(index[is.na(.x)]))
        ) |>
        purrr::map(\(.x) unlist(.x[[1]]))

    ## report total count of missing values from all nirs_columns
    missing_count <- .data |>
        dplyr::summarise(
            dplyr::across(
                dplyr::any_of(names(nirs_columns)),
                \(.x) sum(is.na(.x)))
        ) |>
        dplyr::summarise(
            count = rowSums(dplyr::across(dplyr::where(is.numeric)))
        ) |>
        dplyr::pull(count)

    ## function to find the longest continuous sequence of missing values
    find_longest_NA_sequence <- function(x) {
        if (!any(is.na(x))) return(0)

        rle_result <- rle(is.na(x))
        NA_sequence <- max(rle_result$lengths[rle_result$values])
        return(NA_sequence)
    }

    ## report longest continuouse sequence of missing values from
    ## all nirs_columns
    missing_sequence <- .data |>
        dplyr::summarise(
            dplyr::across(
                dplyr::any_of(names(nirs_columns)),
                \(.x) find_longest_NA_sequence(.x))
        ) |>
        dplyr::summarise(
            length = max(dplyr::across(dplyr::where(is.numeric)))
        ) |>
        dplyr::pull(length)

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
                    \(.x) zoo::na.approx(
                        .x, na.rm = FALSE, rule = 2, maxgap = maxgap)),
            )

        cli::cli_alert_info(paste(
            "Missing data interpolated at {.val {missing_count}} total indices.",
            "The longest continuous sequence was {.val {missing_sequence}} indices."
        ))

    } else if (handle_missing_values == "omit") {
        ## omit method not implemented. Redundant?
        ## TODO implement

        cli::cli_abort(paste(
            "{.arg handle_missing_values} = {.val {handle_missing_values}}",
            "is not currently implemented."
        ))

        #     missing_indices <- .data |>
        #         dplyr::filter(
        #             dplyr::if_any(
        #                 dplyr::any_of(names(nirs_columns)),
        #                 \(.x) is.na(.x))) |>
        #         dplyr::pull(index)
        #
        #     data_nomissing <- .data |>
        #         tidyr::drop_na(dplyr::any_of(names(nirs_columns)))

    } else if (handle_missing_values == "none") {
        data_nomissing <- .data
    }

    ## metadata ==================================================
    metadata$missing_indices <- missing_indices

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
# (raw_data <- raw_data |>
#     dplyr::mutate(
#         dplyr::across(
#             smo2_left_VL,
#             \(.x) dplyr::if_else(dplyr::between(Time, 100, 200), NA, .x)),
#         dplyr::across(
#             smo2_right_VL,
#             \(.x) dplyr::if_else(dplyr::between(Time, 484, 585), NA, .x)),
#     ))
# # (df <- remove_outliers(
# #     .data = raw_data,
# #     sample_rate = 1,
# #     remove_fixed_values = c(66.4, 70.9),
# #     remove_outliers = TRUE,
# #     k = 30, t0 = 3
# # ))
# # raw_data |> dplyr::filter(is.na(smo2_right_VL))
# (df <- handle_missing_data(
#     .data = raw_data,
#     handle_missing_values = "interpolate"
#     # maxgap = 30
# ))
# # df |> dplyr::filter(is.na(smo2_right_VL))
# attributes(df)
