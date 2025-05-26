#' Shift Data Range
#'
#' Shift the data range of mNIRS signals (e.g. shift to positive values) while
#' preserving the absolute dynamic range and relative scaling across columns.
#'
#' @param data A dataframe containing mNIRS data.
#' @param nirs_columns A `list()` of character vectors indicating the column
#' names for data signals to be shifted. Columns grouped together in a vector
#' will preserve relative scaling. Separate columns will shift to their own
#' specified values. Should match column names in the dataframe exactly. See
#' *Details*.
#' @param shift_to A numeric scalar to which the specified data columns
#' will be shifted to.
#' @param position Indicates how to shift values.
#'
#' - `"minimum"` *(default)* will shift selected columns' minimum values to the
#' specified `shift_to` value.
#' - `"maximum"` will shift selected columns by their maximum values.
#' - `"first"` will shift selected columns by their first existing values.
#' @param mean_samples An integer scalar representing the number of samples
#' over which the `position` is determined. e.g., `mean_samples = 1` looks for
#' the single minimum, maximum, or first value. `mean_samples = 30` would
#' use the mean of the lowest, highest, or first `30` samples.
#' @param shift_by *(optional)* specified data columns can be shifted by a set
#' amount.
#'
#' @details
#' `nirs_columns` can be used to group data columns together to preserve
#' relative scaling across mNIRS signals.
#'
#' - `list(A, B, C)` will shift each column separately within their own
#' values. Absolute dynamic range is preserved, but relative values will be
#' shifted between columns.
#' - `list(c(A, B, C))` will shift all columns together to a common specified
#' value. Absolute dynamic range and relative scaling are both preserved
#' across the group of columns.
#' - `list(c(A, B), c(C, D))` will shift columns `A` and `B` to a common value,
#' and columns `C` and `D` to a separate common value. This is a way to create
#' multiple groups of data columns where relative scaling is preserved within
#' groups, but not across groups of mNIRS signals.
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#' metadata available with `attributes()`.
#'
#' @export
shift_dataframe <- function(
        data,
        nirs_columns = list(),
        shift_to = 0,
        position = c("minimum", "maximum", "first"),
        mean_samples = 1,
        shift_by = NULL
) {

    position <- match.arg(position)
    metadata <- attributes(data)

    ## TODO convert sym(nirs_columns) to strings?

    if (
        length(nirs_columns) == 0 &
        !is.null(metadata$nirs_columns)
    ) {
        ## "global" condition from metadata$nirs_columns
        nirs_columns <- metadata$nirs_columns
    }

    ## validation: `nirs_columns` must match expected dataframe names
    if (!all(unlist(nirs_columns) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg nirs_columns} must be a list of names.",
            "Make sure column names match exactly."))
    }

    ## validation: `shift_to` or `shift_by` must be numeric scalar
    if (
        !(is.numeric(shift_to) | is.numeric(shift_by)) |
        !(length(shift_to) == 1 | length(shift_by) == 1)
    ) {
        cli::cli_abort(paste(
            "Either {.arg shift_to} or {.arg shift_by} must be a single",
            "{.cls numeric} scalar."))
    }

    ## validation: `mean_samples` must be numeric scalar
    if (!is.numeric(mean_samples) | !length(mean_samples) == 1) {
        cli::cli_abort(paste(
            "{.arg mean_samples} must be a single {.cls numeric} scalar."))
    }

    ## shift range ================================
    ## TODO NOT IN USE; only marginally faster than more trustworthy
    ## ZOO::rollapply option
    # rolling_mean_fast <- function(x, width) {
    #     n <- length(x)
    #     half_width <- floor(width / 2)
    #
    #     # Create index matrix for vectorised operations
    #     indices <- outer(
    #         seq_len(n), seq_len(width),
    #         \(i, j) {
    #             idx <- i - half_width + j - 1
    #             ifelse(idx >= 1 & idx <= n, idx, NA)
    #         })
    #
    #     # Extract values using matrix indexing
    #     values <- matrix(x[indices], nrow = n)
    #
    #     # Calculate means with na.rm = TRUE
    #     result <- apply(
    #         values, 1,
    #         \(row) {
    #             valid_vals <- row[!is.na(row)]
    #             if (length(valid_vals) > 0) {
    #                 mean(valid_vals)
    #             } else {NA}
    #         })
    #
    #     return(result)
    # }

    if (position %in% c("minimum", "maximum")) {

        shift_mean_value <- data |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(unlist(nirs_columns)),
                    \(.x) zoo::rollapply(
                        .x, width = mean_samples, FUN = mean,
                        align = "center", partial = TRUE, na.rm = TRUE)),
                    # \(.x) rolling_mean_fast(.x, mean_samples)),
            ) |>
            dplyr::summarise(
                dplyr::across(
                    dplyr::any_of(unlist(nirs_columns)),
                    \(.x) if (position == "minimum") {
                        min(.x, na.rm = TRUE)
                    } else if (position == "maximum") {
                        max(.x, na.rm = TRUE)
                    })
            )

    } else if (position == "first") {

        shift_mean_value <- head(data, mean_samples) |>
            dplyr::summarise(
                dplyr::across(
                    dplyr::any_of(unlist(nirs_columns)),
                    \(.x) mean(.x, na.rm = TRUE))
            )

    }

    y <- purrr::map(
        if (is.list(nirs_columns)) {
            nirs_columns
        } else {list(nirs_columns)},
        \(.col)
        data |>
            dplyr::select(dplyr::any_of(.col)) |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::any_of(.col),
                    \(.x) if (!is.null(shift_by) & is.null(shift_to)) {

                        .x + shift_by

                    } else if (position == "minimum") {

                        .x - min(shift_mean_value[.col]) + shift_to

                    } else if (position == "maximum") {

                        .x - max(shift_mean_value[.col]) + shift_to

                    } else if (position == "first") {

                        .x - dplyr::first(
                            rowMeans(shift_mean_value[.col])) + shift_to

                    }
                ),
            )
    ) |>
        dplyr::bind_cols(
            dplyr::select(data, -dplyr::any_of(unlist(nirs_columns)))
        ) |>
        dplyr::relocate(names(data))

    ## Metadata =================================
    metadata$nirs_columns <- unique(
        c(metadata$nirs_columns, unlist(nirs_columns)))

    y <- create_mnirs_data(y, metadata)

    return(y)
}
#
## troubleshooting ===================================
# library(tidyverse)
# library(JAPackage)
# library(mNIRS)
# #
# (df <- read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_columns = c("ICG_VL" = "9", "ICG_SCM" = "10", "smo2" = "7"),
#     sample_column = c("Sample" = "1"),
#     # event_column = c("Event" = "11"),
# ) |> dplyr::slice(-1))
#
# y <- shift_dataframe(
#     # data = tibble(ICG_VL = 1:10, ICG_SCM = -10:-1),
#     data = df,
#     nirs_columns = list("ICG_VL", "ICG_SCM", "smo2"),
#     shift_to = 0,
#     position = "max",
#     mean_samples = 100,
#     shift_by = NULL
# ) |>
#     print()
# attributes(y)
#
# ggplot(y) +
#     {list( ## Settings
#         aes(x = Sample),
#         # coord_cartesian(xlim = c(NA, 200)),
#         theme_JA(legend.position = "top"),
#         NULL)} + ## Settings
#     {list( ## Data
#         geom_hline(yintercept = 0, linetype = "dotted"),
#         geom_hline(yintercept = 10, linetype = "dotted"),
#         geom_line(aes(y = ICG_VL, colour = "VL")),
#         geom_line(aes(y = ICG_SCM, colour = "SCM")),
#         geom_line(aes(y = smo2, colour = "smo2")),
#         NULL)} ## Data


# ## test function to get `enquote(...)` method working
# test <- function(..., shift_by = 1) {
#     # A <- dplyr::enquo(A)
#     # data |> select(!!A)
#
#     args <- dplyr::enquos(...)
#     # data |> select(!!!args)
#
#
#
#     y <- purrr::map(
#         args,
#         \(.col)
#         data |>
#             dplyr::select(!!.col) |>
#             dplyr::mutate(
#                 dplyr::across(
#                     !!.col,
#                     \(.x) .x + shift_by),
#             )
#     ) |>
#         dplyr::bind_cols(
#             dplyr::select(data, -c(!!!args))
#         ) |>
#         dplyr::relocate(names(data))
#
#     return(y)
# }
# test(ICG_VL, ICG_SCM, one)
