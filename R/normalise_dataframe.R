#' Normalise data range
#'
#' Normalise the range of mNIRS signals while preserving the relative
#' scaling across columns.
#'
#' @param data A dataframe containing mNIRS data.
#' @param nirs_columns A `list()` of character vectors indicating the column
#' names for data signals to be shifted. Columns grouped together in a vector
#' will preserve relative scaling. Separate columns will shift to their own
#' specified values. Should match column names in the dataframe exactly. See
#' *Details*.
#' @param normalise_range A numeric vector in the form `c(min, max)`,
#' indicating the range of values to which the specified mNIRS data
#' columns will be re-scaled.
#'
#' @details
#' `nirs_columns` can be used to group data columns together to preserve
#' relative scaling across mNIRS signals.
#'
#' - `list(A, B, C)` will rescale each column separately. Relative values will
#' be shifted between columns.
#' - `list(c(A, B, C))` will rescale all columns together to a common range.
#' value. Relative scaling is preserved across the group of columns.
#' - `list(c(A, B), c(C, D))` will rescale columns `A` and `B` to a common
#' range, and columns `C` and `D` to a separate common range. This is a way to
#' create multiple groups of data columns where relative scaling is preserved
#' within groups, but not across groups of mNIRS signals.
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
normalise_dataframe <- function(
        data,
        nirs_columns = list(),
        normalise_range = c(0, 100)
) {

    metadata <- attributes(data)

    if (
        length(nirs_columns) == 0 &
        !is.null(attributes(data)$nirs_columns)
    ) {
        ## "global" condition from metadata$nirs_columns
        nirs_columns <- names(metadata$nirs_columns)
    }

    ## validation: `nirs_columns` must match expected dataframe names
    if (!all(unlist(nirs_columns) %in% names(data))) {
        cli::cli_abort(paste(
            "{.arg nirs_columns} must be a list of names.",
            "Make sure column names match exactly."))
    }

    ## validation: `normalise_range` must be numeric vector
    if (!rlang::is_double(normalise_range) | !length(normalise_range) == 2) {
        cli::cli_abort(paste(
            "{.arg normalise_range} must be a {.cls numeric} vector with",
            "{.val c(minimum, maximum)} values."))
    }

    ## normalise range ================================

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
                    \(.x) (.x - min(dplyr::pick(dplyr::any_of(.col)),
                                    na.rm = TRUE)) /
                        diff(range(dplyr::pick(dplyr::any_of(.col)),
                                   na.rm = TRUE)) *
                        diff(normalise_range) + min(normalise_range)),
            )
    ) |>
        dplyr::bind_cols(
            dplyr::select(data, -dplyr::any_of(unlist(nirs_columns)))
        ) |>
        dplyr::relocate(names(data))

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
# y <- normalise_dataframe(
#     data = df,
#     nirs_columns = list(c("ICG_VL", "ICG_SCM")),
#     normalise_range = c(0, 100)
# ) |>
#     print()
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
