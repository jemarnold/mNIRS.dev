#' Rescale Data Range
#'
#' Resize the range (min and max values) of data channels to a new dynamic range,
#' while preserving the relative scaling across channels. e.g. rescale the range
#' of data to `c(0,100)`.
#'
#' @param data A dataframe.
#' @param nirs_columns A `list()` of character vectors indicating the column
#'  names for data channels to be rescaled (see *Details*).
#' @param rescale_range A numeric vector in the form `c(min, max)`,
#'  indicating the range of output values to which data channels will be rescaled.
#'
#' @details
#' `nirs_columns = list()` can be used to group data columns to preserve
#' relative scaling. channels grouped together in a vector will preserve
#' relative scaling across channels. Should match column names in the dataframe
#' exactly.
#'
#'  \describe{
#'      \item{`nirs_columns = list("A", "B", "C")`}{will rescale each column
#'      separately. Relative scaling will be lost between data channels.}
#'      \item{`nirs_columns = list(c("A", "B", "C"))`}{will rescale all columns
#'      together. Relative scaling is preserved across the group of data channels.}
#'      \item{`nirs_columns = list(c("A", "B"), c("C", "D"))`}{will rescale columns
#'      `A` and `B` together, and columns `C` and `D` together. Relative scaling
#'      is preserved within each group, but not across groups of data channels.}
#'  }
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#'  metadata available with `attributes()`.
#'
#' @export
rescale_data <- function(
        data,
        nirs_columns = list(),
        rescale_range = c(0, 100)
) {

    metadata <- attributes(data)

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

    ## validation: `rescale_range` must be numeric vector
    if (!is.numeric(rescale_range) | !length(rescale_range) == 2) {
        cli::cli_abort(paste(
            "{.arg rescale_range} must be a {.cls numeric} vector with",
            "{.val c(min, max)} values."))
    }

    ## rescale range ================================

    y <- lapply(
        if (is.list(nirs_columns)) {
            nirs_columns
        } else {list(nirs_columns)},
        \(.col) {
            data |>
                dplyr::select(tidyselect::any_of(.col)) |>
                dplyr::mutate(
                    dplyr::across(
                        tidyselect::any_of(.col),
                        \(.x) (.x - min(dplyr::pick(tidyselect::any_of(.col)),
                                        na.rm = TRUE)) /
                            diff(range(dplyr::pick(tidyselect::any_of(.col)),
                                       na.rm = TRUE)) *
                            diff(rescale_range) + min(rescale_range)),
                )
        }) |>
        dplyr::bind_cols(
            dplyr::select(data, -tidyselect::any_of(unlist(nirs_columns)))
        ) |>
        dplyr::relocate(names(data))

    ## Metadata =================================
    metadata$nirs_columns <- unique(
        c(metadata$nirs_columns, unlist(nirs_columns)))

    y <- create_mNIRS_data(y, metadata)

    return(y)
}
#
## troubleshooting ===================================
# library(ggplot2)
# library(mNIRS)
# #
# (df <- read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_columns = c("ICG_VL" = "9", "ICG_SCM" = "10", "smo2" = "7"),
#     sample_column = c("Sample" = "1"),
#     # event_column = c("Event" = "11"),
# ) |> dplyr::slice(-1))
# #
# y <- rescale_data(
#     data = df,
#     nirs_columns = list(c("ICG_VL", "ICG_SCM")),
#     rescale_range = c(0, 100)
# ) |> print()
# attributes(y)
#
# ggplot(y) +
#     {list( ## Settings
#         aes(x = Sample),
#         # coord_cartesian(xlim = c(NA, 200)),
#         JAPackage::theme_JA(legend.position = "top"),
#         NULL)} + ## Settings
#     {list( ## Data
#         geom_hline(yintercept = 0, linetype = "dotted"),
#         geom_hline(yintercept = 10, linetype = "dotted"),
#         geom_line(aes(y = ICG_VL, colour = "VL")),
#         geom_line(aes(y = ICG_SCM, colour = "SCM")),
#         geom_line(aes(y = smo2, colour = "smo2")),
#         NULL)} ## Data
