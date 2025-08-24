#' Rescale Data Range
#'
#' Resize the range (min and max values) of data channels to a new dynamic range,
#' while preserving the relative scaling across channels. e.g. rescale the range
#' of data to `c(0,100)`.
#'
#' @param data A dataframe.
#' @param nirs_channels A `list()` of character vectors indicating the column
#'  names for data channels to be rescaled (see *Details*).
#' @param rescale_range A numeric vector in the form `c(min, max)`,
#'  indicating the range of output values to which data channels will be rescaled.
#'
#' @details
#' `nirs_channels = list()` can be used to group data channels to preserve
#' relative scaling. channels grouped together in a vector will preserve
#' relative scaling across those channels. Should match column names in the
#' dataframe exactly.
#'
#'  \describe{
#'      \item{`nirs_channels = list("A", "B", "C")`}{will rescale each channel
#'      separately. Relative scaling will be lost between data channels.}
#'      \item{`nirs_channels = list(c("A", "B", "C"))`}{will rescale all channels
#'      together. Relative scaling is preserved across the group of data channels.}
#'      \item{`nirs_channels = list(c("A", "B"), c("C", "D"))`}{will rescale channels
#'      `A` and `B` together, and channels `C` and `D` together. Relative scaling
#'      is preserved within each group, but not across groups of data channels.}
#'  }
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#'  metadata available with `attributes()`.
#'
#' @export
rescale_data <- function(
        data,
        nirs_channels = list(),
        rescale_range = c(0, 100)
) {

    metadata <- attributes(data)

    if (
        length(nirs_channels) == 0 &
        !is.null(metadata$nirs_channels)
    ) {
        ## "global" condition from metadata$nirs_channels
        nirs_channels <- metadata$nirs_channels
    }

    ## validation: `nirs_channels` must match expected dataframe names
    if (!all(unlist(nirs_channels) %in% names(data))) {
        cli_abort(paste(
            "{.arg nirs_channels} must be a list of names.",
            "Make sure channel names match exactly."))
    }

    ## validation: `rescale_range` must be numeric vector
    if (!is.numeric(rescale_range) | !length(rescale_range) == 2) {
        cli_abort(paste(
            "{.arg rescale_range} must be a {.cls numeric} vector with",
            "{.val c(min, max)} values."))
    }

    ## rescale range ================================

    y <- lapply(
        if (is.list(nirs_channels)) {
            nirs_channels
        } else {list(nirs_channels)},
        \(.col) {
            data |>
                dplyr::select(dplyr::any_of(.col)) |>
                dplyr::mutate(
                    dplyr::across(
                        dplyr::any_of(.col),
                        \(.x) (.x - min(dplyr::pick(dplyr::any_of(.col)),
                                        na.rm = TRUE)) /
                            diff(range(dplyr::pick(dplyr::any_of(.col)),
                                       na.rm = TRUE)) *
                            diff(rescale_range) + min(rescale_range)),
                )
        }) |>
        dplyr::bind_cols(
            dplyr::select(data, -dplyr::any_of(unlist(nirs_channels)))
        ) |>
        dplyr::relocate(names(data))

    ## Metadata =================================
    metadata$nirs_channels <- unique(
        c(metadata$nirs_channels, unlist(nirs_channels)))

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
#     nirs_channels = c("ICG_VL" = "9", "ICG_SCM" = "10", "smo2" = "7"),
#     sample_channel = c("Sample" = "1"),
#     # event_channel = c("Event" = "11"),
# ) |> dplyr::slice(-1))
# #
# y <- rescale_data(
#     data = df,
#     nirs_channels = list(c("ICG_VL", "ICG_SCM")),
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
