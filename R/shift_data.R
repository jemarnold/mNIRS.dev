#' Shift Data Range
#'
#' Shift the range of data channels while preserving the absolute dynamic
#' range and/or relative scaling across channels. e.g. shift data range to positive
#' values, or shift the start value of a recording to zero.
#'
#' @param data A dataframe.
#' @param nirs_columns A `list()` of character vectors indicating the column
#' names for data channels to be shifted (see *Details*).
#' @param shift_to A numeric scalar to which the specified data channels
#' will be shifted to.
#' @param position Indicates how to shift values.
#' \describe{
#'   \item{`"minimum"`}{will shift selected channels' minimum values
#'   to the specified `shift_to` value (*default*).}
#'   \item{`"maximum"`}{will shift selected channels by their maximum values.}
#'   \item{`"first"`}{will shift selected channels by their first values.}
#' }
#' @param mean_samples An integer scalar representing the number of samples
#' over which the `position` is determined. e.g., `mean_samples = 1` looks for
#' the single minimum, maximum, or first value. `mean_samples = 30` would
#' use the mean of the lowest, highest, or first `30` samples.
#' @param shift_by (*Optional*). A numeric scalar by which the data signals can
#' be shifted by a set amount.
#'
#' @details
#' `nirs_columns = list()` can be used to group data columns to preserve
#' absolute or relative scaling. channels grouped together in a vector
#' will preserve relative scaling across channels. Should match column names
#' in the dataframe exactly.
#'
#' \describe{
#'   \item{`nirs_columns = list("A", "B", "C")`}{will shift each column separately.
#'   Absolute dynamic range for each data channel is preserved, but relative
#'   scaling will be lost between data channels.}
#'   \item{`nirs_columns = list(c("A", "B", "C"))`}{will shift all columns together.
#'   Absolute dynamic range and relative scaling are both preserved across the
#'   group of data channels.}
#'   \item{`nirs_columns = list(c("A", "B"), c("C", "D"))`}{will shift columns `A`
#'   and `B` together, and columns `C` and `D` together. Absolute dynamic range
#'   and relative scaling are preserved within each group, but not across groups
#'   of data channels.}
#' }
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#' metadata available with `attributes()`.
#'
#' @export
shift_data <- function(
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
    ## TODO NOT IN USE; seems to only be marginally faster than more
    ## established zoo::rollapply() function
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
        }) |>
        dplyr::bind_cols(
            dplyr::select(data, -tidyselect::any_of(unlist(nirs_columns)))
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
# (data <- read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_columns = c(ICG_VL = "9", ICG_SCM = "10", smo2 = "7"),
#     sample_column = c(Sample = "1"),
#     # event_column = c(Event = "11"),
# ) |> dplyr::slice(-1))
# #
# y <- shift_data(
#     # data = tibble(ICG_VL = 1:10, ICG_SCM = -10:-1),
#     data = data,
#     nirs_columns = list("ICG_VL", "ICG_SCM", "smo2"),
#     shift_to = 0,
#     position = "maximum",
#     mean_samples = 1,
#     shift_by = NULL
# ) |>
#     print()
# # attributes(y)
# #
# ggplot2::ggplot(y) +
#     {list( ## Settings
#         ggplot2::aes(x = Sample),
#         # coord_cartesian(xlim = c(NA, 200)),
#         # theme_JA(legend.position = "top"),
#         NULL)} + ## Settings
#     {list( ## Data
#         ggplot2::geom_hline(yintercept = 0, linetype = "dotted"),
#         ggplot2::geom_hline(yintercept = 10, linetype = "dotted"),
#         ggplot2::geom_line(ggplot2::aes(y = ICG_VL, colour = "VL")),
#         ggplot2::geom_line(ggplot2::aes(y = ICG_SCM, colour = "SCM")),
#         ggplot2::geom_line(ggplot2::aes(y = smo2, colour = "smo2")),
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
