#' Shift Data Range
#'
#' Move the range of data channels in a dataframe up or down, while preserving
#' the absolute dynamic range and/or relative scaling across channels. e.g. shift
#' data range to positive values, or shift the mean of the first X number of
#' samples in a recording to zero.
#'
#' @param data A dataframe.
#' @param nirs_channels A `list()` of character vectors indicating the column
#'  names for data channels to be shifted (see *Details*).
#'  \describe{
#'      \item{`nirs_channels = list("A", "B", "C")`}{will shift each channel
#'      separately, losing relative scaling.}
#'      \item{`nirs_channels = list(c("A", "B", "C"))`}{will shift all channels
#'      together, preserving relative scaling.}
#'      \item{`nirs_channels = list(c("A", "B"), c("C", "D"))`}{will shift channels
#'      `A` and `B` together, and channels `C` and `D` together, preserving
#'      relative scaling within, but not across each group.}
#'  }
#' @param sample_channel A character scalar indicating the time or sample data
#'  channel. Must match `data` column names exactly. Will be taken from metadata
#'  if not defined explicitly.
#' @param shift_to A positive or negative numeric scalar in units of `nirs_channels`
#'  to which the specified data channels will be shifted.
#' @param shift_by An *optional* positive or negative numeric scalar by which the
#'  data signals can be shifted by a set amount, in units of the `nirs_channels`.
#' @param span A numeric scalar in units of `sample_channel`, defining the range
#'  over which the `position` is determined. `span = 0` takes the single
#'  minimum, maximum, or first value. `span = 30` would use the mean of the samples
#'  within the lowest, highest, or first `30` units of the `sample_channel`.
#' @param position Indicates how to shift values.
#'  \describe{
#'      \item{`"minimum"`}{will shift selected channels' minimum values
#'      to the specified `shift_to` value (the *default*).}
#'      \item{`"maximum"`}{will shift selected channels by their maximum values.}
#'      \item{`"first"`}{will shift selected channels by their first values.}
#'  }
#'
#' @details
#' `nirs_channels = list()` can be used to group data channels to preserve
#' absolute or relative scaling. Channels grouped together in a vector
#' will preserve relative scaling across those channels. Should match column names
#' in the dataframe exactly.
#'
#'  \describe{
#'      \item{`nirs_channels = list("A", "B", "C")`}{will shift each channel
#'      separately. Absolute dynamic range for each data channel is preserved, but
#'      relative scaling will be lost between data channels.}
#'      \item{`nirs_channels = list(c("A", "B", "C"))`}{will shift all channels
#'      together. Absolute dynamic range and relative scaling are both preserved
#'      across the group of data channels.}
#'      \item{`nirs_channels = list(c("A", "B"), c("C", "D"))`}{will shift channels
#'      `A` and `B` together, and channels `C` and `D` together. Absolute dynamic
#'      range and relative scaling are preserved within each group, but not across
#'      groups of data channels.}
#'  }
#'
#' Channels not explicitly specified will not be shifted, but will be passed
#' through to the output dataframe.
#'
#' If both `shift_to` and `shift_by` are defined, only `shift_to` will be used.
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#'  metadata available with `attributes()`.
#'
#' @export
shift_data <- function(
        data,
        nirs_channels = list(),
        sample_channel = NULL,
        shift_to = NULL,
        shift_by = NULL,
        span = 0,
        position = c("minimum", "maximum", "first")
) {
    position <- match.arg(position)
    metadata <- attributes(data)

    ## TODO shift by sample range, not only first samples
    ## TODO convert sym(nirs_channels) to strings?

    ## `nirs_channels` must be defined and grouped explicitly
    if (is.null(nirs_channels) || length(nirs_channels) == 0) {
        cli_abort("{.arg nirs_channels} should be defined and grouped explicitly.")
    } else if (!is.character(unlist(nirs_channels)) ||
               !all(unlist(nirs_channels) %in% names(data))) {
        cli_abort(paste(
            "{.arg nirs_channels} must be a list of column names within",
            "{.arg data}. Make sure column names match exactly."))
    }

    ## define `sample_channel` manually overrides metadata
    if (is.null(sample_channel) & is.null(metadata$sample_channel)) {
        cli_abort(paste(
            "{.arg sample_channel} not found in metadata. Please check your data",
            "attributes or define {.arg sample_channel} explicitly."))
    } else if (is.null(sample_channel) & !is.null(metadata$sample_channel)) {
        sample_channel <- metadata$sample_channel
    } else if (!is.character(sample_channel) || !sample_channel %in% names(data)) {
        cli_abort(paste(
            "{.arg sample_channel} must be a column name within your {.arg data}.",
            "Make sure column names match exactly."))
    }

    ## validation: `shift_to` or `shift_by` must be numeric scalar
    if (!(is.numeric(shift_to) | is.numeric(shift_by)) ||
        !(length(shift_to) == 1 | length(shift_by) == 1)) {
        cli_abort(paste(
            "Either {.arg shift_to} or {.arg shift_by} must be a single",
            "{.cls numeric} scalar."))
    }

    ## validation: `span` must be numeric scalar
    if (!is.numeric(span) | !length(span) == 1) {
        cli_abort("{.arg span} must be a single-element {.cls numeric} value.")
    }

    ## shift range ================================
    if (position %in% c("minimum", "maximum")) {

        rolling_means <- apply(data[unlist(nirs_channels)], 2, \(.x) {
            dx <- mean(diff(.x), na.rm = TRUE)
            k <- ifelse(span == 0, 1, round(span / dx))
            zoo::rollmean(.x, k = k, align = "left")
        })

        shift_values <- if (position == "minimum") {
            apply(rolling_means, 2, min, na.rm = TRUE)
        } else if (position == "maximum") {
            apply(rolling_means, 2, max, na.rm = TRUE)
        }

    } else if (position == "first") {

        head_max <- data[[sample_channel]][1] + span
        head_data <- data[data[[sample_channel]] <= head_max, ][unlist(nirs_channels)]
        shift_values <- colMeans(head_data, na.rm = TRUE)
    }

    nirs_list <- if (is.list(nirs_channels)) nirs_channels else list(nirs_channels)

    for (cols in nirs_list) {
        if (!is.numeric(shift_to) & is.numeric(shift_by)) {
            data[cols] <- data[cols] + shift_by
        } else if (position == "minimum") {
            data[cols] <- data[cols] - min(shift_values[cols]) + shift_to
        } else if (position == "maximum") {
            data[cols] <- data[cols] - max(shift_values[cols]) + shift_to
        } else if (position == "first") {
            data[cols] <- data[cols] - mean(shift_values[cols]) + shift_to
        }
    }

    ## Metadata =================================
    metadata$nirs_channels <- unique(
        c(metadata$nirs_channels, unlist(nirs_channels)))
    metadata$sample_channel <- sample_channel

    return(create_mNIRS_data(data, metadata))
}
