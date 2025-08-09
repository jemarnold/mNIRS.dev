#' Plot mNIRS.data objects
#'
#' Create a simple plot for objects returned from [create_mNIRS.data()].
#'
#' @param data Object of class `mNIRS.data` as returned from [create_mNIRS.data()]
#' @param na.omit A logical indicating whether missing data (`NA`) should be omitted
#' (`TRUE`) for better display of disconnected lines, or included (`FALSE`, the
#' default) to better recognise where data are missing.
#' @param ... Additional arguments (*currently not used*).
#'
#' @return A [ggplot2][ggplot2::ggplot()] object.
#'
#' @importFrom ggplot2 ggplot aes waiver expansion scale_x_continuous
#'  scale_y_continuous geom_line geom_point
#'
#' @export
plot.mNIRS.data <- function(data, na.omit = FALSE, ...) {

    # if (!requireNamespace("ggplot2", quietly = TRUE)) {
    #     cli::cli_abort(paste(
    #         "Package {.pkg ggplot2} is required for plotting.",
    #         "Please install it."))
    # }
    rlang::check_installed("ggplot2", reason = "to plot mNIRS data")

    nirs_columns <- attributes(data)$nirs_columns
    sample_column <- attributes(data)$sample_column

    plot <- data |>
        ## pivot all `nirs_columns` to `y` and plot by group
        tidyr::pivot_longer(
            cols = tidyselect::all_of(nirs_columns),
            names_to = "nirs_columns",
            values_to = "y") |>
        ## remove empty rows for geom_line
        ( \(.df) if (na.omit) {tidyr::drop_na(.df, y)} else {.df})() |>
        ggplot() +
        aes(x = .data[[sample_column]], y = y,
            colour = nirs_columns) +
        theme_mNIRS() +
        scale_x_continuous(
            # name = sample_column,
            breaks = if (rlang::is_installed("scales")) {
                scales::breaks_pretty(n = 6)
            } else {
                waiver()
            },
            expand = expansion(mult = 0.01)) +
        scale_y_continuous(
            name = "mNIRS Signals",
            breaks = if (rlang::is_installed("scales")) {
                scales::breaks_pretty(n = 6)
            } else {
                waiver()
            },
            expand = expansion(mult = 0.01)) +
        geom_line()

    return(plot)
}
