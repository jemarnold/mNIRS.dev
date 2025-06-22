#' Methods for mNIRS.data objects
#'
#' Methods defined for objects returned from [create_mnirs_data()].
#'
#' @param x object of class `mNIRS.data` as returned from [create_mnirs_data()]
#' @param ... further arguments passed through, see description of return value
#' for details.
#'
#' @return
#' \describe{
#'   \item{`plot`}{Returns a simple plot of mNIRS data with
#'   [ggplot2][ggplot2::ggplot()].}
#' }
#'
#' @export
plot.mNIRS.data <- function(x, ...) {

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        cli::cli_abort(
            "Package 'ggplot2' is required for plotting. Please install it.")
    }

    data <- x

    nirs_columns <- attributes(data)$nirs_columns
    sample_column <- attributes(data)$sample_column

    plot <- data |>
        tidyr::pivot_longer(
            cols = tidyr::all_of(nirs_columns),
            names_to = "nirs_columns",
            values_to = "y") |>
        ggplot2::ggplot() +
        ggplot2::aes(x = .data[[sample_column]], y = y,
                     colour = nirs_columns) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(
            text = ggplot2::element_text(colour = "black"),
            plot.title = ggplot2::element_text(
                size = ggplot2::rel(1.2), lineheight = 1.1),
            plot.subtitle = ggplot2::element_text(lineheight = 1.1),
            plot.caption = ggplot2::element_text(colour = "grey50"),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.title = ggplot2::element_text(face = "bold"),
            legend.position = "top",
            legend.justification = "right",
            legend.margin = ggplot2::margin(rep(1, 4)),
            legend.box.spacing = ggplot2::unit(3, "pt"),
            strip.background = ggplot2::element_rect(fill = "grey95"),
            strip.text = ggplot2::element_text(
                margin = ggplot2::margin(rep(3, 4))),
        ) +
        ggplot2::scale_x_continuous(
            # name = sample_column,
            breaks = scales::breaks_pretty(n = 6),
            expand = ggplot2::expansion(mult = 0.01)) +
        ggplot2::scale_y_continuous(
            name = "mNIRS Signals",
            breaks = scales::breaks_pretty(n = 6),
            expand = ggplot2::expansion(mult = 0.01)) +
        ggplot2::geom_line()

    return(plot)
}
