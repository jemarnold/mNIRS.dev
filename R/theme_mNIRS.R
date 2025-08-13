#' mNIRS ggplot2 theme
#'
#' A [ggplot2][ggplot2::ggplot2-package] theme for display.
#'
#' @param base_size Base font size, given in pts.
#' @param base_family Base font family.
#' @param border Define either a *partial* or *full* border around plots.
#' @param ... Additional arguments to add to [theme()][ggplot2::theme()].
#'
#' @return A [ggplot2][ggplot2::ggplot()] object.
#'
#' @importFrom ggplot2 theme_bw theme element_rect element_line element_text
#'  element_blank rel margin unit
#'
#' @export
theme_mNIRS <- function(
        base_size = 14,
        base_family = "sans",
        border = c("partial", "full"),
        ...
) {
    rlang::check_installed("ggplot2", reason = "to plot mNIRS data")
    args <- list(...)

    half_line = base_size/2
    border = match.arg(border)

    switch(
        border,
        "partial" = {
            panel.border = element_blank()
            axis.line = element_line()},
        "full" = {
            panel.border = element_rect(colour = "black", linewidth = 1)
            axis.line = element_blank()}
    )

    theme_bw(base_size = base_size, base_family = base_family) +
        theme(
            text = element_text(colour = "black"),
            plot.title = element_text(size = rel(1.2), lineheight = 1.1),
            plot.subtitle = element_text(lineheight = 1.1),
            plot.caption = element_text(colour = "grey50"),
            panel.border = panel.border,
            axis.line = axis.line,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title = element_text(face = "bold"),
            legend.position = "top",
            legend.justification = "right",
            legend.title = element_blank(),
            legend.margin = margin(rep(1, 4)),
            legend.box.spacing = unit(half_line/2, "pt"),
            strip.background = element_rect(fill = "grey95"),
            strip.text = element_text(margin = margin(rep(half_line/2, 4))),
        ) + theme(args)
}
