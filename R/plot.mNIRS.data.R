#' Plot mNIRS.data objects
#'
#' Create a simple plot for objects returned from [create_mNIRS_data()].
#'
#' @param x Object of class `mNIRS.data` as returned from [create_mNIRS_data()]
#' @param ... Additional arguments,
#'  \describe{
#'      \item{`na.omit = FALSE`}{A logical indicating whether missing data (`NA`)
#'          should be omitted (`TRUE`) for better display of disconnected lines,
#'          or included (`FALSE`, *the default*) to better recognise where data
#'          are missing.}
#'  }
#'
#' @return A [ggplot2][ggplot2::ggplot()] object.
#'
#' @importFrom ggplot2 ggplot aes waiver expansion scale_x_continuous
#'  scale_y_continuous geom_line geom_point
#'
#' @export
plot.mNIRS.data <- function(x, ...) {

    rlang::check_installed("ggplot2", reason = "to plot mNIRS data")

    args <- list(...)
    na.omit <- args$na.omit %||% FALSE
    display_time <- args$display_time %||% FALSE

    nirs_columns <- attributes(x)$nirs_columns
    sample_column <- attributes(x)$sample_column

    ## pivot all `nirs_columns` to `y` and plot by group
    plot <- tidyr::pivot_longer(data = x,
                                cols = dplyr::all_of(nirs_columns),
                                names_to = "nirs_columns",
                                values_to = "y"
    ) |>
        ## remove empty rows for geom_line
        ( \(.df) if (na.omit) {tidyr::drop_na(.df, y)} else {.df})() |>
        ggplot() +
        aes(x = .data[[sample_column]], y = y,
            colour = nirs_columns) +
        theme_mNIRS() +
        scale_x_continuous(
            name = if (display_time) {
                paste(sample_column, "(h:mm:ss)")
            } else {waiver()},
            breaks = if (display_time) {
                breaks_timespan(n = 8)
            } else if (rlang::is_installed("scales")) {
                scales::breaks_pretty(n = 8)
            } else {waiver()},
            labels = if (display_time) {
                format_hmmss
            } else {waiver()},
            expand = expansion(mult = 0.01)) +
        scale_y_continuous(
            name = "mNIRS Signals",
            breaks = if (rlang::is_installed("scales")) {
                scales::breaks_pretty(n = 6)
            } else {waiver()},
            expand = expansion(mult = 0.01)) +
        scale_colour_mNIRS() +
        guides(colour = guide_legend(override.aes = list(linewidth = 1))) +
        geom_line()

    return(plot)
}




#' Custom mNIRS colour palette
#'
#' @param n Number of colours to return, in order.
#'
#' @return Named character vector of hex colours.
#'
#' @examples
#' \dontrun{
#' scales::show_col(mNIRS_palette())
#' }
#'
#' @export
mNIRS_palette <- function(n = NULL) {
    mNIRS_colours <- c(
        "#0080ff",      ## "VL"
        "#ba2630",      ## "FCR"
        "#5b8c52",      ## "BB" "#7dbf70"
        "#ff80ff",      ## "VM"
        "#ff7f00",      ## "SCM"
        "#00468Bff",    ## "TA"
        "#db5555",      ## "ECR"
        "#42B540FF",    ## "DL"
        "#9f79ee",      ## "RF"
        "#8b4726",      ## "PS"
        "#ED0000FF",    ## "O2Hb"
        "#0000ff")      ## "HHb"
    if (is.null(n)) {return(mNIRS_colours)}
    if (n <= length(mNIRS_colours)) return(mNIRS_colours[seq_len(n)])

    ## interpolate if more colours needed, but this probably won't look good!
    grDevices::colorRampPalette(mNIRS_colours)(n)
}





#' Scales for custom mNIRS palette
#'
#' @param ... Arguments passed to discrete_scale.
#'
#' @seealso [mNIRS_palette()]
#' @rdname scale_colour_my_palette
#' @export
scale_colour_mNIRS <- function(...) {
    ggplot2::discrete_scale(aesthetics = "colour",
                            palette = mNIRS_palette,
                            na.value = "grey10",
                            ...)
}




#' @rdname scale_colour_my_palette
#' @export
scale_fill_mNIRS <- function(...) {
    ggplot2::discrete_scale(aesthetics = "fill",
                            palette = mNIRS_palette,
                            na.value = "grey10",
                            ...)
}




#' Format Timespan Data as h:mm:ss
#'
#' Convert numeric timespan data to `h:mm:ss` format for pretty plotting. Inspired
#' by [ggplot2::scale_x_time()].
#'
#' @param x A numeric vector.
#'
#' @details
#' If all values are less than 3600 (1 hour), then format is returned as `mm:ss`.
#' If any value is greater than 3600, format is returned as `h:mm:ss` with leading
#' zeroes.
#'
#' @return A character vector the same length as `x`.
#'
#' @examples
#' \dontrun{
#' x = 0:120
#' y = sin(2 * pi * x / 15) + rnorm(length(x), 0, 0.2)
#' ggplot(data.frame(x, y), aes(x = x, y = y)) +
#'     scale_x_continuous(breaks = breaks_timespan(),
#'                        labels = format_hmmss) +
#'     geom_line()
#' }
#'
#' @keywords internal
#' @export
format_hmmss <- function(x) {
    x <- as.numeric(x)
    sign <- ifelse(x < 0, "-", "")
    hrs <- abs(x) %/% 3600
    mins <- (abs(x) %% 3600) %/% 60
    secs <- abs(x) %% 60

    if (any(hrs > 0, na.rm = TRUE)) {
        sprintf("%s%d:%02d:%02d", sign, hrs, mins, secs)
    } else {
        sprintf("%s%02d:%02d", sign, mins, secs)
    }
}



#' Breaks for Timespan Data
#'
#' Pretty timespan breaks for plotting in units of 5, 15, 30, 60 sec, etc.
#' Modified from [scales::breaks_timespan()].
#'
#' @param unit The time unit used to interpret numeric data input (*default* to
#'  *"secs"*).
#' @param n Desired number of breaks. You may get slightly more or fewer breaks
#'  than requested.
#'
#' @details
#' ...
#'
#' @return Returns a function for generating breaks
#'
#' @examples
#' \dontrun{
#' x = 0:120
#' y = sin(2 * pi * x / 15) + rnorm(length(x), 0, 0.2)
#' ggplot(data.frame(x, y), aes(x = x, y = y)) +
#'     scale_x_continuous(breaks = breaks_timespan()) +
#'     geom_line()
#' }
#'
#' @keywords internal
#' @export
breaks_timespan <- function(
        unit = c("secs", "mins", "hours", "days", "weeks"),
        n = 5
) {
    unit <- match.arg(unit)
    force(n)
    function(x) {
        x <- as.numeric(as.difftime(x, units = unit), units = "secs")
        rng <- range(x, na.rm = TRUE)
        diff <- rng[2] - rng[1]
        if (diff <= 2 * 60) {
            scale <- 1
        } else if (diff <= 2 * 3600) {
            scale <- 60
        } else if (diff <= 2 * 86400) {
            scale <- 3600
        } else if (diff <= 2 * 604800) {
            scale <- 86400
        } else {
            scale <- 604800
        }
        # Define nice steps for each unit
        nice_steps <- switch(
            unit,
            "secs" = c(1, 2, 5, 10, 15, 20, 30, 60, 120),
            "mins" = c(1, 2, 5, 10, 15, 20, 30, 60, 120) * 60,
            "hours" = c(0.25, 0.5, 1, 2, 3, 4, 6, 8, 12, 24) * 3600)
        rng <- rng/scale
        breaks <- labeling::extended(
            rng[1], rng[2], n, Q = nice_steps, only.loose = FALSE)
        as.numeric(as.difftime(breaks * scale, units = "secs"))
    }
}




#' Custom mNIRS ggplot2 theme
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
