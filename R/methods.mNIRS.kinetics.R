#' Methods for mNIRS.kinetics objects
#'
#' Generic methods defined for objects returned from [process_kinetics()].
#'
#' @param x Object of class `mNIRS.kinetics` returned from [process_kinetics()].
#' @param ... Additional arguments.
#'  \describe{
#'      \item{`plot_coefs`}{`plot_coefs = TRUE` will display kinetics coefficients
#'          at the top left of the plot.}
#'      \item{`plot_diagnostics`}{`plot_diagnostics = TRUE` will display kinetics
#'      model diagnostic metrics at the bottom right of the plot.}
#'      \item{`plot_residuals`}{`plot_diagnostics = TRUE` will display kinetics
#'      model residuals along the bottom of the plot. Residual values are correctly
#'      scaled about the zero dotted line}
#'  }
#'
#' @name mNIRS.kinetics-methods
#' @rdname mNIRS.kinetics-methods
#' @method print mNIRS.kinetics
#'
#' @return Return:
#'      \item{`print`}{Returns a model summary}
#'      \item{`plot`}{Returns a [ggplot2][ggplot2::ggplot()] object}
#'
#' @export
print.mNIRS.kinetics <- function(x, ...) {

    list2env(x, envir = environment())
    x_name <- names(data)[1]
    y_name <- names(data)[2]

    coefs <- coefs |>
        dplyr::mutate(
            dplyr::across(tidyselect::where(is.numeric),
                          \(.x) signif_trailing(.x, 3)))

    if (exists("diagnostics")) {
        diagnostics <- diagnostics |>
            dplyr::mutate(
                dplyr::across(tidyselect::where(is.numeric),
                              \(.x) signif_trailing(.x, 3)))
    }

    if (method == "monoexponential") {

        cat("\n")
        cat("Monoexponential (Association) Nonlinear nls() Regression")
        cat("\n")
        cat("  model:        ", y_name, " ~ SSmonoexp(",
            x_name, ", A, B, TD, tau)", sep = "")
        cat("\n")
        cat("  data:         data.frame(x = ", x_name,
            ", y = ", y_name, ")", sep = "")
        cat("\n")
        cat("  equation:    ", equation)
        cat("\n\n")
        cat("  Model Coefficients:")
        cat("\n")
        print(as.data.frame(coefs))
        cat("\n")
        cat("  Model Diagnostics:")
        cat("\n")
        print(as.data.frame(diagnostics))
        cat("\n")

    } else if (method == "sigmoidal") {

        cat("\n")
        cat("Sigmoidal (Generalised Logistic) Nonlinear nls() Regression")
        cat("\n")
        cat("  model:        ", y_name, " ~ SSfpl(",
            x_name, ", A, B, xmid, scal)", sep = "")
        cat("\n")
        cat("  data:         data.frame(x = ", x_name,
            ", y = ", y_name, ")", sep = "")
        cat("\n")
        cat("  equation:    ", equation)
        cat("\n\n")
        cat("  Model Coefficients:")
        cat("\n")
        print(as.data.frame(coefs))
        cat("\n")
        cat("  Model Diagnostics:")
        cat("\n")
        print(as.data.frame(diagnostics))
        cat("\n")

    } else if (method == "half_time") {

        cat("\n")
        cat("Non-parametric Half Response Time")
        cat("\n")
        cat("  data:         data.frame(x = ", x_name,
            ", y = ", y_name, ")", sep = "")
        cat("\n")
        cat("  equation:    ", equation)
        cat("\n\n")
        cat("  Model Coefficients:")
        cat("\n")
        print(as.data.frame(coefs))
        cat("\n")

    } else if (method == "peak_slope") {

        cat("\n")
        cat("Peak Linear Regression Rolling Slope")
        cat("\n")
        cat("  data:         data.frame(x = ", x_name,
            ", y = ", y_name, ")", sep = "")
        cat("\n")
        cat("  equation:    ", equation)
        cat("\n\n")
        cat("  Model Coefficients:")
        cat("\n")
        print(as.data.frame(coefs))
        cat("\n")

    } else {

        cat("\n")
        cat("NO MODEL FOUND")
        cat("\n")

    }

}





#' @rdname mNIRS.kinetics-methods
#' @method plot mNIRS.kinetics
#' @importFrom ggplot2 ggplot aes waiver expansion scale_x_continuous
#'  scale_y_continuous scale_colour_manual guides guide_legend geom_line
#'  geom_point geom_segment geom_vline arrow geom_hline annotate
#'
#' @export
plot.mNIRS.kinetics <- function(x,  ...) {

    rlang::check_installed("ggplot2", reason = "to plot mNIRS data")

    list2env(x, envir = environment())
    x_name <- names(data)[1]
    y_name <- names(data)[2]
    fitted <- names(data)[3]

    args <- list(...)
    display_time <- args$display_time %||% FALSE

    ## show additional information on plot
    if (!exists("coefs") || !"plot_coefs" %in% names(args) || !args$plot_coefs) {
        coefs_plot <- NULL
    } else {
        coefs_text <- coefs |>
            dplyr::mutate(
                dplyr::across(tidyselect::where(is.numeric),
                              \(.x) signif_trailing(.x, 1)))
        coefs_text <- paste(names(coefs_text), coefs_text,
                            sep = " = ", collapse = "\n")
        coords <- c(min(data[[x_name]], na.rm = TRUE),
                    max(data[[y_name]], na.rm = TRUE))
        coefs_plot <- annotate("text", x = coords[1], y = coords[2],
                               label = coefs_text, size = 4.2,
                               hjust = "left", vjust = "top")
    }

    if (!exists("diagnostics") || !"plot_diagnostics" %in% names(args) ||
        !args$plot_diagnostics) {
        diagnostics_plot <- NULL
    } else {
        diagnostics <- diagnostics |>
            dplyr::mutate(
                dplyr::across(tidyselect::matches(c("AIC", "BIC")),
                              \(.x) signif(.x, 4)),
                dplyr::across(-tidyselect::matches(c("AIC", "BIC")),
                              \(.x) signif_trailing(.x, 3))
            )
        diag_text <- paste(names(diagnostics), diagnostics,
                           sep = " = ", collapse = "\n")
        coords <- c(max(data[[x_name]], na.rm = TRUE),
                    min(data[[y_name]], na.rm = TRUE))
        diagnostics_plot <- annotate("text", x = coords[1], y = coords[2],
                                     label = diag_text, size = 4.2,
                                     hjust = "right", vjust = "bottom")
    }

    if (!exists("residuals") || is.function(residuals) ||
        !"plot_residuals" %in% names(args) || !args$plot_residuals) {
        residuals_plot <- NULL
    } else {
        coords <- c(min(data[[x_name]], na.rm = TRUE),
                    min(data[[y_name]], na.rm = TRUE))
        max_residuals <- max(residuals, na.rm = TRUE)
        mean_residuals <- mean(residuals, na.rm = TRUE)
        adjusted_hline <- coords[2] - max_residuals - mean_residuals
        data$resid_adj <- NA_real_
        data$resid_adj[which(!is.na(data[[fitted]]))] <- residuals + adjusted_hline
        residuals_plot <- list(
            geom_hline(yintercept = adjusted_hline,
                       colour = "grey30", linetype = "dotted"),
            annotate("text", x = coords[1], y = adjusted_hline,
                     label = "residuals", size = 4, colour = "grey30",
                     hjust = "left", vjust = -0.1),
            geom_line(#data = data[!is.na(data[[fitted]]),],
                      aes(y = resid_adj, colour = "residuals"), na.rm = TRUE)
        )
    }


    ggplot(data, aes(x = .data[[x_name]])) +
        theme_mNIRS(legend.position = "top") +
        scale_x_continuous(
            name = if (display_time) {
                paste(x_name, "(h:mm:ss)")
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
            name = y_name,
            breaks = if (rlang::is_installed("scales")) {
                scales::breaks_pretty(n = 6)
            } else {waiver()},
            expand = expansion(mult = 0.01)) +
        scale_colour_manual(
            name = NULL,
            breaks = c(y_name, "fitted", "residuals"),
            values = c(mNIRS_palette(1), "black", "grey"),
            limits = force) +
        guides(colour = guide_legend(override.aes = list(linewidth = 1))) +
        geom_vline(xintercept = x0, linetype = "dotted") +
        coefs_plot +
        diagnostics_plot +
        residuals_plot +
        geom_line(aes(y = .data[[y_name]], colour = y_name),
                  linewidth = 0.8) +
        {if (method == "monoexponential") {list(
            geom_line(
                data = data[!is.na(data[[fitted]]),],
                aes(y = .data[[fitted]], colour = "fitted"), linewidth = 1),
            geom_segment(
                data = tibble(x = coefs$MRT, y = coefs[[6]]),
                aes(x = x, xend = x, y = y, yend = -Inf),
                arrow = arrow(), linewidth = 1),
            geom_point(
                data = tibble(x = coefs$MRT, y = coefs[[6]]),
                aes(x = x, y = y, colour = "fitted"),
                size = 4, shape = 21, stroke = 1.2)
        )}} +
        {if (method == "sigmoidal") {list(
            geom_line(
                data = data[!is.na(data[[fitted]]),],
                aes(y = .data[[fitted]], colour = "fitted"), linewidth = 1),
            ## TODO 2025-08-10 report xmid_yvalue
            geom_segment(
                data = tibble(x = coefs$xmid, y = coefs[[5]]),
                aes(x = x, xend = x, y = y, yend = -Inf),
                arrow = arrow(), linewidth = 1),
            geom_point(
                data = tibble(x = coefs$xmid, y = coefs[[5]]),
                aes(x = x, y = y, colour = "fitted"),
                size = 4, shape = 21, stroke = 1.2)
        )}} +
        {if (method == "half_time") {list(
            geom_segment(
                data = tibble(x = coefs[[3]] + x0,
                              y = coefs$half_value),
                aes(x = x, xend = x, y = y, yend = -Inf),
                arrow = arrow(), linewidth = 1),
            geom_point(
                data = tibble(
                    x = c(x0,
                          data[[x_name]][data[[y_name]] == coefs$B][1],
                          coefs[[3]] + x0),
                    y = c(coefs$A, coefs$B, coefs$half_value)),
                aes(x = x, y = y, colour = "fitted"),
                size = 3, shape = 21, stroke = 1)
        )}} +
        {if (method == "peak_slope") {list(
            geom_line(
                # data = data[!is.na(data[[fitted]]),],
                aes(y = .data[[fitted]], colour = "fitted"),
                linewidth = 1, na.rm = TRUE),
            geom_segment(
                data = tibble(
                    x = coefs[[1]] + x0,
                    y = data[[fitted]][data[[x_name]] %in% x]),
                aes(x = x, xend = x, y = y, yend = -Inf),
                arrow = arrow(), linewidth = 1),
            geom_point(
                data = tibble(
                    x = coefs[[1]] + x0,
                    y = data[[fitted]][data[[x_name]] %in% x]),
                aes(x = x, y = y, colour = "fitted"),
                size = 3, shape = 21, stroke = 1)
        )}}


}
