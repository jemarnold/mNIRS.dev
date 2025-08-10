#' Methods for mNIRS.kinetics objects
#'
#' Methods defined for objects returned from [process_kinetics()].
#'
#' @param x Object of class `mNIRS.kinetics` returned from [process_kinetics()].
#' @param ... Additional arguments.
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
    if (x$method == "monoexponential") {

        cat("\n")
        cat("Monoexponential Nonlinear nls() Regression")
        cat("\n")
        cat("  model:        ", names(x$data)[2], " ~ SSmonoexp(",
            names(x$data)[1], ", A, B, TD, tau)", sep = "")
        cat("\n")
        if (!is.null(x$call$data)) {
            cat("  data:        ", deparse(x$call$data))
        } else {
            cat("  data:        data.frame(x = ", names(x$data)[1],
                ", y = ", names(x$data)[2], ")", sep = "")
        }
        cat("\n")
        cat("  equation:     y ~ A + (B - A) * (1 - exp((TD - x))/tau)")
        cat("\n\n")
        cat("  Model Coefficients")
        cat("\n")
        print(round(as.data.frame(x$coefs), 3))
        cat("\n")
        cat("  Model Diagnostics")
        cat("\n")
        print(round(as.data.frame(x$diagnostics), 3))
        cat("\n")

    } else if (x$method == "sigmoidal") {

        cat("\n")
        cat("Sigmoidal (Generalised Logistic) Nonlinear nls() Regression")
        cat("\n")
        cat("  model:        ", names(x$data)[2], " ~ SSfpl(",
            names(x$data)[1], ", A, B, xmid, scal)", sep = "")
        cat("\n")
        if (!is.null(x$call$data)) {
            cat("  data:        ", x$call$data)
        } else {
            cat("  data:        data.frame(x = ", names(x$data)[1],
                ", y = ", names(x$data)[2], ")", sep = "")
        }
        cat("\n")
        cat("  equation:     y ~ A + (B - A) / (1 + exp((xmid - x) / scal))")
        cat("\n\n")
        cat("  Model Coefficients")
        cat("\n")
        print(round(as.data.frame(x$coefs), 3))
        cat("\n")
        cat("  Model Diagnostics")
        cat("\n")
        print(round(as.data.frame(x$diagnostics), 3))
        cat("\n")

    } else if (x$method == "half_time") {

        cat("\n")
        cat("Non-parametric Half-Time")
        cat("\n")
        if (!is.null(x$call$data)) {
            cat("  data:        ", x$call$data)
        } else {
            cat("  data:        data.frame(x = ", names(x$data)[1],
                ", y = ", names(x$data)[2], ")", sep = "")
        }
        cat("\n")
        cat("  equation:     half_value ~ A + (B - A) / 2")
        cat("\n\n")
        cat("  Model Coefficients")
        cat("\n")
        print(round(as.data.frame(x$coefs), 3))
        cat("\n")

    } else if (x$method == "peak_slope") {

        cat("\n")
        cat("Peak Linear Slope")
        cat("\n")
        if (!is.null(x$call$data)) {
            cat("  data:        ", x$call$data)
        } else {
            cat("  data:        data.frame(x = ", names(x$data)[1],
                ", y = ", names(x$data)[2], ")", sep = "")
        }
        cat("\n")
        cat("  equation:     <under development>")
        cat("\n\n")
        cat("  Model Coefficients")
        cat("\n")
        print(round(as.data.frame(x$coefs), 3))
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
#'  geom_point geom_segment geom_vline arrow
#'
#' @export
plot.mNIRS.kinetics <- function(x,  ...) {

    rlang::check_installed("ggplot2", reason = "to plot mNIRS data")

    list2env(x, envir = environment())
    colnames(data) <- c("x", "y", "fitted")

    # mNIRS_colours <- c(
    #     "#0080ff",   "#ba2630", "#7dbf70",   "#ff80ff", "#ff7f00",
    #     "#00468Bff", "#db5555",   "#42B540FF", "#9f79ee", "#8b4726")
    #
    # mNIRS_palette <- function(...) {
    #     cols <- c(...)
    #     colours <- c(
    #         "VL"   = "#0080ff",
    #         "FCR"  = "#ba2630",
    #         "BB"   = "#7dbf70",
    #         "VM"   = "#ff80ff",
    #         "SCM"  = "#ff7f00",
    #         "TA"   = "#00468Bff",
    #         "ECR"  = "#db5555",
    #         "DL"   = "#42B540FF",
    #         "RF"   = "#9f79ee",
    #         "PS"   = "#8b4726",
    #         "O2Hb" = "#ff0000",
    #         "HHb"  = "#0000ff")
    #     if (is.null(cols)) {return(colours)}
    #     colours[[cols]]
    # }

    ggplot(data, aes(x = x)) +
        theme_mNIRS(legend.position = "top") +
        scale_x_continuous(
            name = deparse(rlang::sym(call$x)),
            breaks = if (rlang::is_installed("scales")) {
                scales::breaks_pretty(n = 8)
            } else {waiver()},
            expand = expansion(mult = 0.01)) +
        scale_y_continuous(
            name = deparse(rlang::sym(call$y)),
            breaks = if (rlang::is_installed("scales")) {
                scales::breaks_pretty(n = 6)
            } else {waiver()},
            expand = expansion(mult = 0.01)) +
        scale_colour_manual(
            name = NULL,
            values = setNames(c(scales::hue_pal()(1), "black"),
                              c(deparse(rlang::sym(call$y)), "fitted")),
            limits = force) +
        geom_vline(xintercept = x0, linetype = "dotted") +
        geom_line(aes(y = y, colour = deparse(rlang::sym(call$y)))) +
        {if (x$method == "monoexponential") {list(
            geom_line(
                data = data[!is.na(data$fitted),],
                aes(y = fitted, colour = "fitted"), linewidth = 0.8),
            geom_segment(
                data = tibble::tibble(x = coefs$MRT, y = coefs[[6]]),
                aes(xend = x, y = y, yend = -Inf),
                arrow = arrow(), linewidth = 0.8),
            geom_point(
                data = tibble::tibble(x = coefs$MRT, y = coefs[[6]]),
                aes(y = y, colour = "fitted"),
                size = 4, shape = 21, stroke = 1.2)
        )}} +
        {if (x$method == "sigmoidal") {list(
            geom_line(
                data = data[!is.na(data$fitted),],
                aes(y = fitted, colour = "fitted"), linewidth = 0.8),
            ## TODO 2025-08-10 report xmid_yvalue
            geom_segment(
                data = tibble::tibble(x = coefs$xmid, y = coefs[[5]]),
                aes(xend = x, y = y, yend = -Inf),
                arrow = arrow(), linewidth = 0.8),
            geom_point(
                data = tibble::tibble(x = coefs$xmid, y = coefs[[5]]),
                aes(y = y, colour = "fitted"),
                size = 4, shape = 21, stroke = 1.2)
        )}} +
        {if (x$method == "half_time") {list(
            # annotate( ## horizontal
            #     geom = "segment",
            #     x = coefs[[1]] + x0, xend = coefs[[3]] + x0,
            #     y = coefs[[2]], yend = coefs[[2]],
            #     linewidth = 0.8, linetype = "dashed"),
            # annotate( ## vertical
            #     geom = "segment",
            #     x = coefs[[3]] + x0, xend = coefs[[3]] + x0,
            #     y = coefs[[2]], yend = coefs[[4]],
            #     linewidth = 0.8, linetype = "dashed"),
            # annotate( ## half-horizontal
            #     geom = "segment",
            #     x = coefs[[5]] + x0, xend = coefs[[3]] + x0,
            #     y = coefs$half_value, yend = coefs$half_value,
            #     linewidth = 0.8, linetype = "dashed"),
            geom_segment(
                data = tibble::tibble(x = coefs[[5]] + x0, y = coefs[[7]]),
                aes(xend = x, y = y, yend = -Inf),
                arrow = arrow(), linewidth = 0.8),
            geom_point(
                data = tibble::tibble(
                    x = c(coefs[[1]], coefs[[3]], coefs[[5]]) + x0,
                    y = c(coefs[[2]], coefs[[4]], coefs[[7]])),
                aes(y = y, colour = "fitted"),
                size = 3, shape = 21, stroke = 1)
        )}} +
        {if (x$method == "peak_slope") {list(
            geom_line(
                data = data[!is.na(data$fitted),],
                aes(y = fitted, colour = "fitted"), linewidth = 0.8),
            geom_segment(
                data = tibble::tibble(
                    x = coefs[[1]] + x0,
                    y = data$fitted[data$x %in% x]),
                aes(xend = x, y = y, yend = -Inf),
                arrow = arrow(), linewidth = 0.8),
            geom_point(
                data = tibble::tibble(
                    x = coefs[[1]] + x0,
                    y = data$fitted[data$x %in% x]),
                aes(y = y, colour = "fitted"),
                size = 3, shape = 21, stroke = 1)
        )}} +
        NULL


}
