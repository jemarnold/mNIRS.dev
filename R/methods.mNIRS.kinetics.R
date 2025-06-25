#' Methods for mNIRS.kinetics objects
#'
#' Methods defined for objects returned from [process_kinetics()].
#'
#' @param x Object of class `mNIRS.kinetics` returned from [process_kinetics()].
#' @param ... Additional arguments (see *Value*).
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
            cat("  data:        ", x$call$data)
        } else {
            cat("  data:        data.frame(x = ", names(x$data)[1],
                ", y = ", names(x$data)[2], ")", sep = "")
        }
        cat("\n")
        cat("  equation:     y ~ A + (B - A) * (1 - exp((TD - x))/tau)")
        cat("\n\n")
        cat("  Model Coefficients")
        cat("\n")
        print(round(unlist(x$coefs), 4))
        cat("\n")
        cat("  Fit Criteria")
        cat("\n")
        print(round(unlist(x$fit_criteria), 3))
        cat("\n")

    } else if (x$method == "sigmoidal") {

        cat("\n")
        cat("Sigmoidal (Generalised Logistic) Nonlinear nls() Regression")
        cat("\n")
        cat("  model:        ", names(x$data)[2], " ~ SSlogis(",
            names(x$data)[1], ", Asym, xmid, scal)", sep = "")
        cat("\n")
        if (!is.null(x$call$data)) {
            cat("  data:        ", x$call$data)
        } else {
            cat("  data:        data.frame(x = ", names(x$data)[1],
                ", y = ", names(x$data)[2], ")", sep = "")
        }
        cat("\n")
        cat("  equation:     y ~ Asym / (1 + exp((xmid - x) / scal))")
        cat("\n\n")
        cat("  Model Coefficients")
        cat("\n")
        print(round(unlist(x$coefs), 4))
        cat("\n")
        cat("  Fit Criteria")
        cat("\n")
        print(round(unlist(x$fit_criteria), 3))
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
        cat("\n\n")
        cat("  Model Coefficients")
        cat("\n")
        print(round(unlist(x$coefs), 4))
        cat("\n")

    } else if (x$method == "peak_slope") {

        cat("\n")
        cat("UNDER DEVELOPMENT")
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
#'  geom_point geom_vline
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
            name = deparse(call$x),
            breaks = if (rlang::is_installed("scales")) {
                scales::breaks_pretty(n = 6)
            } else {
                waiver()
            },
            expand = expansion(mult = 0.01)) +
        scale_y_continuous(
            name = deparse(call$y),
            breaks = if (rlang::is_installed("scales")) {
                scales::breaks_pretty(n = 6)
            } else {
                waiver()
            },
            expand = expansion(mult = 0.01)) +
        scale_colour_discrete(
            name = NULL,
            # values = mNIRS_palette(),
            limits = force) +
        # guides(colour = guide_legend(
        #     nrow = 1, byrow = FALSE,
        #     override.aes = list(shape = NA, linewidth = 5, alpha = 1))) +
        geom_vline(xintercept = x0, linetype = "dotted") +
        geom_line(aes(y = y, colour = deparse(call$y))) +
        geom_line(aes(y = fitted))

}
