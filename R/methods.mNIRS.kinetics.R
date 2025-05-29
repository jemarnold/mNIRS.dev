#' Methods for mNIRS.kinetics objects
#'
#' Methods defined for objects returned from [process_kinetics()].
#'
#' @param x object of class `mNIRS.kinetics` as returned from
#' [process_kinetics()]
#' @param ... further arguments passed through, see description of return value
#' for details.
#'
#' @return
#' \describe{
#'   \item{`print`}{Prints the mNIRS kinetics model summary}
#'   \item{`plot`}{Returns a plot of mNIRS kinetics}
#' }
#'
#' @name mNIRS.kinetics-methods


### methods for mNIRS.kinetics objects

#' @rdname mNIRS.kinetics-methods
#' @method print mNIRS.kinetics
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

    } else if (x$method == "logistic") {

        cat("\n")
        cat("Logistic (Sigmoidal) Nonlinear nls() Regression")
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

    } else {

        cat("\n")
        cat("NO MODEL FOUND")
        cat("\n")

    }

}




#' @rdname mNIRS.kinetics-methods
#' @method plot mNIRS.kinetics
#' @import ggplot2
#' @export

plot.mNIRS.kinetics <- function(x,  ...) {

    theme_mNIRS <- function(base_size = 12, ...) {

        half_line = base_size/2

        theme_bw(base_size = base_size) +
            theme(
                text = element_text(colour = "black"),
                plot.title = element_text(size = rel(1.2), lineheight = 1.1),
                plot.subtitle = element_text(lineheight = 1.1),
                panel.border = element_blank(),
                axis.line = element_line(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.title = element_text(face = "bold"),
                legend.position = "none",
                legend.justification = "right",
                legend.margin = margin(rep(1, 4)),
                legend.box.spacing = unit(half_line/2, "pt"),
                strip.background = element_rect(fill = "grey95"),
                strip.text = element_text(margin = margin(rep(half_line/2, 4))),
            ) + theme(...)
    }

    breaks_timespan <- function(
        unit = c("secs", "mins", "hours"),
        n = 5
    ) {
        unit <- rlang::arg_match(unit)
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


    format_hmmss <- function(x) {
        x <- as.numeric(x)
        sign <- ifelse(x < 0, "-", "")
        hrs <- abs(x) %/% 3600
        mins <- abs(x) %/% 60
        secs <- abs(x) %% 60

        if (any(hrs > 0, na.rm = TRUE)) {
            sprintf("%s%d:%02d:%02d", sign, hrs, mins, secs)
        } else {
            sprintf("%s%02d:%02d", sign, mins, secs)
        }
    }


    mNIRS_colours <- c(
        "#0080ff",   "#ba2630", "#7dbf70",   "#ff80ff", "#ff7f00",
        "#00468Bff", "#db5555",   "#42B540FF", "#9f79ee", "#8b4726")

    mNIRS_palette <- function(...) {
        cols <- c(...)
        colours <- c(
            "VL"   = "#0080ff",
            "FCR"  = "#ba2630",
            "BB"   = "#7dbf70",
            "VM"   = "#ff80ff",
            "SCM"  = "#ff7f00",
            "TA"   = "#00468Bff",
            "ECR"  = "#db5555",
            "DL"   = "#42B540FF",
            "RF"   = "#9f79ee",
            "PS"   = "#8b4726",
            "O2Hb" = "#ff0000",
            "HHb"  = "#0000ff")
        if (is.null(cols)) {return(colours)}
        colours[[cols]]
    }


    ggplot(x$data) +
        aes(x = .data[[x$call$x]]) +
        theme_mNIRS(legend.position = "top") +
        (if (grepl("time", x$call$x, ignore.case = TRUE)) {
            scale_x_continuous(
                name = paste(deparse(x$call$x), "(mm:ss)"),
                breaks = breaks_timespan(n = 8),
                labels = format_hmmss,
                expand = expansion(mult = 0.02)
            )
        } else {
            scale_x_continuous(
                name = deparse(x$call$x),
                breaks = scales::breaks_pretty(n = 8),
                expand = expansion(mult = 0.02)
            )
        }) +
        scale_y_continuous(
            name = deparse(x$call$y),
            breaks = scales::breaks_pretty(n = 8),
            expand = expansion(mult = 0.02)
        ) +
        scale_colour_manual(
            name = NULL,
            values = mNIRS_colours,
            limits = force) +
        guides(colour = guide_legend(
            nrow = 1, byrow = FALSE,
            override.aes = list(shape = NA, linewidth = 5, alpha = 1))) +
        geom_vline(xintercept = x$x0, linetype = "dotted") +
        geom_line(aes(y = .data[[x$call$y]], colour = deparse(x$call$y))) +
        geom_line(aes(y = fitted), linewidth = 0.8)
}
