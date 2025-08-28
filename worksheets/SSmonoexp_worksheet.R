## Setup ==========================================================
suppressPackageStartupMessages({
    library(JAPackage)
    library(mnirs)
    library(tidyverse)
})

devtools::load_all()

options(digits = 5, digits.secs = 3, scipen = 3,
        dplyr.summarise.inform = FALSE,
        tibble.print_min = 20)

# camcorder::gg_record(
#   dir = "ggplots",
#   width = 220,
#   height = 220*2/3,
#   dpi = 300,
#   units = "mm",
#   device = "png",
#   bg = "white")
# camcorder::gg_stop_recording()
#
## self start monoexp ====================================
# monoexp_equation <- function(x, A, B, TD, tau) {
#     ifelse(x <= TD, A, A + (B-A) * (1 - exp((TD - x) / tau)))
# }
#
# monoexp_init <- function(
#         mCall, LHS, data, ...
# ) {
#     ## self-start parameters for nls of monoexponential fit function
#     ## https://www.statforbiology.com/2020/stat_nls_selfstarting/#and-what-about-nls
#     xy <- sortedXyData(mCall[["x"]], LHS, data)
#     x <- xy[, "x"] ## na.omit(xy[, "x"])?
#     y <- xy[, "y"] ## na.omit(xy[, "y"])?
#
#     ## TRUE == UP, FALSE == DOWN
#     direction <- mean(head(y, length(y) / 4)) < mean(tail(y, length(y) / 4))
#
#     A <- min(y) * direction + max(y) * !direction
#     B <- max(y) * direction + min(y) * !direction
#     ## TD finds the first x value > 0 and where y has changed by
#     ## greater than 10% of the B-A amplitude in the appropriate direction
#     TD <- x[min(which(x > 0 & abs(y - A) > abs(B - A) / 10))]
#     ## tau finds the greater of either x > 0 or
#     ## the nearest x value to 63.2% of the B-A amplitude
#     tau <- abs(TD - x[max(c(
#         min(which(x > 0)),
#         which.min(abs(y - (A + 0.632 * (B - A))))))])
#
#     setNames(c(A, B, TD, tau), mCall[c("A", "B", "TD", "tau")])
# }
#
# SSmonoexp <- selfStart(
#     ## nls fit function
#     ## nls(y ~ SSmonoexp(x, A, B, TD, tau), data)
#     model = monoexp_equation,
#     initial = monoexp_init,
#     parameters = c("A", "B", "TD", "tau"))
#
## Data Wrangling ================================
# Generate sample data
set.seed(13)
true_x <- seq(0, 60, by = 2)
true_A <- 10
true_B <- 100
true_TD <- 15
true_tau <- 8
true_y <- monoexponential(true_x, true_A, true_B, true_TD, true_tau)
true_y <- true_y + rnorm(length(true_x), 0, 3)  # Add noise
## singular_data
{
    singular_data <- tibble(
        xs = -30:180,
        ys = c(21.49, 21.73, 21.84, 21.72, 21.6, 21.79, 21.97, 22.13, 22.04,
              22.01, 21.91, 21.82, 21.74, 21.81, 21.84, 21.74, 21.46, 21.36,
              21.35, 21.36, 21.44, 21.32, 21.36, 21.5, 21.69, 21.77, 21.86,
              22, 21.82, 21.67, 21.79, 21.74, 21.35, 21.3, 21.36, 21.34, 21.22,
              20.93, 20.81, 20.78, 20.73, 20.58, 20.55, 20.48, 20.47, 20.4,
              20.28, 20.25, 20.26, 20.26, 20.29, 20.15, 20.27, 20.19, 20.14,
              19.96, 19.97, 19.95, 19.95, 19.96, 19.94, 19.9, 19.92, 19.92,
              19.9, 19.86, 19.84, 19.88, 19.87, 19.91, 19.93, 19.83, 19.99,
              20.06, 19.83, 19.96, 19.89, 19.98, 19.91, 19.97, 19.98, 19.97,
              19.98, 20.02, 20.05, 20.18, 20.1, 20.13, 20.12, 20.16, 20.16,
              20.18, 20.24, 20.24, 20.31, 20.35, 20.24, 20.41, 20.41, 20.46,
              20.45, 20.51, 20.58, 20.62, 20.55, 20.6, 20.55, 20.65, 20.63,
              20.6, 20.56, 20.52, 20.5, 20.48, 20.38, 20.31, 20.3, 20.27, 20.22,
              20.27, 20.12, 20.18, 20.18, 20.16, 20.16, 20.19, 20.27, 20.15,
              20.23, 20.27, 20.32, 20.2, 20.23, 20.25, 20.26, 20.32, 20.31,
              20.25, 20.34, 20.27, 20.23, 20.35, 20.33, 20.41, 20.34, 20.4,
              20.4, 20.36, 20.43, 20.34, 20.38, 20.36, 20.35, 20.49, 20.45,
              20.44, 20.49, 20.5, 20.59, 20.56, 20.42, 20.45, 20.43, 20.46,
              20.42, 20.48, 20.46, 20.57, 20.58, 20.58, 20.6, 20.64, 20.69,
              20.69, 20.7, 20.7, 20.68, 20.61, 20.58, 20.52, 20.46, 20.45,
              20.42, 20.47, 20.62, 20.67, 20.69, 20.74, 20.7, 20.69, 20.7,
              20.73, 20.65, 20.63, 20.6, 20.68, 20.62, 20.65, 20.71, 20.78,
              20.65, 20.7, 20.76, 20.81, 20.79, 20.82, 20.86, 20.94, 20.91,
              20.84, 20.93),
    )
} ## singular_data
true_data <- tibble(xcol = true_x, ycol = true_y)

plot <- ggplot(true_data) +
    {list( ## Settings
        aes(xcol, ycol),
        scale_y_continuous(
            # limits = c(0, NA),
            breaks = scales::breaks_pretty(),
        ),
        theme_JA(),
        NULL)} + ## Settings
    {list( ## Data
        geom_hline(yintercept = 10, linetype = "dotted"),
        geom_line(),
        geom_point(),
        NULL)} ## Data

plot
#

## update model ===========================================
# nls(y ~ SSmonoexp(x, A, B, TD, tau), data = data)
# nls(y ~ monoexp_equation(x, A, B, TD, tau), data = data,
#     start = list(A = 10, B = 100, TD = 15, tau = 10))
#
# nls(y ~ SSmonoexp(x, A, B, TD, tau), data = data) |>
#     (\(.x) update(
#         object = .x,
#         y ~ SSmonoexp(x, A = 10, B, TD, tau),
#         start = within(as.list(.x$m$getPars()), rm(A))))()
# nls(y ~ monoexp_equation(x, A, B, TD, tau), data = data,
#     start = list(A = 10, B = 100, TD = 15, tau = 10)) |>
#     mnirs::update_fixed_coefs(A = 10)
#
# nls(y ~ SSlogis(x, Asym, xmid, scal), data = data) |>
#     mnirs::update_fixed_coefs()
#
## monoexp kinetics function ============================

## intake vector NIRS data
## intake vector SAMPLE/TIME
## intake scalar sample number == 0
## ...
## output fitted values
## output model
## output coefs
## output fit criteria


# process_kinetics_test(
process_kinetics(
    # x = true_data$x,
    # y = true_data$y,
    x = true_x,
    y = true_y,
    # x = "xcol",
    # y = "ycol",
    # x = xcol,
    # y = ycol, #error
    # data = true_data,
    x0 = 8, #truea_x[8],
    method = "peak_slope",
    width = 10
    # B = 100,
    ) |> plot()
#
# process_kinetics_test(x = true_y)



fit_monoexp <- mnirs::process_kinetics(
    true_x, true_y, x0 = true_x[8], method = "monoexp", Q = 100)$model


(fitted <- predict(fit_monoexp, newdata = data.frame(
    x = JAPackage::SeqRange(range(true_x)-true_x[8], by = 0.1))))

(plot2 <- plot +
        geom_line(
            data = tibble(),
            aes(x = JAPackage::SeqRange(range(true_x), by = 0.1),
                y = fitted,
                colour = "monoexp"))
)

mnirs::process_kinetics(true_x, true_y, x0 = true_x[8], method = "half_time")
(half_data <- mnirs::process_kinetics(x=true_x, y=true_y, method = "half_time")$coefs)

plot2 +
    geom_point(
        data = tibble(),
        aes(x = half_data[["half_time"]],
            y = half_data[["half_value"]]), size = 3, colour = "dodgerblue")





## test func =========================================================
test <- function(x, y = NULL, data = NULL) {

    if (!is.null(x)) {
    #     x_name <- "index"
    #     x_exp <- substitute(index)
    #     y_name <- rlang::as_name(rlang::enquo(y))
    #     y_exp <- as.symbol(y_name)
    #     y <- x
    #     x <- seq_along(y)
    # } else {
        x_name <- rlang::as_name(rlang::enquo(x))
        # x_exp <- as.symbol(x_name)
        y_name <- rlang::as_name(rlang::enquo(y))
        # y_exp <- as.symbol(y_name)
    }
        x_name
    # if (is.null(data)) {
    #     data <- tibble::tibble(x, y)
    #     colnames(data) <- c(x_name, y_name)
    # }
    #
    #
    # print(data)

    # nls(as.formula(paste0(y_name, " ~ SSmonoexp(", x_name, ", A, B, TD, tau)")),
    #     data = data,
    #     na.action = na.exclude)

}

# test(x = "xcol", y = ycol, data = true_data)
test(x = true_x, y = true_y)


## peak slope testing ==============================================
devtools::load_all()
true_y[12:13] <- NA

slope(true_y, true_x, na.rm = TRUE)
