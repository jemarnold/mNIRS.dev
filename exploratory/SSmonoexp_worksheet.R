## Setup ==========================================================
suppressPackageStartupMessages({
    library(JAPackage)
    library(tidyverse)
})

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
monoexp_equation <- function(x, A, B, TD, tau) {
    ifelse(x <= TD, A, A + (B-A) * (1 - exp((TD - x) / tau)))
}

monoexp_init <- function(
        mCall, LHS, data, ...
) {
    ## self-start parameters for nls of monoexponential fit function
    ## https://www.statforbiology.com/2020/stat_nls_selfstarting/#and-what-about-nls
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"] ## na.omit(xy[, "x"])?
    y <- xy[, "y"] ## na.omit(xy[, "y"])?

    ## TRUE == UP, FALSE == DOWN
    direction <- mean(head(y, length(y) / 4)) < mean(tail(y, length(y) / 4))

    A <- min(y) * direction + max(y) * !direction
    B <- max(y) * direction + min(y) * !direction
    ## TD finds the first x value > 0 and where y has changed by
    ## greater than 10% of the B-A amplitude in the appropriate direction
    TD <- x[min(which(x > 0 & abs(y - A) > abs(B - A) / 10))]
    ## tau finds the greater of either x > 0 or
    ## the nearest x value to 63.2% of the B-A amplitude
    tau <- abs(TD - x[max(c(
        min(which(x > 0)),
        which.min(abs(y - (A + 0.632 * (B - A))))))])

    setNames(c(A, B, TD, tau), mCall[c("A", "B", "TD", "tau")])
}

SSmonoexp <- selfStart(
    ## nls fit function
    ## nls(y ~ SSmonoexp(x, A, B, TD, tau), data)
    model = monoexp_equation,
    initial = monoexp_init,
    parameters = c("A", "B", "TD", "tau"))
#
## Data Wrangling ================================
# Generate sample data
set.seed(13)
true_x <- seq(0, 60, by = 2)
true_A <- 10
true_B <- 100
true_TD <- 15
true_tau <- 8
true_y <- monoexp_equation(true_x, true_A, true_B, true_TD, true_tau)
true_y <- true_y + rnorm(length(true_x), 0, 3)  # Add noise
true_data <- tibble(x = true_x, y = true_y)

plot <- ggplot(true_data) +
    {list( ## Settings
        aes(x, y),
        scale_y_continuous(
            limits = c(0, NA),
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
#     mNIRS::update_fixed_coefs(A = 10)
#
# nls(y ~ SSlogis(x, Asym, xmid, scal), data = data) |>
#     mNIRS::update_fixed_coefs()
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

# process_kinetics_test <- function(
#         x,
#         y = NULL,
#         x0 = 0,
#         method = c("monoexponential", "logistic", "half-time", "peak-slope"),
#         ...
# ) {
#
#     ## set c(x, y) when y missing
#     if (is.null(y)) {
#         y <- x
#         x <- seq_along(y)
#     }
#
#     ## construct the dataframe
#     x <- x - x0
#     df <- data.frame(x = x, y = y)
#
#     ## create the model and update for any fixed coefs
#     model <- nls(y ~ SSmonoexp(x, A, B, TD, tau), data = df) |>
#         update_fixed_coefs(...)
#
#     fitted <- as.vector(fitted(model))
#     coefs <- c(..., coef(model))
#     coefs <- coefs[match(c("A", "B", "TD", "tau"), names(coefs))]
#     AIC <- AIC(model)
#     BIC <- BIC(model)
#     R2 <- 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2)
#     RMSE <- sqrt(mean(summary(model)$residuals^2))
#     RSE <- summary(model)$sigma
#     MAE <- mean(abs(summary(model)$residuals))
#     MAPE <- mean(abs(summary(model)$residuals/y)) * 100
#
#     structure(
#         list(
#             model = model,
#             fitted = fitted,
#             coefs = coefs,
#             fit_criteria = c(
#                 AIC = AIC, BIC = BIC, R2 = R2, RMSE = RMSE,
#                 RSE = RSE, MAE = MAE, MAPE = MAPE),
#             call = match.call()),
#         class = "mNIRS.kinetics")
# }

# process_kinetics_test(true_x, true_y, x0 = true_x[8], method = "monoexp", B = 100, Q = 100)
#
# process_kinetics_test(x = true_y)


fit_monoexp <- mNIRS::process_kinetics(
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


# (spl <- smooth.spline(true_x, true_y))
# str(spl)
# structure(
#     list(
#         x = ux,
#         y = fit$ty,
#         w = wbar,
#         yin = ybar,
#         tol = tol,
#         data = if (keep.data) list(x = x, y = y, w = w),
#         no.weights = no.wgts,
#         n = n,
#         lev = lev,
#         cv = cv,
#         cv.crit = cv.crit,
#         pen.crit = sum(wbar * (ybar - fit$ty)^2),
#         crit = fit$crit,
#         df = df,
#         spar = if (spar.is.lambda) NA else fit$spar,
#         ratio = if (spar.is.lambda) NA else fit$parms[["ratio"]],
#         lambda = fit$parms[["low"]],
#         iparms = c(fit$iparms, errorI = if (fit$ier) fit$ier else NA),
#         auxM = if (keep.stuff) list(
#             XWy = fit$scratch[seq_len(nk)],
#             XWX = fit$scratch[nk + seq_len(4 * nk)],
#             Sigma = fit$scratch[5 * nk + seq_len(4 * nk)],
#             R = fit$scratch[9 * nk + seq_len(4 * nk)]),
#         fit = structure(list(knot = knot, nk = nk, min = ux[1L], range = r.ux, coef = fit$coef),
#                         class = "smooth.spline.fit"),
#         call = match.call()),
#     class = "smooth.spline")
#
## half-time ========================================


# process_kinetics.half_time <- function(
#         x,
#         y = NULL,
#         x0 = 0,
#         method = c("monoexponential", "logistic", "half-time", "peak-slope"),
#         ...
# ) {
#
#     ## set c(x, y) when y missing
#     if (is.null(y)) {
#         y <- x
#         x <- seq_along(y)
#     }
#
#     x <- x - x0
#
#     ## TRUE == UP, FALSE == DOWN
#     (direction <- mean(head(y, length(y) / 4)) < mean(tail(y, length(y) / 4)))
#
#     (A <- mean(y[ifelse(all(x >= 1), x[1], which(x < 1))]))
#     (B <- ifelse(direction, max(y), min(y)))
#     (peak_sample <- x[y == B])
#     (half_value <- A + diff(c(A, B))/2)
#     (half_time <- ifelse(direction, x[y > half_value][1], x[y < half_value][1]))
#
#     coefs <- c(A = A, B = B, half_time = half_time, half_value = half_value)
#
#     structure(
#         list(
#             coefs = coefs,
#             call = match.call()),
#         class = "mNIRS.kinetics")
# }

mNIRS::process_kinetics(true_x, true_y, x0 = true_x[8], method = "half_time")
(half_data <- mNIRS::process_kinetics(x=true_x, y=true_y, method = "half_time")$coefs)

plot2 +
    geom_hline(yintercept = half_data["half_value"], linetype = "dotted") +
    geom_vline(xintercept = half_data["half_time"], linetype = "dotted")
