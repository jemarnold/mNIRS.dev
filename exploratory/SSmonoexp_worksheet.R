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
## Data Wrangling ================================
# Generate sample data
set.seed(123)
x <- seq(0, 60, by = 2)
true_A <- 10
true_B <- 100
true_TD <- 15
true_tau <- 8
y <- true_A + (true_B - true_A) * (1 - exp(-((x - true_TD)/true_tau))) * ifelse(x >= true_TD, 1, 0)
y <- y + rnorm(length(x), 0, 3)  # Add noise
data <- tibble(x = x, y = y)

ggplot(data) +
    {list( ## Settings
        aes(x = x, y = y),
        theme_JA(),
        NULL)} + ## Settings
    {list( ## Data
        geom_line(),
        geom_point(),
        NULL)} ## Data


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






nls(y ~ SSmonoexp(x, A, B, TD, tau), data = data)
nls(y ~ SSmonoexp(x, A, B, TD, tau), data = data) |>
    (\(.x) update(
        object = .x,
        y ~ monoexp_equation(x, A = 10, B, TD, tau),
        start = within(as.list(.x$m$getPars()), rm(A))))()
nls(y ~ monoexp_equation(x, A=10, B, TD, tau), data = data,
    start = list(B = 100, TD = 15, tau = 10))

# nls.multstart::nls_multstart(
#     formula = y ~ monoexp_equation(x, A, B, TD, tau),
#     data = data,
#     start_lower = c(A = 0, B = 0, TD = 0, tau = 0),
#     start_upper = c(A = 100, B = 100, TD = 60, tau = 100),
#     iter = 100,
#     # supp_errors = "Y"
# )





# Create a modified self-starting function system that supports fixed parameters
create_SSmonoexp <- function(fixed_params = list()) {


    # Create a version of the formula with only free parameters
    param_names <- c("A", "B", "TD", "tau")
    free_params <- setdiff(param_names, names(fixed_params))

    monoexp_eq_wrapper <- function(x, B, TD, tau) {
        list2env(fixed_params, envir = .GlobalEnv)
        ifelse(x <= TD, A, A + (B-A) * (1 - exp((TD - x) / tau)))
    }

    # Initial parameter estimation function that only estimates free parameters
    monoexp_init <- function(
        mCall, LHS, data, ...
    ) {
        ## self-start parameters for nls of monoexponential fit function
        ## https://www.statforbiology.com/2020/stat_nls_selfstarting/#and-what-about-nls
        xy <- sortedXyData(mCall[["x"]], LHS, data)
        x <- xy[, "x"]
        y <- xy[, "y"]

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

        # All initial estimates
        all_inits <- c(A = A, B = B, TD = TD, tau = tau)

        # Only return estimates for free parameters
        setNames(all_inits[free_params], mCall[free_params])
    }

    # Create and return a selfStart function
    selfStart(monoexp_eq_wrapper, monoexp_init, parameters = free_params)
}

# Create a model where A is fixed to 10
SSmonoexp_fixedA <- create_SSmonoexp(fixed_params = list(A = 10))

# Use it in nls
(fit <- nls(y ~ SSmonoexp_fixedA(x, B, TD, tau), data = data))





SSmonoexp_fixed <- function(x, B, TD, tau) SSmonoexp(x, A = 10, B, TD, tau, data = data)
nls(formula = y ~ SSmonoexp_fixed(x, B, TD, tau), start = list(B = 100, TD = 15, tau = 15),
    control = list(maxiter = 50, tol = 5e-8, warnOnly = TRUE), algorithm = "port",
    weights = sqrt(y), na.action = na.exclude, lower = 0, upper = 1)



test <- function(x, ...) {
    # list2env(fixed_params, envir = .GlobalEnv)
    args <- list(...)
    # list2env(args, envir = .GlobalEnv)
    args
}
test(A=10)



SSmonoexp <- function(x, A, B, TD, tau) {
    # The main model function
    ifelse(x <= TD, A, A + (B-A) * (1 - exp((TD - x) / tau)))
}

# Function to create the initial estimates
.SSmonoexp.init <- function(mCall, LHS, data, ...) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    x <- xy[, "x"]
    y <- xy[, "y"]

    ## TRUE == UP, FALSE == DOWN
    direction <- mean(head(y, length(y) / 4)) < mean(tail(y, length(y) / 4))

    A <- min(y) * direction + max(y) * !direction
    B <- max(y) * direction + min(y) * !direction
    TD <- x[min(which(x > 0 & abs(y - A) > abs(B - A) / 10))]
    tau <- abs(TD - x[max(c(
        min(which(x > 0)),
        which.min(abs(y - (A + 0.632 * (B - A))))))])

    setNames(c(A, B, TD, tau), mCall[c("A", "B", "TD", "tau")])
}

# Create a selfStart version
SSmonoexp <- selfStart(SSmonoexp, .SSmonoexp.init, parameters = c("A", "B", "TD", "tau"))
nls(y ~ SSmonoexp(x, A = 10, B, TD, tau), data = data)
