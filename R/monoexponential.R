#' Monoexponential Function
#'
#' A function to calculate a four-parameter monoexponential curve.
#'
#' @param x A numeric vector for the predictor variable at which to calculate
#' the response variable.
#' @param A A numeric parameter for the starting value.
#' @param B A numeric parameter for the ending asymptote value.
#' @param TD A numeric parameter for the time delay in the units of `x`
#' before inflection.
#' @param tau A numeric parameter for the time constant of the exponential
#' inflection in the units of `x`.
#'
#' @return A numeric vector of fitted values from a monoexponential function.
#'
#' @examples
#' set.seed(13)
#' x <- seq(0, 60, by = 2)
#' A <- 10
#' B <- 100
#' TD <- 15
#' tau <- 8
#' y <- monoexponential(x, A, B, TD, tau) + rnorm(length(x), 0, 3)
#' data <- data.frame(x, y)
#'
#' model <- nls(y ~ SSmonoexp(x, A, B, TD, tau), data = data)
#' model
#' fitted <- fitted(model)
#'
#' plot(x, y)
#' lines(x, y)
#' points(x, y)
#' lines(x, fitted, col = "red")
#'
#' @export
monoexponential <- function(x, A, B, TD, tau) {
    ifelse(x <= TD, A, A + (B - A) * (1 - exp((TD - x) / tau)))
}




#' @export
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

    setNames(c(A, B, TD, tau), mCall[c("A", "B", "TD", "tau")])
}




#' Self-Starting `nls` Monoexponential Model
#'
#' This `selfStart` model evaluates the four-parameter
#' [monoexponential][mNIRS::monoexponential()] function. It creates initial
#' estimates for the parameters `A`, `B`, `TD`, and `tau`.
#'
#' @param x A numeric vector at which to evaluate the model.
#' @param A A numeric parameter for the starting value.
#' @param B A numeric parameter for the ending asymptote value.
#' @param TD A numeric parameter for the time delay in the units of `x`
#' before inflection.
#' @param tau A numeric parameter for the time constant of the exponential
#' inflection in the units of `x`.
#'
#' @return A numeric vector of the same length as `x`. The value of the
#' expression `ifelse(x <= TD, A, A + (B - A) * (1 - exp((TD - x) / tau)))`.
#'
#' @seealso [nls][stats::nls()], [selfStart][stats::selfStart()]
#'
#' @examples
#' set.seed(13)
#' x <- seq(0, 60, by = 2)
#' A <- 10
#' B <- 100
#' TD <- 15
#' tau <- 8
#' y <- monoexponential(x, A, B, TD, tau) + rnorm(length(x), 0, 3)
#' data <- data.frame(x, y)
#'
#' model <- nls(y ~ SSmonoexp(x, A, B, TD, tau), data = data)
#' model
#' fitted <- fitted(model)
#'
#' plot(x, y)
#' lines(x, y)
#' points(x, y)
#' lines(x, fitted, col = "red")
#'
#' @export
SSmonoexp <- selfStart(
    model = monoexponential,
    initial = monoexp_init,
    parameters = c("A", "B", "TD", "tau"))
