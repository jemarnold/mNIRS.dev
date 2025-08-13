#' Monoexponential Function with 4 Parameters
#'
#' Calculate a four-parameter monoexponential curve.
#'
#' @param x A numeric predictor variable at which to evaluate the response
#'  variable `y`.
#' @param A A numeric parameter for the starting (baseline) value of the response
#'  variable `y`.
#' @param B A numeric parameter for the ending (asymptote) value of the response
#'  variable `y`.
#' @param TD A numeric parameter for the time delay before exponential inflection
#'  of the curve, in units of the predictor variable `x`.
#' @param tau A numeric parameter for the time constant `tau (𝜏)` of the
#'  exponential curve, in units of the predictor variable `x`. `tau` is equal to
#'  the reciprocal of `k` (`tau = 1/k`), where `k` is the rate constant of the
#'  same function.
#'
#' @return A numeric vector for the response variabel `y` of the same length as
#'  the predictor variable `x`.
#'
#' @examples
#' set.seed(13)
#' x <- seq(0, 60, by = 2)
#' A <- 10; B <- 100; TD <- 15; tau <- 8
#' y <- monoexponential(x, A, B, TD, tau) + rnorm(length(x), 0, 3)
#' data <- data.frame(x, y)
#'
#' model <- nls(y ~ SSmonoexp(x, A, B, TD, tau), data = data)
#' model
#'
#' \dontrun{
#' plot(x, y)
#' lines(x, y)
#' points(x, y)
#' lines(x, fitted(model), col = "red")
#' }
#'
#' @rdname monoexponential
#' @export
monoexponential <- function(x, A, B, TD, tau) {
    ifelse(x <= TD, A, A + (B - A) * (1 - exp((TD - x) / tau)))
}




#' Initiate Self-Starting `nls` Monoexponential Model
#'
#' [monoexp_init()]: Returns initial values for the parameters in a `selfStart`
#' model.
#'
#' @param mCall A matched call to the function `model`.
#' @param data A data frame in which to interpret the variables in `mCall`.
#' @param LHS The expression from the left-hand side of the model formula in the
#'  call to `nls`.
#' @param ... Additional arguments.
#'
#' @return [monoexp_init()]: Initial starting estimates for parameters in the model
#'  called by [SSmonoexp()].
#'
#' @name SSmonoexp
#' @rdname SSmonoexp
#' @export
monoexp_init <- function(
        mCall, data, LHS, ...
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





#' Self-Starting `nls` Four-Parameter Monoexponential Model
#'
#' [SSmonoexp()]: Creates initial coefficient estimates for a `selfStart` model
#' for the four-parameter [monoexponential()] function. For the parameters `A`, `B`,
#' `TD`, and `tau`.
#'
#' @inheritParams monoexponential
#'
#' @return [SSmonoexp()]: A numeric vector for the response variabel `y` of the
#'  same length as the predictor variable `x`. Returned from the expression
#'  `ifelse(x <= TD, A, A + (B - A) * (1 - exp((TD - x) / tau)))`.
#'
#' @seealso [stats::nls()], [stats::selfStart()], [monoexponential()]
#'
#' @examples
#' set.seed(13)
#' x <- seq(0, 60, by = 2)
#' A <- 10; B <- 100; TD <- 15; tau <- 8
#' y <- monoexponential(x, A, B, TD, tau) + rnorm(length(x), 0, 3)
#' data <- data.frame(x, y)
#'
#' model <- nls(y ~ SSmonoexp(x, A, B, TD, tau), data = data)
#' model
#'
#' \dontrun{
#' plot(x, y)
#' lines(x, y)
#' points(x, y)
#' lines(x, fitted(model), col = "red")
#' }
#'
#' @rdname SSmonoexp
#' @export
SSmonoexp <- selfStart(
    model = monoexponential,
    initial = monoexp_init,
    parameters = c("A", "B", "TD", "tau"))
