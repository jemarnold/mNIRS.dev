#' Process Kinetics
#'
#' Fit an mNIRS kinetics event with parametric or non-parametric models.
#'
#' @param x A numeric vector giving the predictor variable for `y` if `y` is
#' specified. Otherwise, `x` is assumed to specify the response variable.
#' @param y A numeric vector giving the response variable. If `y` is missing
#' or NULL, the predictor variable is assumed to be specified by `x`, with
#' `seq_along(x)` as the index predictor variable.
#' @param x0 A numeric scalar indicating the value of the predictor variable `x`
#' or index `seq_along(x)` representing the beginning of the kinetics event.
#' @param method Indicates how to process the kinetics.
#' - *"monoexponential"* ...
#' - *"logistic"* ...
#' - *"half_time"* ...
#' - *"peak_slope"* ...
#' @param ... Additional arguments. Used to define fixed parameters which will
#' not be optimised by the kinetics methods. e.g. `A = 10` will define `SSmonoexp(x, A = 10, B, TD, tau)`
#'
#' @return A list `L` of class `mNIRS.kinetics` with components `L$...`:
#' - `model` The model object.
#' - `data` A dataframe of the input and fitted model data.
#' - `fitted` A dataframe of the fitted values returned by the model.
#' - `coefs` A dataframe of the model coefficients, including manually fixed
#' parameters.
#' - `fit_criteria` A vector of the model fit criteria (e.g. `AIC`, `BIC`,
#' `RMSE`, etc.).
#'
#' @examples
#' ...
#'
#' @export
process_kinetics <- function(
        x,
        y = NULL,
        x0 = 0,
        method = c("monoexponential", "logistic", "half_time", "peak_slope"),
        ...
) {

    method <- match.arg(method)

    class(x) <- method

    UseMethod("process_kinetics", x)
}





#' @export
process_kinetics.monoexponential <- function(
        x,
        y = NULL,
        x0 = 0,
        method = c("monoexponential", "logistic", "half_time", "peak_slope"),
        ...
) {

    ## set c(x, y) when y missing
    if (is.null(y)) {
        y <- x
        x <- seq_along(y)
    }

    ## construct the dataframe
    df <- data.frame(x = x - x0, y)

    ## create the model and update for any fixed coefs
    model <- nls(y ~ SSmonoexp(x, A, B, TD, tau), data = df) |>
        mNIRS::update_fixed_coefs(...)

    fitted <- as.vector(fitted(model))
    df$fitted <- fitted
    coefs <- c(..., coef(model))
    coefs <- coefs[match(c("A", "B", "TD", "tau"), names(coefs))]
    AIC <- AIC(model)
    BIC <- BIC(model)
    R2 <- 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2)
    RMSE <- sqrt(mean(summary(model)$residuals^2))
    RSE <- summary(model)$sigma
    MAE <- mean(abs(summary(model)$residuals))
    MAPE <- mean(abs(summary(model)$residuals/y)) * 100

    structure(
        list(
            model = model,
            data = df,
            fitted = fitted,
            coefs = data.frame(coefs),
            fit_criteria = data.frame(
                AIC = AIC, BIC = BIC, R2 = R2, RMSE = RMSE,
                RSE = RSE, MAE = MAE, MAPE = MAPE),
            call = match.call()),
        class = "mNIRS.kinetics")
}




#' @export
process_kinetics.logistic <- function(
        x,
        y = NULL,
        x0 = 0,
        method = c("monoexponential", "logistic", "half_time", "peak_slope"),
        ...
) {

    ## set c(x, y) when y missing
    if (is.null(y)) {
        y <- x
        x <- seq_along(y)
    }

    ## construct the dataframe
    df <- data.frame(x = x - x0, y)

    ## create the model and update for any fixed coefs
    model <- nls(y ~ SSlogis(x, Asym, xmid, scal), data = df) |>
        mNIRS::update_fixed_coefs(...)

    fitted <- as.vector(fitted(model))
    df$fitted <- fitted
    coefs <- c(..., coef(model))
    coefs <- coefs[match(c("Asym", "xmid", "scal"), names(coefs))]
    AIC <- AIC(model)
    BIC <- BIC(model)
    R2 <- 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2)
    RMSE <- sqrt(mean(summary(model)$residuals^2))
    RSE <- summary(model)$sigma
    MAE <- mean(abs(summary(model)$residuals))
    MAPE <- mean(abs(summary(model)$residuals/y)) * 100

    structure(
        list(
            model = model,
            data = df,
            fitted = fitted,
            coefs = data.frame(coefs),
            fit_criteria = data.frame(
                AIC = AIC, BIC = BIC, R2 = R2, RMSE = RMSE,
                RSE = RSE, MAE = MAE, MAPE = MAPE),
            call = match.call()),
        class = "mNIRS.kinetics")
}




#' @export
process_kinetics.half_time <- function(
        x,
        y = NULL,
        x0 = 0,
        method = c("monoexponential", "logistic", "half_time", "peak_slope"),
        ...
) {

    ## set c(x, y) when y missing
    if (is.null(y)) {
        y <- x
        x <- seq_along(y)
    }

    x <- x - x0

    ## TRUE == UP, FALSE == DOWN
    (direction <- mean(head(y, length(y) / 4)) < mean(tail(y, length(y) / 4)))

    (A <- mean(y[ifelse(all(x >= 1), x[1], which(x < 1))]))
    (B <- ifelse(direction, max(y), min(y)))
    (peak_sample <- x[y == B])
    (half_value <- A + diff(c(A, B))/2)
    (half_time <- ifelse(direction, x[y > half_value][1], x[y < half_value][1]))

    coefs <- c(A = A, B = B, half_time = half_time, half_value = half_value)

    structure(
        list(
            coefs = coefs,
            call = match.call()),
        class = "mNIRS.kinetics")
}
