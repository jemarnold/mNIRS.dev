#' Process Kinetics
#'
#' Fit an mNIRS kinetics event with parametric or non-parametric models.
#'
#' @param x A numeric vector giving the predictor variable for `y` if `y` is
#' specified. Otherwise, `x` is assumed to specify the response variable.
#' @param y A numeric vector giving the response variable. If `y` is missing
#' or NULL, the predictor variable is assumed to be specified by `x`, with
#' `seq_along(x)` as the index predictor variable.
#' @param data A dataframe containing at least `x` or `x` and `y`...
#' @param x0 A numeric scalar indicating the value of the predictor variable `x`
#' or index `seq_along(x)` representing the beginning of the kinetics event.
#' @param method Indicates how to process the kinetics.
#' - *"monoexponential"* ...
#' - *"logistic"* ...
#' - *"half_time"* ...
#' - *"peak_slope"* ...
#' @param ... Additional arguments. Used to define fixed parameters which will
#' not be optimised by the kinetics methods. e.g. `A = 10` will define
#' `SSmonoexp(x, A = 10, B, TD, tau)`
#'
#' @return A list `L` of class `mNIRS.kinetics` with components `L$...`:
#' \describe{
#'   \item{`model`}{The model object.}
#'   \item{`data`}{A dataframe of the input and fitted model data.}
#'   \item{`fitted`}{A vector of the fitted values returned by the model.}
#'   \item{`coefs`}{A dataframe of the model coefficients, including manually
#'   fixed parameters.}
#'   \item{`fit_criteria`}{A dataframe of the model fit criteria
#'   (`AIC`, `BIC`, `R2`, `RMSE`, `RSE`, `MAE`, `MAPE`).}
#' }
#'
#' @export
process_kinetics <- function(
        x,
        y = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "logistic", "half_time", "peak_slope"),
        ...
) {

    method <- match.arg(method)

    x_exp <- substitute(x)
    x_name <- deparse(x_exp)

    class(x_name) <- method

    UseMethod("process_kinetics", x_name)
}





#' @export
process_kinetics.monoexponential <- function(
        x,
        y = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "logistic", "half_time", "peak_slope"),
        ...
) {
    x_exp <- substitute(x)
    x_name <- deparse(x_exp)
    y_exp <- substitute(y)
    y_name <- deparse(y_exp)

    if (!(is.null(data) | missing(data)) & !is.data.frame(data)) {
        ## data must be a dataframe
        cli::cli_abort("{.arg data} must be a dataframe")

    } else if (!(is.null(data) | missing(data)) & is.data.frame(data)) {
        if (x_name %in% names(data)) {
            ## deparse(substitute(x)) works for unquoted x
            x <- data[[x_name]]
        } else if (x_exp %in% names(data)) {
            ## substitute(x) works for quoted x, fails for unquoted x
            x <- data[[x_exp]]
        } else {
            cli::cli_abort("{.arg x} not found in {.arg data}")
        }

        if (is.null(y_exp) | missing(y_exp)) {
            y <- x
            x <- seq_along(y)
        } else if (y_name %in% names(data)) {
            ## deparse(substitute(y)) works for unquoted y
            y <- data[[y_name]]
        } else if (y_exp %in% names(data)) {
            ## substitute(y) works for quoted y, fails for unquoted y
            y <- data[[y_exp]]
        } else {
            cli::cli_abort("{.arg y} not found in {.arg data}")
        }

        data <- data[c(x_name, y_name)]

    } else if (is.null(data) | missing(data)) {
        if (is.null(y) | missing(y)) {
            y <- x
            x <- seq_along(y)
        } else {
            x <- x
            y <- y
        }

        data <- tibble::tibble(!!x_name := x, !!y_name := y)
    }

    x <- x - x0
    df <- tibble::tibble(x, y)

    ## create the model and update for any fixed coefs
    model <- tryCatch(
        nls(y ~ SSmonoexp(x, A, B, TD, tau),
            data = df,
            na.action = na.exclude) |>
            mNIRS::update_fixed_coefs(...),
        error = function(e) {
            cat("Error in nls(", y_exp, " ~ SSmonoexp(", x_exp,
                ", A, B, TD, tau)) : ", e$message, "\n", sep = "")
            NA})

    if (is.na(model[1])) {
        fitted <- NA_real_
        data$fitted <- NA_real_
        coefs <- NA_real_
        fit_criteria <- tibble::tibble(
            AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
            RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)
    } else {
        fitted <- as.vector(fitted(model))
        data$fitted <- fitted
        coefs <- c(..., coef(model))
        coefs <- coefs[match(c("A", "B", "TD", "tau"), names(coefs))]
        coefs <- tibble::as_tibble(as.list(coefs))

        fit_criteria <- tibble::tibble(
            AIC = AIC(model),
            BIC = BIC(model),
            R2 = 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2),
            RMSE = sqrt(mean(summary(model)$residuals^2)),
            RSE = summary(model)$sigma,
            MAE = mean(abs(summary(model)$residuals)),
            MAPE = mean(abs(summary(model)$residuals/y)) * 100,
        )
    }

    ## save call
    model_equation <- as.formula(y ~ A + (B - A) * (1 - exp((TD - x) / tau)))
    return_call <- match.call()
    return_call$model_equation <- list(call = list(formula = model_equation))

    out <- structure(
        list(
            method = "monoexponential",
            model = model,
            model_equation = model_equation,
            data = data,
            fitted = fitted,
            x0 = x0,
            coefs = coefs,
            fit_criteria = fit_criteria,
            call = return_call),
        class = "mNIRS.kinetics")

    return(out)
}




#' @export
process_kinetics.logistic <- function(
        x,
        y = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "logistic", "half_time", "peak_slope"),
        ...
) {
    x_exp <- substitute(x)
    x_name <- deparse(x_exp)
    y_exp <- substitute(y)
    y_name <- deparse(y_exp)

    if (!(is.null(data) | missing(data)) & !is.data.frame(data)) {
        ## data must be a dataframe
        cli::cli_abort("{.arg data} must be a dataframe")

    } else if (!(is.null(data) | missing(data)) & is.data.frame(data)) {
        if (x_name %in% names(data)) {
            ## deparse(substitute(x)) works for unquoted x
            x <- data[[x_name]]
        } else if (x_exp %in% names(data)) {
            ## substitute(x) works for quoted x, fails for unquoted x
            x <- data[[x_exp]]
        } else {
            cli::cli_abort("{.arg x} not found in {.arg data}")
        }

        if (is.null(y_exp) | missing(y_exp)) {
            y <- x
            x <- seq_along(y)
        } else if (y_name %in% names(data)) {
            ## deparse(substitute(y)) works for unquoted y
            y <- data[[y_name]]
        } else if (y_exp %in% names(data)) {
            ## substitute(y) works for quoted y, fails for unquoted y
            y <- data[[y_exp]]
        } else {
            cli::cli_abort("{.arg y} not found in {.arg data}")
        }

        data <- data[c(x_name, y_name)]

    } else if (is.null(data) | missing(data)) {
        if (is.null(y) | missing(y)) {
            y <- x
            x <- seq_along(y)
        } else {
            x <- x
            y <- y
        }

        data <- tibble::tibble(!!x_name := x, !!y_name := y)
    }

    x <- x - x0
    df <- tibble::tibble(x, y)

    ## create the model and update for any fixed coefs
    model <- tryCatch(
        nls(y ~ SSlogis(x, Asym, xmid, scal),
            data = df,
            na.action = na.exclude) |>
            mNIRS::update_fixed_coefs(...),
        error = function(e) {
            cat("Error in nls(", y_exp, " ~ SSlogis(", x_exp,
                ", Asym, xmid, scal)) : ", e$message, "\n", sep = "")
            NA})

    if (is.na(model[1])) {
        ## define return components as NA for errored model
        fitted <- NA_real_
        df$fitted <- NA_real_
        coefs <- NA_real_
        fit_criteria <- tibble::tibble(
            AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
            RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)
    } else {
        fitted <- as.vector(fitted(model))
        df$fitted <- fitted
        coefs <- c(..., coef(model))
        coefs <- coefs[match(c("Asym", "xmid", "scal"), names(coefs))]
        coefs <- tibble::as_tibble(as.list(coefs))

        fit_criteria <- tibble::tibble(
            AIC = AIC(model),
            BIC = BIC(model),
            R2 = 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2),
            RMSE = sqrt(mean(summary(model)$residuals^2)),
            RSE = summary(model)$sigma,
            MAE = mean(abs(summary(model)$residuals)),
            MAPE = mean(abs(summary(model)$residuals/y)) * 100,
        )
    }

    ## save call
    model_equation <- as.formula(y ~ Asym / (1 + exp((xmid - x) / scal)))
    return_call <- match.call()
    return_call$model_equation <- list(call = list(formula = model_equation))

    out <- structure(
        list(
            method = "logistic",
            model = model,
            model_equation = model_equation,
            data = data,
            fitted = fitted,
            x0 = x0,
            coefs = coefs,
            fit_criteria = fit_criteria,
            call = return_call),
        class = "mNIRS.kinetics")

    return(out)
}




#' @export
process_kinetics.half_time <- function(
        x,
        y = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "logistic", "half_time", "peak_slope"),
        ...
) {
    x_exp <- substitute(x)
    x_name <- deparse(x_exp)
    y_exp <- substitute(y)
    y_name <- deparse(y_exp)

    if (!(is.null(data) | missing(data)) & !is.data.frame(data)) {
        ## data must be a dataframe
        cli::cli_abort("{.arg data} must be a dataframe")

    } else if (!(is.null(data) | missing(data)) & is.data.frame(data)) {
        if (x_name %in% names(data)) {
            ## deparse(substitute(x)) works for unquoted x
            x <- data[[x_name]]
        } else if (x_exp %in% names(data)) {
            ## substitute(x) works for quoted x, fails for unquoted x
            x <- data[[x_exp]]
        } else {
            cli::cli_abort("{.arg x} not found in {.arg data}")
        }

        if (is.null(y_exp) | missing(y_exp)) {
            y <- x
            x <- seq_along(y)
        } else if (y_name %in% names(data)) {
            ## deparse(substitute(y)) works for unquoted y
            y <- data[[y_name]]
        } else if (y_exp %in% names(data)) {
            ## substitute(y) works for quoted y, fails for unquoted y
            y <- data[[y_exp]]
        } else {
            cli::cli_abort("{.arg y} not found in {.arg data}")
        }

        data <- data[c(x_name, y_name)]

    } else if (is.null(data) | missing(data)) {
        if (is.null(y) | missing(y)) {
            y <- x
            x <- seq_along(y)
        } else {
            x <- x
            y <- y
        }

        data <- tibble::tibble(!!x_name := x, !!y_name := y)
    }

    x <- x - x0
    df <- tibble::tibble(x, y)

    ## TRUE == UP, FALSE == DOWN
    (direction <- mean(head(y, length(y) / 4)) < mean(tail(y, length(y) / 4)))

    (A <- mean(y[ifelse(all(x >= 1), x[1], which(x < 1))]))
    (B <- ifelse(direction, max(y), min(y)))
    (peak_sample <- x[y == B])
    (half_value <- A + diff(c(A, B))/2)
    (half_time <- ifelse(direction, x[y > half_value][1], x[y < half_value][1]))

    model = NA
    fitted <- NA_real_
    data$fitted <- NA_real_
    coefs <- c(A = A, B = B, half_time = half_time, half_value = half_value)
    coefs <- tibble::as_tibble(as.list(coefs))
    fit_criteria <- tibble::tibble(
        AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
        RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)

    ## save call
    model_equation <- as.formula(TODO ~ add + half_time + formula)
    return_call <- match.call()
    return_call$model_equation <- list(call = list(formula = model_equation))

    out <- structure(
        list(
            method = "half_time",
            model = model,
            model_equation = model_equation,
            data = data,
            fitted = fitted,
            x0 = x0,
            coefs = coefs,
            fit_criteria = fit_criteria,
            call = return_call),
        class = "mNIRS.kinetics")

    return(out)
}
