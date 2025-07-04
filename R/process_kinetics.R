#' Process Kinetics
#'
#' Fit mNIRS kinetics vector data with a parametric or non-parametric curve fitting
#' model.
#'
#' @param x A numeric vector specifying the predictor variable for `y` if `y` is
#'  defined. Otherwise, `x` is assumed to define the response variable.
#' @param y (*Optional*). A numeric vector specifying the response variable. If `y`
#'  is not defined, the response variable is assumed to be defined by `x`, with
#'  `idx = seq_along(x)` as the predictor variable.
#' @param data (*Optional*). A dataframe containing at least the response variable
#'  (`x`), or the predictor and response variables (`x` and `y`).
#' @param x0 A numeric scalar indicating the value of the predictor variable `x`
#'  or `idx` representing the start of the kinetics event.
#' @param method Indicates how to process the kinetics.
#'  \describe{
#'      \item{`method = "monoexponential"`}{...}
#'      \item{`method = "sigmoidal"`}{...}
#'      \item{`method = "half_time"`}{...}
#'      \item{`method = "peak_slope"`}{...}
#'  }
#' @param ... Additional arguments. Used to define fixed parameters which will
#'  not be optimised by the kinetics methods. e.g. `A = 10` will define
#'  `SSmonoexp(x, A = 10, B, TD, tau)`
#'
#' @details
#' `method %in% c("monoexponential", "sigmoidal")` use [nls()][stats::nls()]
#' for nonlinear (weighted) least-squares estimates.
#'
#' @seealso [stats::nls()], [stats::SSasymp()], [stats::SSlogis()],
#'
#' @return A list `L` of class `mNIRS.kinetics` with components `L$...`:
#'      \item{`model`}{The model object.}
#'      \item{`data`}{A dataframe of the input and fitted model data.}
#'      \item{`fitted`}{A vector of the fitted values returned by the model.}
#'      \item{`coefs`}{A dataframe of the model coefficients, including manually
#'      fixed parameters.}
#'      \item{`fit_criteria`}{A dataframe of the model fit criteria
#'      (`AIC`, `BIC`, `R2`, `RMSE`, `RSE`, `MAE`, `MAPE`).}
#'
#' @export
process_kinetics <- function(
        x,
        y = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
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
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        ...
) {
    if (!is.null(data) & !is.data.frame(data)) {
        ## data must be a dataframe
        cli::cli_abort("{.arg data} must be a dataframe")

    } else if (!is.null(data) & is.data.frame(data)) {

        ## deparse(substitute()) works for unquoted
        x_exp <- substitute(x) ## symbol from unquoted object name of x
        x_name <- tryCatch(
            rlang::as_name(x), ## as_name() works for quoted
            error = \(e) {
                ## for unquoted "object not found" error:
                if (grepl("object", e$message)) {
                    ## deparse() works for unquoted
                    gsub('^"|"$', '', deparse(x_exp)) ## quoted object name of x
                }})

        if (x_name %in% names(data)) {
            x <- data[[x_name]]
        } else {
            cli::cli_abort("{.arg x} not found in {.arg data}")
        }

        if (is.null(y) | missing(y)) {
            y_exp <- substitute(x) ## symbol from unquoted object name of x
            y_name <- x_name ## quoted object name of x
            x_exp <- substitute(index)
            x_name <- "index"
            y <- x
            x <- seq_along(y)

            data[[x_name]] <- x

        } else {
            ## deparse(substitute()) works for unquoted
            y_exp <- substitute(y) ## symbol from unquoted object name of y
            y_name <- tryCatch(
                rlang::as_name(y), ## as_name() works for quoted
                error = \(e) {
                    ## for unquoted "object not found" error:
                    if (grepl("object", e$message)) {
                        ## deparse() works for unquoted
                        gsub('^"|"$', '', deparse(y_exp)) ## quoted object name of y
                    }})

            if (y_name %in% names(data)) {
                y <- data[[y_name]]
            } else {
                cli::cli_abort("{.arg y} not found in {.arg data}")
            }
        }

        data <- data[c(x_name, y_name)]

    } else if (is.null(data)) {
        if (is.null(y)) {
            y_exp <- substitute(x) ## symbol from unquoted object name of x
            y_name <- deparse(y_exp) ## quoted object name of x
            x_exp <- substitute(index)
            x_name <- "index"
            y <- x
            x <- seq_along(y)
        } else {
            x_exp <- substitute(x) ## symbol from unquoted object name of x
            x_name <- deparse(x_exp) ## quoted object name of x
            y_exp <- substitute(y) ## symbol from unquoted object name of y
            y_name <- deparse(y_exp) ## quoted object name of y
            x <- x
            y <- y
        }

        data <- tibble::tibble(x, y)
        names(data) <- c(x_name, y_name)
    }

    x <- x - x0
    df <- tibble::tibble(x, y)
    fitted_name <- paste0(y_name, "_fitted")

    ## create the model and update for any fixed coefs
    model <- tryCatch(
        nls(y ~ SSmonoexp(x, A, B, TD, tau),
            data = df,
            na.action = na.exclude) |>
            update_fixed_coefs(...),
        error = function(e) {
            cat("Error in nls(", y_exp, " ~ SSmonoexp(", x_exp,
                ", A, B, TD, tau)) : ", e$message, "\n", sep = "")
            NA})

    if (is.na(model[1])) {
        fitted <- NA_real_
        data[[fitted_name]] <- NA_real_
        coefs <- NA_real_
        fit_criteria <- tibble::tibble(
            AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
            RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)
    } else {
        fitted <- as.vector(fitted(model))
        data[[fitted_name]] <- fitted
        coefs <- c(..., coef(model))
        coefs <- coefs[match(c("A", "B", "TD", "tau"), names(coefs))]
        coefs <- tibble::as_tibble(as.list(coefs))

        fit_criteria <- tibble::tibble(
            AIC = stats::AIC(model),
            BIC = stats::BIC(model),
            R2 = 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2),
            RMSE = sqrt(mean(summary(model)$residuals^2)),
            RSE = summary(model)$sigma,
            MAE = mean(abs(summary(model)$residuals)),
            MAPE = mean(abs(summary(model)$residuals/y)) * 100,
        )
    }

    ## save call
    return_call <- match.call()
    model_equation <- as.formula(y ~ A + (B - A) * (1 - exp((TD - x) / tau)))
    # return_call$model_equation <- list(call = list(formula = model_equation))

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
process_kinetics.sigmoidal <- function(
        x,
        y = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        ...
) {
    if (!is.null(data) & !is.data.frame(data)) {
        ## data must be a dataframe
        cli::cli_abort("{.arg data} must be a dataframe")

    } else if (!is.null(data) & is.data.frame(data)) {

        ## deparse(substitute()) works for unquoted
        x_exp <- substitute(x) ## symbol from unquoted object name of x
        x_name <- tryCatch(
            rlang::as_name(x), ## as_name() works for quoted
            error = \(e) {
                ## for unquoted "object not found" error:
                if (grepl("object", e$message)) {
                    ## deparse() works for unquoted
                    gsub('^"|"$', '', deparse(x_exp)) ## quoted object name of x
                }})

        if (x_name %in% names(data)) {
            x <- data[[x_name]]
        } else {
            cli::cli_abort("{.arg x} not found in {.arg data}")
        }

        if (is.null(y) | missing(y)) {
            y_exp <- substitute(x) ## symbol from unquoted object name of x
            y_name <- x_name ## quoted object name of x
            x_exp <- substitute(index)
            x_name <- "index"
            y <- x
            x <- seq_along(y)

            data[[x_name]] <- x

        } else {
            ## deparse(substitute()) works for unquoted
            y_exp <- substitute(y) ## symbol from unquoted object name of y
            y_name <- tryCatch(
                rlang::as_name(y), ## as_name() works for quoted
                error = \(e) {
                    ## for unquoted "object not found" error:
                    if (grepl("object", e$message)) {
                        ## deparse() works for unquoted
                        gsub('^"|"$', '', deparse(y_exp)) ## quoted object name of y
                    }})

            if (y_name %in% names(data)) {
                y <- data[[y_name]]
            } else {
                cli::cli_abort("{.arg y} not found in {.arg data}")
            }
        }

        data <- data[c(x_name, y_name)]

    } else if (is.null(data)) {
        if (is.null(y)) {
            y_exp <- substitute(x) ## symbol from unquoted object name of x
            y_name <- deparse(y_exp) ## quoted object name of x
            x_exp <- substitute(index)
            x_name <- "index"
            y <- x
            x <- seq_along(y)
        } else {
            x_exp <- substitute(x) ## symbol from unquoted object name of x
            x_name <- deparse(x_exp) ## quoted object name of x
            y_exp <- substitute(y) ## symbol from unquoted object name of y
            y_name <- deparse(y_exp) ## quoted object name of y
            x <- x
            y <- y
        }

        data <- tibble::tibble(x, y)
        names(data) <- c(x_name, y_name)
    }

    x <- x - x0
    df <- tibble::tibble(x, y)
    fitted_name <- paste0(y_name, "_fitted")

    ## create the model and update for any fixed coefs
    model <- tryCatch(
        nls(y ~ stats::SSlogis(x, Asym, xmid, scal),
            data = df,
            na.action = na.exclude) |>
            update_fixed_coefs(...),
        error = function(e) {
            cat("Error in nls(", y_exp, " ~ SSlogis(", x_exp,
                ", Asym, xmid, scal)) : ", e$message, "\n", sep = "")
            NA})

    if (is.na(model[1])) {
        ## define return components as NA for errored model
        fitted <- NA_real_
        df[[fitted]] <- NA_real_
        coefs <- NA_real_
        fit_criteria <- tibble::tibble(
            AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
            RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)
    } else {
        fitted <- as.vector(fitted(model))
        df[[fitted]] <- fitted
        coefs <- c(..., coef(model))
        coefs <- coefs[match(c("Asym", "xmid", "scal"), names(coefs))]
        coefs <- tibble::as_tibble(as.list(coefs))

        fit_criteria <- tibble::tibble(
            AIC = stats::AIC(model),
            BIC = stats::BIC(model),
            R2 = 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2),
            RMSE = sqrt(mean(summary(model)$residuals^2)),
            RSE = summary(model)$sigma,
            MAE = mean(abs(summary(model)$residuals)),
            MAPE = mean(abs(summary(model)$residuals/y)) * 100,
        )
    }

    ## save call
    return_call <- match.call()
    model_equation <- as.formula(y ~ Asym / (1 + exp((xmid - x) / scal)))
    # return_call$model_equation <- list(call = list(formula = model_equation))

    out <- structure(
        list(
            method = "sigmoidal",
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
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        ...
) {
    if (!is.null(data) & !is.data.frame(data)) {
        ## data must be a dataframe
        cli::cli_abort("{.arg data} must be a dataframe")

    } else if (!is.null(data) & is.data.frame(data)) {

        ## deparse(substitute()) works for unquoted
        x_exp <- substitute(x) ## symbol from unquoted object name of x
        x_name <- tryCatch(
            rlang::as_name(x), ## as_name() works for quoted
            error = \(e) {
                ## for unquoted "object not found" error:
                if (grepl("object", e$message)) {
                    ## deparse() works for unquoted
                    gsub('^"|"$', '', deparse(x_exp)) ## quoted object name of x
                }})

        if (x_name %in% names(data)) {
            x <- data[[x_name]]
        } else {
            cli::cli_abort("{.arg x} not found in {.arg data}")
        }

        if (is.null(y) | missing(y)) {
            y_exp <- substitute(x) ## symbol from unquoted object name of x
            y_name <- x_name ## quoted object name of x
            x_exp <- substitute(index)
            x_name <- "index"
            y <- x
            x <- seq_along(y)

            data[[x_name]] <- x

        } else {
            ## deparse(substitute()) works for unquoted
            y_exp <- substitute(y) ## symbol from unquoted object name of y
            y_name <- tryCatch(
                rlang::as_name(y), ## as_name() works for quoted
                error = \(e) {
                    ## for unquoted "object not found" error:
                    if (grepl("object", e$message)) {
                        ## deparse() works for unquoted
                        gsub('^"|"$', '', deparse(y_exp)) ## quoted object name of y
                    }})

            if (y_name %in% names(data)) {
                y <- data[[y_name]]
            } else {
                cli::cli_abort("{.arg y} not found in {.arg data}")
            }
        }

        data <- data[c(x_name, y_name)]

    } else if (is.null(data)) {
        if (is.null(y)) {
            y_exp <- substitute(x) ## symbol from unquoted object name of x
            y_name <- deparse(y_exp) ## quoted object name of x
            x_exp <- substitute(index)
            x_name <- "index"
            y <- x
            x <- seq_along(y)
        } else {
            x_exp <- substitute(x) ## symbol from unquoted object name of x
            x_name <- deparse(x_exp) ## quoted object name of x
            y_exp <- substitute(y) ## symbol from unquoted object name of y
            y_name <- deparse(y_exp) ## quoted object name of y
            x <- x
            y <- y
        }

        data <- tibble::tibble(x, y)
        names(data) <- c(x_name, y_name)
    }

    x <- x - x0
    df <- tibble::tibble(x, y)

    ## determine overall trend using direct least squares calculation
    valid_idx <- !is.na(x) & !is.na(y)
    x_clean <- x[valid_idx]
    y_clean <- y[valid_idx]

    x_mean <- mean(x_clean)
    y_mean <- mean(y_clean)

    ## covariance between x & y (+ve when they move in same direction)
    numerator <- sum((x_clean - x_mean) * (y_clean - y_mean), na.rm = TRUE)
    ## variance of x (spread of x around mean of x)
    denominator <- sum((x_clean - x_mean)^2, na.rm = TRUE)
    ## best-fit line gradient faster than calling `lm()`
    overall_slope <- if (denominator == 0) {0} else {numerator / denominator}
    ## TRUE == UP, FALSE == DOWN
    direction <- overall_slope >= 0

    A_sample <- ifelse(all(x > 0), x[1], 0)
    A <- mean(y[ifelse(all(x > 0), x[1], which(x <= 0))])
    B <- ifelse(direction, max(y), min(y))
    B_sample <- x[y == B]
    half_value <- A + diff(c(A, B))/2
    half_sample <- ifelse(direction, x[y >= half_value][1], x[y <= half_value][1])
    nirs_value <- y[x == half_sample]

    model = NA
    fitted <- NA_real_
    data$fitted <- NA_real_
    coefs <- c(A_sample = A_sample, A = A,
               B_sample = B_sample, B = B,
               half_sample = half_sample,
               half_value = half_value,
               nirs_value = nirs_value)
    coefs <- tibble::as_tibble(as.list(coefs))
    fit_criteria <- tibble::tibble(
        AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
        RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)

    ## save call
    return_call <- match.call()
    model_equation <- as.formula(half_value ~ A + (B - A) / 2)
    # return_call$model_equation <- list(call = list(formula = model_equation))

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





#' @export
process_kinetics.peak_slope <- function(
        x,
        y = NULL,
        data = NULL,
        x0 = 0,
        width,
        align = c("center", "left", "right"),
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        ...
) {
    if (!is.null(data) & !is.data.frame(data)) {
        ## data must be a dataframe
        cli::cli_abort("{.arg data} must be a dataframe")

    } else if (!is.null(data) & is.data.frame(data)) {

        ## deparse(substitute()) works for unquoted
        x_exp <- substitute(x) ## symbol from unquoted object name of x
        x_name <- tryCatch(
            rlang::as_name(x), ## as_name() works for quoted
            error = \(e) {
                ## for unquoted "object not found" error:
                if (grepl("object", e$message)) {
                    ## deparse() works for unquoted
                    gsub('^"|"$', '', deparse(x_exp)) ## quoted object name of x
                }})

        if (x_name %in% names(data)) {
            x <- data[[x_name]]
        } else {
            cli::cli_abort("{.arg x} not found in {.arg data}")
        }

        if (is.null(y) | missing(y)) {
            y_exp <- substitute(x) ## symbol from unquoted object name of x
            y_name <- x_name ## quoted object name of x
            x_exp <- substitute(index)
            x_name <- "index"
            y <- x
            x <- seq_along(y)

            data[[x_name]] <- x

        } else {
            ## deparse(substitute()) works for unquoted
            y_exp <- substitute(y) ## symbol from unquoted object name of y
            y_name <- tryCatch(
                rlang::as_name(y), ## as_name() works for quoted
                error = \(e) {
                    ## for unquoted "object not found" error:
                    if (grepl("object", e$message)) {
                        ## deparse() works for unquoted
                        gsub('^"|"$', '', deparse(y_exp)) ## quoted object name of y
                    }})

            if (y_name %in% names(data)) {
                y <- data[[y_name]]
            } else {
                cli::cli_abort("{.arg y} not found in {.arg data}")
            }
        }

        data <- data[c(x_name, y_name)]

    } else if (is.null(data)) {
        if (is.null(y)) {
            y_exp <- substitute(x) ## symbol from unquoted object name of x
            y_name <- deparse(y_exp) ## quoted object name of x
            x_exp <- substitute(index)
            x_name <- "index"
            y <- x
            x <- seq_along(y)
        } else {
            x_exp <- substitute(x) ## symbol from unquoted object name of x
            x_name <- deparse(x_exp) ## quoted object name of x
            y_exp <- substitute(y) ## symbol from unquoted object name of y
            y_name <- deparse(y_exp) ## quoted object name of y
            x <- x
            y <- y
        }

        data <- tibble::tibble(x, y)
        names(data) <- c(x_name, y_name)
    }

    x <- x - x0
    df <- tibble::tibble(x, y)

    slopes <- rolling_slope(y, x, width, align, na.rm = TRUE)

    peak_model <- peak_directional_slope(y, x, width, align, na.rm = TRUE)



    model <- NA
    data$fitted <- NA_real_
    data$fitted[x %in% peak_model$x_fitted] <- peak_model$y_fitted
    coefs <- c(sample = peak_model$x,
               peak_model = peak_model$slope,
               nirs_value = y[x %in% peak_model$x])
    coefs <- tibble::as_tibble(as.list(coefs))
    fit_criteria <- tibble::tibble(
        AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
        RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)

    ## save call
    return_call <- match.call()
    model_equation <- as.formula(
        TODO ~ sum((x_window - x_mean) * (y_window - y_mean)) /
            sum((x_window - x_mean)^2))

    out <- structure(
        list(
            method = "peak_slope",
            model = model,
            model_equation = model_equation,
            data = data,
            fitted = slopes,
            x0 = x0,
            width = width,
            align = align,
            coefs = coefs,
            fit_criteria = fit_criteria,
            call = return_call),
        class = "mNIRS.kinetics")

    return(out)
}
