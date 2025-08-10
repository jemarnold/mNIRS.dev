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
#'  or `idx` representing the start of the kinetics event (*default x0 = 0*).
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
#'      \item{`data`}{A dataframe of input and fitted model data.}
#'      \item{`fitted`}{A vector of fitted values returned by the model.}
#'      \item{`coefs`}{A vector of model coefficients, including manually
#'      fixed parameters.}
#'      \item{`fit_criteria`}{A vector of model fit criteria
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




## TODO fix sigmoidal (custom self-start function?)
## TODO 2025-07-19 fix fitted models up to first peak with no greater peaks within X samples
## TODO 2025-07-19 fix half-time to use neighbouring median around peak value B
## TODO 2025-08-10 width in units of x
## DONE TODO 2025-08-10 pre-process function for x/y_exp, _name
## DONE TODO 2025-08-10 export tibbles instead of named vectors

#' @export
process_kinetics.monoexponential <- function(
        x,
        y = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        ...
) {
    intake <- process_names_for_kinetics(x = x, y = y, data = data, x0 = x0)
    data <- intake$data
    df <- intake$df
    x <- intake$x
    x_exp <- intake$x_exp
    x_name <- intake$x_name
    y <- intake$y
    y_exp <- intake$y_exp
    y_name <- intake$y_name
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

    ## process coefs, fit_criteria, fitted
    model_output <- process_model(model)
    data[[fitted_name]] <- model_output$fitted
    ## include explicitly defined coefs
    coefs <- c(..., model_output$coefs)
    coefs <- coefs[match(c("A", "B", "TD", "tau"), names(coefs))]
    ## convert named vector to dataframe
    coefs <- tibble::as_tibble(as.list(coefs))
    ## calculate MRT
    coefs$MRT <- coefs$TD + coefs$tau
    ## predict value for y at MRT x value
    coefs[[paste0(y_name, "_MRT")]] <- predict(model, tibble::tibble(x = coefs$MRT))

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
            fitted = model_output$fitted,
            x0 = x0,
            coefs = coefs,
            fit_criteria = model_output$fit_criteria,
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
    intake <- process_names_for_kinetics(x = x, y = y, data = data, x0 = x0)
    data <- intake$data
    df <- intake$df
    x <- intake$x
    x_exp <- intake$x_exp
    x_name <- intake$x_name
    y <- intake$y
    y_exp <- intake$y_exp
    y_name <- intake$y_name
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

    ## process coefs, fit_criteria, fitted
    model_output <- process_model(model)
    data[[fitted_name]] <- model_output$fitted
    ## include explicitly defined coefs
    coefs <- c(..., model_output$coefs)
    coefs <- coefs[match(c("Asym", "xmid", "scal"), names(coefs))]
    ## convert named vector to dataframe
    coefs <- tibble::as_tibble(as.list(coefs))


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
            fitted = model_output$fitted,
            x0 = x0,
            coefs = coefs,
            fit_criteria = model_output$fit_criteria,
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
    intake <- process_names_for_kinetics(x = x, y = y, data = data, x0 = x0)
    data <- intake$data
    df <- intake$df
    x <- intake$x
    x_exp <- intake$x_exp
    x_name <- intake$x_name
    y <- intake$y
    y_exp <- intake$y_exp
    y_name <- intake$y_name

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
    B_sample <- x[y == B][1]
    half_value <- A + diff(c(A, B))/2
    half_sample <- ifelse(direction, x[y >= half_value][1], x[y <= half_value][1])
    nirs_value <- y[x == half_sample][1]

    coefs <- c(A_sample, A, B_sample, B, half_sample, half_value, nirs_value)
    names(coefs) <- c(paste0("A_", x_name), "A",
                      paste0("B_", x_name), "B",
                      paste0("half_", x_name),
                      "half_value",
                      paste0(y_name, "_value"))
    coefs <- tibble::as_tibble(as.list(coefs))

    ## save call
    return_call <- match.call()
    model_equation <- as.formula(half_value ~ A + (B - A) / 2)
    # return_call$model_equation <- list(call = list(formula = model_equation))

    out <- structure(
        list(
            method = "half_time",
            model_equation = model_equation,
            data = data,
            x0 = x0,
            coefs = coefs,
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
    intake <- process_names_for_kinetics(x = x, y = y, data = data, x0 = x0)
    data <- intake$data
    df <- intake$df
    x <- intake$x
    x_exp <- intake$x_exp
    x_name <- intake$x_name
    y <- intake$y
    y_exp <- intake$y_exp
    y_name <- intake$y_name

    fitted_name <- paste0(y_name, "_fitted")

    align <- match.arg(align)
    slopes <- rolling_slope(y, x, width, align, na.rm = TRUE)
    peak_slope_model <- peak_directional_slope(y, x, width, align, na.rm = TRUE)

    data[[fitted_name]] <- NA_real_
    data[[fitted_name]][x %in% peak_slope_model$x_fitted] <- peak_slope_model$y_fitted
    coefs <- c(peak_slope_model$x[1],
               y[x %in% peak_slope_model$x][1],
               data[[fitted_name]][x %in% peak_slope_model$x][1],
               peak_slope_model$slope)
    names(coefs) <- c(x_name, y_name, fitted_name, "peak_slope")
    coefs <- tibble::as_tibble(as.list(coefs))

    ## save call
    return_call <- match.call()
    model_equation <- as.formula(
        TODO ~ sum((x_window - x_mean) * (y_window - y_mean)) /
            sum((x_window - x_mean)^2))

    out <- structure(
        list(
            method = "peak_slope",
            model_equation = model_equation,
            data = data,
            slopes = slopes,
            x0 = x0,
            width = width,
            align = align,
            coefs = coefs,
            call = return_call),
        class = "mNIRS.kinetics")

    return(out)
}







#' @keywords internal
process_names_for_kinetics <- function(
        x,
        y,
        data,
        x0
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

    return(list(
        data = data,
        df = df,
        x = x,
        x_exp = x_exp,
        x_name = x_name,
        y = y,
        y_exp = y_exp,
        y_name = y_name))
}




#' @keywords internal
process_model <- function(
        model
) {
    if (is.na(model[1])) {
        fitted <- NA_real_
        coefs <- tibble::tibble(A = NA_real_, B = NA_real_, TD = NA_real_,
                                tau = NA_real_, MRT = NA_real_)
        fit_criteria <- tibble::tibble(
            AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
            RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)
    } else {
        y <- model$m$getEnv()$y
        fitted <- as.vector(fitted(model))
        coefs <- coef(model)
        fit_criteria <- tibble::tibble(
            AIC = stats::AIC(model),
            BIC = stats::BIC(model),
            R2 = 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2),
            RMSE = sqrt(mean(summary(model)$residuals^2)),
            RSE = summary(model)$sigma,
            MAE = mean(abs(summary(model)$residuals)),
            MAPE = mean(abs(summary(model)$residuals/y)) * 100)
    }

    return(list(
        fitted = fitted,
        coefs = coefs,
        fit_criteria = fit_criteria))
}
