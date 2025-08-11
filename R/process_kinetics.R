#' Fit Kinetics
#'
#' Process parametric curve fitting or non-parametric estimation of mNIRS kinetics
#' on vector data.
#'
#' @param y A numeric vector of the response variable, or the name of the variable.
#' @param x An *optional* numeric vector of the predictor variable, or the name
#'  of the variable. If `x = NULL`, uses `x = seq_along(y)`.
#' @param data An *optional* dataframe containing the predictor and response
#'  variables named in `x` and `y`.
#' @param x0 A numeric scalar indicating the value of the predictor variable `x`
#'  representing the start of the kinetics event (*default `x0 = 0`*).
#' @param method Indicates which model to evaluate the kinetics event
#'  (see *Details* for method parametrisation).
#'  \describe{
#'      \item{`method = "monoexponential"`}{A four-parameter monoexponential
#'          association function in the form:
#'          `ifelse(x <= TD, A, A + (B - A) * (1 - exp((TD - x) / tau)))`.}
#'      \item{`method = "sigmoidal"`}{A four-parameter generalised logistic
#'          (sigmoidal) function in the form:
#'          `A + (B - A) / (1 + exp((xmid - x) / scal))`.}
#'      \item{`method = "half_time"`}{A non-parametric estimate of the time
#'          to recover half of the total reoxygenation amplitude.}
#'      \item{`method = "peak_slope"`}{A non-parametric estimate of the time
#'          to reach the peak rolling linear regression slope within a window
#'          defined by `width`.}
#'  }
#' @param verbose A logical. `TRUE` (*default*) will return warnings and
#' messages which can be used for data error checking. `FALSE` will silence these
#' messages. Errors will always be returned.
#' @param ... Additional arguments.
#'  \describe{
#'      \item{`width`}{A numeric scalar defining the window width (in units of
#'          the predictor variable `x`) for rolling slope calculations (only used
#'          for `method =` *`"peak_slope"`*).}
#'      \item{`align = c(`*`"center", "left", "right"`*`)`}{Specifies the window
#'          alignment of `width` as *"center"* (*the default*), *"left"*,
#'          or *"right"*. Where *"left"* is *forward looking*, and *"right"*
#'          is *backward looking* by the window `width` from the current
#'          observation (only used for `method =` *`"peak_slope"`*).}
#'      \item{*fixed parameters*}{Parameters (coefficients) of the parametric
#'          models (*"monoexponential"* and *"sigmoidal"*) can be defined
#'          a priori and fixed, to exclude them from the model fitting
#'          optimisation. e.g., `A = 10` will define the function
#'          `SSmonoexp(x, A = 10, B, TD, tau)`.}
#'  }
#'
#' @details
#' `method %in% c("monoexponential", "sigmoidal")` use [nls()][stats::nls()]
#' for nonlinear (weighted) least-squares estimates.
#'
#' @seealso [stats::nls()], [stats::SSasymp()], [stats::SSfpl()],
#'
#' @return A list `L` of class `mNIRS.kinetics` with components `L$...`:
#'      \item{`method`}{The kinetics method used.}
#'      \item{`model`}{The model object.}
#'      \item{`model_equation`}{The equation of the kinetics model used.}
#'      \item{`data`}{A dataframe of original and fitted model data.}
#'      \item{`fitted`}{A vector of fitted values returned by the model.}
#'      \item{`residuals`}{A vector of residuals between original and fitted
#'          values returned by the model.}
#'      \item{`x0`}{The value of the predictor variable indicating the start of
#'          kinetics.}
#'      \item{`coefs`}{A dataframe of model coefficients, including manually
#'      fixed parameters.}
#'      \item{`diagnostics`}{A dataframe of model goodness-of-fit metrics
#'      (`AIC`, `BIC`, `R2`, `RMSE`, `RSE`, `SNR`, `MAE`, `MAPE`).}
#'      \item{`call`}{The model call.}
#'
#' @examples
#' set.seed(13)
#' x1 <- seq(-10, 60, by = 2)
#' A <- 10; B <- 100; TD <- 5; tau <- 12
#' y1 <- monoexponential(x, A, B, TD, tau) + rnorm(length(x), 0, 3)
#'
#' ## monoexponential kinetics ===============================
#' model <- process_kinetics(y1, x1, method = "monoexponential")
#' model
#'
#' \dontrun{
#' ## add coefs & diagnostics text
#' coef_text <- paste(names(model$coefs), round(model$coefs, 1),
#'                    sep = " = ", collapse = "\n")
#' diag_text <- paste(names(model$diagnostics), round(model$diagnostics, 2),
#'                    sep = " = ", collapse = "\n")
#'
#' ## require(ggplot2)
#' plot(model) +
#'     ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
#'     ggplot2::geom_line(ggplot2::aes(y = model$residuals)) +
#'     ggplot2::annotate("text", x = 2, y = 100,
#'                       label = coef_text, size = 4, hjust = 0, vjust = 1) +
#'     ggplot2::annotate("text", x = 58, y = 0,
#'                       label = diag_text, size = 4, hjust = 1, vjust = -0.3)
#' }
#'
#' ## sigmoidal kinetics ===============================
#' model <- process_kinetics(y1, x1, method = "sigmoidal")
#' model
#'
#' \dontrun{
#' ## add coefs & diagnostics text
#' coef_text <- paste(names(model$coefs), round(model$coefs, 1),
#'                    sep = " = ", collapse = "\n")
#' diag_text <- paste(names(model$diagnostics), round(model$diagnostics, 2),
#'                    sep = " = ", collapse = "\n")
#'
#' ## require(ggplot2)
#' plot(model) +
#'     ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
#'     ggplot2::geom_line(ggplot2::aes(y = model$residuals)) +
#'     ggplot2::annotate("text", x = 2, y = 100,
#'                       label = coef_text, size = 4, hjust = 0, vjust = 1) +
#'     ggplot2::annotate("text", x = 58, y = 0,
#'                       label = diag_text, size = 4, hjust = 1, vjust = -0.3)
#' }
#'
#' ## half recovery time ===============================
#' model <- process_kinetics(y1, x1, method = "half_time")
#' model
#'
#' \dontrun{
#' ## add coefs & diagnostics text
#' coef_text <- paste(names(model$coefs), round(model$coefs, 1),
#'                    sep = " = ", collapse = "\n")
#'
#' ## require(ggplot2)
#' plot(model) +
#'     ggplot2::annotate("text", x = 2, y = 100,
#'                       label = coef_text, size = 4, hjust = 0, vjust = 1)
#' }
#'
#' ## peak slope ===============================
#' model <- process_kinetics(y1, x1, method = "peak_slope", width = 10)
#' model
#'
#' \dontrun{
#' ## add coefs & diagnostics text
#' coef_text <- paste(names(model$coefs), round(model$coefs, 1),
#'                    sep = " = ", collapse = "\n")
#'
#' ## require(ggplot2)
#' plot(model) +
#'     ggplot2::annotate("text", x = 2, y = 100,
#'                       label = coef_text, size = 4, hjust = 0, vjust = 1)
#' }
#'
#' @usage NULL
#' @rdname process_kinetics
#' @export
process_kinetics <- function(
        y,
        x = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    method <- match.arg(method)

    # y_name <- deparse(substitute(y))
    y <- as_name(enquo(y))

    class(y) <- method

    UseMethod("process_kinetics", y)
}

## TODO 2025-07-19 fix fitted models up to first peak with no greater peaks within X samples
## TODO 2025-07-19 fix half-time to use neighbouring median around peak value B




#' @rdname process_kinetics
#' @export
process_kinetics.monoexponential <- function(
        y,
        x = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    args <- list(...)

    # intake <- eval(substitute(process_names_for_kinetics(y, x, data, x0)))
    # intake <- eval(call("process_names_for_kinetics",
    #                     y = substitute(y),
    #                     x = substitute(x),
    #                     data = data, x0 = x0))
    intake <- process_names_for_kinetics(y, x, data, x0, verbose)
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

    ## process coefs, diagnostics, fitted
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
    coefs[[paste0("MRT_", y_name)]] <- predict(model, tibble::tibble(x = coefs$MRT))

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
            residuals = model_output$residuals,
            x0 = x0,
            coefs = coefs,
            diagnostics = model_output$diagnostics,
            call = return_call),
        class = "mNIRS.kinetics")

    return(out)
}




#' @rdname process_kinetics
#' @export
process_kinetics.sigmoidal <- function(
        y,
        x = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    args <- list(...)

    # intake <- eval(substitute(process_names_for_kinetics(y, x, data, x0)))
    intake <- process_names_for_kinetics(y, x, data, x0, verbose)
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
        nls(y ~ stats::SSfpl(x, A, B, xmid, scal),
            data = df,
            na.action = na.exclude) |>
            update_fixed_coefs(...),
        error = function(e) {
            cat("Error in nls(", y_exp, " ~ SSfpl(", x_exp,
                ", A, B, xmid, scal)) : ", e$message, "\n", sep = "")
            NA})

    ## process coefs, diagnostics, fitted
    model_output <- process_model(model)
    data[[fitted_name]] <- model_output$fitted
    ## include explicitly defined coefs
    coefs <- c(..., model_output$coefs)
    coefs <- coefs[match(c("A", "B", "xmid", "scal"), names(coefs))]
    ## convert named vector to dataframe
    coefs <- tibble::as_tibble(as.list(coefs))
    ## predict value for y at xmid
    coefs[[paste0("xmid_", y_name)]] <- predict(model, tibble::tibble(x = coefs$xmid))

    ## save call
    return_call <- match.call()
    model_equation <- as.formula(y ~ A + (B - A) / (1 + exp((xmid - x) / scal)))
    # return_call$model_equation <- list(call = list(formula = model_equation))

    out <- structure(
        list(
            method = "sigmoidal",
            model = model,
            model_equation = model_equation,
            data = data,
            fitted = model_output$fitted,
            residuals = model_output$residuals,
            x0 = x0,
            coefs = coefs,
            diagnostics = model_output$diagnostics,
            call = return_call),
        class = "mNIRS.kinetics")

    return(out)
}




#' @rdname process_kinetics
#' @export
process_kinetics.half_time <- function(
        y,
        x = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    args <- list(...)

    # intake <- eval(substitute(process_names_for_kinetics(y, x, data, x0)))
    intake <- process_names_for_kinetics(y, x, data, x0, verbose)
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




#' @rdname process_kinetics
#' @export
process_kinetics.peak_slope <- function(
        y,
        x = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    args <- list(...)
    if ("width" %in% names(args)) {
        width <- args$width
    }

    align_choices <- c("center", "left", "right")
    if ("align" %in% names(args)) {
        align <- match.arg(args$align, choices = align_choices)
    } else {
        align <- match.arg("center", choices = align_choices)
    }

    # intake <- eval(substitute(process_names_for_kinetics(y, x, data, x0)))
    intake <- process_names_for_kinetics(y, x, data, x0, verbose)
    data <- intake$data
    df <- intake$df
    x <- intake$x
    x_exp <- intake$x_exp
    x_name <- intake$x_name
    y <- intake$y
    y_exp <- intake$y_exp
    y_name <- intake$y_name
    fitted_name <- paste0(y_name, "_fitted")

    slopes <- rolling_slope(y, x, width, align, na.rm = TRUE)
    peak_slope <- peak_directional_slope(y, x, width, align, na.rm = TRUE)

    data[[fitted_name]] <- NA_real_
    data[[fitted_name]][x %in% peak_slope$x_fitted] <- peak_slope$y_fitted
    coefs <- c(peak_slope$x[1],
               y[x %in% peak_slope$x][1],
               data[[fitted_name]][x %in% peak_slope$x][1],
               peak_slope$slope)
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
## new version not entirely working...
process_names_for_kinetics <- function(y, x, data, x0, verbose = TRUE) {
    if (is.null(data)) {
        if (!is.numeric(y)) {
            cli::cli_abort("{.arg y} must be a {.cls numeric} vector.")
        } else if (is.null(x)) {
            x_exp <- substitute(index) ## for x = NULL, name as "index"
            x_name <- "index"
            y_exp <- substitute(y) ## symbol from unquoted object name of y
            y_name <- deparse(y_exp) ## quoted string name of y
            y <- y
            x <- seq_along(y)
        } else if (!is.numeric(x)) {
            cli::cli_abort("{.arg x} must be a {.cls numeric} vector.")
        } else if (length(x) != length(y)) {
            cli::cli_abort("{.arg x} and {.arg y} must have the same length.")
        } else {
            x_exp <- substitute(x) ## symbol from unquoted object name of x
            x_name <- deparse(x_exp) ## quoted string name of x
            y_exp <- substitute(y) ## symbol from unquoted object name of y
            y_name <- deparse(y_exp) ## quoted string name of y
            y <- y
            x <- x
        }

        data <- tibble::tibble(x, y)
        names(data) <- c(x_name, y_name)
    } else if (!is.data.frame(data)) {
        ## data must be a dataframe
        cli::cli_abort("{.arg data} must be a dataframe.")
    } else if (is.data.frame(data)) {
        y_exp <- substitute(y) ## symbol from unquoted object name of y
        # y_name <- as_name(y_exp) ## quoted string name of y
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
            cli::cli_abort("{.arg y = {y_name}} not found in {.arg data}.")
        }

        if (is.null(x)) {
            x_exp <- substitute(index) ## for x = NULL, name as "index"
            x_name <- "index"
            x <- seq_along(y)
            data[[x_name]] <- x
            if (verbose) {
                cli::cli_alert_info("{.arg x = {x_name}} added to {.arg data}.")
            }
        } else {
            x_exp <- substitute(x) ## symbol from unquoted object name of x
            # x_name <- as_name(x_exp) ## quoted string name of x
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
                cli::cli_abort("{.arg x = {x_name}} not found in {.arg data}.")
            }
        }

        data <- data[c(x_name, y_name)]
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

## tried this inside the generic function. doesn't like that
# y_quo <- enquo(y)
# x_quo <- enquo(x)
# data_quo <- enquo(data)
#
# y_name <- as_name(y_quo)
# x_name <- if (!quo_is_null(x_quo)) {as_name(x_quo)} else {"index"}
# data_name <- if (!quo_is_null(data_quo)) {as_name(data_quo)} else {NULL}
#
# if (is.null(data)) {
#     if (!is.numeric(y)) {
#         cli::cli_abort("{.arg y = {y_name}} must be a {.cls numeric} vector.")
#     } else {
#         y <- eval_tidy(y_quo)
#     }
#
#     if (is.null(x)) {
#         x <- seq_along(y)
#     } else if (!is.numeric(x)) {
#         cli::cli_abort("{.arg x = {x_name}} must be a {.cls numeric} vector.")
#     } else if (length(x) != length(y)) {
#         cli::cli_abort(paste(
#             "{.arg x = {x_name}} and {.arg y = {y_name}}",
#             "must have the same length."))
#     } else {
#         x <- eval_tidy(x_quo)
#     }
#
#     data <- tibble(!!x_name := x, !!y_name := y)
#     data_name <- "data"
# } else if (!is.data.frame(data)) {
#     cli::cli_abort("{.arg data = {data_name}} must be a dataframe.")
# } else if (!has_name(data, y_name)) {
#     cli::cli_abort("{.arg y = {y_name}} not found in {.arg data = {data_name}}.")
# } else { ## is.data.frame & has_name
#     y <- data[[y_name]]
#
#     if (quo_is_null(x_quo)) {
#         x <- seq_along(y)
#         data[[x_name]] <- x
#         if (verbose) {
#             cli::cli_alert_info(
#                 "{.arg x = {x_name}} added to {.arg data = {data_name}}.")
#         }
#     } else if (!has_name(data, x_name)) {
#         cli::cli_abort("{.arg x = {x_name}} not found in {.arg data = {data_name}}.")
#     } else {
#         x <- data[[x_name]]
#     }
#
#     data <- data[c(x_name, y_name)]
# }
#
# x <- x - x0
# df <- tibble::tibble(x, y)




##old version corrected for x & y swap
# process_names_for_kinetics <- function(y, x, data, x0) {
#     if (!is.null(data) & !is.data.frame(data)) {
#         ## data must be a dataframe
#         cli::cli_abort("{.arg data} must be a dataframe")
#
#     } else if (!is.null(data) & is.data.frame(data)) {
#         ## deparse(substitute()) works for unquoted
#         y_exp <- substitute(y) ## symbol from unquoted object name of y
#         y_name <- tryCatch(
#             rlang::as_name(y), ## as_name() works for quoted
#             error = \(e) {
#                 ## for unquoted "object not found" error:
#                 if (grepl("object", e$message)) {
#                     ## deparse() works for unquoted
#                     gsub('^"|"$', '', deparse(y_exp)) ## quoted object name of y
#                 }})
#
#         if (y_name %in% names(data)) {
#             y <- data[[y_name]]
#         } else {
#             cli::cli_abort("{.arg y} not found in {.arg data}")
#         }
#
#         if (is.null(x) | missing(x)) {
#             x_exp <- substitute(index)
#             x_name <- "index"
#             x <- seq_along(y)
#
#             data[[x_name]] <- x
#
#         } else {
#             ## deparse(substitute()) works for unquoted
#             x_exp <- substitute(x) ## symbol from unquoted object name of x
#             x_name <- tryCatch(
#                 rlang::as_name(x), ## as_name() works for quoted
#                 error = \(e) {
#                     ## for unquoted "object not found" error:
#                     if (grepl("object", e$message)) {
#                         ## deparse() works for unquoted
#                         gsub('^"|"$', '', deparse(x_exp)) ## quoted object name of x
#                     }})
#
#             if (x_name %in% names(data)) {
#                 x <- data[[x_name]]
#             } else {
#                 cli::cli_abort("{.arg x} not found in {.arg data}")
#             }
#         }
#
#         data <- data[c(x_name, y_name)]
#
#     } else if (is.null(data)) {
#         if (is.null(x)) {
#             y_exp <- substitute(y) ## symbol from unquoted object name of x
#             y_name <- deparse(y_exp) ## quoted object name of x
#             x_exp <- substitute(index)
#             x_name <- "index"
#             y <- y
#             x <- seq_along(y)
#         } else {
#             x_exp <- substitute(x) ## symbol from unquoted object name of x
#             x_name <- deparse(x_exp) ## quoted object name of x
#             y_exp <- substitute(y) ## symbol from unquoted object name of y
#             y_name <- deparse(y_exp) ## quoted object name of y
#             x <- x
#             y <- y
#         }
#
#         data <- tibble::tibble(x, y)
#         names(data) <- c(x_name, y_name)
#     }
#
#     x <- x - x0
#     df <- tibble::tibble(x, y)
#
#     return(list(
#         data = data,
#         df = df,
#         x = x,
#         x_exp = x_exp,
#         x_name = x_name,
#         y = y,
#         y_exp = y_exp,
#         y_name = y_name))
# }




#' @keywords internal
process_model <- function(
        model
) {
    if (is.na(model[1])) {
        fitted <- NA_real_
        residuals <- NA_real_
        coefs <- tibble::tibble(A = NA_real_, B = NA_real_, TD = NA_real_,
                                tau = NA_real_, MRT = NA_real_,
                                xmid = NA_real_, scal = NA_real_)
        diagnostics <- tibble::tibble(
            AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
            RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)
    } else {
        y <- model$m$getEnv()$y
        fitted <- as.vector(fitted(model))
        residuals <- as.vector(residuals(model))
        coefs <- coef(model)
        diagnostics <- tibble::tibble(
            AIC = stats::AIC(model),
            BIC = stats::BIC(model),
            R2 = 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2),
            RMSE = sqrt(mean(summary(model)$residuals^2)),
            RSE = summary(model)$sigma,
            SNR = diff(range(fitted, na.rm = TRUE)) / RSE,
            MAE = mean(abs(summary(model)$residuals)),
            MAPE = mean(abs(summary(model)$residuals/y)) * 100)
    }

    return(list(
        fitted = fitted,
        residuals = residuals,
        coefs = coefs,
        diagnostics = diagnostics))
}
