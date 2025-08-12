#' Fit Kinetics
#'
#' Perform parametric curve fitting or non-parametric estimation of mNIRS kinetics
#' on vector data.
#'
#' @param y A numeric vector of the response variable, or the name of the
#'  variable within a dataframe.
#' @param x An *optional* numeric vector of the predictor variable, or the name
#'  of the variable within a dataframe. If `x = NULL`, uses `x = seq_along(y)`.
#' @param data An *optional* dataframe containing the predictor and response
#'  variables named in `x` and `y`. Names for `x` and `y` must be in quotations.
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
#' Model Parameterisation
#'
#' "monoexponential": A four-parameter monoexponential association function
#'  in the form: `ifelse(x <= TD, A, A + (B - A) * (1 - exp((TD - x) / tau)))`.
#'  - `A`: text
#'  - `B`: text
#'  - `TD`: text
#'  - `tau`: text
#'  - `MRT`: text
#'
#' "sigmoidal": A four-parameter generalised logistic (sigmoidal) function
#'  in the form: `A + (B - A) / (1 + exp((xmid - x) / scal))`.
#'  - `A`: text
#'  - `B`: text
#'  - `xmid`: text
#'  - `scal`: text
#'
#' "half_time": A non-parametric estimate of the time to recover half of
#'  the total reoxygenation amplitude.
#'  - `A`: text
#'  - `B`: text
#'  - `half_value`: text
#'
#' "peak_slope": A non-parametric estimate of the time to reach the peak
#'  rolling linear regression slope within a window defined by `width`.
#'  - `A`: text
#'  - `B`: text
#'  - `x_fitted`: text
#'  - `peak_slope`: text
#'
#' @seealso [stats::nls()], [stats::SSasymp()], [stats::SSfpl()],
#'
#' @return A list `L` of class `mNIRS.kinetics` with components `L$...`:
#'      \item{`method`}{The kinetics method used.}
#'      \item{`model`}{The model object.}
#'      \item{`equation`}{The equation of the kinetics model used.}
#'      \item{`data`}{A dataframe of original and fitted model data.}
#'      \item{`fitted`}{A vector of fitted values returned by the model.}
#'      \item{`residuals`}{A vector of residuals between original and fitted
#'          values returned by the model.}
#'      \item{`x0`}{The value of the predictor variable indicating the start of
#'          kinetics.}
#'      \item{`coefs`}{A dataframe of model coefficients, including manually
#'      fixed parameters.}
#'      \item{`diagnostics`}{A dataframe of model goodness-of-fit metrics
#'      (`AIC`, `BIC`, `R2adj` `RMSE`, `MAE`, `MAPE`).}
#'      \item{`call`}{The model call.}
#'
#' @examples
#' set.seed(13)
#' x1 <- seq(-10, 60, by = 2)
#' A <- 10; B <- 100; TD <- 5; tau <- 12
#' y1 <- monoexponential(x1, A, B, TD, tau) + rnorm(length(x1), 0, 3)
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
## generic function =====================================
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

    y <- as_name(enquo(y))

    class(y) <- method

    UseMethod("process_kinetics", y)
}

## TODO 2025-07-19 fix fitted models up to first peak with no greater peaks within X samples
## TODO 2025-07-19 fix half-time to use neighbouring median around peak value B




#' @rdname process_kinetics
#' @export
## monoexponential =====================================
process_kinetics.monoexponential <- function(
        y,
        x = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    method <- match.arg(method)
    args <- list(...)

    intake <- pre_process_kinetics_names(y, x, data, x0, verbose)
    df <- intake$df
    x <- intake$df$x
    y <- intake$df$y
    data <- intake$data
    x_name <- names(intake$data)[1]
    y_name <- names(intake$data)[2]
    fitted_name <- paste0(y_name, "_fitted")

    ## custom list of permissable fixed coefs
    fixed_coefs <- unlist(args[names(args) %in% c("A", "B", "TD")])

    ## create the model and update for any fixed coefs
    model <- tryCatch(
        nls(y ~ SSmonoexp(x, A, B, TD, tau),
            data = df,
            na.action = na.exclude) |>
            update_fixed_coefs(...), ## TODO 2025-08-11 supply from fixed_coefs
        error = function(e) {
            cat("Error in nls(", y_name, " ~ SSmonoexp(", x_name,
                ", A, B, TD, tau)) : ", e$message, "\n", sep = "")
            NA})

    ## process coefs, diagnostics, fitted
    model_output <- process_model(model)
    data[[fitted_name]] <- model_output$fitted
    ## include explicitly defined coefs
    coefs <- c(fixed_coefs, model_output$coefs) |>
        (\(.x) .x[match(names(formals(SSmonoexp)), names(.x))] )() |>
        (\(.x) .x[!is.na(.x)])() |>
        (\(.x) as_tibble(as.list(.x)))()

    ## calculate MRT
    coefs$MRT <- coefs$TD + coefs$tau
    ## predict value for y at MRT x value
    coefs[[paste0("MRT_", y_name)]] <- predict(model, tibble(x = coefs$MRT))

    equation <- paste("y ~ A + (B - A) * (1 - exp((TD - x) / tau))")
    diagnostics <- as_tibble(as.list(model_output$diagnostics))

    out <- structure(
        list(
            method = method,
            model = model,
            equation = equation,
            data = data,
            fitted = model_output$fitted,
            residuals = model_output$residuals,
            x0 = x0,
            coefs = coefs,
            diagnostics = diagnostics,
            call = match.call()),
        class = "mNIRS.kinetics")

    return(out)
}




#' @rdname process_kinetics
#' @export
## sigmoidal =====================================
process_kinetics.sigmoidal <- function(
        y,
        x = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    method <- match.arg(method)
    args <- list(...)

    intake <- pre_process_kinetics_names(y, x, data, x0, verbose)
    df <- intake$df
    x <- intake$df$x
    y <- intake$df$y
    data <- intake$data
    x_name <- names(intake$data)[1]
    y_name <- names(intake$data)[2]
    fitted_name <- paste0(y_name, "_fitted")

    ## custom list of permissable fixed coefs
    fixed_coefs <- unlist(args[names(args) %in% c("A", "B")])

    ## create the model and update for any fixed coefs
    model <- tryCatch(
        nls(y ~ SSfpl(x, A, B, xmid, scal),
            data = df,
            na.action = na.exclude) |>
            update_fixed_coefs(...), ## TODO 2025-08-11 supply from fixed_coefs
        error = function(e) {
            cat("Error in nls(", y_name, " ~ SSfpl(", x_name,
                ", A, B, xmid, scal)) : ", e$message, "\n", sep = "")
            NA})

    ## process coefs, diagnostics, fitted
    model_output <- process_model(model)
    data[[fitted_name]] <- model_output$fitted
    ## include explicitly defined coefs
    coefs <- c(fixed_coefs, model_output$coefs) |>
        (\(.x) .x[match(names(formals(SSfpl)), names(.x))] )() |>
        (\(.x) .x[!is.na(.x)])() |>
        (\(.x) as_tibble(as.list(.x)))()

    ## predict value for y at xmid
    coefs[[paste0("xmid_", y_name)]] <- predict(model, tibble(x = coefs$xmid))

    equation <- paste("y ~ A + (B - A) / (1 + exp((xmid - x) / scal))")
    diagnostics <- as_tibble(as.list(model_output$diagnostics))

    out <- structure(
        list(
            method = method,
            model = model,
            equation = equation,
            data = data,
            fitted = model_output$fitted,
            residuals = model_output$residuals,
            x0 = x0,
            coefs = coefs,
            diagnostics = diagnostics,
            call = match.call()),
        class = "mNIRS.kinetics")

    return(out)
}




#' @rdname process_kinetics
#' @export
## half_time =====================================
process_kinetics.half_time <- function(
        y,
        x = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    method <- match.arg(method)
    args <- list(...)

    intake <- pre_process_kinetics_names(y, x, data, x0, verbose)
    df <- intake$df
    x <- intake$df$x
    y <- intake$df$y
    data <- intake$data
    x_name <- names(intake$data)[1]
    y_name <- names(intake$data)[2]

    ## custom list of permissable fixed coefs
    ## TODO 2025-08-11 allow for fixing A & B in half_time
    fixed_coefs <- unlist(args[names(args) %in% c("A", "B")])

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
                      paste0("half_", y_name, "_value"))
    coefs <- as_tibble(as.list(coefs))

    equation <- paste0("half_", x_name, " ~ interp_x(y = A + (B - A) / 2)")

    out <- structure(
        list(
            method = method,
            equation = equation,
            data = data,
            x0 = x0,
            coefs = coefs,
            call = match.call()),
        class = "mNIRS.kinetics")

    return(out)
}




#' @rdname process_kinetics
#' @export
## peak_slope =====================================
process_kinetics.peak_slope <- function(
        y,
        x = NULL,
        data = NULL,
        x0 = 0,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    method <- match.arg(method)
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

    intake <- pre_process_kinetics_names(y, x, data, x0, verbose)
    df <- intake$df
    x <- intake$df$x
    y <- intake$df$y
    data <- intake$data
    x_name <- names(intake$data)[1]
    y_name <- names(intake$data)[2]
    fitted_name <- paste0(y_name, "_fitted")

    rolling_slopes <- rolling_slope(y, x, width, align, na.rm = TRUE)
    peak_slope <- peak_directional_slope(y, x, width, align, na.rm = TRUE)

    data[[fitted_name]] <- NA_real_
    data[[fitted_name]][x %in% peak_slope$x_fitted] <- peak_slope$y_fitted
    residuals <- c(na.omit(data[[2]] - data[[fitted_name]]))
    coefs <- c(peak_slope$x[1],
               y[x %in% peak_slope$x][1],
               data[[fitted_name]][x %in% peak_slope$x][1],
               peak_slope$slope)
    names(coefs) <- c(x_name, y_name, fitted_name, "peak_slope")
    coefs <- as_tibble(as.list(coefs))

    equation <- paste0("peak_slope ~ max(rolling_slope(y ~ x, width = ", width, "))")

    out <- structure(
        list(
            method = method,
            equation = equation,
            data = data,
            fitted = peak_slope$y_fitted,
            residuals = residuals,
            rolling_slopes = rolling_slopes,
            x0 = x0,
            width = width,
            align = align,
            coefs = coefs,
            call = match.call()),
        class = "mNIRS.kinetics")

    return(out)
}






#' @keywords internal
## TODO 2025-08-11 new version not entirely working for unquotted data colnames
## TODO 2025-08-11 obj `y` is being substituted and enquosed as `y` instead of
## whatever `y = y1` was assigned to in the outer function. Therefore,
## `y_name = "y"` is not being found in the supplied dataframe ¯\_(ツ)_/¯
## but rlang::as_name(y) properly recognises `y = yy`, it just doesn't
## recognise `yy` as an environment object????
pre_process_kinetics_names <- function(y, x, data, x0, verbose = TRUE) {
    if (is.null(data)) {
        y_exp <- substitute(y) ## symbol from unquoted object name of y
        y_name <- deparse(y_exp) ## quoted string name of y

        x_exp <- if (!is.null(x)) {substitute(x)} else {substitute(index)}
        x_name <- if (!is.null(x)) {deparse(x_exp)} else {"index"}

        if (!is.numeric(y)) {
            cli::cli_abort("{.arg y = {y_name}} must be a {.cls numeric} vector.")
        } else if (is.null(x)) {
            x <- seq_along(y)
        } else if (!is.numeric(x)) {
            cli::cli_abort("{.arg x = {x_name}} must be a {.cls numeric} vector.")
        } else if (length(x) != length(y)) {
            cli::cli_abort(paste(
                "{.arg x = {x_name}} and {.arg y = {y_name}}",
                "must have the same length."))
        }

        data <- tibble(x, y)
        names(data) <- c(x_name, y_name)
    } else if (!is.data.frame(data)) {
        cli::cli_abort("{.arg data} must be a dataframe.")
    } else if (is.data.frame(data)) {
        y_exp <- substitute(y) ## symbol from unquoted object name of y
        ## TODO 2025-08-11 UNQUOTED WITH DATA IS FAILING HERE
        y_name <- as_name(y) ## works for quoted, object not found error for sym
        # y_name <- tryCatch(
        #     as_name(y), ## works for quoted, object not found error for sym
        #     error = \(e) {
        #         ## for unquoted "object not found" error:
        #         if (grepl("object", e$message)) {
        #             ## deparse() works for unquoted
        #             gsub('^"|"$', '', deparse(y_exp)) ## quoted object name of y
        #         }})

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
            x_name <- as_name(x) ## works for quoted, object not found error for sym
            # x_name <- tryCatch(
            #     as_name(x), ## works for quoted, object not found error for sym
            #     error = \(e) {
            #         ## for unquoted "object not found" error:
            #         if (grepl("object", e$message)) {
            #             ## deparse() works for unquoted
            #             gsub('^"|"$', '', deparse(x_exp)) ## quoted object name of x
            #         }})

            if (x_name %in% names(data)) {
                x <- data[[x_name]]
            } else {
                cli::cli_abort("{.arg x = {x_name}} not found in {.arg data}.")
            }
        }

        data <- data[c(x_name, y_name)]
    }

    x <- x - x0
    df <- tibble(x, y)

    return(list(
        data = data,
        df = df))
}



##old version corrected for x & y swap
# pre_process_kinetics_names <- function(y, x, data, x0) {
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
process_model <- function(model) {
    if (is.na(model[1])) {
        fitted <- NA_real_
        residuals <- NA_real_
        coefs <- c(A = NA_real_, B = NA_real_, TD = NA_real_,
                   tau = NA_real_, MRT = NA_real_,
                   xmid = NA_real_, scal = NA_real_)
        diagnostics <- c(
            AIC = NA_real_, BIC = NA_real_,
            R2adj = NA_real_, RMSE = NA_real_,
            MAE = NA_real_, MAPE = NA_real_)
    } else {
        y <- model$m$getEnv()$y
        fitted <- c(fitted(model))
        residuals <- c(residuals(model))
        ## MSE & RMSE https://stackoverflow.com/a/43123619/15389854
        RSS <- c(crossprod(residuals))
        n <- length(residuals)
        total_var <- c(crossprod(y - mean(y, na.rm = TRUE)))
        coefs <- coef(model)

        diagnostics <- c(
            AIC = AIC(model),
            BIC = BIC(model),
            ## see ?performance::r2 & ?performance::r2_efron
            # R2 = 1 - RSS / total_var,
            ## see https://stats.stackexchange.com/a/584551
            R2adj = 1 - ((RSS / (n - length(coefs))) / ((total_var / (n - 1)))),
            RMSE = sqrt(RSS / n),
            MAE = mean(abs(residuals)),
            MAPE = mean(abs(residuals / y), na.rm = TRUE) * 100)
    }

    return(list(
        fitted = fitted,
        residuals = residuals,
        coefs = coefs,
        diagnostics = diagnostics))
}
