#' Fit Kinetics
#'
#' Perform parametric curve fitting or non-parametric estimation of mNIRS kinetics
#' on vector data.
#'
#' @param y A numeric vector of the response variable, or the name of the
#'  variable within a dataframe.
#' @param x An *optional* numeric vector of the predictor variable, or the name
#'  of the variable within a dataframe. *Defaults* to using the index of
#'  `x = seq_along(y)`.
#' @param data An *optional* dataframe containing the predictor and response
#'  variables named in `x` and `y`. Names for `x` and `y` must be in quotations.
#' @param x0 (*Default = 0*) A numeric scalar indicating the value of the predictor variable `x`
#'  representing the start of the kinetics event.
#' @param window (*Default = 30*) A numeric scalar indicating the local window in units of
#'  the predictor variable `x` after the kinetics extreme (peak or trough) value
#'  to look for subsequent greater extremes. The kinetics model will be fit to the
#'  data up to the first local extreme with no subsequent greater extremes within
#'  the lesser of either the `window` or the limits of the data.
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
#' @return A list of class `mNIRS.kinetics` with components `L$...`:
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
#' ## require(ggplot2)
#' plot(model, plot_coefs = TRUE, plot_diagnostics = TRUE, plot_residuals = TRUE)
#' }
#'
#' ## sigmoidal kinetics ===============================
#' model <- process_kinetics(y1, x1, method = "sigmoidal")
#' model
#'
#' \dontrun{
#' ## require(ggplot2)
#' plot(model, plot_coefs = TRUE, plot_diagnostics = TRUE, plot_residuals = TRUE)
#' }
#'
#' ## half recovery time ===============================
#' model <- process_kinetics(y1, x1, method = "half_time")
#' model
#'
#' \dontrun{
#' ## require(ggplot2)
#' plot(model, plot_coefs = TRUE, plot_diagnostics = TRUE, plot_residuals = TRUE)
#' }
#'
#' ## peak slope ===============================
#' model <- process_kinetics(y1, x1, method = "peak_slope", width = 10)
#' model
#'
#' \dontrun{
#' ## require(ggplot2)
#' plot(model, plot_coefs = TRUE, plot_diagnostics = TRUE, plot_residuals = TRUE)
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
        window = 30,
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
        window = 30,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    method <- match.arg(method)
    args <- list(...)

    intake <- pre_process_kinetics_names(y = y, x = x, data = data,
                                         x0 = x0, verbose = verbose)
    df <- intake$df
    data <- intake$data
    x_name <- names(data)[1]
    y_name <- names(data)[2]
    extreme <- setNames(intake$extreme, c(x_name, y_name))
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
    ## add fitted data to original data, NA where excluded after first extreme
    data[data[[x_name]] %in% df$x, fitted_name] <- model_output$fitted
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
            extreme = extreme,
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
        window = 30,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    method <- match.arg(method)
    args <- list(...)

    intake <- pre_process_kinetics_names(y = y, x = x, data = data,
                                         x0 = x0, verbose = verbose)
    df <- intake$df
    data <- intake$data
    x_name <- names(data)[1]
    y_name <- names(data)[2]
    extreme <- setNames(intake$extreme, c(x_name, y_name))
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
    ## add fitted data to original data, NA where excluded after first extreme
    data[data[[x_name]] %in% df$x, fitted_name] <- model_output$fitted
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
            extreme = extreme,
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
        window = 30,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    method <- match.arg(method)
    args <- list(...)

    intake <- pre_process_kinetics_names(y = y, x = x, data = data,
                                         x0 = x0, verbose = verbose)
    df <- intake$df
    x <- intake$df$x
    y <- intake$df$y
    data <- intake$data
    x_name <- names(data)[1]
    y_name <- names(data)[2]
    extreme <- setNames(intake$extreme, c(x_name, y_name))

    ## custom list of permissable fixed coefs
    ## TODO 2025-08-11 allow for fixing A & B in half_time
    fixed_coefs <- unlist(args[names(args) %in% c("A", "B")])

    ## detect trend direction positive = TRUE, negative = FALSE
    trend <- slope(y, x) >= 0

    ## set A_x = 0 (adjusted for x0)
    A_x <- ifelse(all(x > 0), x[1], 0)
    ## find mean baseline x < 0
    A <- mean(y[ifelse(all(x > 0), x[1], which(x <= 0))], na.rm = TRUE)
    ## set B to the previously found extreme value
    B_x <- extreme[[x_name]]
    B <- extreme[[y_name]]
    half_y <- A + (B - A)/2
    ## take the first x where y >= half_y
    half_x <- ifelse(trend, x[y >= half_y], x[y <= half_y])[1]
    ## take the first y that actually exists where y >= half_y
    half_nirs_value <- y[y >= half_y][1]

    coefs <- c(A_x, A, B_x, B, half_x, half_y, half_nirs_value)
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
            extreme = extreme,
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
        window = 30,
        method = c("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        verbose = TRUE,
        ...
) {
    method <- match.arg(method)
    args <- list(...)
    width <- args$width %||% width ## fails with object 'width' not found

    align_choices <- c("center", "left", "right")
    align <- match.arg(args$align %||% "center", choices = align_choices)

    intake <- pre_process_kinetics_names(y = y, x = x, data = data,
                                         x0 = x0, verbose = verbose)
    df <- intake$df
    x <- intake$df$x
    y <- intake$df$y
    data <- intake$data
    x_name <- names(data)[1]
    y_name <- names(data)[2]
    extreme <- setNames(intake$extreme, c(x_name, y_name))
    fitted_name <- paste0(y_name, "_fitted")

    rolling_slopes <- rolling_slope(y, x, width, align, na.rm = TRUE)
    peak_slope <- peak_slope(y, x, width, align, na.rm = TRUE)

    ## add fitted data to original data, NA beyond range of local slope
    data[[fitted_name]] <- NA_real_
    data[[fitted_name]][x %in% peak_slope$x_fitted] <- peak_slope$y_fitted
    ## residuals between y and y_fitted
    residuals <- c(na.omit(data[[y_name]] - data[[fitted_name]]))
    coefs <- c(peak_slope$x[1],
               y[x %in% peak_slope$x][1],
               peak_slope$y[1],
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
            extreme = extreme,
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
pre_process_kinetics_names <- function(
        y,
        x = NULL,
        data = NULL,
        x0 = 0,
        window = 30,
        verbose = TRUE
) {
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
                "must be the same length."))
        }

        data <- setNames(tibble(x, y), c(x_name, y_name))
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

    ## TODO 2025-08-12 implement extreme$x & extreme$y
    first_extreme <- find_first_extreme(y = y, x = x, window = window)
    x <- first_extreme$x
    y <- first_extreme$y
    df <- tibble(x, y)

    return(list(
        data = data,
        df = df,
        extreme = first_extreme$extreme))
}



#' @keywords internal
find_first_extreme <- function(y, x = NULL, window = 30) {
    if (is.null(x)) {x <- seq_along(y)} ## where `x` is not defined

    if (length(x) != length(y)) {
        cli::cli_abort("{.arg x} and {.arg} must be the same length.")
    }

    x <- round(x, 10); y <- round(y, 10) ## avoid floating point precision issues

    ## filter for positive x values
    positive_x <- x[x > 0]
    positive_y <- y[x > 0]

    ## find local extrema
    n <- length(positive_x)
    y_lag <- c(NA, positive_y[-n])
    y_lead <- c(positive_y[-1], NA)

    ## detect trend direction
    trend <- slope(positive_y, positive_x) >= 0

    is_extreme <- if (trend) {
        ## positive is TRUE, looking for maxima
        positive_y > y_lag & positive_y >= y_lead
    } else {
        ## negative is FALSE, looking for minima
        positive_y < y_lag & positive_y <= y_lead
    }

    is_extreme[1] <- FALSE ## remove boundary effect at start
    is_extreme[n] <- TRUE ## add boundary effect at end for monotonic
    extreme_indices <- which(is_extreme)

    if (length(extreme_indices) == 0) {
        ## this shouldn't happen because I've added end boundary effect
        cli::cli_abort("{.fn find_first_extreme} failed. No extrema found.")
    }

    ## check for greater extrema within each local window
    for (extreme_idx in extreme_indices) {
        extreme_x <- positive_x[extreme_idx]
        extreme_y <- positive_y[extreme_idx]

        # Find y values within window
        window_end <- extreme_x + window
        window_y <- y[x >= extreme_x & x <= window_end]

        ## check if this extreme is the greatest in the window
        is_dominant <- if (trend) {
            extreme_y >= max(window_y, na.rm = TRUE)
        } else {
            extreme_y <= min(window_y, na.rm = TRUE)
        }

        if (is_dominant) {
            ## subset x & y by is_extreme
            y <- y[x <= window_end]
            x <- x[x <= window_end]

            return(list(
                x = x,
                y = y,
                extreme = tibble(x = extreme_x, y = extreme_y)
            ))
        } ## end of if (is_dominant)
    } ## end of for loop

    ## no qualifying peak found.
    ## this shouldn't happen because I've added end boundary effect
    return(list(x = x, y = y, extreme = tibble(x = NA, y = NA)))
}



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
