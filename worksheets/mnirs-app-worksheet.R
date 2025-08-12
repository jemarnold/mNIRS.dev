## Setup ==========================================================
# suppressPackageStartupMessages({
#     # library(JAPackage)
#     library(mNIRS)
#     library(tidyverse)
# })
devtools::load_all()

# options(digits = 5, digits.secs = 3, scipen = 3,
#         dplyr.summarise.inform = FALSE,
#         tibble.print_min = 20)

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
# upload_file <- r"(C:\R-Projects\mNIRS.dev\inst\extdata\train.red_interval_example.csv)"
upload_file <- r"(C:\R-Projects\mNIRS.dev\inst\extdata\oxysoft_interval_example.xlsx)"
# upload_file <- r"(C:\R-Projects\mNIRS.dev\inst\extdata\moxy_ramp_example.xlsx)"

data_raw <- read_data(
    file_path = upload_file,
    # nirs_columns = c(SmO2 = "SmO2 Live"),
    # sample_column = c(time = "hh:mm:ss"),
    # nirs_columns = c(smo2="SmO2", smo2_2="SmO2"),
    # sample_column = c(time = "Timestamp (seconds passed)"),
    nirs_columns = c(O2Hb = 5, HHb = 6),
    sample_column = c(sample = 1),
    event_column = c(event = 8)
) |>
    dplyr::mutate(
        # time = round(time - dplyr::first(time), 1),
        # time = sample/50,
    ) |>
    # slice_head(by = time, n = 1) |>
    downsample_data(
        # sample_column = "time",
        downsample_rate = 10
    ) |>
    print()

dplyr::filter(data_raw, !is.na(event))
# plot(data_raw)
(sample_column <- attributes(data_raw)$sample_column)
(fit_sample_name <- paste0("fit_", sample_column))

# event_sample <- c(370, 1085)
event_sample <- c(24675, 66670)
# event_sample <- 876




## TODO
## iterate over sub-dataframes / NIRS channels
## input (list item per event):
## - processed mNIRS.data
## - list of sub-dataframes
## - list of vectors of NIRS channels
## - list of vectors of kinetics methods
## - list of named vector (or list) of additional args (e.g. width)
## output list:
## - aggregate table of coefs
## - aggregate table of diagnostics
## - list of plots per event; lines (colours) per NIRS channel





data_list <- prepare_kinetics_data(
    data_raw,
    event_sample = event_sample,
    event_label = c("E7"),
    fit_window = c(30*50, 120*50)
) |>
    print()


model_list <- tidyr::expand_grid(
    .df = data_list,
    .nirs = attributes(data_raw)$nirs_columns,
    .method = "monoexp") |>
    purrr::pmap(
        \(.df, .nirs, .method)
        process_kinetics(y = .nirs,
                         x = fit_sample_name,
                         data = .df,
                         method = .method,
                         width = 10*50)
    ) |>
    print()

process_kinetics(y = "O2Hb", x = fit_sample_name, data_list[[1]])

## coef table =====================================
coef_data <- purrr::imap(
    model_list,
    \(.x, idx)
    tibble::as_tibble(as.list(c(.x$coefs))) |>
        dplyr::mutate(
            event = idx,
            channel = names(.x$data)[2],
            across(where(is.numeric), \(.x) round(.x, 2))
        ) |>
        dplyr::relocate(event, channel)
) |>
    purrr::list_rbind() |>
    print()


## kinetics plot ======================================
# model_list[[1]]$data

(sample_column <- attributes(data_raw)$sample_column)
(nirs_columns <- attributes(data_raw)$nirs_columns)
(nirs_fitted <- paste0(nirs_columns, "_fitted"))
(fit_sample <- paste0("fit_", sample_column))

display_data_list <- purrr::map(
    model_list,
    \(.x)
    .x$data |>
        dplyr::select(dplyr::matches(c(fit_sample, nirs_columns, "fitted")))
) |>
    (\(.l) split(.l, factor(names(.l), levels = unique(names(.l)))))() |>
    purrr::map(\(.l) purrr::reduce(.l, dplyr::full_join, by = fit_sample)) |>
    print()


display_method <- unique(unlist(lapply(model_list, \(.df) .df$method)))

(display_coefs <- coef_data[grepl(event_sample[1], coef_data$event),])

(display_data <- display_data_list[grepl(event_sample[1], names(display_data_list))][[1]])

ggplot(display_data) +
    aes(x = .data[[fit_sample]]) +
    theme_mNIRS(legend.position = "top") +
    scale_x_continuous(
        # name = deparse(sym(call$x)),
        breaks = if (rlang::is_installed("scales")) {
            scales::breaks_pretty(n = 8)
        } else {
            waiver()
        },
        expand = expansion(mult = 0.01)) +
    scale_y_continuous(
        # name = deparse(sym(call$y)),
        breaks = if (rlang::is_installed("scales")) {
            scales::breaks_pretty(n = 6)
        } else {
            waiver()
        },
        expand = expansion(mult = 0.01)) +
    scale_colour_manual(
        name = NULL,
        aesthetics = c("fill", "colour"),
        values = setNames(
            c(scales::hue_pal()(length(nirs_columns)), "black"),
            c(nirs_columns, "fitted")),
        limits = force) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    purrr::map(nirs_columns,
               \(.x) geom_line(aes(y = .data[[.x]], colour = .x),
                               linewidth = 1)) +
    {if (display_method == "monoexponential") {
        purrr::map(
            nirs_columns,
            \(.x) {
                coef_channel <- display_coefs[display_coefs$channel == .x,]
                nirs_fitted <- paste0(.x, "_fitted")
                MRT_nirs_value <- paste0("MRT_", .x)

                list(
                    geom_line(aes(y = .data[[nirs_fitted]], colour = "fitted"),
                              linewidth = 0.8),
                    geom_segment(
                        data = tibble::tibble(
                            x = coef_channel$MRT,
                            y = coef_channel[[MRT_nirs_value]]),
                        aes(x = x, xend = x, y = y, yend = -Inf),
                        arrow = arrow(), linewidth = 0.8),
                    geom_point(
                        data = tibble::tibble(
                            x = coef_channel$MRT,
                            y = coef_channel[[MRT_nirs_value]]),
                        aes(x = x, y = y, colour = "fitted"),
                        size = 4, shape = 21, stroke = 1)
                )
            })
    }} +
    {if (display_method == "half_time") {
        purrr::map(
            nirs_columns,
            \(.x) {
                coef_channel <- display_coefs[display_coefs$channel == .x,]

                A_name <- paste0("A_", fit_sample)
                B_name <- paste0("B_", fit_sample)
                half_name <- paste0("half_", fit_sample)

                list(
                    geom_segment(
                        data = tibble::tibble(
                            x = coef_channel[[half_name]],
                            y = coef_channel$half_value),
                        aes(x = x, xend = x, y = y, yend = -Inf),
                        arrow = arrow(), linewidth = 0.8),
                    geom_point(
                        data = tibble::tibble(
                            x = c(coef_channel[[A_name]],
                                  coef_channel[[B_name]],
                                  coef_channel[[half_name]]),
                            y = c(coef_channel$A,
                                  coef_channel$B,
                                  coef_channel$half_value)),
                        aes(x = x, y = y, colour = "fitted"),
                        size = 3, shape = 21, stroke = 1),
                    NULL)
            })
    }} +
    {if (display_method == "peak_slope") {
        purrr::map(
            nirs_columns,
            \(.x) {
                coef_channel <- display_coefs[display_coefs$channel == .x,]
                nirs_fitted <- paste0(.x, "_fitted")

                list(
                    geom_line(
                        aes(y = .data[[nirs_fitted]], colour = "fitted"),
                        linewidth = 0.8),
                    geom_segment(
                        data = tibble::tibble(
                            x = coef_channel[[fit_sample]],
                            y = coef_channel[[nirs_fitted]]),
                        aes(x = x, xend = x, y = y, yend = -Inf),
                        arrow = arrow(), linewidth = 0.8),
                    geom_point(
                        data = tibble::tibble(
                            x = coef_channel[[fit_sample]],
                            y = coef_channel[[nirs_fitted]]),
                        aes(x = x, y = y, colour = "fitted"),
                        size = 3, shape = 21, stroke = 1)
                )
            })
    }}

#
## process_kinetics example ===================================
set.seed(13)
x1 <- seq(-10, 60, by = 2)
A <- 10; B <- 100; TD <- 5; tau <- 12
y1 <- monoexponential(x1, A, B, TD, tau) + rnorm(length(x1), 0, 3)

## monoexponential kinetics ===============================
model <- process_kinetics(y1, x1, method = "monoexponential")
model

## add coefs & diagnostics text
coef_text <- paste(names(model$coefs), round(model$coefs, 1),
                   sep = " = ", collapse = "\n")
diag_text <- paste(names(model$diagnostics), round(model$diagnostics, 2),
                   sep = " = ", collapse = "\n")

# \dontrun{
## require(ggplot2)
plot(model) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes(y = model$residuals)) +
    ggplot2::annotate("text", x = 2, y = 100,
                      label = coef_text, size = 4, hjust = 0, vjust = 1) +
    ggplot2::annotate("text", x = 58, y = 0,
                      label = diag_text, size = 4, hjust = 1, vjust = -0.3)
# }

## sigmoidal kinetics ===============================
model <- process_kinetics(y1, x1, method = "sigmoidal")
model

## add coefs & diagnostics text
coef_text <- paste(names(model$coefs), round(model$coefs, 1),
                   sep = " = ", collapse = "\n")
diag_text <- paste(names(model$diagnostics), round(model$diagnostics, 2),
                   sep = " = ", collapse = "\n")

# \dontrun{
## require(ggplot2)
plot(model) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes(y = model$residuals)) +
    ggplot2::annotate("text", x = 2, y = 100,
                      label = coef_text, size = 4, hjust = 0, vjust = 1) +
    ggplot2::annotate("text", x = 58, y = 0,
                      label = diag_text, size = 4, hjust = 1, vjust = -0.3)
# }

## half recovery time ===============================
model <- process_kinetics(y1, x1, method = "half_time")
model

## add coefs & diagnostics text
coef_text <- paste(names(model$coefs), round(model$coefs, 1),
                   sep = " = ", collapse = "\n")

# \dontrun{
## require(ggplot2)
plot(model) +
    ggplot2::annotate("text", x = 2, y = 100,
                      label = coef_text, size = 4, hjust = 0, vjust = 1)
# }

## peak slope ===============================
model <- process_kinetics(y1, x1, method = "peak_slope", width = 10)
model

## add coefs & diagnostics text
coef_text <- paste(names(model$coefs), round(model$coefs, 1),
                   sep = " = ", collapse = "\n")

# \dontrun{
## require(ggplot2)
plot(model) +
    ggplot2::annotate("text", x = 2, y = 100,
                      label = coef_text, size = 4, hjust = 0, vjust = 1)
# }

## metaprogramming worksheet =======================================
library(rlang)

set.seed(13)
x1 <- seq(-10, 10, by = 2)
y1 <- 2 + 0.4 * x1 + 0.04 * x1^2 + rnorm(length(x1), 0, 3)
mydata <- tibble::tibble(xx = x1/2, yy = y1)

myfun <- function(y, x = NULL, data = NULL) {
    y_quo <- enquo(y)
    x_quo <- enquo(x)
    data_quo <- enquo(data)

    y_name <- as_name(y_quo)
    x_name <- if (!quo_is_null(x_quo)) {as_name(x_quo)} else {"index"}
    data_name <- if (!quo_is_null(data_quo)) {as_name(data_quo)} else {NULL}

    if (is.null(data)) {
        if (!is.numeric(y)) {
            cli::cli_abort("{.arg y = {y_name}} must be a {.cls numeric} vector.")
        } else {
            y <- eval_tidy(y_quo)
        }

        if (is.null(x)) {
            x <- seq_along(y)
        } else if (!is.numeric(x)) {
            cli::cli_abort("{.arg x = {x_name}} must be a {.cls numeric} vector.")
        } else if (length(x) != length(y)) {
            cli::cli_abort(paste(
                "{.arg x = {x_name}} and {.arg y = {y_name}}",
                "must have the same length."))
        } else {
            x <- eval_tidy(x_quo)
        }

        data <- tibble(!!x_name := x, !!y_name := y)
        data_name <- "data"
    } else if (!is.data.frame(data)) {
        cli::cli_abort("{.arg data = {data_name}} must be a dataframe.")
    } else if (!has_name(data, y_name)) {
        cli::cli_abort("{.arg y = {y_name}} not found in {.arg data = {data_name}}.")
    } else { ## is.data.frame & has_name
        y <- data[[y_name]]

        if (quo_is_null(x_quo)) {
            x <- seq_along(y)
            data[[x_name]] <- x
            # if (verbose) {
                cli::cli_alert_info(
                    "{.arg x = {x_name}} added to {.arg data = {data_name}}.")
            # }
        } else if (!has_name(data, x_name)) {
            cli::cli_abort("{.arg x = {x_name}} not found in {.arg data = {data_name}}.")
        } else {
            x <- data[[x_name]]
        }

        data <- data[c(x_name, y_name)]
    }

    # x <- x - x0
    df <- tibble::tibble(x, y)

    return(rlang::list2(
        !!data_name := head(data),
        df = head(df),
        quo = c(y_quo, x_quo, data_quo),
        names = c(y_name, x_name, data_name)
    ))
}

myfun(y1, x = NULL, data = NULL)
myfun(y1, x = x1, data = NULL)
myfun(y1, data = mydata) ##
myfun(yy, x = NULL, data = mydata)
myfun(yy, x1, data = mydata) ##
myfun(yy, xx, data = mydata)
myfun("yy", "xx", data = mydata)
myfun("yy", xx, data = mydata)
myfun(yy, "xx", data = mydata)
myfun("y1", xx, data = mydata) ##
myfun(yy, "x1", data = mydata) ##


## generic function ================================
library(rlang)

x1 <- 1:5
y1 <- round(x1 + rnorm(length(x1), 0, 0.1), 2)
mydata <- tibble::tibble(xx = x1/2, yy = y1)

namesfun <- function(y, x = NULL, data = NULL) {
    y_quo <- enquo(y)
    x_quo <- enquo(x)
    data_quo <- enquo(data)

    y_name <- as_name(y_quo)
    x_name <- if (!quo_is_null(x_quo)) {as_name(x_quo)} else {"index"}
    data_name <- if (!quo_is_null(data_quo)) {as_name(data_quo)} else {NULL}

    if (is.null(data)) {
        if (!is.numeric(y)) {
            cli::cli_abort("{.arg y = {y_name}} must be a {.cls numeric} vector.")
        } else {
            y <- eval_tidy(y_quo)
        }

        if (is.null(x)) {
            x <- seq_along(y)
        } else if (!is.numeric(x)) {
            cli::cli_abort("{.arg x = {x_name}} must be a {.cls numeric} vector.")
        } else if (length(x) != length(y)) {
            cli::cli_abort(paste(
                "{.arg x = {x_name}} and {.arg y = {y_name}}",
                "must have the same length."))
        } else {
            x <- eval_tidy(x_quo)
        }

        data <- tibble(!!x_name := x, !!y_name := y)
        data_name <- "data"
    } else if (!is.data.frame(data)) {
        cli::cli_abort("{.arg data = {data_name}} must be a dataframe.")
    } else if (!has_name(data, y_name)) {
        cli::cli_abort("{.arg y = {y_name}} not found in {.arg data = {data_name}}.")
    } else { ## is.data.frame & has_name
        y <- data[[y_name]]

        if (quo_is_null(x_quo)) {
            x <- seq_along(y)
            data[[x_name]] <- x
            # if (verbose) {
                cli::cli_alert_info(
                    "{.arg x = {x_name}} added to {.arg data = {data_name}}.")
            # }
        } else if (!has_name(data, x_name)) {
            cli::cli_abort("{.arg x = {x_name}} not found in {.arg data = {data_name}}.")
        } else {
            x <- data[[x_name]]
        }

        data <- data[c(x_name, y_name)]
    }

    # x <- x - x0
    df <- tibble::tibble(x, y)

    return(rlang::list2(
        !!data_name := head(data),
        df = head(df),
        # quo = c(y_quo, x_quo, data_quo),
        names = c(y_name, x_name, data_name)
    ))
}

genfun <- function(y, x = NULL, data = NULL, method = c("A"), ...) {
    method <- match.arg(method)
    y_name <- as_name(enquo(y))
    class(y_name) <- method

    UseMethod("genfun", y_name)
}

# genfun(y1)

genfun.A <- function(y, x = NULL, data = NULL, method = c("A"), ...) {

    print(y)
    print(eval_tidy(y))
    print(substitute(y))
    print(deparse(substitute(y)))
    print(enquo(y))

    namesfun(y, x, data)
}


genfun(y1, x = NULL, data = NULL)
genfun(y1, x = x1, data = NULL)
# genfun(y1, x = NULL, data = mydata) ##
genfun(yy, x = NULL, data = mydata)
# genfun(yy, x1, data = mydata) ##
genfun(yy, xx, data = mydata)
genfun("yy", "xx", data = mydata)
genfun("yy", xx, data = mydata)
genfun(yy, "xx", data = mydata)
genfun("y1", xx, data = mydata) ##
genfun(yy, "x1", data = mydata) ##






## function nesting =====================================
inner <- function(x, data = NULL) {
    x_quo <- enquo(x)
    data_quo <- enquo(data)

    x_name <- as_name(x_quo)
    data_name <- if (!quo_is_null(data_quo)) as_name(data_quo) else NULL

    if (is.null(data)) {
        x_value <- eval_tidy(x_quo)
    } else if (has_name(data, x_name)) {
        # x_value <- eval_tidy(x_quo, data)
        x_value <- data[[x_name]]
    } else {
        cli::cli_abort("{.arg x = {x_name}} not found in {.arg data = {data_name}}.")
    }

    df <- tibble::tibble(!!x_name := x_value)

    list(
        df = df,
        value = x_value,
        names = c(x_name, data_name),
        level = "inner"
    )
}

inner(x1, data = NULL)
# inner(x1, data = mydata) ##
inner(xx, data = mydata)
inner("xx", data = mydata)



middle <- function(x, data = NULL) {
    x_quo <- enquo(x)
    data_quo <- enquo(data)

    x_name <- as_name(x_quo)
    data_name <- if (!quo_is_null(data_quo)) as_name(data_quo) else NULL

    inner_result <- inject(inner(!!x_quo, data))

    list(
        df = inner_result$df,
        value = inner_result$value,
        middle_names = c(x_name, data_name),
        inner_names = inner_result$names,
        level = "middle"
    )
}

middle(x1, data = NULL)
middle(x1, data = mydata) ##
middle(xx, data = mydata)
middle("xx", data = mydata)

outer <- function(x, data = NULL) {
    x_quo <- enquo(x)
    data_quo <- enquo(data)

    x_name <- as_name(x_quo)
    data_name <- if (!quo_is_null(data_quo)) as_name(data_quo) else NULL

    middle_result <- inject(middle(!!x_quo, data))

    list(
        df = middle_result$df,
        value = middle_result$value,
        outer_names = c(x_name, data_name),
        middle_names = middle_result$middle_names,
        inner_names = middle_result$inner_names,
        level = "outer"
    )
}


outer(x1, data = NULL)
outer(x1, data = mydata) ##
outer(xx, data = mydata)
outer("xx", data = mydata)


# Tests
test_that("NSE functions work with numeric objects", {
    my_var <- 42
    result <- outer(my_var)

    expect_equal(result$value, 42)
    expect_equal(result$name, "my_var")
    expect_equal(result$level, "outer")
    expect_equal(result$middle_result$inner_result$value, 42)
})

test_that("NSE functions work with dataframe columns", {
    df <- data.frame(col1 = 1:3, col2 = 4:6)

    # Unquoted column name
    result <- outer(col1, data = df)
    expect_equal(result$value, 1:3)
    expect_equal(result$name, "col1")

    # Expression with columns
    result2 <- outer(col1 + col2, data = df)
    expect_equal(result2$value, c(5, 7, 9))
    expect_equal(result2$name, "col1 + col2")
})

test_that("All levels preserve name and value", {
    test_val <- 100
    result <- outer(test_val)

    expect_equal(result$name, "test_val")
    expect_equal(result$middle_result$name, "test_val")
    expect_equal(result$middle_result$inner_result$name, "test_val")

    expect_equal(result$value, 100)
    expect_equal(result$middle_result$value, 100)
    expect_equal(result$middle_result$inner_result$value, 100)
})


## single function re-write ==========================================

onefun <- function(
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

    y_quo <- enquo(y)
    x_quo <- enquo(x)
    data_quo <- enquo(data)

    y_name <- as_name(y_quo)
    x_name <- if (!quo_is_null(x_quo)) {as_name(x_quo)} else {"index"}
    data_name <- if (!quo_is_null(data_quo)) {as_name(data_quo)} else {NULL}

    if (is.null(data)) {
        if (!is.numeric(y)) {
            cli::cli_abort("{.arg y = {y_name}} must be a {.cls numeric} vector.")
        } else {
            y <- eval_tidy(y_quo)
        }

        if (is.null(x)) {
            x <- seq_along(y)
        } else if (!is.numeric(x)) {
            cli::cli_abort("{.arg x = {x_name}} must be a {.cls numeric} vector.")
        } else if (length(x) != length(y)) {
            cli::cli_abort(paste(
                "{.arg x = {x_name}} and {.arg y = {y_name}}",
                "must have the same length."))
        } else {
            x <- eval_tidy(x_quo)
        }

        data <- tibble(!!x_name := x, !!y_name := y)
        data_name <- "data"
    } else if (!is.data.frame(data)) {
        cli::cli_abort("{.arg data = {data_name}} must be a dataframe.")
    } else if (!has_name(data, y_name)) {
        cli::cli_abort("{.arg y = {y_name}} not found in {.arg data = {data_name}}.")
    } else { ## is.data.frame & has_name
        y <- data[[y_name]]

        if (quo_is_null(x_quo)) {
            x <- seq_along(y)
            data[[x_name]] <- x
            if (verbose) {
                cli::cli_alert_info(
                    "{.arg x = {x_name}} added to {.arg data = {data_name}}.")
            }
        } else if (!has_name(data, x_name)) {
            cli::cli_abort("{.arg x = {x_name}} not found in {.arg data = {data_name}}.")
        } else {
            x <- data[[x_name]]
        }

        data <- data[c(x_name, y_name)]
    }

    x <- x - x0
    df <- tibble::tibble(x, y)
    fitted_name <- paste0(y_name, "_fitted")

    ## custom list of permissable fixed coefs
    fixed_coefs <- unlist(args[names(args) %in% c("A", "B", "TD")])

    # process_model <- function(model) {
    #     if (is.na(model[1])) {
    #         fitted <- NA_real_
    #         residuals <- NA_real_
    #         coefs <- c(A = NA_real_, B = NA_real_, TD = NA_real_,
    #                         tau = NA_real_, MRT = NA_real_,
    #                         xmid = NA_real_, scal = NA_real_)
    #         diagnostics <- c(
    #             AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
    #             RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)
    #     } else {
    #         y <- model$m$getEnv()$y
    #         fitted <- as.vector(fitted(model))
    #         residuals <- as.vector(residuals(model))
    #         coefs <- coef(model)
    #         diagnostics <- c(
    #             AIC = AIC(model),
    #             BIC = BIC(model),
    #             R2 = 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2),
    #             RMSE = sqrt(mean(summary(model)$residuals^2)),
    #             RSE = summary(model)$sigma,
    #             SNR = diff(range(fitted, na.rm = TRUE)) / summary(model)$sigma,
    #             MAE = mean(abs(summary(model)$residuals)),
    #             MAPE = mean(abs(summary(model)$residuals/y)) * 100)
    #     }
    #
    #     return(list(
    #         fitted = fitted,
    #         residuals = residuals,
    #         coefs = coefs,
    #         diagnostics = diagnostics))
    # }

    if (method == "monoexponential") {
        ## create the model and update for any fixed coefs
        model <- tryCatch(
            nls(y ~ mNIRS:::SSmonoexp(x, A, B, TD, tau),
                data = df,
                na.action = na.exclude) |>
                mNIRS:::update_fixed_coefs(...), ## TODO 2025-08-11 supply from fixed_coefs
            error = function(e) {
                cat("Error in nls(", y_name, " ~ SSmonoexp(", x_name,
                    ", A, B, TD, tau)) : ", e$message, "\n", sep = "")
                NA})

        ## process coefs, diagnostics, fitted
        model_output <- process_model(model)
        data[[fitted_name]] <- model_output$fitted
        ## include explicitly defined coefs
        coefs <- c(fixed_coefs, model_output$coefs) |>
            (\(.x) .x[match(names(formals(mNIRS:::SSmonoexp)), names(.x))] )() |>
            (\(.x) .x[!is.na(.x)])() |>
            (\(.x) as_tibble(as.list(.x)))()

        ## calculate MRT
        coefs$MRT <- coefs$TD + coefs$tau
        ## predict value for y at MRT x value
        coefs[[paste0("MRT_", y_name)]] <- predict(model, tibble(x = coefs$MRT))

        model_equation <- as.formula(y ~ A + (B - A) * (1 - exp((TD - x) / tau)))
        fitted <- model_output$fitted
        residuals <- model_output$residuals
        diagnostics <- model_output$diagnostics

    } else if (method == "sigmoidal") {
        ## create the model and update for any fixed coefs
        model <- tryCatch(
            nls(y ~ SSfpl(x, A, B, xmid, scal),
                data = df,
                na.action = na.exclude) |>
                mNIRS:::update_fixed_coefs(...), ## TODO 2025-08-11 supply from fixed_coefs
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

        model_equation <- as.formula(y ~ A + (B - A) / (1 + exp((xmid - x) / scal)))
        fitted <- model_output$fitted
        residuals <- model_output$residuals
        diagnostics <- model_output$diagnostics

    } else if (method == "half_time") {
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
        ## TODO 2025-08-11 need to allow fixing A & B for half_time
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

        model <- NULL
        model_equation <- as.formula(half_value ~ A + (B - A) / 2)
        fitted <- NULL
        residuals <- NULL
        diagnostics <- NULL

    } else if (method == "peak_slope") {
        if ("width" %in% names(args)) {
            width <- args$width
        }

        align_choices <- c("center", "left", "right")
        if ("align" %in% names(args)) {
            align <- match.arg(args$align, choices = align_choices)
        } else {
            align <- match.arg("center", choices = align_choices)
        }

        ## calculate all rolling slopes
        slopes <- rolling_slope(y, x, width, align, na.rm = TRUE)
        ## calculate peak rolling slope in the appropraite direction
        peak_slope <- peak_directional_slope(y, x, width, align, na.rm = TRUE)

        data[[fitted_name]] <- NA_real_
        data[[fitted_name]][x %in% peak_slope$x_fitted] <- peak_slope$y_fitted
        coefs <- c(peak_slope$x[1],
                   y[x %in% peak_slope$x][1],
                   data[[fitted_name]][x %in% peak_slope$x][1],
                   peak_slope$slope)
        names(coefs) <- c(x_name, y_name, fitted_name, "peak_slope")

        model <- NULL
        model_equation <- as.formula(
            TODO ~ sum((x_window - x_mean) * (y_window - y_mean)) /
                sum((x_window - x_mean)^2))
        fitted <- peak_slope$y_fitted
        residuals <- NULL
        diagnostics <- NULL
    }

    out <- structure(
        list(
            method = method,
            model = model,
            model_equation = model_equation,
            data = data,
            fitted = fitted,
            residuals = residuals,
            x0 = x0,
            coefs = as_tibble(as.list(coefs)),
            diagnostics = as_tibble(as.list(diagnostics)),
            call = match.call()),
        class = "mNIRS.kinetics")

    return(out)
}

set.seed(13)
x1 <- seq(-10, 60, by = 2)
A <- 10; B <- 100; TD <- 5; tau <- 12
y1 <- monoexponential(x1, A, B, TD, tau) + rnorm(length(x1), 0, 3)
mydata <- tibble::tibble(xx = x1/2, yy = y1)
rm(A); rm(B); rm(TD); rm(tau)
# plot(x1, y1)

process_model <- function(model) {
    if (is.na(model[1])) {
        fitted <- NA_real_
        residuals <- NA_real_
        coefs <- c(A = NA_real_, B = NA_real_, TD = NA_real_,
                   tau = NA_real_, MRT = NA_real_,
                   xmid = NA_real_, scal = NA_real_)
        diagnostics <- c(
            AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, RMSE = NA_real_,
            RSE = NA_real_, MAE = NA_real_, MAPE = NA_real_)
    } else {
        y <- model$m$getEnv()$y
        fitted <- as.vector(fitted(model))
        residuals <- as.vector(residuals(model))
        coefs <- coef(model)
        diagnostics <- c(
            AIC = AIC(model),
            BIC = BIC(model),
            R2 = 1 - sum((y - fitted)^2)/sum((y - mean(y, na.rm = TRUE))^2),
            RMSE = sqrt(mean(summary(model)$residuals^2)),
            RSE = summary(model)$sigma,
            SNR = diff(range(fitted, na.rm = TRUE)) / summary(model)$sigma,
            MAE = mean(abs(summary(model)$residuals)),
            MAPE = mean(abs(summary(model)$residuals/y)) * 100)
    }

    return(list(
        fitted = fitted,
        residuals = residuals,
        coefs = coefs,
        diagnostics = diagnostics))
}

.method <- "monoexp"
onefun(y1, x = NULL, data = NULL, width = 5, method = .method)
onefun(y1, x = x1, data = NULL, width = 5, method = .method)
onefun(y1, x = NULL, data = mydata, width = 5, method = .method) ##
onefun(yy, x = NULL, data = mydata, width = 5, method = .method)
onefun(yy, x1, data = mydata, width = 5, method = .method) ##
onefun(yy, xx, data = mydata, width = 5, method = .method)
onefun("yy", "xx", data = mydata, width = 5, method = .method)
onefun("yy", xx, data = mydata, width = 5, method = .method)
onefun(yy, "xx", data = mydata, width = 5, method = .method)
onefun("y1", xx, data = mydata, width = 5, method = .method) ##
onefun(yy, "x1", data = mydata, width = 5, method = .method) ##

onefun(yy, xx, data = mydata, width = 5, method = .method)
onefun(yy, xx, data = mydata, width = 5, method = .method, A = 20)

