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

data_raw <- mNIRS::read_data(
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
    mNIRS::downsample_data(
        # sample_column = "time",
        downsample_rate = 10
    ) |>
    print()

filter(data_raw, !is.na(event))
# plot(data_raw)
(sample_column <- attributes(data_raw)$sample_column)
(fit_sample_name <- paste0("fit_", sample_column))

# event_sample <- c(370, 1085)
event_sample <- c(24675, 66670)
# event_sample <- 876

data_list <- prepare_kinetics_data(
    data_raw,
    event_sample = event_sample,
    event_label = c("E7"),
    fit_window = c(30*50, 120*50)
) |>
    print()

## TODO
## iterate over prepared_kinetics_data_list
## dataframe / NIRS channel
## output list:
## - aggregate table of coefs
## - aggregate table of diagnostics



model_list <- tidyr::expand_grid(
    .df = data_list,
    .nirs = attributes(data_raw)$nirs_columns,
    .method = "monoexp") |>
    purrr::pmap(
        \(.df, .nirs, .method)
        process_kinetics(x = fit_sample_name,
                         y = .nirs,
                         data = .df,
                         method = .method,
                         width = 10*50)
    ) |>
    print()

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
                              linewidth = 1),
                    geom_segment(
                        data = tibble::tibble(
                            x = coef_channel$MRT,
                            y = coef_channel[[MRT_nirs_value]]),
                        aes(x = x, xend = x, y = y, yend = -Inf),
                        arrow = arrow(), linewidth = 1)
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
                        linewidth = 1),
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
x <- seq(-10, 60, by = 2)
A <- 10; B <- 100; TD <- 5; tau <- 12
y <- monoexponential(x, A, B, TD, tau) + rnorm(length(x), 0, 3)

## monoexponential kinetics ===============================
model <- process_kinetics(x, y, method = "monoexponential")
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
model <- process_kinetics(x, y, method = "sigmoidal")
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
model <- process_kinetics(x, y, method = "half_time")
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
model <- process_kinetics(x, y, method = "peak_slope", width = 10)
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
