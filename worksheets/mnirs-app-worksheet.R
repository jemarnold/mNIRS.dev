## Setup ==========================================================
suppressPackageStartupMessages({
    # library(JAPackage)
    library(mNIRS)
    library(tidyverse)
})
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
sample_column <- attributes(data_raw)$sample_column
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



kinetics_model_list <- purrr::pmap(
    tidyr::expand_grid(
        .df = data_list,
        .nirs = attributes(data_raw)$nirs_columns),
    \(.df, .nirs)
    process_kinetics(x = fit_sample_name,
                     y = .nirs,
                     data = .df,
                     method = "sigmoid",
                     width = 10*50)
) |>
    print()

## coef table =====================================
coef_data <- purrr::imap(
    kinetics_model_list,
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
# kinetics_model_list[[1]]$data

(sample_column <- attributes(data_raw)$sample_column)
(nirs_columns <- attributes(data_raw)$nirs_columns)
(nirs_fitted <- paste0(nirs_columns, "_fitted"))
(fit_sample <- paste0("fit_", sample_column))

display_data_list <- purrr::map(
    kinetics_model_list,
    \(.x)
    .x$data |>
        dplyr::select(dplyr::matches(c(fit_sample, nirs_columns, "fitted")))
) |>
    (\(.l) split(.l, factor(names(.l), levels = unique(names(.l)))))() |>
    purrr::map(\(.l) purrr::reduce(.l, full_join, by = fit_sample)) |>
    print()

(coef_data <- coef_data[1:2,])

(display_data <- display_data_list[[1]])

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
    map(nirs_columns,
        \(.x) geom_line(aes(y = .data[[.x]], colour = .x),
                        linewidth = 1)) +
    map(nirs_columns,
        \(.x) {
            coef_channel <- coef_data[coef_data$channel == .x,]
            nirs_fitted <- paste0(.x, "_fitted")
            MRT_nirs_value <- paste0(.x, "_MRT")

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
        }) +
    # map(nirs_columns,
    #     \(.x) {
    #         coef_channel <- coef_data[coef_data$channel == .x,]
    #
    #         A_name <- paste0("A_", fit_sample)
    #         B_name <- paste0("B_", fit_sample)
    #         half_name <- paste0("half_", fit_sample)
    #
    #         list(
    #             geom_segment(
    #                 data = tibble::tibble(
    #                     x = coef_channel[[half_name]],
    #                     y = coef_channel$half_value),
    #                 aes(x = x, xend = x, y = y, yend = -Inf),
    #                 arrow = arrow(), linewidth = 0.8),
    #             geom_point(
    #                 data = tibble::tibble(
    #                     x = c(coef_channel[[A_name]],
    #                           coef_channel[[B_name]],
    #                           coef_channel[[half_name]]),
    #                     y = c(coef_channel$A,
    #                           coef_channel$B,
    #                           coef_channel$half_value)),
    #                 aes(x = x, y = y, colour = "fitted"),
    #                 size = 3, shape = 21, stroke = 1),
    #         NULL)
    #     }) +
    # map(nirs_columns,
    #     \(.x) {
    #         coef_channel <- coef_data[coef_data$channel == .x,]
    #         nirs_fitted <- paste0(.x, "_fitted")
    #
    #         list(
    #             geom_line(
    #                 aes(y = .data[[nirs_fitted]], colour = "fitted"),
    #                 linewidth = 1),
    #             geom_segment(
    #                 data = tibble::tibble(
    #                     x = coef_channel[[fit_sample]],
    #                     y = coef_channel[[nirs_fitted]]),
    #                 aes(x = x, xend = x, y = y, yend = -Inf),
    #                 arrow = arrow(), linewidth = 0.8),
    #             geom_point(
    #                 data = tibble::tibble(
    #                     x = coef_channel[[fit_sample]],
    #                     y = coef_channel[[nirs_fitted]]),
    #                 aes(x = x, y = y, colour = "fitted"),
    #                 size = 3, shape = 21, stroke = 1)
    #         )
    #     }) +
    NULL


