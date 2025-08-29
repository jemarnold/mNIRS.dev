

inputs
- dataframe of mNIRS.data
- list of nirs channels (detect from metadata)
- list of event groups ## number of list items should match number in other lists
- list of methods
- list of fit_span (default c(30, 180))
- list of end_kinetics_span (default 15)
- list of x0 (default 0)

- allow a master list like purrr::pmap

myfun(data,
      event_labels = c(1, 2, 3, 4, 5),
      nirs_channels = list(c("A", "B"), "A", "C"),
      group_events = list(c(1, 2), c(3, 4), 5),
      methods = list(c("monoexponential", "sigmoidal"),
                     "monoexponential",
                     "peak_slope"),
      end_kinetics_span = list(30, 15, 15),
),



output
- print list of model summaries (methods.mNIRS.kinetics)
- list of process_kinetics$out
- list of plots
- multiplot ncol = 2 subplot by event
- table of coefs (event, nirs_channel, method, span)
- table of diagnostics




process_kinetics <- function(
        data,
        nirs_channels = list(),
        sample_channel = NULL,
        event_channel = NULL,
        event_samples = NULL,
        event_labels = NULL,
        group_events = list("distinct", "ensemble"),
        method = list("monoexponential", "sigmoidal", "half_time", "peak_slope"),
        fit_span = list(c(30, 180)),
        end_kinetics_span = list(20),
        x0 = list(0),
        peak_slope_span = list(10),
        sample_rate = NULL,
        verbose = TRUE,
        ...
) {

}



## Worksheet ========================================
devtools::load_all()

file_path <- r"(C:\R-Projects\mnirs.dev\inst\extdata\train.red_interval_example.csv)"
# file_path <- r"(C:\R-Projects\mnirs.dev\inst\extdata\oxysoft_interval_example.xlsx)"
# file_path <- r"(C:\R-Projects\mnirs.dev\inst\extdata\moxy_ramp_example.xlsx)"

data <- read_data(
    file_path = file_path,
    # nirs_channels = c(smo2_left = "SmO2 Live", smo2_right = "SmO2 Live(2)"),
    # sample_channel = c(time = "hh:mm:ss"),
    nirs_channels = c(smo2_left = "SmO2", smo2_right = "SmO2"),
    sample_channel = c(time = "Timestamp (seconds passed)"),
    # nirs_channels = c(O2Hb = 5, HHb = 6),
    # sample_channel = c(sample = 1),
    # event_channel = c(event = 8),
    time_from_zero = TRUE,
) |>
    resample_data(resample_rate = 10, na.rm = TRUE) |>
    print()


plot(data)
# dplyr::filter(data, !is.na(event))
nirs_channels <- attributes(data)$nirs_channels
# (sample_channel <- attributes(data)$sample_channel)
# (fit_sample_name <- paste0("fit_", sample_channel))

event_sample <- c(370, 1085)
# event_sample <- c(24675, 66670)
# event_sample <- 876

kinetics_list <- prepare_kinetics_data(
    data,
    event_sample = event_sample,
    fit_span = c(30, 120),
)
kinetics_list

model_list <- tidyr::expand_grid(
    .data = kinetics_list,
    .nirs = nirs_channels,
) |>
    purrr::pmap(\(.data, .nirs) {
        process_kinetics(
            y = .nirs,
            x = "fit_time",
            data = .data,
            method = "monoexp",
            peak_slope_span = 10)
    })

model_list[[1]]$coef |> attributes()
plot(model_list[[1]], plot_coefs = TRUE, plot_diagnostics = TRUE, plot_residuals = TRUE)



## extract list of coefficient tables
coef_list <- lapply(model_list, \(.x) .x$coef)

## convert list to dataframe and add identifiers
coefs <- do.call(rbind, Map(tibble::tibble, coef_list))
coefs$event <- factor(names(coef_list))
coefs$channel <- rep(nirs_channels, length(coef_list) / length(nirs_channels))

## round numeric columns
numeric_cols <- sapply(coefs, is.numeric)
coefs[numeric_cols] <- lapply(coefs[numeric_cols], \(.x) round(.x, 1))

## relocate columns
coefs <- coefs[c("event", "channel", names(coefs)[!names(coefs) %in% c("event", "channel")])]

coefs


## TODO 2025-08-28 re-write in base r
kinetics_data <- lapply(model_list, \(.x) .x$data) |>
    purrr::imap_dfr(\(.x, .idx) dplyr::mutate(.x, event = .idx)) |>
    dplyr::summarise(
        .by = c(fit_time, event),
        dplyr::across(dplyr::where(is.numeric), \(.x) mean(.x, na.rm = TRUE)),
    ) |>
    dplyr::mutate(
        event = factor(event, labels = c("End Ramp", "End VB")),
    )

ggplot(kinetics_data) +
    {list( ## Settings
        aes(x = fit_time, group = event),
        facet_wrap(~ event),
        labs(title = glue::glue(
            "Reoxygenation Kinetics for ",
            "<span style = 'color:{Palette_JA('Left')}'>**Left**</span> ",
            "and ",
            "<span style = 'color:{Palette_JA('Right')}'>**Right**</span> ",
            "Quadriceps")),
        coord_cartesian(
            xlim = c(NA, NA),
            ylim = c(NA, NA)),
        theme_JA(panel.spacing = unit(10, "mm"),
                 plot.margin = margin(2, r = 5, 2, 2, unit = "mm")),
        scale_x_continuous(
            name = "Time (mm:ss)",
            labels = format_hmmss,
            breaks = breaks_timespan(n=8),
            expand = expansion(mult = 0.005)),
        scale_y_continuous(
            name = expression(bold(SmO['2']~'(%)')),
            n.breaks = 6,
            expand = expansion(mult = 0.0)),
        scale_colour_manual(
            name = NULL,
            aesthetics = c("colour", "fill"),
            values = Palette_JA(),
            limits = force),
        NULL)} + ## Settings
    {list( ## Data
        geom_vline(xintercept = 0, linetype = "dotted"),
        geom_line(aes(y = smo2_left_vl_norm, colour = "Left"), linewidth = 1),
        geom_line(aes(y = smo2_left_vl_norm_fitted), na.rm = TRUE),

        geom_line(aes(y = smo2_right_vl_norm, colour = "Right"), linewidth = 1),
        geom_line(aes(y = smo2_right_vl_norm_fitted), na.rm = TRUE),

        geom_segment(
            data = coefs,
            aes(x = MRT, xend = MRT, y = MRT_fitted, yend = -Inf),
            colour = "grey10", arrow = arrow(), linewidth = 0.8),
        geom_point(
            data = coefs,
            aes(x = MRT, y = MRT_fitted),
            colour = "grey10", size = 4, shape = 21, stroke = 1.2),

        ggpp::geom_table(
            data = tibble(event = unique(coefs$event), x = Inf, y = -Inf,
                          tb = list(coefs[1:2, c(8, 4:6)], coefs[3:4, c(8, 4:6)])),
            aes(x = x, y = y, label = tb),
            hjust = 1, vjust = 0),
        NULL)} ## Data
