## Setup ==========================================================
suppressPackageStartupMessages({
    library(JAPackage)
    library(mNIRS)
    library(tidyverse)
})

options(digits = 5, digits.secs = 3, scipen = 3,
        dplyr.summarise.inform = FALSE,
        tibble.print_min = 20)

camcorder::gg_record(
  # dir = "ggplots",
  width = 220,
  height = 220*2/3,
  dpi = 300,
  units = "mm",
  device = "png",
  bg = "white")
# camcorder::gg_stop_recording()
#
## Data Wrangling ================================
file_path = "C:/OneDrive - UBC/FLIA Clinical Assessments/Raw Data/CSI-O CB/CB12-RVL-2025-03-07.xlsx"
# file_path = r"(C:\OneDrive - UBC\FLIA Clinical Assessments\Raw Data\CSI-O CB\CBpre-LVL-2024-1126.xlsx)"
nirs_columns = c(smo2_left_VL = "SmO2 Live",
                 thb_left_VL = "THb")
# sample_column = "hh:mm:ss"

# df <- mNIRS::read_data(
#     r"(C:\OneDrive - UBC\HLI\Trail Runners\Processed Data\M02-Data.xlsx)",
#     nirs_column = c("SmO2_VL", "SmO2_GC"),
#     sample_column = "Time",
#     event_column = "Event",
#     .keep_all = FALSE,
#     sample_rate = 1
# ) |>
df <- mNIRS::read_data(
    file_path = r"(C:\OneDrive - UBC\Group Projects\EPL\Matt Fliss\matt pitshark.csv)",
    nirs_columns = c("smo2_673" = "SmO2_673", "smo2_702" ="SmO2_702"),
    sample_column = c("time" = "Timestamp (seconds passed)")
) |>
    remove_outliers(
        # remove_fixed_values = c(0, 1),
        remove_outliers = TRUE,
        # sample_rate = 1,
    ) |>
    # mutate(
    #     smo2 = case_when(
    #         between(time, 1000, 2000) ~ NA,
    #         TRUE ~ smo2),
    # ) |>
    # handle_missing_data(
    #     handle_missing_values = "interpolate"
    # ) |>
    # filter_data(
    #     # handle_missing_values = "interpolate",
    #     filter_method = "low-pass",
    #     filter_parameters = list(c(n = 2, fc = 0.5)),
    #     # sample_rate = 1,
    # ) |>
    # normalise_data(
    #     shift_minimum = "none",
    #     # shift_value = 0,
    #     normalise = "global",
    #     normalise_range = c(0, 5)
    # ) |>
    mutate(
        across(
            c(smo2_673),
            \(.x) smooth.spline(x = index, y = .x)$y,
            .names = "{.col}_filt"),
    ) |>
    print()

# df |>
#     filter(!is.na(Event))
#
# attributes(df)

ggplot(df) +
    {list( ## Settings
        aes(x = time),
        theme_JA(),
        scale_x_continuous(
            name = "time",
            limits = c(0, 180),
        ),
        scale_y_continuous(
            name = "smo2_673",
            breaks = scales::breaks_pretty(),
        ),
        NULL)} + ## Settings
    {list( ## Data
        # geom_hline(yintercept = c(0), linetype = "dotted"),
        # geom_vline(xintercept = filter(df, grepl("Exercise", Event))$Time),
        geom_line(aes(y = smo2_673, colour = "VL")),
        geom_line(
            data = ~ filter(.x, between(time, 89, 105)),
            aes(y = smo2_673_filt), colour = "springgreen3", linewidth = 2),
        geom_line(
            data = ~ filter(.x, between(time, 105, 125)),
            aes(y = smo2_673_filt), colour = "dodgerblue", linewidth = 2),
        # geom_line(aes(y = smo2_702, colour = "GC")),
        NULL)} ## Data
