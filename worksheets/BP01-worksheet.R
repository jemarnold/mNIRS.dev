(data <- mNIRS::read_data(
    file_path = "C:/OneDrive - UBC/Body Position Study/Raw Data/BP01-oxysoft-2025-04-01.xlsx",
    nirs_columns = c("PS_O2Hb" = "2",
                     "PS_HHb" = "3",
                     "VL_O2Hb" = "5",
                     "VL_HHb" = "6"),
    sample_column = c("sample" = "1"),
    event_column = c("label" = "...11"),
    .keep_all = FALSE))
# attributes(data)
# #
(data_list <- mNIRS::prepare_kinetics_data(
    data,
    nirs_columns = c("PS_HHb", "VL_HHb"),
    # sample_column = NULL,
    # event_column = "label",
    # event_index = c(1000),
    event_label = c("end RP", "end UP", "end stage"),
    fit_baseline_window = 30*50,
    fit_kinetics_window = 120*50,
    # display_baseline_window = 40,
    # display_kinetics_window = 240,
    group_kinetics_events = list(c(1, 3, 5), c(2, 4)) #"ensemble"
))


data_list2 <- purrr::map(
    data_list,
    \(.data)
    # .data |>
    #     dplyr::mutate(
    #         dplyr::across(
    #             dplyr::matches("HHb"),
    #             \(.x) mNIRS::process_kinetics(
    #                 x = display_index,
    #                 y = .x,
    #                 # x0 = 0,
    #                 method = "monoexp"
    #             )$fitted,
    #             .names = "{gsub('HHb', 'fitted', .col)}"
    #         )
    #     )

    .data |>
        dplyr::mutate(
            time = display_index/50
        ) |>
        dplyr::reframe(
            dplyr::across(
                dplyr::matches("HHb"),
                \(.x) mNIRS::process_kinetics(
                    x = time,
                    y = .x,
                    # x0 = 0,
                    method = "monoexp"
                )$coefs
            )
        )
) |>
    print()

library(tidyverse)
ggplot(data_list[[1]]) +
    {list( ## Settings
        aes(x = display_index, y = VL_HHb),
        # labs(title = glue::glue(
        #     "<span style = 'color:{Palette_JA('Right')}'> </span> "
        # )),
        coord_cartesian(
            xlim = c(NA, NA),
            ylim = c(NA, NA)),
        JAPackage::theme_JA(legend.position = "top"),
        scale_x_continuous(
            name = "sample",
            breaks = scales::breaks_pretty(),
            # name = "Time (mm:ss)",
            # labels = format_hmmss,
            # expand = expansion(mult = 0.01)
        ),
        scale_y_continuous(
            name = "VL_HHb",
            breaks = scales::breaks_pretty(),
            # sec.axis = sec_axis(
            #     name = "",
            #        transform = ~ (. * (y2max - y2min) - y2max * y1min) / (y1max - y1min) + y2min,
            # # aes(y = (yvar - y2min) * (y1max - y1min) / (y2max - y2min) + y1min)
            # ),
            # expand = expansion(mult = 0.01)
        ),
        # scale_colour_manual(
        #   name = NULL,
        #   aesthetics = c("colour", "fill"),
        #   # breaks = c(),
        #   # values = scales::hue_pal()(2),
        #   # values = Palette_JA(),
        #   # labels = c(),
        #   limits = force),
        # guides(colour = guide_legend(
        #     nrow = 1, byrow = FALSE,
        #     override.aes = list(shape = NA, linewidth = 5, alpha = 1))),
        NULL)} + ## Settings
    {list( ## Data
        geom_vline(xintercept = 0, linetype = "dotted"),
        # geom_vline(
        #     xintercept = filter(data, grepl("end RP|end UP|end stage", data$label))$sample,
        #     linetype = "dotted"),
        geom_line(),
        geom_line(aes(y = VL_fitted, colour = "VL_fitted")),

        geom_line(data = data_list[[2]], aes(colour = "data2")),
        geom_line(data = data_list[[2]], aes(y = VL_fitted, colour = "VL_fitted2")),
        NULL)} ## Data
