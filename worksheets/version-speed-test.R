
remotes::install_github("jemarnold/mNIRS.dev@b832de3", force = TRUE)
library(mNIRS)
devtools::load_all()

(\(n_reps = 10) { ## benchmark function
    start_time <- Sys.time()

    file_path <- system.file("extdata/oxysoft_interval_example.xlsx",
                             package = "mNIRS")

    f1 <- \() {



        data_raw <- read_data(
            file_path = file_path,
            nirs_columns = c(O2Hb = 5, HHb = 6),
            sample_column = c(sample = 1),
            event_column = c(event = 8),
            verbose = FALSE
        ) #|>
            # dplyr::mutate(
            #     # time = round(time - dplyr::first(time), 1),
            #     time = sample/50,
            #     across(c(O2Hb, HHb), \(.x) filter_data(.x, "butterworth", n = 2, W = 0.05))
            # )

        # nirs_columns <- attributes(data_raw)$nirs_columns
        # fitted_name <- paste0(nirs_columns, "_fitted")
        # sample_column <- attributes(data_raw)$sample_column
        # fit_sample_name <- paste0("fit_", attributes(data_raw)$sample_column)
        #
        # event_samples <- c(24675, 66670)
        #
        # data_list <- prepare_kinetics_data(
        #     data_raw,
        #     event_sample = event_samples,
        #     event_label = c("E7"),
        #     fit_window = c(30*50, 120*50),
        #     verbose = FALSE
        # )
        #
        # # plot(data_list[[1]])
        #
        # model_list <- tidyr::expand_grid(
        #     .df = data_list,
        #     .nirs = attributes(data_raw)$nirs_columns,
        #     .method = "peak_slope") |>
        #     purrr::pmap(
        #         \(.df, .nirs, .method)
        #         process_kinetics(y = .nirs,
        #                          x = fit_sample_name,
        #                          data = .df,
        #                          method = .method,
        #                          window = 25*50,
        #                          width = 5*50)
        #     )
        #
        # # plot(model_list[[1]])
        # ## coef table =====================================
        # coef_data <- purrr::imap(model_list, \(.x, idx) {
        #     tibble::as_tibble(as.list(c(.x$coefs))) |>
        #         dplyr::mutate(
        #             event = idx,
        #             channel = names(.x$data)[2],
        #             across(where(is.numeric), \(.x) round(.x, 2))
        #         ) |>
        #         dplyr::relocate(event, channel)
        # }) |>
        #     purrr::list_rbind()
        #
        # return(coef_data)
    }

    t1 <- system.time(replicate(n_reps, f1()))["elapsed"]

    cli::cli_alert_info("Reps = {.val {n_reps}}")
    cli::cli_alert_info("Performance Time = {.val {round(t1/n_reps, 2)}} s")
    total_time <- Sys.time() - start_time
    cli::cli_alert_info(
        "Total runtime = {.val {round(as.numeric(total_time, units = 'secs'), 3)}} s")

})()

## old read_data 10x 1.23s
## new read_Data 10x 2.16s
## new sigmoidal 5x 3.58, 10x 3.35
## old sigmoidal <broken>
## old monoexp 5x 2.46 10x 2.54
## new monoexp 5x 3.48 10x 3.19
## new peak_slope 7.94
## old peak_slope 3.98
3.48 / 2.46
3.19 / 2.54
