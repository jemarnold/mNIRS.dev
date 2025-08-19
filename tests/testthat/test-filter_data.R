test_that("filter_data smooth-spline works", {
    # devtools::load_all()
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mNIRS")

    data_raw <- read_data(
        file_path = file_path,
        nirs_columns = c(smo2 = "SmO2 Live"),
        sample_column = c(time = "hh:mm:ss"),
        verbose = FALSE
    ) |>
        dplyr::mutate(
            smo2 = filter_data(smo2, method = "smooth-spline")
        ) |>
})
