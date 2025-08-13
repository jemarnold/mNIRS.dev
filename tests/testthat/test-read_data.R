test_that("read_data moxy.perfpro works", {
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mNIRS")

    expect_warning(
        df.moxy.perfpro <- read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 Live",
                             smo2_right = "SmO2 Live(2)"),
            sample_column = c(time = "hh:mm:ss"),
            numeric_time = FALSE,
            keep_all = FALSE,
            verbose = TRUE),
        "non-sequential or repeating") |>
        expect_message("Estimated sample rate")

    expect_s3_class(df.moxy.perfpro, "mNIRS.data")

    expect_s3_class(df.moxy.perfpro, "data.frame")

    expect_s3_class(df.moxy.perfpro$time, "POSIXct")

    expect_type(df.moxy.perfpro$time, "double")

    expect_true(
        all(c("nirs_columns", "sample_column", "sample_rate") %in%
                names(attributes(df.moxy.perfpro))))

    expect_equal(attr(df.moxy.perfpro, "sample_rate"), 2)

    expect_s3_class(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 Live",
                             smo2_right = "SmO2 Live(2)"),
            sample_column = c(time = "hh:mm:ss"),
            numeric_time = FALSE,
            keep_all = FALSE,
            verbose = FALSE)$time,
        "POSIXct")

    expect_error(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "smo2_doesnt_exist"),
            verbose = FALSE),
        "not detected")

    expect_warning(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2 = "SmO2 Live",
                             smo2 = "SmO2 Live(2)"),
            verbose = TRUE),
        "Duplicate input column") |>
        expect_message("Sample rate set to 1 Hz")

    expect_message(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 Live",
                             smo2_right = "SmO2 Live(2)"),
            sample_column = NULL,
            verbose = TRUE),
        "No `sample_column` provided")
})



test_that("read_data train.red works", {
    file_path <- system.file("extdata/train.red_interval_example.csv",
                             package = "mNIRS")

    expect_warning(
        tr_data <- read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 unfiltered",
                             smo2_right = "SmO2 unfiltered"),
            sample_column = c(time = "Timestamp (seconds passed)"),
            keep_all = FALSE,
            verbose = TRUE),
        "non-sequential or repeating") |>
        expect_message("Estimated sample rate")

    expect_silent(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 unfiltered",
                             smo2_right = "SmO2 unfiltered"),
            sample_column = c(time = "Timestamp (seconds passed)"),
            keep_all = FALSE,
            verbose = FALSE)
    )

    expect_s3_class(tr_data, "mNIRS.data")

    expect_s3_class(tr_data, "data.frame")

    expect_type(tr_data$time, "double")

    expect_true(
        all(c("nirs_columns", "sample_column", "sample_rate") %in%
                names(attributes(tr_data))))

    expect_equal(attr(tr_data, "sample_rate"), 10)

    expect_error(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "smo2_doesnt_exist"),
            verbose = FALSE),
        "not detected")

    expect_warning(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2 = "SmO2 unfiltered",
                             smo2 = "SmO2 unfiltered"),
            verbose = TRUE),
        "Duplicate input column") |>
        expect_message("Sample rate set to 1 Hz")

    expect_message(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 unfiltered",
                             smo2_right = "SmO2 unfiltered"),
            sample_column = NULL,
            verbose = TRUE),
        "No `sample_column` provided")
})



test_that("read_data oxysoft works", {
    file_path <- system.file("extdata/oxysoft_interval_example.xlsx",
                             package = "mNIRS")

    expect_length(
        oxy_data <- read_data(
            file_path = file_path,
            nirs_columns = c(HHb_VL = 5,
                             O2Hb_VL = 6),
            sample_column = c(sample = 1),
            verbose = FALSE),
        3)

    expect_type(oxy_data$sample, "double")

    expect_s3_class(oxy_data, "mNIRS.data")

    expect_s3_class(oxy_data, "data.frame")

    expect_true(
        all(c("nirs_columns", "sample_column", "sample_rate") %in%
                names(attributes(oxy_data))))

    expect_equal(attr(oxy_data, "sample_rate"), 50)

    expect_error(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "smo2_doesnt_exist"),
            verbose = FALSE),
        "not detected")

    expect_warning(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2 = 5,
                             smo2 = 6),
            sample_column = c(sample = 1),
            verbose = TRUE),
        "Duplicate input column") |>
        expect_message("Oxysoft detected sample rate")

    expect_message(
        read_data(
            file_path = file_path,
            nirs_columns = c(HHb_VL = 5,
                             ICG_VL = 6),
            sample_column = NULL,
            verbose = TRUE),
        "No `sample_column` provided") |>
        expect_message("Oxysoft detected sample rate")
})




test_that("read_data VMPro app works", {
    file_path <- system.file("extdata/vo2master_moxyunit_example.xlsx",
                             package = "mNIRS")

    expect_length(
        vmpro_data <- read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2[%]",
                             smo2_right = "SmO2 -  2[%]"),
            sample_column = c(time = "Time[utc]"),
            numeric_time = FALSE,
            keep_all = TRUE,
            verbose = FALSE),
        5)

    expect_s3_class(vmpro_data, "mNIRS.data")

    expect_s3_class(vmpro_data, "data.frame")

    expect_s3_class(vmpro_data$time, "POSIXct")

    expect_true(
        all(c("nirs_columns", "sample_column", "sample_rate") %in%
                names(attributes(vmpro_data))))

    expect_equal(attr(vmpro_data, "sample_rate"), 1)

    expect_error(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "smo2_doesnt_exist"),
            verbose = FALSE),
        "not detected")

    expect_warning(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2 = "SmO2[%]",
                             smo2 = "SmO2 -  2[%]"),
            sample_column = c(time = "Time[utc]"),
            verbose = TRUE),
        "Duplicate input column") |>
        expect_message("Estimated sample rate")

    expect_message(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2[%]",
                             smo2_right = "SmO2 -  2[%]"),
            sample_column = NULL,
            verbose = TRUE),
        "No `sample_column` provided")
})




