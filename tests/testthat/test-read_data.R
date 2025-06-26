test_that("read_data moxy.perfpro works", {
    file_path <- system.file("extdata",
                             "moxy_ramp_example.xlsx",
                             package = "mNIRS")

    expect_warning(
        df.moxy.perfpro <- read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "smo2_left_VL",
                             smo2_right = "smo2_right_VL"),
            sample_column = c(time = "Time"),
            event_column = c(event = "Event"),
            .numeric_time = TRUE,
            .keep_all = FALSE,
            .verbose = TRUE),
        "non-sequential or repeating")

    expect_s3_class(df.moxy.perfpro, "mNIRS.data")

    expect_s3_class(df.moxy.perfpro, "data.frame")

    expect_true(
        all(c("nirs_columns", "sample_column",
              "event_column", "sample_rate") %in%
                names(attributes(df.moxy.perfpro))))

    expect_equal(attr(df.moxy.perfpro, "sample_rate"), 2)

    expect_type(df.moxy.perfpro$time, "double")

    expect_s3_class(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "smo2_left_VL",
                             smo2_right = "smo2_right_VL"),
            sample_column = c(time = "Time"),
            .numeric_time = FALSE,
            .keep_all = FALSE,
            .verbose = FALSE)$time,
        "POSIXct")

    expect_error(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "smo2_doesnt_exist"),
            .verbose = FALSE),
        "not detected")

    expect_warning(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2 = "smo2_left_VL",
                             smo2 = "smo2_right_VL"),
            .verbose = TRUE),
        "Duplicate input column")

    expect_message(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "smo2_left_VL",
                             smo2_right = "smo2_right_VL"),
            sample_column = NULL,
            .verbose = TRUE),
        "No `sample_column` provided")
})

test_that("read_data train.red works", {
    file_path <- system.file("extdata",
                             "train.red_interval_example.csv",
                             package = "mNIRS")

    expect_warning(
        train.red <- read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 unfiltered",
                             smo2_right = "SmO2 unfiltered"),
            sample_column = c(time = "Timestamp (seconds passed)"),
            .keep_all = FALSE,
            .verbose = TRUE),
        "non-sequential or repeating")

    expect_s3_class(train.red, "mNIRS.data")

    expect_s3_class(train.red, "data.frame")

    expect_true(
        all(c("nirs_columns", "sample_column", "sample_rate") %in%
                names(attributes(train.red))))

    expect_equal(attr(train.red, "sample_rate"), 10)

    expect_type(train.red$time, "double")

    expect_error(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "smo2_doesnt_exist"),
            .verbose = FALSE),
        "not detected")

    expect_warning(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2 = "SmO2 unfiltered",
                             smo2 = "SmO2 unfiltered"),
            .verbose = TRUE),
        "Duplicate input column")

    expect_message(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 unfiltered",
                             smo2_right = "SmO2 unfiltered"),
            sample_column = NULL,
            .verbose = TRUE),
        "No `sample_column` provided")
})
