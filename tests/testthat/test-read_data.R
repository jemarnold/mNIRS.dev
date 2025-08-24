test_that("read_data moxy.perfpro works", {
    # devtools::load_all()
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mNIRS")

    expect_warning(
        df <- read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 Live",
                             smo2_right = "SmO2 Live(2)"),
            sample_column = c(time = "hh:mm:ss"),
            time_to_numeric = FALSE,
            keep_all = FALSE,
            verbose = TRUE),
        "non-sequential or repeating") |>
        expect_message("Estimated sample rate")

    expect_s3_class(df, "mNIRS.data")
    expect_s3_class(df, "data.frame")
    expect_s3_class(df$time, "POSIXct")
    expect_false(all(class(df$time) %in% "numeric"))
    expect_type(df$time, "double")

    expect_true(
        all(c("nirs_columns", "sample_column", "sample_rate") %in%
                names(attributes(df))))

    expect_equal(attr(df, "sample_rate"), 2)

    expect_warning(
        df <- read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 Live",
                             smo2_right = "SmO2 Live(2)"),
            sample_column = c(time = "hh:mm:ss"),
            time_to_numeric = TRUE,
            keep_all = FALSE,
            verbose = TRUE),
        "non-sequential or repeating") |>
        expect_message("Estimated sample rate")
    expect_true(all(class(df$time) %in% "numeric"))
    expect_false(all(class(df$time) %in% "POSIXct"))
    ## check that time diffs should be 0 < Δ < 1 with proper POSIXct import
    expect_gt(sum(diff(head(df$time, 100)) < 1 & diff(head(df$time, 100)) > 0), 0)
    expect_lt(sum(diff(head(df$time, 100)) %in% c(0, 1)), 99)

    ## column name blank space should error
    expect_error(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 Live",
                             smo2_right = "SmO2 Live(2)"),
            sample_column = c(time = "hh:mm:ss"),
            event_column = " ",
            verbose = FALSE),
        "not detected")

    ## column name empty space should not error convert to NULL
    expect_no_error(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 Live",
                             smo2_right = "SmO2 Live(2)"),
            sample_column = c(time = "hh:mm:ss"),
            event_column = "",
            verbose = FALSE)
        )

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
        "Duplicated input column names detected") |>
        expect_message("Adding an `index` column by row number") |>
        expect_message("No `sample_column` provided")

    expect_message(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 Live",
                             smo2_right = "SmO2 Live(2)"),
            sample_column = NULL,
            verbose = TRUE),
        "Adding an `index` column by row number") |>
        expect_message("No `sample_column` provided")
})




test_that("read_data train.red works", {
    # devtools::load_all()
    file_path <- system.file("extdata/train.red_interval_example.csv",
                             package = "mNIRS")

    expect_warning(
        df <- read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 unfiltered",
                             smo2_right = "SmO2 unfiltered"),
            sample_column = c(time = "Timestamp (seconds passed)"),
            verbose = TRUE),
        "non-sequential or repeating values") |>
        expect_message("Estimated sample rate")

    expect_silent(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 unfiltered",
                             smo2_right = "SmO2 unfiltered"),
            sample_column = c(time = "Timestamp (seconds passed)"),
            verbose = FALSE)
    )

    expect_s3_class(df, "mNIRS.data")
    expect_s3_class(df, "data.frame")
    expect_type(df$time, "double")
    expect_gte(df$time[1], 0)

    expect_equal(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2 unfiltered",
                             smo2_right = "SmO2 unfiltered"),
            sample_column = c(time = "Timestamp (seconds passed)"),
            time_from_zero = TRUE,
            verbose = FALSE)$time[1],
    0)

    expect_true(
        all(c("nirs_columns", "sample_column", "sample_rate") %in%
                names(attributes(df))))

    expect_equal(attr(df, "sample_rate"), 10)

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
        "Duplicated input column names detected") |>
        expect_message("Adding an `index` column by row number") |>
        expect_message("Sample rate set to")
})



test_that("read_data oxysoft works", {
    # devtools::load_all()
    file_path <- system.file("extdata/oxysoft_interval_example.xlsx",
                             package = "mNIRS")

    expect_length(
        df <- read_data(
            file_path = file_path,
            nirs_columns = c(HHb_VL = 5,
                             O2Hb_VL = 6),
            sample_column = c(sample = 1),
            verbose = FALSE),
        4)

    expect_type(df$sample, "double")
    expect_type(df$time, "double")
    expect_equal(df$sample[1:10]/50, df$time[1:10])
    expect_s3_class(df, "mNIRS.data")
    expect_s3_class(df, "data.frame")

    expect_true(
        all(c("nirs_columns", "sample_column", "sample_rate") %in%
                names(attributes(df))))

    expect_equal(attr(df, "sample_rate"), 50)

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
        "Duplicated input column names detected") |>
        expect_message("Oxysoft detected sample rate") |>
        expect_message("`time` column in seconds added")

    expect_message(
        read_data(
            file_path = file_path,
            nirs_columns = c(HHb_VL = 5,
                             ICG_VL = 6),
            sample_column = NULL,
            verbose = TRUE),
        "No `sample_column` provided") |>
        expect_message("Oxysoft detected sample rate") |>
        expect_message("`time` column in seconds added")
})




test_that("read_data VMPro app works", {
    # devtools::load_all()
    file_path <- system.file("extdata/vo2master_moxyunit_example.xlsx",
                             package = "mNIRS")

    expect_length(
        df <- read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2[%]",
                             smo2_right = "SmO2 -  2[%]"),
            sample_column = c(time = "Time[utc]"),
            time_to_numeric = FALSE,
            keep_all = TRUE,
            verbose = FALSE),
        5)

    expect_s3_class(df, "mNIRS.data")
    expect_s3_class(df, "data.frame")
    expect_s3_class(df$time, "POSIXct")
    expect_false(all(class(df$time) %in% "numeric"))

    df <- read_data(
        file_path = file_path,
        nirs_columns = c(smo2_left = "SmO2[%]",
                         smo2_right = "SmO2 -  2[%]"),
        sample_column = c(time = "Time[utc]"),
        time_to_numeric = TRUE,
        keep_all = TRUE,
        verbose = FALSE)

    expect_true(all(class(df$time) %in% "numeric"))
    expect_false(all(class(df$time) %in% "POSIXct"))

    expect_true(
        all(c("nirs_columns", "sample_column", "sample_rate") %in%
                names(attributes(df))))

    expect_equal(attr(df, "sample_rate"), 1)

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
        "Duplicated input column names detected") |>
        expect_message("Estimated sample rate = ")

    expect_message(
        read_data(
            file_path = file_path,
            nirs_columns = c(smo2_left = "SmO2[%]",
                             smo2_right = "SmO2 -  2[%]"),
            sample_column = NULL,
            verbose = TRUE),
        "Adding an `index` column by row number") |>
        expect_message("Sample rate set to")
})




