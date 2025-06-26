test_that("read_data moxy.perfpro works", {
    file_path <- system.file("extdata", "moxy_ramp_example.xlsx", package = "mNIRS")

    df.moxy.perfpro <- read_data(
        file_path = file_path,
        nirs_columns = c(smo2_left = "smo2_left_VL",
                         smo2_right = "smo2_right_VL"),
        sample_column = c(time = "Time"),
        event_column = c(event = "Event"),
        .keep_all = FALSE,
        .verbose = FALSE)

    ## expect class
    expect_s3_class(df, "mNIRS.data")
    ## expect sample rate equal to 2
    expect_equal(attr(df, "sample_rate"), 2)
    ## expect known warning about repeating time values
    expect_warning(df, "non-sequential or repeating")
})
