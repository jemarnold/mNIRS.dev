test_that("plot.mNIRS.data moxy.perfpro works", {
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mNIRS")

    df.moxy.perfpro <- read_data(
        file_path = file_path,
        nirs_columns = c(smo2_left = "SmO2 Live",
                         smo2_right = "SmO2 Live(2)"),
        verbose = FALSE)

    expect_silent(plot(df.moxy.perfpro))

    expect_s3_class(plot(df.moxy.perfpro), "gg")
})


test_that("plot.mNIRS.data train.red works", {
    file_path <- system.file("extdata/train.red_interval_example.csv",
                             package = "mNIRS")

    train.red <- read_data(
        file_path = file_path,
        nirs_columns = c(smo2_left = "SmO2 unfiltered",
                         smo2_right = "SmO2 unfiltered"),
        verbose = FALSE)

    expect_silent(plot(train.red))

    expect_s3_class(plot(train.red), "gg")
})
