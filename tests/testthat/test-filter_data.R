test_that("filter_data smooth-spline works", {
    devtools::load_all()
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mNIRS")

    df <- read_data(
        file_path = file_path,
        nirs_columns = c(smo2 = "SmO2 Live(2)"),
        sample_column = c(time = "hh:mm:ss"),
        verbose = FALSE
    )

    smo2 <- df$smo2

    ## return numeric vector
    expect_type(filter_data(smo2, "smooth-spline", verbose = FALSE), "double")
    expect_type(filter_data(smo2, "butterworth", W = 0.02), "double")
    expect_type(filter_data(smo2, "moving-average", width = 10), "double")

    ## output length equals input length
    expect_length(filter_data(smo2, "smooth-spline", verbose = FALSE), length(smo2))
    expect_length(filter_data(smo2, "butterworth", W = 0.02), length(smo2))
    expect_length(filter_data(smo2, "moving-average", width = 10), length(smo2))

    ## reduces variance
    smooth_spline <- filter_data(smo2, "smooth-spline", verbose = FALSE)
    butterworth <- filter_data(smo2, "butterworth", W = 0.02)
    moving_avg <- filter_data(smo2, "moving-average", width = 10)

    expect_lt(var(smooth_spline), var(smo2))
    expect_lt(var(butterworth), var(smo2))
    expect_lt(var(moving_avg), var(smo2))

    ## invalid method
    expect_error(filter_data(smo2, "invalid-method"))
    ## smooth-spline spar
    expect_message(filter_data(smo2, "smooth-spline", verbose = TRUE),
                   "`spar` set to")

    ## handle NA
    df_na <- read_data(
        file_path = file_path,
        nirs_columns = c(smo2 = "SmO2 Live"),
        sample_column = c(time = "hh:mm:ss"),
        verbose = FALSE
    )

    smo2_na <- df_na$smo2

    smooth_spline <- filter_data(smo2_na, "smooth-spline", verbose = FALSE, na.rm = TRUE)
    butterworth <- filter_data(smo2_na, "butterworth", W = 0.02, na.rm = TRUE)
    moving_avg <- filter_data(smo2_na, "moving-average", width = 10, na.rm = TRUE)

    expect_lt(var(smooth_spline, na.rm = TRUE), var(smo2_na, na.rm = TRUE))
    expect_lt(var(butterworth, na.rm = TRUE), var(smo2_na, na.rm = TRUE))
    expect_lt(var(moving_avg, na.rm = TRUE), var(smo2_na, na.rm = TRUE))

    expect_error(filter_data(smo2_na, "smooth-spline", verbose = FALSE, na.rm = FALSE),
                 "missing or infinite values in inputs")
    expect_error(filter_data(smo2_na, "butterworth", W = 0.02, na.rm = FALSE),
                 "time series contains internal NAs")
    expect_type(filter_data(smo2_na, "moving-average", width = 10, na.rm = FALSE),
                "double")

    expect_no_error(filter_data(smo2_na, "smooth-spline", verbose = FALSE, na.rm = TRUE))
    expect_no_error(filter_data(smo2_na, "butterworth", W = 0.02, na.rm = TRUE))
    expect_no_error(filter_data(smo2_na, "moving-average", width = 10, na.rm = TRUE))
    expect_type(filter_data(smo2_na, "smooth-spline", verbose = FALSE, na.rm = TRUE),
                "double")
    expect_type(filter_data(smo2_na, "butterworth", W = 0.02, na.rm = TRUE),
                "double")
    expect_type(filter_data(smo2_na, "moving-average", width = 10, na.rm = TRUE),
                "double")

    ## edge cases
    expect_error(filter_data(c()), "`x` must be a")
    expect_error(filter_data(c(1), "smooth-spline"),
                 "'tol' must be strictly positive and finite")
    expect_type(filter_data(c(1), "butterworth", W = 0.02), "double")
    expect_type(filter_data(c(1), "moving-average", width = 10), "double")
    expect_equal(filter_data(smo2, "moving-average", width = 1), smo2)

    ## reproducibility
    result1 <- filter_data(smo2, "smooth-spline", verbose = FALSE)
    result2 <- filter_data(smo2, "smooth-spline", verbose = FALSE)
    expect_equal(result1, result2)

    ## visual check
    # plot(df_na) +
    #     scale_colour_manual(breaks = c("smo2", "smooth_spline",
    #                                    "butterworth", "moving-average"),
    #                         values = mNIRS_palette(4)) +
    #     geom_line(aes(y = smooth_spline, colour = "smooth_spline")) +
    #     geom_line(aes(y = butterworth, colour = "butterworth")) +
    #     geom_line(aes(y = moving_avg, colour = "moving-average"))
})
