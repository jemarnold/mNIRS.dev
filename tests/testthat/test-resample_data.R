# devtools::load_all()
test_that("resample_data passthrough works", {
    data <- data.frame(time = 1:10, value = rnorm(10))
    expect_equal(resample_data(data, "time", 1, NULL, verbose = FALSE), data)
    expect_equal(resample_data(data, "time", 1, 0), data)
    expect_equal(resample_data(data, "time", 1, 10, -1), data)
})

test_that("resample_data validates inputs", {
    data <- data.frame(time = 1:10, value = rnorm(10))

    # Missing channel
    expect_error(resample_data(data, "missing_col", 1, 2),
                 "`sample_channel` not found.")

    # Non-numeric time channel
    data$time <- letters[1:10]
    expect_error(resample_data(data, "time", 1, 2),
                 "must be a.*numeric.*vector")

    # Invalid sample rates
    data$time <- 1:10
    resample_data(data, "time", 0, 2) |>
        expect_message("`sample_rate` should be defined explicitly") |>
        expect_message("Estimated `sample_rate` = ") |>
        expect_message("Output is resampled at")
})

test_that("resample_data upsamples correctly", {
    data <- data.frame(
        time = c(0, 1, 2),
        value = c(10, 20, 30)
    )

    result <- resample_data(data, "time", 1, 4, verbose = FALSE)

    expect_equal(nrow(result), 9)  # 0 to 2 at 0.25 intervals
    expect_equal(result$time, seq(0, 2, by = 0.25))
    expect_equal(result$value[1], 10)
    expect_equal(result$value[5], 20)
    expect_equal(result$value[9], 30)
})

test_that("resample_data downsamples correctly", {
    data <- data.frame(
        time = seq(0, 2, by = 0.1),
        value = seq(10, 30, length.out = 21)
    )

    result <- resample_data(data, "time", 10, 1, verbose = FALSE)

    expect_equal(nrow(result), 3)  # 0, 1, 2
    expect_equal(result$time, c(0, 1, 2))
    expect_equal(result$value[1], 10)
    expect_equal(result$value[2], 20)
    expect_equal(result$value[3], 30)
})

test_that("resample_data handles resample_rate == sample_rate", {
    data <- data.frame(time = 1:3, value = c(10, 20, 30))
    result <- resample_data(data, "time", 1, 1, verbose = FALSE)
    expect_equal(result, data, ignore_attr = TRUE)

    data <- data.frame(
        time = c(1:2, 2, 3:9, 9, 10:17, 17, 18:21)/10+0.1,
        value = seq(10, by = 1, len = 24)
    )



    expect_warning(result <- resample_data(data, "time", 10, 10, verbose = FALSE),
                   "collapsing to unique 'x' values")
    expect_equal(range(result$time), floor(range(data$time)*10)/10)
    expect_equal(result$value[2], mean(data$value[2:3]))
    expect_equal(result$value[9], mean(data$value[10:11]))
    expect_equal(result$value[17], mean(data$value[19:20]))
})

test_that("resample_data handles multiple numeric columns", {
    data <- data.frame(
        time = c(0, 1, 2),
        value1 = c(10, 20, 30),
        value2 = c(5, 15, 25)
    )

    result <- resample_data(data, "time", 1, 2, verbose = FALSE)

    expect_equal(ncol(result), 3)
    expect_true(all(c("time", "value1", "value2") %in% names(result)))
    expect_equal(result$value1[1], 10)
    expect_equal(result$value1[3], 20)
    expect_equal(result$value1[5], 30)
    expect_equal(result$value2[1], 5)
    expect_equal(result$value2[3], 15)
    expect_equal(result$value2[5], 25)
})

test_that("resample_data handles categorical columns", {
    data <- data.frame(
        time = c(0, 1, 2),
        value = c(10, 20, 30),
        category = c("A", "B", "C")
    )

    result <- resample_data(data, "time", 1, 4, verbose = FALSE)

    expect_true("category" %in% names(result))
    expect_equal(result$category[1], "A")  # forward fill
    expect_equal(result$category[5], "B")  # at t=1
    expect_equal(result$category[9], "C")  # at t=2

    result <- resample_data(data, "time", 1, 0.5, verbose = FALSE)

    expect_true("category" %in% names(result))
    expect_equal(result$category[1], "A")  # forward fill
    expect_equal(result$category[2], "C")  # at t=2
})

test_that("resample_data handles edge cases", {
    # Single row
    data <- data.frame(time = 1, value = 10)
    expect_error(resample_data(data, "time", 1, 2, verbose = FALSE),
                 "needs at least two non-NA values")
})


test_that("resample_data works on Moxy", {
    # devtools::load_all()
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mNIRS")

    df <- read_data(
        file_path = file_path,
        nirs_channels = c(smo2 = "SmO2 Live(2)"),
        sample_channel = c(time = "hh:mm:ss"),
        verbose = FALSE
    )[1:15, ]

    df$time <- df$time+0.01

    ## works with metadata
    expect_message(result <- resample_data(df, resample_rate = 1),
                   "`sample_rate` = .*2.*Hz") |>
        expect_message("Output is resampled at .*1.*Hz")
    expect_equal(result$time, 0:7)
    expect_s3_class(result, "mNIRS.data")

    ## time-weighted average
    df2 <- df |>
        dplyr::mutate(
            diff = c(diff(time), tail(diff(time), 1)),
            time = floor(time * 1) / 1,
        ) |>
        dplyr::summarise(
            .by = time,
            smo2 = stats::weighted.mean(smo2, diff, na.rm = TRUE)
        )

    ## expect close enough to time-weighted average
    expect_equal(result, df2, ignore_attr = TRUE,tolerance = 2)

    ## should overwrite metadata
    df3 <- resample_data(df, sample_rate = 2, resample_rate = 1.1, verbose = FALSE)
    expect_equal(attributes(df3)$sample_rate, 1.1)
})
