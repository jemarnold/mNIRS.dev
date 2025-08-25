test_that("shift_data shifts by constant value", {
    data <- data.frame(time = 1:5,
                       ch1 = c(10, 20, 30, 40, 50),
                       ch2 = c(5, 15, 25, 35, 45))
    result <- shift_data(data, list("ch1", "ch2"), "time", shift_by = 5)

    expect_equal(result$ch1, data$ch1+5)
    expect_equal(result$ch2, data$ch2+5)
})

test_that("shift_data shifts to minimum position", {
    data <- data.frame(time = 1:5, ch1 = c(10, 5, 15, 20, 25))
    result <- shift_data(data, list("ch1"), "time", shift_to = 0, position = "minimum")

    expect_equal(min(result$ch1), 0)
    ## only one value should equal zero
    expect_equal(sum(result$ch1 %in% 0), 1)
})

test_that("shift_data shifts to maximum position", {
    data <- data.frame(time = 1:5, ch1 = c(10, 5, 15, 20, 25))
    result <- shift_data(data, list("ch1"), "time", shift_to = 100, position = "maximum")

    expect_equal(max(result$ch1), 100)

    ## only one value should equal 100
    expect_equal(sum(result$ch1 %in% 100), 1)
})

test_that("shift_data shifts to first position with span", {
    data <- data.frame(time = seq(0, 4, by = 1), ch1 = c(10, 20, 30, 40, 50))
    result <- shift_data(data, nirs_channels = list("ch1"), sample_channel = "time",
                         shift_to = 0, span = 2, position = "first")

    ## data[1] + span = time %in% c(0, 1, 2)
    ## mean of ch1 = c(10, 20, 30) is shifted to 0
    expect_equal(mean(result$ch1[1:3]), 0)
})

test_that("shift_data handles unevenly sampled data with span", {
    set.seed(13)
    # 0.5 Hz target (2 second intervals) with ±0.1s error
    base_times <- seq(0, 10, by = 2)
    time_error <- runif(length(base_times), -0.1, 0.1)

    data <- data.frame(
        time = round(base_times + time_error, 2),
        ch1 = c(10, 5, 20, 15, 25, 30)
    )

    # Test with 4-second span (should include ~2 samples per window)
    result <- shift_data(data, list("ch1"), sample_channel = "time",
                         shift_to = 0, span = 4, position = "first")

    # First window should average first ~2 values within 4s of start
    first_window_idx <- which(data$time <= data$time[1] + 4)
    expected_mean <- mean(data$ch1[first_window_idx])

    expect_equal(result$ch1[1], data$ch1[1] - expected_mean + 0)
    expect_true(sum(data$time <= 4) >= 2)
    expect_equal(which(data$time <= 4), first_window_idx)

    ## unequal sampling and position = "minimum"
    result_min <- shift_data(data, list("ch1"), sample_channel = "time",
                         shift_to = 0, span = 4, position = "minimum")

    x <- data$time
    min_mean <- Inf
    idx <- NA_real_
    for (i in seq_along(x)) {
        window_idx <- which(x >= x[i] & x <= x[i] + 4)
        if (length(window_idx) > 1) {
            # print(window_idx)
            window_mean <- mean(data$ch1[window_idx])
            # print(window_mean)
            min_mean <- min(min_mean, window_mean)
            if (min_mean == window_mean) idx <- window_idx
        }
    }
    expect_equal(mean(data$ch1[idx]) - mean(result_min$ch1[idx]), min(data$ch1))
})

test_that("shift_data handles multiple channel groups", {
    data <- data.frame(time = 1:2, ch1 = c(10, 20), ch2 = c(15, 25), ch3 = c(5, 35))
    channels <- list(c("ch1", "ch2"), "ch3")
    result <- shift_data(data, channels, "time", shift_to = 0)

    ## check grouping together: min shuold come from ch1 and ch3
    expect_true(any(result$ch1 == 0, na.rm = TRUE))
    expect_false(any(result$ch2 == 0, na.rm = TRUE))
    expect_true(any(result$ch3 == 0, na.rm = TRUE))
    ## check both shifted together maintaining relative scaling
    expect_equal(result$ch1 - result$ch2, data$ch1 - data$ch2)
    ## check both shifted independently
    expect_false(isTRUE(all.equal(result$ch1 - result$ch3,
                                  data$ch1 - data$ch3)))
})

test_that("shift_data preserves non-channel columns", {
    data <- data.frame(time = 1:3, ch1 = c(10, 20, 30), other = c("A", "B", "C"))
    result <- shift_data(data, list("ch1"), "time", shift_by = 5)

    expect_equal(result$time, c(1, 2, 3))
    expect_equal(result$other, c("A", "B", "C"))
    expect_equal(result$ch1, c(15, 25, 35))
})

test_that("shift_data validates position argument", {
    data <- data.frame(ch1 = c(10, 20))
    expect_error(shift_data(data, list("ch1"), position = "invalid"),
                 "'arg' should be one of")
})

test_that("shift_data handles empty channel list", {
    data <- data.frame(time = 1:3, value = c(10, 20, 30))
    expect_error(shift_data(data, list(), shift_by = 5),
                 "`nirs_channels` should be defined and grouped explicitly")
})





test_that("shift_data works on Moxy", {
    # devtools::load_all()
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mNIRS")

    df <- read_data(
        file_path = file_path,
        nirs_channels = c(smo2_left = "SmO2 Live",
                          smo2_right = "SmO2 Live(2)"),
        sample_channel = c(time = "hh:mm:ss"),
        verbose = FALSE
    ) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::matches("smo2"), \(.x)
                replace_invalid(.x, c(0, 100))
            )
        )

    df_shifted <- shift_data(
        df,
        nirs_channels = c("smo2_left", "smo2_right"),
        sample_channel = NULL,
        shift_to = 0,
        shift_by = NULL,
        span = 0,
        position = c("minimum", "maximum", "first")
    )

    # plot(df) + ggplot2::ylim(0, 100)
    # plot(df_shifted) + ggplot2::ylim(0, 100)

    ## check grouping together: min value should come from smo2_right
    expect_false(any(df_shifted$smo2_left == 0, na.rm = TRUE))
    expect_true(any(df_shifted$smo2_right == 0, na.rm = TRUE))
    ## check both shifted together maintaining relative scaling
    expect_equal(df_shifted$smo2_left - df_shifted$smo2_right,
                 df$smo2_left - df$smo2_right)

})

test_that("shift_data works on Train.Red", {
    # devtools::load_all()
    file_path <- system.file("extdata/train.red_interval_example.csv",
                             package = "mNIRS")

    df <- read_data(
        file_path = file_path,
        nirs_channels = c(smo2_left = "SmO2",
                          smo2_right = "SmO2",
                          dhb_left = "HBDiff",
                          dhb_right = "HBDiff"),
        sample_channel = c(time = "Timestamp (seconds passed)"),
        verbose = FALSE,
        keep_all = TRUE,
    )

    df_shifted <- shift_data(
        df,
        nirs_channels = list("smo2_left", "smo2_right", c("dhb_left", "dhb_right")),
        sample_channel = NULL,
        shift_to = 0,
        shift_by = NULL,
        span = 0,
        position = c("minimum", "maximum", "first")
    )

    # plot(df) + ggplot2::ylim(0, 100)
    # plot(df_shifted) + ggplot2::ylim(0, 100)

    ## check grouping together: min value should come from each group
    expect_true(any(df_shifted$smo2_left == 0, na.rm = TRUE))
    expect_true(any(df_shifted$smo2_right == 0, na.rm = TRUE))
    expect_true(any(df_shifted$dhb_left == 0, na.rm = TRUE))
    expect_false(any(df_shifted$dhb_right == 0, na.rm = TRUE))
    ## check both shifted together maintaining relative scaling
    expect_equal(df_shifted$dhb_left - df_shifted$dhb_right,
                 df$dhb_left - df$dhb_right)
    ## check both shifted independently
    expect_false(isTRUE(all.equal(df_shifted$smo2_left - df_shifted$smo2_right,
                                  df$smo2_left - df$smo2_right)))

})
