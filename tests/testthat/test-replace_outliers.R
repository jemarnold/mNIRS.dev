test_that("replace_outliers works correctly", {
    # devtools::load_all()
    x <- c(1, 2, 3, 16, 5, 6, 7)
    result <- replace_outliers(x, width = 3)
    expect_equal(result[4], median(x))

    ## no outliers
    x_clean <- 1:7
    result_clean <- replace_outliers(x_clean, width = 3)
    expect_equal(result_clean, x_clean)

    ## edge cases
    expect_error(replace_outliers(c(1), width = 1),
                 "`width` must be half the length")
    expect_equal(sum(replace_outliers(rep(1, 7), width = 3)), 7)
    expect_error(replace_outliers(rep(NA, 7), width = 3),
                 "`x` must be a")

    ## na.rm NA handling
    x_na <- c(1, 2, 3, NA, 5, 6, 7)
    expect_error(replace_outliers(x_na, width = 3, na.rm = FALSE),
                 "missing value where TRUE/FALSE needed")
    expect_true(is.na(replace_outliers(x_na, width = 3, na.rm = TRUE)[4]))

    x_na <- c(1, 2, NA, 35, 5, 6, 7)
    expect_error(replace_outliers(x_na, width = 1, na.rm = FALSE),
                 "missing value where TRUE/FALSE needed")
    expect_true(is.na(replace_outliers(x_na, width = 3, na.rm = TRUE)[3]))
    expect_equal(replace_outliers(x_na, width = 3, na.rm = TRUE)[4],
                 median(x_na, na.rm = TRUE))

})




# (\(y_length = 1000, n = 100, na_prop = 0.1) { ## benchmark function
#     start_time <- Sys.time()
#     ## create a vector with noise
#     x <- seq_len(y_length)
#     y <- rnorm(1, 0, 2) * x + rnorm(y_length, mean = 0, sd = 20)
#     y[sample(length(y), round(y_length * na_prop))] <- NA
#     ## add random outliers
#     n_outliers <- round(y_length * na_prop/2)
#     outlier_idx <- sample(length(y), n_outliers)
#     outlier_values <- rnorm(n_outliers, mean = 0, sd = sd(y, na.rm = TRUE) * 5)
#     y[outlier_idx] <- y[outlier_idx] + outlier_values
#     x <- y
#
#     f1 <- \() {}
#     f2 <- \() {}
#
#     val1 <- as.vector(replicate(n, f1()))
#     val2 <- as.vector(replicate(n, f2()))
#     MAE <- round(mean(abs(val1 - val2), na.rm = TRUE), 10)
#
#     t1 <- system.time(replicate(n, f1()))["elapsed"]
#     t2 <- system.time(replicate(n, f2()))["elapsed"]
#
#     times <- c(t1/n, t2/n)
#     faster <- which.min(times)
#     fun_names <- c("f1()", "f2()")
#     faster_fun <- fun_names[faster]
#
#     total_time <- Sys.time() - start_time
#
#     cli::cli_alert_success("{.arg {faster_fun}} is faster by ")
#     cli::cli_alert_info("Reps = {.val {n}}")
#     cli::cli_alert_info("Total runtime = {.val {round(as.numeric(total_time, units = 'secs'), 3)}} s")
#
#     data.frame(fun = fun_names,
#                mean = c(mean(val1, na.rm = TRUE), mean(val2, na.rm = TRUE)),
#                MAE = MAE,
#                time = sprintf("%d ms — %d%%",
#                               round(times*1000),
#                               round(c(t1/t2, t2/t1)*100)))
# })()
