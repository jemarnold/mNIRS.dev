## slope
test_that("slope returns correct structure", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- slope(y)

    expect_type(result, "double")
    expect_equal(length(result), 1)
    expect_gt(result, 0)
})

test_that("slope calculates slopes correctly", {
    y <- c(1, 3, 5, 7, 9)
    result_null <- slope(y)
    result_seq <- slope(y, x = seq_along(y))
    expect_equal(result_null, result_seq)

    expect_gt(result_null, 0)

    ## horizontal
    expect_equal(slope(1:5, 1:5), 1)
    ## single value
    expect_true(is.na(slope(5)))
    ## all identical values
    expect_true(slope(rep(5, 10)) == 0)
    ## all NA
    expect_true(is.na(slope(rep(NA, 5))))
    ## all x values identical
    ## TODO 2025-08-13 This should probably error to NA?
    # expect_true(is.na(slope(1:5, rep(1, 5))))
    expect_true(slope(1:5, rep(1, 5)) == 0)

    ## all invalid
    expect_true(is.na(slope(list())))
    expect_true(is.na(slope(NULL)))
    expect_true(is.na(slope(rep(NA, 4))))
    expect_true(is.na(slope(rep(NaN, 4))))
    expect_true(is.na(slope(rep(Inf, 4))))
})

test_that("slope handles NA and returns same as lm model", {
    y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
    result <- slope(y)
    lm_result <- lm(y ~ seq_along(y))

    expect_equal(result, coef(lm_result)[[2]])

    ## boundary NA
    y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, NA)
    result <- slope(y)
    lm_result <- lm(y ~ seq_along(y))

    expect_equal(result, coef(lm_result)[[2]])

    # ggplot(tibble(x = seq_along(y), y = y)) +
    #     aes(x, y) +
    #     scale_x_continuous(breaks = scales::breaks_pretty(n=10)) +
    #     geom_point(shape = 21, size = 3, stroke = 1) +
    #     ggplot2::geom_abline(intercept = coef(lm_result)[[1]],
    #                          slope = coef(lm_result)[[2]])

    ## speed test
    speed_result <- (\(y_length = 100, n_reps = 500, na_prop = 0.1) {
        x <- 1:y_length
        y <- rnorm(1, 0, 2) * x + rnorm(y_length, mean = 0, sd = 2)
        y[sample(length(y), round(y_length * na_prop))] <- NA

        f1 <- \() coef(lm(y ~ seq_along(y)))[[2]]
        f2 <- \() slope(y)

        val1 <- as.vector(replicate(n_reps, f1()))
        val2 <- as.vector(replicate(n_reps, f2()))
        MAE <- round(mean(abs(val1 - val2), na.rm = TRUE), 10)

        t1 <- system.time(replicate(n_reps, f1()))["elapsed"]
        t2 <- system.time(replicate(n_reps, f2()))["elapsed"]

        data.frame(
            method = c("lm", "slope"),
            mean = c(mean(val1, na.rm = TRUE), mean(val2, na.rm = TRUE)),
            MAE = MAE,
            time_ms = round(c(t1, t2) / n_reps * 1000, 2),
            speedup = round(c(t2/t1, t1/t2), 1))
    })()

    expect_equal(speed_result$mean[[1]], speed_result$mean[[2]])
    expect_equal(speed_result$MAE[[1]], 0)
    expect_gte(speed_result$time_ms[[1]], speed_result$time_ms[[2]])
})










## rolling_slope
test_that("rolling_slope returns correct structure", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- rolling_slope(y, width = 3)

    expect_type(result, "double")
    expect_equal(length(result), length(y))
    expect_true(all(result >= 0))
})

test_that("rolling_slope calculates slopes correctly", {
    y <- c(1, 3, 5, 7, 9)
    result_null <- rolling_slope(y, width = 3)
    result_seq <- rolling_slope(y, x = seq_along(y), width = 3)
    expect_equal(result_null, result_seq)

    expect_true(all(result_null > 0))

    ## horizontal
    expect_true(all(rolling_slope(1:5, 1:5, width = 3) == 1))
    ## single value returns 0
    ## TODO 2025-08-13 or NA?
    expect_equal(rolling_slope(5, width = 3), 0)
    ## two values
    result <- rolling_slope(c(1, 3), width = 3)
    expect_length(result, 2)
    expect_equal(diff(result), 0)
    ## all identical values
    expect_true(all(rolling_slope(rep(5, 10), width = 3) == 0))
    ## all NA
    expect_true(is.na(rolling_slope(rep(NA, 5), width = 3)))
    ## all x values identical
    ## TODO 2025-08-13 This should probably error to NA?
    # expect_true(all(is.na(rolling_slope(y = 1:5, x = rep(1, 5), width = 3))))
    expect_true(all(rolling_slope(y = 1:5, x = rep(1, 5), width = 3) == 0))
})

test_that("rolling_slope returns same as lm model", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
    result <- rolling_slope(y, width = 3)
    x <- seq_along(y)
    width <- 3
    n <- length(y)
    lm_result <- sapply(1:length(y), \(.x) {
        start_x <- max(x[1], .x - width/2)
        end_x <- min(x[n], .x + width/2)
        window_idx <- which(x >= start_x & x <= end_x)
        ## exception from lm() NA handling for all NA
        if (sum(!is.na(y[window_idx])) > 1) {
            coef(lm(y[window_idx] ~ x[window_idx]))[[2]]
        } else {NA}
    })

    expect_equal(result, lm_result)

    # ggplot(tibble(x = seq_along(y), y = y)) +
    #     aes(x, y) +
    #     scale_x_continuous(breaks = scales::breaks_pretty(n=10)) +
    #     geom_line() +
    #     geom_point(aes(y = result, colour = "rolling_slope"),
    #                shape = 21, size = 5, stroke = 1) +
    #     geom_point(aes(y = lm_result, colour = "lm"),
    #                shape = 21, size = 3, stroke = 1)

    # ## speed test
    # speed_result <- (\(y_length = 100, n_reps = 10, na_prop = 0.1) {
    #     x <- 1:y_length
    #     y <- rnorm(1, 0, 2) * x + rnorm(y_length, mean = 0, sd = 2)
    #     # y[sample(length(y), round(y_length * na_prop))] <- NA
    #     width <- 10
    #
    #     f1 <- \() sapply(1:length(y), \(.x) {
    #         start_x <- max(x[1], .x - width/2)
    #         end_x <- min(x[y_length], .x + width/2)
    #         window_idx <- which(x >= start_x & x <= end_x)
    #         ## exception from lm() NA handling for all NA
    #         if (sum(!is.na(y[window_idx])) > 1) {
    #             coef(lm(y[window_idx] ~ x[window_idx]))[[2]]
    #         } else {NA}
    #     })
    #
    #     f2 <- \() rolling_slope(y, width = width)
    #
    #     val1 <- as.vector(replicate(n_reps, f1()))
    #     val2 <- as.vector(replicate(n_reps, f2()))
    #     MAE <- round(mean(abs(val1 - val2), na.rm = TRUE), 10)
    #
    #     t1 <- system.time(replicate(n_reps, f1()))["elapsed"]
    #     t2 <- system.time(replicate(n_reps, f2()))["elapsed"]
    #
    #     data.frame(
    #         method = c("lm", "rolling_slope"),
    #         mean = c(mean(val1, na.rm = TRUE), mean(val2, na.rm = TRUE)),
    #         MAE = MAE,
    #         time_ms = round(c(t1, t2) / n_reps * 1000, 2),
    #         speedup = round(c(t2/t1, t1/t2), 1))
    # })()
    #
    # expect_equal(speed_result$mean[[1]], speed_result$mean[[2]])
    # expect_equal(speed_result$MAE[[1]], 0)
    # expect_gte(speed_result$time_ms[[1]], speed_result$time_ms[[2]])
})

test_that("rolling_slope returns same as zoo::rollapply()", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)

    ## returns 0 for one valid y value
    coef.fn <- \(y) coef(.lm.fit(cbind(1, seq_along(y)), y))[2]
    # ## returns NA for one valid y value
    # coef.fn <- \(y) coef(lm(y ~ seq_along(y)))[[2]]
    rollapply_center <- zoo::rollapply(y,
                                       FUN = coef.fn,
                                       width = 3,
                                       align = "center",
                                       partial = TRUE)
    rollapply_left <- zoo::rollapply(y,
                                     FUN = coef.fn,
                                     width = 4, ## correct for width in units of x
                                     align = "left",
                                     partial = TRUE)
    rollapply_right <- zoo::rollapply(y,
                                      FUN = coef.fn,
                                      width = 4, ## correct for width in units of x
                                      align = "right",
                                      partial = TRUE)
    rolling_slope_center <- rolling_slope(y, width = 3, align = "center")
    rolling_slope_left <- rolling_slope(y, width = 3, align = "left")
    rolling_slope_right <- rolling_slope(y, width = 3, align = "right")

    expect_equal(rollapply_center, rolling_slope_center)
    expect_equal(rollapply_left, rolling_slope_left)
    expect_equal(rollapply_right, rolling_slope_right)
})

test_that("rolling_slope handles different alignments", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)

    result_center <- rolling_slope(y, width = 3, align = "center")
    result_left <- rolling_slope(y, width = 3, align = "left")
    result_right <- rolling_slope(y, width = 3, align = "right")

    expect_length(result_center, length(y))
    expect_length(result_left, length(y))
    expect_length(result_right, length(y))

    ## different align should give different result
    expect_false(identical(result_center, result_left))
    expect_false(identical(result_center, result_right))
    expect_equal(tail(result_right, 7), head(result_left, 7))
})

test_that("rolling_slope handles NA with na.rm", {
    y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)

    ## 2025-08-13 na.rm = FALSE returns NA where any local y is NA
    result <- rolling_slope(y, width = 3, na.rm = FALSE)
    expect_true(all(which(is.na(result)) %in% c(2:4, 8:12)))

    ## 2025-08-13 na.rm = TRUE returns NA where target y is NA
    result_rmna <- rolling_slope(y, width = 3, na.rm = TRUE)
    expect_true(all(which(is.na(result_rmna)) %in% c(3, 9:11)))
})

test_that("rolling_slope handles width in units of x", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    x_idx <- seq_along(y)
    x_5hz <- x_idx/5

    expect_equal(rolling_slope(y, x_idx, width = 3),
                 rolling_slope(y, x_5hz, width = 3/5)/5)

    idx_unequal <- c(0.5, 1.5, 3, 3.5, 4, 5.5, 7, 8, 10, 11)
    n <- length(y)
    width <- 3
    rolling_slope_result <- rolling_slope(y, idx_unequal, width = 3)

    lm_result <- sapply(
        1:n, \(.x) {
            current_x <- idx_unequal[.x]
            start_x <- max(idx_unequal[1], current_x - width / 2)
            end_x <- min(idx_unequal[n], current_x + width / 2)
            window_idx <- which(idx_unequal >= start_x & idx_unequal <= end_x)

            coef(lm(y[window_idx] ~ idx_unequal[window_idx]))[[2]]
        })

    expect_equal(rolling_slope_result, lm_result)

    y <- c(1, 3, 2, 5, 8)
    ## width larger than data
    result <- rolling_slope(y, width = 10)
    expect_length(result, length(y))
    expect_true(var(result) == 0)

    ## width = 1
    result <- rolling_slope(y, width = 1)
    expect_length(result, length(y))
    expect_true(all(result == 0))
})

test_that("rolling_slope partial windows work correctly", {
    y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)

    ## first and last positions should use partial windows
    result <- rolling_slope(y, width = 3, na.rm = FALSE)
    expect_false(is.na(result[1]))
    expect_false(is.na(result[length(y)]))
    expect_equal(result[1], diff(y[1:2]))
    expect_equal(result[2], NA_real_)
    expect_equal(tail(result, 1), diff(tail(y, 2)))
    expect_equal(tail(result, 2)[1], NA_real_)

    result_rmna <- rolling_slope(y, width = 3, na.rm = TRUE)
    expect_false(is.na(result_rmna[1]))
    expect_false(is.na(result_rmna[length(y)]))
    expect_equal(result_rmna[1], diff(y[1:2]))
    expect_equal(result_rmna[2], diff(y[1:2]))
    expect_equal(tail(result_rmna, 1), diff(tail(y, 2)))
    expect_equal(tail(result_rmna, 2)[1], diff(tail(y, 3)[c(2, 3)]))
})

test_that("rolling_slope match.arg works", {
    y <- c(1, 3, 2, 5, 8)

    ## valid align
    expect_no_error(rolling_slope(y, width = 3, align = "center"))

    ## invalid align
    expect_error(rolling_slope(y, width = 3, align = "invalid"),
                 "'arg' should be one of")
})








## peak_slope
test_that("peak_slope returns correct structure", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- peak_slope(y, width = 3)

    expect_type(result, "list")
    expect_named(result, c("x", "y", "slope", "x_fitted", "y_fitted"))
    expect_type(result$x, "double")
    expect_type(result$y, "double")
    expect_type(result$slope, "double")
    expect_type(result$y_fitted, "double")
    expect_type(result$x_fitted, "double")
    expect_true(is.vector(result$y_fitted))
    expect_true(is.vector(result$x_fitted))
    expect_equal(length(result$x_fitted), length(result$y_fitted))
})

test_that("peak_slope calculates slopes correctly", {
    y <- c(1, 3, 5, 7, 9)
    result_null <- peak_slope(y, width = 3)
    result_seq <- peak_slope(y, x = seq_along(y), width = 3)
    expect_equal(result_null, result_seq)

    expect_gt(result_null$slope, 0)
    expect_gte(result_null$x, 1)
    expect_gte(result_null$y, 1)
    expect_lte(diff(result_null$x_fitted), 3) ## width

    y <- c(9, 7, 5, 3, 1)
    result <- peak_slope(y, width = 3)

    expect_lt(result$slope, 0)
    expect_gte(result$x, 1)
    expect_gte(result$y, 1)
    expect_lte(diff(result$x_fitted), 3) ## width

    ## single value returns 0
    ## TODO 2025-08-13 zero or NA?
    expect_error(peak_slope(5, width = 3),
                 "contains insufficient valid data")

    ## horizontal
    result <- peak_slope(rep(5, 10), width = 3)
    expect_true(result$slope == 0)
    expect_true(all(result[c("x", "y", "slope")] %in% c(1, 5, 0)))
    expect_true(all(result$x_fitted %in% 1:2))
    ## all NA
    expect_error(peak_slope(rep(NA, 5), width = 3),
                "contains insufficient valid data")
    ## all x values identical
    ## TODO 2025-08-13 This should probably error to NA?
    # expect_true(all(peak_slope(1:5, rep(1, 5), width = 3) == 0))
})

test_that("peak_slope handles NA values correctly", {
    y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)

    width <- 3
    roll_result <- rolling_slope(y, width = width)
    result <- peak_slope(y, width = width)
    expect_true(!is.na(result$slope))
    expect_true(result$slope > 0)
    expect_type(result$slope, "double")
    expect_gte(result$x, 1)
    expect_lte(diff(range(result$x_fitted)), width)

    # ## visual check
    # ggplot(tibble(x = seq_along(y), y = y)) +
    #     aes(x, y) +
    #     scale_x_continuous(breaks = scales::breaks_pretty(n=10)) +
    #     scale_y_continuous(breaks = scales::breaks_pretty(n=10)) +
    #     geom_line(linewidth = 2) +
    #     geom_line(data = tibble(x = result$x_fitted,
    #                             y = result$y_fitted),
    #               aes(colour = "fitted"), linewidth = 1) +
    #     geom_point(aes(y = roll_result, colour = "rolling"),
    #                shape = 21, size = 3, stroke = 1) +
    #     annotate("point", shape = 21, size = 3, stroke = 1,
    #              x = result$x, y = result$y)


    width <- 4
    roll_result <- rolling_slope(y, width = width)
    result <- peak_slope(y, width = width)
    expect_true(!is.na(result$slope))
    expect_true(result$slope > 0)
    expect_type(result$slope, "double")
    expect_gte(result$x, 1)
    expect_lte(diff(range(result$x_fitted)), width)

    # ## visual check
    # ggplot(tibble(x = seq_along(y), y = y)) +
    #     aes(x, y) +
    #     scale_x_continuous(breaks = scales::breaks_pretty(n=10)) +
    #     scale_y_continuous(breaks = scales::breaks_pretty(n=10)) +
    #     geom_line(linewidth = 2) +
    #     geom_line(data = tibble(x = result$x_fitted,
    #                             y = result$y_fitted),
    #               aes(colour = "fitted"), linewidth = 1) +
    #     geom_point(aes(y = roll_result, colour = "rolling"),
    #                shape = 21, size = 3, stroke = 1) +
    #     annotate("point", shape = 21, size = 3, stroke = 1,
    #              x = result$x, y = result$y)
})

test_that("peak_slope handles width in units of x", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    x_idx <- seq_along(y)
    x_5hz <- x_idx/5

    result_idx <- peak_slope(y, x_idx, width = 3)
    result_5hz <- peak_slope(y, x_5hz, width = 3/5)

    expect_equal(x_5hz[result_idx$x], result_5hz$x)
    expect_equal(result_idx$slope, result_5hz$slope/5)
    expect_equal(x_5hz[result_idx$x_fitted], result_5hz$x_fitted)
    expect_equal(result_idx$y_fitted, result_5hz$y_fitted)

    idx_unequal <- c(0.5, 1.5, 3, 3.5, 4, 5.5, 7, 8, 10, 11)
    n <- length(y)
    width <- 3
    peak_slope_result <- peak_slope(y, idx_unequal, width = 3)
    lm_result <- list(x = NA, slope = NA)

    for (i in 1:n) {
        current_x <- idx_unequal[i]
        start_x <- max(idx_unequal[1], current_x - width / 2)
        end_x <- min(idx_unequal[n], current_x + width / 2)
        window_idx <- which(idx_unequal >= start_x & idx_unequal <= end_x)
        lm_result$x[i] <- current_x
        lm_result$slope[i] <- coef(.lm.fit(
            cbind(1, idx_unequal[window_idx]), y[window_idx]))[2]
    }

    expect_equal(peak_slope_result$slope,
                 lm_result$slope[which.max(lm_result$slope)])
    expect_equal(peak_slope_result$x,
                 lm_result$x[which.max(lm_result$slope)])
})

test_that("peak_slope works with different alignments", {
    # devtools::load_all()
    y <- c(1, 3, 2, 5, 8, 7, 9, 8, 6, 7)
    x <- seq(22, along = y)

    result_center <- peak_slope(y, x, width = 3, align = "center")
    result_left <- peak_slope(y, x, width = 2, align = "left")
    result_right <- peak_slope(y, x, width = 2, align = "right")

    expect_equal(result_center$x, mean(result_center$x_fitted))
    expect_equal(result_center$y, mean(result_center$y_fitted))
    expect_equal(result_left$x, result_left$x_fitted[1])
    expect_equal(result_left$y, result_left$y_fitted[1])
    expect_equal(result_right$x, tail(result_right$x_fitted, 1))
    expect_equal(result_right$y, tail(result_right$y_fitted, 1))

    expect_equal(result_center$x, result_left$x + 1)
    expect_equal(result_center$x, result_right$x - 1)
    expect_equal(result_left$x + 1, result_right$x - 1)

    expect_equal(result_center$slope, result_left$slope)
    expect_equal(result_center$slope, result_right$slope)
    expect_equal(result_left$slope, result_right$slope)
})


