## slope
test_that("slope returns correct structure", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- slope(y)

    expect_type(result, "double")
    expect_equal(length(result), 1)
})

test_that("slope returns same as lm model", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 8, 6, 7)
    x <- seq_along(y)

    slope_result <- slope(y)
    lm_result <- coef(.lm.fit(cbind(1, x), y))[2]

    expect_equal(slope_result, lm_result)
})

test_that("slope works with NULL x", {
    y <- c(1, 3, 5, 7, 9)
    result_null <- slope(y, x = NULL)
    result_seq <- slope(y, x = seq_along(y))

    expect_equal(result_null, result_seq)
})

test_that("slope calculates linear trend correctly", {
    # Perfect linear trend
    y <- c(1, 2, 3, 4, 5)
    x <- c(1, 2, 3, 4, 5)
    result <- slope(y, x)

    # Should be close to 1 for perfect linear trend
    expect_true(all(abs(result[!is.na(result)] - 1) < 1e-10))
})

test_that("slope handles NA values", {
    y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, 14)

    ## na.rm = FALSE
    results_rm_false <- slope(y, na.rm = FALSE)
    expect_true(any(is.na(results_rm_false)))

    ## na.rm = TRUE
    results_rm_true <- slope(y, na.rm = TRUE)
    expect_true(sum(is.na(results_rm_true)) <= sum(is.na(y)))

    expect_equal(results_rm_true[which(!is.na(results_rm_false))],
                 results_rm_false[which(!is.na(results_rm_false))])
})

test_that("slope handles edge cases", {
    ## single value
    expect_true(slope(5) == 0)

    ## all identical values
    expect_true(slope(rep(5, 10)) == 0)

    ## all NA
    expect_true(is.na(slope(rep(NA, 5))))
})

test_that("slope zero denominator handling", {
    # All x values identical
    y <- c(1, 2, 3, 4, 5)
    x <- rep(1, 5)

    expect_true(slope(y, x) == 0)
})








## rolling_slope
test_that("rolling_slope returns correct structure", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- rolling_slope(y, width = 3)

    expect_type(result, "double")
    expect_equal(length(result), length(y))
})

test_that("rolling_slope calculates slopes correctly", {
    # Perfect linear trend
    y <- c(1, 2, 3, 4, 5)
    x <- c(1, 2, 3, 4, 5)
    result <- rolling_slope(y, x, width = 3)

    # Should be close to 1 for perfect linear trend
    expect_true(all(abs(result[!is.na(result)] - 1) < 1e-10))
})

test_that("rolling_slope returns same as lm model", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 8, 6, 7)
    x = seq_along(y)
    n <- length(y)
    width <- 3
    rolling_slope_result <- rolling_slope(y, width = width)
    lm_result <- NA

    for (i in 1:n) {
        current_x <- x[i]
        start_x <- max(x[1], current_x - width / 2)
        end_x <- min(x[n], current_x + width / 2)
        window_idx <- which(x >= start_x & x <= end_x)
        lm_result[i] <- coef(.lm.fit(
            cbind(1, x[window_idx]), y[window_idx]))[2]
    }

    expect_equal(length(rolling_slope_result), length(lm_result))
    expect_equal(rolling_slope_result, lm_result)
})

test_that("rolling_slope returns same as zoo::rollapply()", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 8, 6, 7)

    coef.fn <- function(y) coef(.lm.fit(cbind(1, seq_along(y)), y))[2]
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

    # Different alignments should give different results
    expect_false(identical(result_center, result_left))
    expect_false(identical(result_center, result_right))
    expect_equal(tail(result_right, 7), head(result_left, 7))
})

test_that("rolling_slope works with NULL x", {
    y <- c(1, 3, 5, 7, 9)
    result_null <- rolling_slope(y, x = NULL, width = 3)
    result_seq <- rolling_slope(y, x = seq_along(y), width = 3)

    expect_equal(result_null, result_seq)
})

test_that("rolling_slope handles NA values", {
    y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, 14)

    ## na.rm = FALSE
    results_rm_false <- rolling_slope(y, width = 3, na.rm = FALSE)
    expect_true(any(is.na(results_rm_false)))

    ## na.rm = TRUE
    results_rm_true <- rolling_slope(y, width = 3, na.rm = TRUE)
    expect_true(sum(is.na(results_rm_true)) <= sum(is.na(y)))

    expect_equal(results_rm_true[which(!is.na(results_rm_false))],
                 results_rm_false[which(!is.na(results_rm_false))])
})

test_that("rolling_slope handles edge cases", {
    ## single value
    expect_error(rolling_slope(c(5), width = 3),
                 "should be of length 2 or greater")

    ## two values
    result <- rolling_slope(c(1, 3), width = 3)
    expect_length(result, 2)
    expect_equal(diff(result), 0)

    ## all identical values
    result <- rolling_slope(rep(5, 10), width = 3)
    expect_true(all(result == 0))

    ## all NA
    expect_error(rolling_slope(rep(NA, 5), width = 3, na.rm = TRUE),
                 "should contain at least 2 or more non-NA values")
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
    lm_result <- NA

    for (i in 1:n) {
        current_x <- idx_unequal[i]
        start_x <- max(idx_unequal[1], current_x - width / 2)
        end_x <- min(idx_unequal[n], current_x + width / 2)
        window_idx <- which(idx_unequal >= start_x & idx_unequal <= end_x)
        lm_result[i] <- coef(.lm.fit(
            cbind(1, idx_unequal[window_idx]), y[window_idx]))[2]
    }

    expect_equal(rolling_slope_result, lm_result)
})

test_that("rolling_slope handles width edge cases", {
    y <- c(1, 3, 2, 5, 8)

    # Width larger than data
    result <- rolling_slope(y, width = 10)
    expect_length(result, length(y))
    expect_true(var(result) == 0)

    ## width = 1
    result <- rolling_slope(y, width = 1)
    expect_length(result, length(y))
    expect_true(all(result == 0, na.rm = TRUE))
})

test_that("rolling_slope partial windows work correctly", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- rolling_slope(y, width = 3, align = "center")

    ## first and last positions should use partial windows
    expect_false(is.na(result[1]))
    expect_false(is.na(result[length(y)]))
    expect_equal(result[1], diff(y[1:2]))
    expect_equal(result[2], diff(y[c(1, 3)])/2)
    expect_equal(tail(result, 1), diff(tail(y, 2)))
    expect_equal(tail(result, 2)[1], diff(tail(y, 3)[c(1, 3)])/2)
})

test_that("rolling_slope match.arg works", {
    y <- c(1, 3, 2, 5, 8)

    # Valid alignment
    expect_no_error(rolling_slope(y, width = 3, align = "center"))

    # Invalid alignment should error
    expect_error(
        rolling_slope(y, width = 3, align = "invalid"),
        "'arg' should be one of")
})

test_that("rolling_slope zero denominator handling", {
    # All x values identical
    y <- c(1, 2, 3, 4, 5)
    x <- rep(1, 5)
    result <- rolling_slope(y, x, width = 3)

    expect_true(all(result == 0, na.rm = TRUE))
})







## peak_directional_slope
test_that("peak_directional_slope returns correct structure", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- peak_directional_slope(y, width = 3)

    expect_type(result, "list")
    expect_named(result, c("x", "slope", "x_fitted", "y_fitted"))
    expect_type(result$slope, "double")
    expect_type(result$x, "double")
    expect_type(result$y_fitted, "double")
    expect_type(result$x_fitted, "double")
    expect_true(is.vector(result$y_fitted))
    expect_true(is.vector(result$x_fitted))
})

test_that("peak_directional_slope works with upward trend", {
    y <- c(1, 3, 5, 7, 9)
    result <- peak_directional_slope(y, width = 3)

    expect_gt(result$slope, 0)
    expect_gte(result$x, 1)
    expect_lte(result$x, length(y))
})

test_that("peak_directional_slope works with downward trend", {
    y <- c(9, 7, 5, 3, 1)
    result <- peak_directional_slope(y, width = 3)

    expect_lt(result$slope, 0)
    expect_gte(result$x, 1)
    expect_lte(result$x, length(y))
})

test_that("peak_directional_slope handles NA values correctly", {
    y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, 14)

    ## na.rm = FALSE returns NAs for `rolling_slope()`,
    ## but returns numeric for `peak_directional_slope()`
    results_rm_false <- peak_directional_slope(y, width = 3, na.rm = FALSE)
    expect_true(is.na(results_rm_false$slope) | is.numeric(results_rm_false$slope))
    expect_type(results_rm_false$slope, "double")
    expect_gte(results_rm_false$x, 1)
    expect_lte(results_rm_false$x, length(y))

    ## na.rm = TRUE should
    results_rm_true <- peak_directional_slope(y, width = 3, na.rm = TRUE)
    expect_false(is.na(results_rm_true$slope))
    expect_type(results_rm_true$slope, "double")
    expect_gte(results_rm_true$x, 1)
    expect_lte(results_rm_true$x, length(y))
})

test_that("peak_directional_slope works with custom x values", {
    y <- c(1, 4, 2, 8, 6, 10)
    x <- c(0, 1, 2, 3, 4, 5)
    result <- peak_directional_slope(y, x, width = 3)

    expect_type(result$slope, "double")
    expect_gte(result$x, 1)
    expect_lte(result$x, length(y))
})

test_that("peak_directional_slope handles width in units of x", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    x_idx <- seq_along(y)
    x_5hz <- x_idx/5

    results_idx <- peak_directional_slope(y, x_idx, width = 3)
    results_5hz <- peak_directional_slope(y, x_5hz, width = 3/5)

    expect_equal(x_5hz[results_idx$x], results_5hz$x)
    expect_equal(results_idx$slope, results_5hz$slope/5)
    expect_equal(x_5hz[results_idx$x_fitted], results_5hz$x_fitted)
    expect_equal(results_idx$y_fitted, results_5hz$y_fitted)

    idx_unequal <- c(0.5, 1.5, 3, 3.5, 4, 5.5, 7, 8, 10, 11)
    n <- length(y)
    width <- 3
    peak_slope_result <- peak_directional_slope(y, idx_unequal, width = 3)
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

test_that("peak_directional_slope works with NULL x", {
    y <- c(1, 3, 5, 7, 9)
    result_null <- peak_directional_slope(y, x = NULL, width = 3)
    result_seq <- peak_directional_slope(y, x = seq_along(y), width = 3)

    expect_equal(result_null$slope, result_seq$slope)
    expect_equal(result_null$x, result_seq$x)
})

test_that("peak_directional_slope works with different alignments", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 8, 6, 7)
    x <- seq(22, along = y)

    result_center <- peak_directional_slope(y, x, width = 3, align = "center")
    result_left <- peak_directional_slope(y, x, width = 2, align = "left")
    result_right <- peak_directional_slope(y, x, width = 2, align = "right")

    expect_equal(result_center$x, result_left$x + 1)
    expect_equal(result_center$x, result_right$x - 1)
    expect_equal(result_left$x + 1, result_right$x - 1)

    expect_equal(result_center$y, result_left$y)
    expect_equal(result_center$y, result_right$y)
    expect_equal(result_left$y, result_right$y)
})

test_that("peak_directional_slope handles edge cases", {
    ## single value
    expect_error(peak_directional_slope(y = 5, width = 3),
                 "should be of length 2 or greater")

    ## two values
    result <- peak_directional_slope(y = c(1, 3), width = 3)
    expect_type(result$slope, "double")
    expect_gte(result$x, 1)
    expect_lte(result$x, 2)

    ## all identical values
    result <- peak_directional_slope(y = rep(5, 10), width = 3)
    expect_equal(result$slope, 0)
    expect_gte(result$x, 1)

    ## all NA
    expect_error(
        peak_directional_slope(y = rep(NA, 5), width = 3),
        "should contain at least 2 or more non-NA values")
})

