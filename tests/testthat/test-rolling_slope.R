test_that("slope returns correct structure", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- slope(y)

    expect_type(result, "double")
    expect_equal(length(result), 1)
})

test_that("slope works with NULL x", {
    y <- c(1, 3, 5, 7, 9)
    result_null <- slope(y, x = NULL)
    result_seq <- slope(y, x = seq_along(y))

    expect_equal(result_null, result_seq)
})

test_that("slope calculates slopes correctly", {
    # Perfect linear trend
    y <- c(1, 2, 3, 4, 5)
    x <- c(1, 2, 3, 4, 5)
    result <- slope(y, x)

    # Should be close to 1 for perfect linear trend
    expect_true(all(abs(result[!is.na(result)] - 1) < 1e-10))
})

test_that("slope returns same as lm model", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 8, 6, 7)

    slope_result <- slope(y)
    lm_result <- coef(.lm.fit(cbind(1, seq_along(y)), y))[2]

    expect_equal(slope_result, lm_result)
})

test_that("slope handles NA values", {
    y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, 14)

    ## na.rm = FALSE
    results_rm_false <- slope(y, na.rm = FALSE)
    expect_true(any(is.na(results_rm_false)))

    ## na.rm = TRUE
    results_rm_true <- slope(y, na.rm = TRUE)
    expect_true(sum(is.na(results_rm_true)) <= sum(is.na(y)))
})

test_that("slope handles edge cases", {
    ## single value
    expect_true(is.na(slope(5)))

    ## all identical values
    result <- slope(rep(5, 10))
    expect_true(all(result == 0))

    ## all NA
    expect_true(is.na(slope(rep(NA, 5))))
})

test_that("slope zero denominator handling", {
    # All x values identical
    y <- c(1, 2, 3, 4, 5)
    x <- rep(1, 5)
    result <- slope(y, x)

    expect_true(all(result == 0, na.rm = TRUE))
})









test_that("rolling_slope returns correct structure", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- rolling_slope(y, width = 3)

    expect_type(result, "double")
    expect_equal(length(result), length(y))
})

test_that("rolling_slope works with NULL x", {
    y <- c(1, 3, 5, 7, 9)
    result_null <- rolling_slope(y, x = NULL, width = 3)
    result_seq <- rolling_slope(y, x = seq_along(y), width = 3)

    expect_equal(result_null, result_seq)
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

    for (i in 1:n) {

        start_idx <- head(which(x >= (x[i] - width/2)), 1)
        end_idx <- tail(which(x <= (x[i] + width/2)), 1)
        lm_result[i] <- coef(.lm.fit(
            cbind(1, seq_along(y)[start_idx:end_idx]),
            y[start_idx:end_idx]))[2]
    }
    expect_equal(length(rolling_slope_result), length(lm_result))
    expect_equal(rolling_slope_result, lm_result)
})

test_that("rolling_slope returns same as zoo::rollapply()", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 8, 6, 7)
    x <- seq_along(y)

    coef.fn <- function(y) coef(.lm.fit(cbind(1, seq_along(y)), y))[2]
    rollapply_center <- zoo::rollapply(y,
                                       FUN = coef.fn,
                                       width = 3,
                                       align = "center",
                                       partial = TRUE)
    rollapply_left <- zoo::rollapply(y,
                                       FUN = coef.fn,
                                       width = 3,
                                       align = "left",
                                       partial = TRUE)
    rollapply_right <- zoo::rollapply(y,
                                       FUN = coef.fn,
                                       width = 3,
                                       align = "right",
                                       partial = TRUE)
    rolling_slope_center <- rolling_slope(y, width = 3, align = "center")
    rolling_slope_left <- rolling_slope(y, width = 3, align = "left")
    rolling_slope_right <- rolling_slope(y, width = 3, align = "right")

    n <- length(y)
    width <- 3
    lm_result <- NA
    for (i in 1:n) {

        # ## align left is FORWARD looking
        # ## current observation is at leftmost position of window
        # ## window starts at current x value, extends width units forward
        # start_idx <- i
        # # end_idx <- tail(which(x <= (x[i] + width)), 1)
        # end_idx <- tail(which(1:n <= i+width-1), 1)


        ## align right is BACKWARD looking
        ## current observation is at rightmost position of window
        ## window ends at current x value, extends width units backward
        start_idx <- head(which(1:n >= i-width+1), 1)
        end_idx <- i


        lm_result[i] <- coef(.lm.fit(
            cbind(1, x[start_idx:end_idx]),
            y[start_idx:end_idx]))[2]
        print(c(start_idx, end_idx))
    }

    coef.fn(y[8:10])
    # rollapply_right[1:3+3]
    # rolling_slope_left <- rolling_slope(y, width = 3, align = "left")
    # rolling_slope_right <- rolling_slope(y, width = 3, align = "right")

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
})

test_that("rolling_slope handles NA values", {
    y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, 14)

    ## na.rm = FALSE
    results_rm_false <- rolling_slope(y, width = 3, na.rm = FALSE)
    expect_true(any(is.na(results_rm_false)))

    ## na.rm = TRUE
    results_rm_true <- rolling_slope(y, width = 3, na.rm = TRUE)
    expect_true(sum(is.na(results_rm_true)) <= sum(is.na(y)))
})

test_that("rolling_slope handles edge cases", {
    ## single value
    expect_error(
        rolling_slope(c(5), width = 3),
        "should be of length 2 or greater")

    ## two values
    result <- rolling_slope(c(1, 3), width = 3)
    expect_length(result, 2)
    expect_equal(diff(result), 0)

    ## all identical values
    result <- rolling_slope(rep(5, 10), width = 3)
    expect_true(all(result == 0))

    ## all NA
    expect_error(
        rolling_slope(rep(NA, 5), width = 3, na.rm = TRUE),
        "should contain at least 2 or more non-NA values")
})

test_that("rolling_slope handles width edge cases", {
    y <- c(1, 3, 2, 5, 8)

    # Width larger than data
    result <- rolling_slope(y, width = 10)
    expect_length(result, length(y))

    # Width of 2 (minimum)
    result <- rolling_slope(y, width = 2)
    expect_length(result, length(y))

    ## width = 1
    expect_error(
        rolling_slope(y, width = 1),
        "should be equal to 2 or greater")
})

test_that("rolling_slope partial windows work correctly", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- rolling_slope(y, width = 3, align = "center")

    ## first and last positions should use partial windows
    expect_false(is.na(result[1]))
    expect_false(is.na(result[length(y)]))
    expect_equal(result[1], diff(y[1:2]))
    expect_equal(result[2], diff(y[c(1, 3)])/2)
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








test_that("peak_directional_slope returns correct structure", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- peak_directional_slope(y, width = 3)

    expect_type(result, "list")
    expect_named(result, c("slope", "x", "y_fitted", "x_fitted"))
    expect_type(result$slope, "double")
    expect_type(result$x, "integer")
    expect_type(result$y_fitted, "double")
    expect_type(result$x_fitted, "integer")
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

test_that("peak_directional_slope works with NULL x", {
    y <- c(1, 3, 5, 7, 9)
    result_null <- peak_directional_slope(y, x = NULL, width = 3)
    result_seq <- peak_directional_slope(y, x = seq_along(y), width = 3)

    expect_equal(result_null$slope, result_seq$slope)
    expect_equal(result_null$x, result_seq$x)
})

test_that("peak_directional_slope works with different alignments", {
    y <- c(1, 3, 2, 5, 8, 7, 9, 8, 6, 7)


    # ggplot(tibble::tibble()) +
    #     aes(x = seq_along(y), y = y) +
    #     geom_point() +
    #     geom_line(aes(x = result_center$x_fitted,
    #                   y = result_center$y_fitted,
    #                   colour = "center")) +
    #     geom_line(aes(x = result_left$x_fitted,
    #                   y = result_left$y_fitted,
    #                   colour = "left")) +
    #     geom_line(aes(x = result_right$x_fitted,
    #                   y = result_right$y_fitted,
    #                   colour = "right"))
    #
    #
    # length(y)
    # lm(y ~ x, data.frame(y = y, x = seq_along(y))[1:4,])
    # rolling_slope(y, width = 3, align = "center")
    #
    # mean(y[c(1:3)+0])
    # coef.fn <- function(y) coef(.lm.fit(cbind(1, seq_along(y)), y))[2]
    # coef.fn(y)
    # zoo::rollapply(y, FUN = coef.fn, width = 3, align = "center", partial = TRUE)

    result_center <- peak_directional_slope(y, width = 3, align = "center")
    result_left <- peak_directional_slope(y, width = 3, align = "left")
    result_right <- peak_directional_slope(y, width = 3, align = "right")

    expect_type(result_center$slope, "double")
    expect_type(result_left$slope, "double")
    expect_type(result_right$slope, "double")
    expect_equal(result_center$x, result_left$x+1)
    expect_equal(result_center$x, result_right$x-1)
    expect_equal(result_left$x+1, result_right$x-1)
})

test_that("peak_directional_slope handles edge cases", {
    ## single value
    expect_error(
        peak_directional_slope(5, width = 3),
        "should be of length 2 or greater")

    ## two values
    result <- peak_directional_slope(c(1, 3), width = 3)
    expect_type(result$slope, "double")
    expect_gte(result$x, 1)
    expect_lte(result$x, 2)

    ## all identical values
    y_flat <- rep(5, 10)
    result <- peak_directional_slope(y_flat, width = 3)
    expect_equal(result$slope, 0)
    expect_gte(result$x, 1)

    ## all NA
    expect_error(
        peak_directional_slope(rep(NA, 5), width = 3, na.rm = TRUE),
        "should contain at least 2 or more non-NA values")
})

