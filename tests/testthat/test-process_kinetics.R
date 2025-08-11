test_that("x, y names passthrough works", {
    set.seed(13)
    x1 <- seq(-10, 60, by = 2)
    A <- 10; B <- 100; TD <- 5; tau <- 12
    y1 <- monoexponential(x1, A, B, TD, tau) + rnorm(length(x1), 0, 3)
    mydata <- tibble::tibble(xx = x1, yy = y1)

    index_result <- process_kinetics(y = y1, x = NULL, data = NULL)
    expect_true(all(c("index", "y") %in% names(index_result$data))) ## y1

    x1_result <- process_kinetics(y = y1, x = x1, data = NULL)
    expect_true(all(c("x", "y") %in% names(x1_result$data))) ## x1, y1

    ## x parameters should be unequal
    expect_false(all(
        which(round(index_result$coef, 3) == round(x1_result$coef, 3))
        %in% c(3, 4, 5)))
    ## different x_names
    expect_false(names(index_result$data)[1] == names(x1_result$data)[1])

    expect_error(process_kinetics(y = "y1", x = NULL, data = NULL),
                 "must be a.*numeric.*vector")
    expect_error(process_kinetics(y = y1, x = "x1", data = NULL),
                 "must be a.*numeric.*vector")

    ## expect dataframe error
    expect_error(process_kinetics(y = yy, x = xx, data = A),
                 "must be a dataframe")
    ## expect error for names missing from data
    expect_error(process_kinetics(y = qq, x = zz, data = mydata),
                 "not found in `data`")

    expect_error(process_kinetics(y = yy, x = zz, data = mydata),
                 "not found in `data`")

    quoted_yy_result <- process_kinetics(y = "yy", x = NULL,
                                         data = mydata, verbose = FALSE)
    expect_true(all(c("index", "yy") %in% names(quoted_yy_result$data)))
    expect_true(all(round(index_result$coef, 3) == round(quoted_yy_result$coef, 3)))

    data_quoted_result <- process_kinetics(y = "yy", x = "xx", data = mydata)
    expect_true(all(c("xx", "yy") %in% names(data_quoted_result$data)))
    expect_true(all(round(x1_result$coef, 3) == round(data_quoted_result$coef, 3)))

    ## TODO 2025-08-10 unquoted not currently working
    # data_yy_result <- process_kinetics(y = yy, x = NULL,
    #                                    data = mydata, verbose = FALSE)
    # expect_true(all(c("index", "yy") %in% names(data_yy_result$data)))
    # expect_true(all(round(index_result$coef, 3) == round(data_yy_result$coef, 3)))

    # quoted_xx_result <- process_kinetics(y = yy, x = "xx", data = mydata)
    # expect_true(all(c("xx", "yy") %in% names(quoted_xx_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(quoted_xx_result$coef, 3)))

    # data_unquoted_result <- process_kinetics(y = yy, x = xx, data = mydata)
    # expect_true(all(c("xx", "yy") %in% names(data_unquoted_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(data_unquoted_result$coef, 3)))
})
