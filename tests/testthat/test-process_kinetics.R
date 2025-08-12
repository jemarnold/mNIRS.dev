test_that("monoexponential x, y names passthrough works", {
    set.seed(13)
    x1 <- seq(-10, 60, by = 2)
    A <- 10; B <- 100; TD <- 5; tau <- 12
    y1 <- monoexponential(x1, A, B, TD, tau) + rnorm(length(x1), 0, 3)
    mydata <- tibble(xx = x1, yy = y1)

    .method <- "monoexponential"

    index_result <- process_kinetics(y = y1, x = NULL, data = NULL, method = .method)
    expect_true(all(c("index", "y") %in% names(index_result$data))) ## y1

    x1_result <- process_kinetics(y = y1, x = x1, data = NULL, method = .method)
    expect_true(all(c("x", "y") %in% names(x1_result$data))) ## x1, y1

    ## x parameters should be unequal
    comparison <- round(index_result$coef, 3) == round(x1_result$coef, 3)
    expect_true(all(comparison[c(1, 2, 6)]))
    expect_false(all(comparison[3:5]))

    ## different x_names
    expect_false(names(index_result$data)[1] == names(x1_result$data)[1])

    expect_error(process_kinetics(y = "y1", x = NULL, data = NULL, method = .method),
                 "must be a.*numeric.*vector")
    expect_error(process_kinetics(y = y1, x = "x1", data = NULL, method = .method),
                 "must be a.*numeric.*vector")

    ## expect dataframe error
    expect_error(process_kinetics(y = "yy", x = "xx", data = A, method = .method),
                 "must be a dataframe")
    ## expect error for names missing from data
    expect_error(process_kinetics(y = "qq", x = "zz", data = mydata, method = .method),
                 "not found in `data")

    expect_error(process_kinetics(y = "yy", x = "zz", data = mydata, method = .method),
                 "not found in `data")

    quoted_yy_result <- process_kinetics(y = "yy", x = NULL,
                                         data = mydata, method = .method,
                                         verbose = FALSE)
    expect_true(all(c("index", "yy") %in% names(quoted_yy_result$data)))
    expect_true(all(round(index_result$coef, 3) == round(quoted_yy_result$coef, 3)))

    data_quoted_result <- process_kinetics(y = "yy", x = "xx",
                                           data = mydata, method = .method)
    expect_true(all(c("xx", "yy") %in% names(data_quoted_result$data)))
    expect_true(all(round(x1_result$coef, 3) == round(data_quoted_result$coef, 3)))

    ## TODO 2025-08-10 unquoted not currently working
    # data_yy_result <- process_kinetics(y = yy, x = NULL,
    #                                    data = mydata, method = .method,
    #                                    verbose = FALSE)
    # expect_true(all(c("index", "yy") %in% names(data_yy_result$data)))
    # expect_true(all(round(index_result$coef, 3) == round(data_yy_result$coef, 3)))
    #
    # quoted_xx_result <- process_kinetics(y = yy, x = "xx",
    #                                      data = mydata, method = .method)
    # expect_true(all(c("xx", "yy") %in% names(quoted_xx_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(quoted_xx_result$coef, 3)))
    #
    # data_unquoted_result <- process_kinetics(y = yy, x = xx,
    #                                          data = mydata, method = .method)
    # expect_true(all(c("xx", "yy") %in% names(data_unquoted_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(data_unquoted_result$coef, 3)))
})




test_that("sigmoidal x, y names passthrough works", {
    set.seed(13)
    x1 <- seq(-10, 60, by = 2)
    A <- 10; B <- 100; TD <- 5; tau <- 12
    y1 <- monoexponential(x1, A, B, TD, tau) + rnorm(length(x1), 0, 3)
    mydata <- tibble(xx = x1, yy = y1)

    .method <- "sigmoidal"

    index_result <- process_kinetics(y = y1, x = NULL, data = NULL, method = .method)
    expect_true(all(c("index", "y") %in% names(index_result$data))) ## y1

    x1_result <- process_kinetics(y = y1, x = x1, data = NULL, method = .method)
    expect_true(all(c("x", "y") %in% names(x1_result$data))) ## x1, y1

    ## x parameters should be unequal
    comparison <- round(index_result$coef, 3) == round(x1_result$coef, 3)
    expect_true(all(comparison[c(1, 2, 5)]))
    expect_false(all(comparison[3:4]))

    ## different x_names
    expect_false(names(index_result$data)[1] == names(x1_result$data)[1])

    expect_error(process_kinetics(y = "y1", x = NULL, data = NULL, method = .method),
                 "must be a.*numeric.*vector")
    expect_error(process_kinetics(y = y1, x = "x1", data = NULL, method = .method),
                 "must be a.*numeric.*vector")

    ## expect dataframe error
    expect_error(process_kinetics(y = "yy", x = "xx", data = A, method = .method),
                 "must be a dataframe")
    ## expect error for names missing from data
    expect_error(process_kinetics(y = "qq", x = "zz", data = mydata, method = .method),
                 "not found in `data")

    expect_error(process_kinetics(y = "yy", x = "zz", data = mydata, method = .method),
                 "not found in `data")

    quoted_yy_result <- process_kinetics(y = "yy", x = NULL,
                                         data = mydata, method = .method,
                                         verbose = FALSE)
    expect_true(all(c("index", "yy") %in% names(quoted_yy_result$data)))
    expect_true(all(round(index_result$coef, 3) == round(quoted_yy_result$coef, 3)))

    data_quoted_result <- process_kinetics(y = "yy", x = "xx",
                                           data = mydata, method = .method)
    expect_true(all(c("xx", "yy") %in% names(data_quoted_result$data)))
    expect_true(all(round(x1_result$coef, 3) == round(data_quoted_result$coef, 3)))

    ## TODO 2025-08-10 unquoted not currently working
    # data_yy_result <- process_kinetics(y = yy, x = NULL,
    #                                    data = mydata, method = .method,
    #                                    verbose = FALSE)
    # expect_true(all(c("index", "yy") %in% names(data_yy_result$data)))
    # expect_true(all(round(index_result$coef, 3) == round(data_yy_result$coef, 3)))
    #
    # quoted_xx_result <- process_kinetics(y = yy, x = "xx",
    #                                      data = mydata, method = .method)
    # expect_true(all(c("xx", "yy") %in% names(quoted_xx_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(quoted_xx_result$coef, 3)))
    #
    # data_unquoted_result <- process_kinetics(y = yy, x = xx,
    #                                          data = mydata, method = .method)
    # expect_true(all(c("xx", "yy") %in% names(data_unquoted_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(data_unquoted_result$coef, 3)))
})




test_that("half_time x, y names passthrough works", {
    set.seed(13)
    x1 <- seq(-10, 60, by = 2)
    A <- 10; B <- 100; TD <- 5; tau <- 12
    y1 <- monoexponential(x1, A, B, TD, tau) + rnorm(length(x1), 0, 3)
    mydata <- tibble(xx = x1, yy = y1)

    .method <- "half_time"

    index_result <- process_kinetics(y = y1, x = NULL, data = NULL, method = .method)
    expect_true(all(c("index", "y") %in% names(index_result$data))) ## y1

    x1_result <- process_kinetics(y = y1, x = x1, data = NULL, method = .method)
    expect_true(all(c("x", "y") %in% names(x1_result$data))) ## x1, y1

    ## x parameters should be unequal
    comparison <- round(index_result$coef, 3) == round(x1_result$coef, 3)
    expect_true(all(comparison[c(2, 4, 6, 7)]))
    expect_false(all(comparison[c(1, 3, 5)]))

    ## different x_names
    expect_false(names(index_result$data)[1] == names(x1_result$data)[1])

    expect_error(process_kinetics(y = "y1", x = NULL, data = NULL, method = .method),
                 "must be a.*numeric.*vector")
    expect_error(process_kinetics(y = y1, x = "x1", data = NULL, method = .method),
                 "must be a.*numeric.*vector")

    ## expect dataframe error
    expect_error(process_kinetics(y = "yy", x = "xx", data = A, method = .method),
                 "must be a dataframe")
    ## expect error for names missing from data
    expect_error(process_kinetics(y = "qq", x = "zz", data = mydata, method = .method),
                 "not found in `data")

    expect_error(process_kinetics(y = "yy", x = "zz", data = mydata, method = .method),
                 "not found in `data")

    quoted_yy_result <- process_kinetics(y = "yy", x = NULL,
                                         data = mydata, method = .method,
                                         verbose = FALSE)
    expect_true(all(c("index", "yy") %in% names(quoted_yy_result$data)))
    expect_true(all(round(index_result$coef, 3) == round(quoted_yy_result$coef, 3)))

    data_quoted_result <- process_kinetics(y = "yy", x = "xx",
                                           data = mydata, method = .method)
    expect_true(all(c("xx", "yy") %in% names(data_quoted_result$data)))
    expect_true(all(round(x1_result$coef, 3) == round(data_quoted_result$coef, 3)))

    ## TODO 2025-08-10 unquoted not currently working
    # data_yy_result <- process_kinetics(y = yy, x = NULL,
    #                                    data = mydata, method = .method,
    #                                    verbose = FALSE)
    # expect_true(all(c("index", "yy") %in% names(data_yy_result$data)))
    # expect_true(all(round(index_result$coef, 3) == round(data_yy_result$coef, 3)))
    #
    # quoted_xx_result <- process_kinetics(y = yy, x = "xx",
    #                                      data = mydata, method = .method)
    # expect_true(all(c("xx", "yy") %in% names(quoted_xx_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(quoted_xx_result$coef, 3)))
    #
    # data_unquoted_result <- process_kinetics(y = yy, x = xx,
    #                                          data = mydata, method = .method)
    # expect_true(all(c("xx", "yy") %in% names(data_unquoted_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(data_unquoted_result$coef, 3)))
})




test_that("peak_slope x, y names passthrough works", {
    set.seed(13)
    x1 <- seq(-10, 60, by = 2)
    A <- 10; B <- 100; TD <- 5; tau <- 12
    y1 <- monoexponential(x1, A, B, TD, tau) + rnorm(length(x1), 0, 3)
    mydata <- tibble(xx = x1, yy = y1)

    .method <- "peak_slope"
    .width <- 5

    index_result <- process_kinetics(y = y1, x = NULL,
                                     data = NULL, method = .method,
                                     width = .width)
    expect_true(all(c("index", "y") %in% names(index_result$data))) ## y1

    x1_result <- process_kinetics(y = y1, x = x1,
                                  data = NULL, method = .method,
                                  width = .width)
    expect_true(all(c("x", "y") %in% names(x1_result$data))) ## x1, y1

    ## x parameters should be unequal
    comparison <- round(index_result$coef, 3) == round(x1_result$coef, 3)
    expect_false(all(comparison))

    ## different x_names
    expect_false(names(index_result$data)[1] == names(x1_result$data)[1])

    expect_error(process_kinetics(y = "y1", x = NULL,
                                  data = NULL, method = .method,
                                  width = .width),
                 "must be a.*numeric.*vector")
    expect_error(process_kinetics(y = y1, x = "x1",
                                  data = NULL, method = .method,
                                  width = .width),
                 "must be a.*numeric.*vector")

    ## expect dataframe error
    expect_error(process_kinetics(y = "yy", x = "xx",
                                  data = A, method = .method,
                                  width = .width),
                 "must be a dataframe")
    ## expect error for names missing from data
    expect_error(process_kinetics(y = "qq", x = "zz",
                                  data = mydata, method = .method,
                                  width = .width),
                 "not found in `data")

    expect_error(process_kinetics(y = "yy", x = "zz",
                                  data = mydata, method = .method,
                                  width = .width),
                 "not found in `data")

    quoted_yy_result <- process_kinetics(y = "yy", x = NULL,
                                         data = mydata, method = .method,
                                         width = .width, verbose = FALSE)
    expect_true(all(c("index", "yy") %in% names(quoted_yy_result$data)))
    expect_true(all(round(index_result$coef, 3) == round(quoted_yy_result$coef, 3)))

    data_quoted_result <- process_kinetics(y = "yy", x = "xx",
                                           data = mydata, method = .method,
                                           width = .width)
    expect_true(all(c("xx", "yy") %in% names(data_quoted_result$data)))
    expect_true(all(round(x1_result$coef, 3) == round(data_quoted_result$coef, 3)))

    ## TODO 2025-08-10 unquoted not currently working
    # data_yy_result <- process_kinetics(y = yy, x = NULL,
    #                                    data = mydata, method = .method,
    #                                    width = .width, verbose = FALSE)
    # expect_true(all(c("index", "yy") %in% names(data_yy_result$data)))
    # expect_true(all(round(index_result$coef, 3) == round(data_yy_result$coef, 3)))
    #
    # quoted_xx_result <- process_kinetics(y = yy, x = "xx",
    #                                      data = mydata, method = .method,
    #                                      width = .width)
    # expect_true(all(c("xx", "yy") %in% names(quoted_xx_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(quoted_xx_result$coef, 3)))
    #
    # data_unquoted_result <- process_kinetics(y = yy, x = xx,
    #                                          data = mydata, method = .method,
    #                                          width = .width)
    # expect_true(all(c("xx", "yy") %in% names(data_unquoted_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(data_unquoted_result$coef, 3)))
})
