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



test_that("process_kinetics works for a local environment call", {
    # devtools::load_all()
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mNIRS")

    data_raw <- read_data(
        file_path = file_path,
        nirs_columns = c(smo2 = "SmO2 Live"),
        sample_column = c(time = "hh:mm:ss"),
        verbose = FALSE
    ) |>
        dplyr::mutate(time = round(time - dplyr::first(time), 1))

    nirs_columns <- attributes(data_raw)$nirs_columns
    fitted_name <- paste0(nirs_columns, "_fitted")
    sample_column <- attributes(data_raw)$sample_column
    fit_sample_name <- paste0("fit_", sample_column)
    event_samples <- c(876)

    data_list <- prepare_kinetics_data(
        data_raw,
        event_sample = event_samples,
        fit_window = c(30, 120)
    )

    model <- process_kinetics(y = nirs_columns,
                              x = fit_sample_name,
                              data = data_list[[1]],
                              method = "peak_slope",
                              width = 10)

    ## check class
    expect_equal(class(model), "mNIRS.kinetics")

    ## check list contents
    model_contents <- c("method", "equation", "data", "fitted",
                        "residuals", "rolling_slopes", "x0",
                        "width", "align", "coefs", "call")
    expect_equal(sum(names(model) %in% model_contents),
                 length(model_contents))

    ## check data colnames
    data_colnames <- c(fit_sample_name, nirs_columns, fitted_name)
    expect_equal(sum(names(model$data) %in% data_colnames),
                 length(data_colnames))

    ## length data, residuals, fitted should match
    expect_equal(length(model$residuals), length(model$fitted))
    expect_lt(length(model$residuals), nrow(model$data))
    expect_gt(length(model$residuals), 1)
    expect_lt(length(model$fitted), nrow(model$data))
    expect_gt(length(model$fitted), 1)

    ## expect a dataframe with multiple values
    expect_s3_class(model$data, "data.frame")
    expect_s3_class(model$coefs, "data.frame")
    expect_equal(length(model$data), 3)
    expect_gte(length(model$coefs), 1)

    model_plot <- plot(model)
    expect_s3_class(model_plot, "ggplot")
})




test_that("process_kinetics works inside a purrr::map() call", {
    # devtools::load_all()
    file_path <- system.file("extdata/train.red_interval_example.csv",
                             package = "mNIRS")

    data_raw <- read_data(
        file_path = file_path,
        nirs_columns = c(smo2_left = "SmO2", smo2_right = "SmO2"),
        sample_column = c(time = "Timestamp (seconds passed)"),
        verbose = FALSE
    ) |>
        dplyr::mutate(time = round(time - dplyr::first(time), 1))

    nirs_columns <- attributes(data_raw)$nirs_columns
    fitted_name <- paste0(nirs_columns, "_fitted")
    sample_column <- attributes(data_raw)$sample_column
    fit_sample_name <- paste0("fit_", sample_column)
    event_samples <- c(370, 1085)

    data_list <- prepare_kinetics_data(
        data_raw,
        event_sample = event_samples,
        fit_window = c(30, 120)
    )

    model_list <- tidyr::expand_grid(
        .df = data_list,
        .nirs = nirs_columns,
        .method = "monoexp") |>
        purrr::pmap(
            \(.df, .nirs, .method)
            process_kinetics(y = .nirs,
                             x = fit_sample_name,
                             data = .df,
                             method = .method, width = 10)
        )

    ## check length of model_list
    expect_equal(length(model_list), length(c(nirs_columns, event_samples)))

    ## check model_list names match sample_column and event_samples
    expect_equal(sum(grepl(sample_column, names(model_list))),
                 length(model_list))
    expect_equal(sum(grepl(paste(event_samples, collapse = "|"), names(model_list))),
                 length(model_list))

    ## check class
    expect_equal(class(model_list[[1]]), "mNIRS.kinetics")

    ## check list contents
    model_contents <- c("method", "model", "equation", "data",
                        "fitted", "residuals", "x0", "coefs",
                        "diagnostics", "call")
    expect_equal(sum(names(model_list[[1]]) %in% model_contents),
                 length(model_contents))

    ## check data colnames
    data_colnames <- c(fit_sample_name, nirs_columns[1], fitted_name[1])
    expect_equal(sum(names(model_list[[1]]$data) %in% data_colnames),
                length(data_colnames))

    ## length data, residuals, fitted should match
    expect_equal(length(model_list[[1]]$residuals), length(model_list[[1]]$fitted))
    expect_equal(length(model_list[[1]]$residuals), nrow(model_list[[1]]$data))
    expect_equal(length(model_list[[1]]$fitted), nrow(model_list[[1]]$data))

    ## expect a dataframe with multiple values
    expect_s3_class(model_list[[1]]$data, "data.frame")
    expect_s3_class(model_list[[1]]$coefs, "data.frame")
    expect_s3_class(model_list[[1]]$diagnostics, "data.frame")
    expect_equal(length(model_list[[1]]$data), 3)
    expect_gte(length(model_list[[1]]$coefs), 1)
    expect_gte(length(model_list[[1]]$diagnostics), 1)

    model_plot <- plot(model_list[[1]])
    expect_s3_class(model_plot, "ggplot")

})
