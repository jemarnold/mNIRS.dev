test_that("pre_process_kinetics_names", {
    # devtools::load_all()
    set.seed(13)
    x1 <- seq(-10, 60, by = 2)
    A <- 10; B <- 100; TD <- 5; tau <- 12
    y1 <- monoexponential(x1, A, B, TD, tau) + rnorm(length(x1), 0, 3)
    mydata <- tibble(xx = x1, yy = y1)

    ## env object
    result <- pre_process_kinetics_names(y1)
    expect_type(result, "list")
    ## check length of list
    expect_equal(length(result), 3)
    ## check class
    expect_true(all(sapply(result, \(.x) is.data.frame(.x))))
    ## check num of columns
    expect_true(all(sapply(result, \(.x) length(names(.x)) == 2)))
    ## check colnames
    expect_contains(names(result$data), c("index", "y1"))
    ## check data length
    expect_equal(nrow(result$data), length(x1))
    ## expect extremes within original data
    expect_true(result$extreme$x %in% x1)
    expect_true(result$extreme$y %in% round(y1, 10))

    ## y & x env objects
    result <- pre_process_kinetics_names(y1, x1)
    ## data & df should be equivalent to original data
    expect_equal(result$data, result$df, ignore_attr = TRUE)
    expect_equal(result$data, tibble(x1, y1), ignore_attr = TRUE)
    expect_equal(result$df, tibble(x1, y1), ignore_attr = TRUE)
    ## check colnames
    expect_contains(names(result$data), c("x1", "y1"))

    ## env object in quotes error
    expect_error(pre_process_kinetics_names("y1"),
                 "must be a.*numeric.*vector")

    ## y & x env object in quotes error
    expect_error(pre_process_kinetics_names("y1", "x1"),
                 "must be a.*numeric.*vector")

    ## env dataframe index
    result <- pre_process_kinetics_names(mydata$yy)
    ## TODO 2025-08-14 would prefer the name to be "yy"
    expect_contains(names(result$data), c("index", "mydata$yy"))

    ## env object doesn't exist error
    expect_error(pre_process_kinetics_names(q1),
                 "object 'q1' not found")

    ## env object not numeric error
    expect_error(pre_process_kinetics_names(mydata),
                 "must be a.*numeric.*vector")

    ## data & y quoted
    result <- pre_process_kinetics_names("yy", data = mydata, verbose = F)
    expect_contains(names(result$data), c("index", "yy"))

    ## data & y & x quoted
    result <- pre_process_kinetics_names("yy", "xx", data = mydata)
    expect_contains(names(result$data), c("xx", "yy"))

    ## data & y quoted doesn't exist
    expect_error(pre_process_kinetics_names("qq", data = mydata),
                 "not found in `data`")

    ## data & x quoted doesn't exist
    expect_error(pre_process_kinetics_names("yy", "qq", data = mydata),
                 "not found in `data`")
})




test_that("find_first_extreme for Moxy data", {
    # devtools::load_all()
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mnirs")

    data_raw <- read_data(
        file_path = file_path,
        nirs_channels = c(smo2 = "SmO2 Live"),
        sample_channel = c(time = "hh:mm:ss"),
        verbose = FALSE
    ) |>
        dplyr::mutate(time = round(time - dplyr::first(time), 2))

    nirs_channels <- attributes(data_raw)$nirs_channels
    fitted_name <- paste0(nirs_channels, "_fitted")
    sample_channel <- attributes(data_raw)$sample_channel
    fit_sample_name <- paste0("fit_", sample_channel)
    event_samples <- c(876)

    kinetics_data <- prepare_kinetics_data(
        data_raw,
        event_sample = event_samples,
        fit_span = c(30, 120)
    )[[1]]

    attributes(kinetics_data)$sample_channel <- fit_sample_name

    result <- find_first_extreme(y = kinetics_data[[nirs_channels]],
                                 x = kinetics_data[[fit_sample_name]])

    # plot(kinetics_data, display_time = TRUE) +
    #     geom_vline(xintercept = result$extreme$x) +
    #     geom_hline(yintercept = result$extreme$y)

    expect_equal(length(result$x), length(result$y))
    expect_equal(result$extreme$y, max(kinetics_data[[nirs_channels]]))
    expect_equal(result$extreme$x,
                 kinetics_data$fit_time[which(
                     kinetics_data[[nirs_channels]] == max(kinetics_data[[nirs_channels]]))[1]])
})




test_that("find_first_extreme for Train.Red data", {
    # devtools::load_all()
    file_path <- system.file("extdata/train.red_interval_example.csv",
                             package = "mnirs")

    data_raw <- read_data(
        file_path = file_path,
        nirs_channels = c(smo2_left = "SmO2", smo2_right = "SmO2"),
        sample_channel = c(time = "Timestamp (seconds passed)"),
        verbose = FALSE
    ) |>
        dplyr::mutate(time = round(time - dplyr::first(time), 2))

    nirs_channels <- attributes(data_raw)$nirs_channels
    fitted_name <- paste0(nirs_channels, "_fitted")
    sample_channel <- attributes(data_raw)$sample_channel
    fit_sample_name <- paste0("fit_", sample_channel)
    event_samples <- c(370, 1085)

    kinetics_data <- prepare_kinetics_data(
        data_raw,
        event_sample = event_samples,
        fit_span = c(30, 120),
        group_events = "ensemble"
    )[[1]]

    attributes(kinetics_data)$sample_channel <- fit_sample_name

    result <- find_first_extreme(y = kinetics_data[[nirs_channels[1]]],
                                 x = kinetics_data[[fit_sample_name]])

    # plot(kinetics_data) +
    #     geom_vline(xintercept = result$extreme$x) +
    #     geom_hline(yintercept = result$extreme$y) +
    #     # ggplot2::coord_cartesian(xlim = c(50, 80),
    #     #                          ylim = c(69, 70))
    # NULL

    ## same length
    expect_equal(length(result$x), length(result$y))
    ## extreme$y should be global y max in this case
    expect_equal(result$extreme$y, max(kinetics_data[[nirs_channels[1]]]))
    ## extreme$x should be the first x at global y max
    expect_equal(result$extreme$x,
                 kinetics_data$fit_time[which(
                     kinetics_data[[nirs_channels[1]]] ==
                         max(kinetics_data[[nirs_channels[1]]]))[1]])


    result <- find_first_extreme(y = kinetics_data[[nirs_channels[2]]],
                                 x = kinetics_data[[fit_sample_name]])

    # plot(kinetics_data) +
    #     geom_vline(xintercept = result$extreme$x) +
    #     geom_hline(yintercept = result$extreme$y) +
    #     # ggplot2::coord_cartesian(xlim = c(110, NA),
    #     #                          ylim = c(69, 72))
    # NULL

    ## same length
    expect_equal(length(result$x), length(result$y))
    ## extreme$y should be global y max in this case
    expect_equal(result$extreme$y, max(kinetics_data[[nirs_channels[2]]]))
    ## extreme$y should be last y in this case
    expect_equal(result$extreme$y, tail(kinetics_data[[nirs_channels[2]]], 1))
    ## extreme$x should be last x in this case
    expect_equal(result$extreme$x, tail(kinetics_data[[fit_sample_name]], 1))
    ## extreme$x should be the first x at global y max
    expect_equal(result$extreme$x,
                 kinetics_data$fit_time[which(
                     kinetics_data[[nirs_channels[2]]] ==
                         max(kinetics_data[[nirs_channels[2]]]))[1]])
})




test_that("monoexponential x, y names passthrough works", {
    # devtools::load_all()
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
    # devtools::load_all()
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
    expect_true(all(comparison[c(1, 2, 4)]))
    expect_false(all(comparison[c(3)]))

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
    .span <- 5

    index_result <- process_kinetics(y = y1, x = NULL,
                                     data = NULL, method = .method,
                                     span = .span)
    expect_true(all(c("index", "y") %in% names(index_result$data))) ## y1

    x1_result <- process_kinetics(y = y1, x = x1,
                                  data = NULL, method = .method,
                                  span = .span)
    expect_true(all(c("x", "y") %in% names(x1_result$data))) ## x1, y1

    ## x parameters should be unequal
    comparison <- round(index_result$coef, 3) == round(x1_result$coef, 3)
    expect_false(all(comparison))

    ## different x_names
    expect_false(names(index_result$data)[1] == names(x1_result$data)[1])

    expect_error(process_kinetics(y = "y1", x = NULL,
                                  data = NULL, method = .method,
                                  span = .span),
                 "must be a.*numeric.*vector")
    expect_error(process_kinetics(y = y1, x = "x1",
                                  data = NULL, method = .method,
                                  span = .span),
                 "must be a.*numeric.*vector")

    ## expect dataframe error
    expect_error(process_kinetics(y = "yy", x = "xx",
                                  data = A, method = .method,
                                  span = .span),
                 "must be a dataframe")
    ## expect error for names missing from data
    expect_error(process_kinetics(y = "qq", x = "zz",
                                  data = mydata, method = .method,
                                  span = .span),
                 "not found in `data")

    expect_error(process_kinetics(y = "yy", x = "zz",
                                  data = mydata, method = .method,
                                  span = .span),
                 "not found in `data")

    quoted_yy_result <- process_kinetics(y = "yy", x = NULL,
                                         data = mydata, method = .method,
                                         span = .span, verbose = FALSE)
    expect_true(all(c("index", "yy") %in% names(quoted_yy_result$data)))
    expect_true(all(round(index_result$coef, 3) == round(quoted_yy_result$coef, 3)))

    data_quoted_result <- process_kinetics(y = "yy", x = "xx",
                                           data = mydata, method = .method,
                                           span = .span)
    expect_true(all(c("xx", "yy") %in% names(data_quoted_result$data)))
    expect_true(all(round(x1_result$coef, 3) == round(data_quoted_result$coef, 3)))

    ## TODO 2025-08-10 unquoted not currently working
    # data_yy_result <- process_kinetics(y = yy, x = NULL,
    #                                    data = mydata, method = .method,
    #                                    span = .span, verbose = FALSE)
    # expect_true(all(c("index", "yy") %in% names(data_yy_result$data)))
    # expect_true(all(round(index_result$coef, 3) == round(data_yy_result$coef, 3)))
    #
    # quoted_xx_result <- process_kinetics(y = yy, x = "xx",
    #                                      data = mydata, method = .method,
    #                                      span = .span)
    # expect_true(all(c("xx", "yy") %in% names(quoted_xx_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(quoted_xx_result$coef, 3)))
    #
    # data_unquoted_result <- process_kinetics(y = yy, x = xx,
    #                                          data = mydata, method = .method,
    #                                          span = .span)
    # expect_true(all(c("xx", "yy") %in% names(data_unquoted_result$data)))
    # expect_true(all(round(x1_result$coef, 3) == round(data_unquoted_result$coef, 3)))
})



test_that("process_kinetics works for a local environment call", {
    # devtools::load_all()
    file_path <- system.file("extdata/moxy_ramp_example.xlsx",
                             package = "mnirs")

    data_raw <- read_data(
        file_path = file_path,
        nirs_channels = c(smo2 = "SmO2 Live"),
        sample_channel = c(time = "hh:mm:ss"),
        verbose = FALSE
    ) |>
        dplyr::mutate(
            time = round(time - dplyr::first(time), 2),
            smo2 = replace_invalid(smo2, c(100), width = 10, return = "median"))

    nirs_channels <- attributes(data_raw)$nirs_channels
    fitted_name <- paste0(nirs_channels, "_fitted")
    sample_channel <- attributes(data_raw)$sample_channel
    fit_sample_name <- paste0("fit_", sample_channel)
    event_samples <- c(876)

    kinetics_data <- prepare_kinetics_data(
        data_raw,
        event_sample = event_samples,
        fit_span = c(30, 180)
    )[[1]]

    attributes(kinetics_data)$sample_channel <- fit_sample_name

    model <- process_kinetics(y = nirs_channels,
                              x = fit_sample_name,
                              data = kinetics_data,
                              method = "peak_slope",
                              span = 10)

    ## check class
    expect_s3_class(model, "mnirs.kinetics")

    ## check list contents
    model_contents <- c("method", "equation", "data", "fitted",
                        "residuals", "rolling_slopes", "x0", "extreme",
                        "span", "align", "coefs", "call")
    expect_equal(sum(names(model) %in% model_contents),
                 length(model_contents))

    ## check data colnames
    data_colnames <- c(fit_sample_name, nirs_channels, fitted_name)
    expect_equal(sum(names(model$data) %in% data_colnames),
                 length(data_colnames))

    ## length data, residuals, fitted should match
    expect_equal(length(model$residuals), length(model$fitted))
    expect_lt(length(model$residuals), nrow(model$data))
    expect_gt(length(model$residuals), 1)
    expect_lt(length(model$fitted), nrow(model$data))
    expect_gt(length(model$fitted), 1)

    ## expect a dataframe with multiple values
    expect_true(all(sapply(model[c("data", "coefs")],
                           \(.x) is.data.frame(.x))))
    expect_equal(length(model$data), 3)
    expect_gte(length(model$coefs), 1)

    ## visual check
    model_plot <- plot(model, display_time = TRUE, plot_coefs = TRUE,
                       plot_diagnostics = TRUE, plot_residuals = TRUE)
    expect_s3_class(model_plot, "ggplot")
})




test_that("process_kinetics works inside a purrr::map() call", {
    # devtools::load_all()

    rlang::check_installed("purrr", reason = "to test purrr::pmap (probably temp)")

    file_path <- system.file("extdata/train.red_interval_example.csv",
                             package = "mnirs")

    data_raw <- read_data(
        file_path = file_path,
        nirs_channels = c(smo2_left = "SmO2", smo2_right = "SmO2"),
        sample_channel = c(time = "Timestamp (seconds passed)"),
        verbose = FALSE
    ) |>
        dplyr::mutate(time = round(time - dplyr::first(time), 2))

    nirs_channels <- attributes(data_raw)$nirs_channels
    fitted_name <- paste0(nirs_channels, "_fitted")
    sample_channel <- attributes(data_raw)$sample_channel
    fit_sample_name <- paste0("fit_", sample_channel)
    event_samples <- c(370, 1085)

    kinetics_data <- prepare_kinetics_data(
        data_raw,
        event_sample = event_samples,
        fit_span = c(30, 120),
        group_events = "distinct"
    )

    model_list <- tidyr::expand_grid(
        .df = kinetics_data,
        .nirs = nirs_channels,
        .method = "monoexp") |>
        purrr::pmap(
            \(.df, .nirs, .method)
            process_kinetics(y = .nirs,
                             x = fit_sample_name,
                             data = .df,
                             method = .method,
                             span = 10)
        )


    ## check length of model_list
    expect_equal(length(model_list), length(c(nirs_channels, event_samples)))

    ## check model_list names match sample_channel and event_samples
    expect_equal(sum(grepl(sample_channel, names(model_list))),
                 length(model_list))
    expect_equal(sum(grepl(paste(event_samples, collapse = "|"), names(model_list))),
                 length(model_list))

    ## check class
    expect_true(all(sapply(model_list, \(.x) class(.x)) == "mnirs.kinetics"))

    model <- model_list[[1]]

    ## check list contents
    model_contents <- c("method", "model", "equation", "data",
                        "fitted", "residuals", "x0", "extreme",
                        "coefs", "diagnostics", "call")
    expect_equal(sum(names(model) %in% model_contents),
                 length(model_contents))

    ## check data colnames
    data_colnames <- c(fit_sample_name, nirs_channels[1], fitted_name[1])
    expect_equal(sum(names(model$data) %in% data_colnames),
                length(data_colnames))

    ## length data, residuals, fitted should match
    expect_equal(length(model$residuals), length(model$fitted))
    expect_lte(length(model$residuals), nrow(model$data))
    expect_gt(length(model$residuals), 1)
    expect_lte(length(model$fitted), nrow(model$data))
    expect_gt(length(model$fitted), 1)

    ## expect a dataframe with multiple values
    expect_true(all(sapply(model[c("data", "coefs", "diagnostics")],
                           \(.x) is.data.frame(.x))))
    expect_equal(ncol(model$data), 3)
    expect_gte(ncol(model$coefs), 1)
    expect_gte(ncol(model$diagnostics), 1)

    ## visual check
    model_plot <- plot(model, display_time = TRUE, plot_coefs = TRUE,
                       plot_diagnostics = TRUE, plot_residuals = TRUE)
    expect_s3_class(model_plot, "ggplot")
})




test_that("process_kinetics works with large oxysoft file", {
    # devtools::load_all()
    file_path <- system.file("extdata/oxysoft_interval_example.xlsx",
                             package = "mnirs")

    data_raw <- read_data(
        file_path = file_path,
        nirs_channels = c(O2Hb = 5, HHb = 6),
        sample_channel = c(sample = 1),
        event_channel = c(event = 8),
        verbose = FALSE,
    ) |>
        dplyr::mutate(
            across(c(O2Hb, HHb), \(.x) filter_data(.x, "butterworth", n = 2, W = 0.05))
        )

    nirs_channels <- attributes(data_raw)$nirs_channels
    fitted_name <- paste0(nirs_channels, "_fitted")
    sample_channel <- attributes(data_raw)$sample_channel
    fit_sample_name <- paste0("fit_", attributes(data_raw)$sample_channel)

    event_samples <- c(24675, 66670)

    data_list <- prepare_kinetics_data(
        data_raw,
        event_sample = event_samples/50,
        event_label = c("E7"),
        fit_span = c(30, 80),
    )

    # plot(data_list[[1]])

    model_list <- tidyr::expand_grid(
        .df = data_list,
        .nirs = attributes(data_raw)$nirs_channels,
        .method = "monoexp") |>
        purrr::pmap(
            \(.df, .nirs, .method)
            process_kinetics(y = .nirs,
                             x = fit_sample_name,
                             data = .df,
                             method = .method,
                             end_kinetics_span = 25,
                             span = 5)
        )

    # plot(model    _list[[1]])

    ## check length of model_list
    expect_equal(length(model_list), length(c(nirs_channels, event_samples)))

    ## check model_list names match sample_channel and event_samples
    expect_equal(sum(grepl(sample_channel, names(model_list))),
                 length(model_list))
    expect_equal(sum(grepl(paste(event_samples/50, collapse = "|"), names(model_list))),
                 length(model_list))

    ## check class
    expect_true(all(sapply(model_list, \(.x) class(.x)) == "mnirs.kinetics"))

    model <- model_list[[2]]

    ## check list contents
    model_contents <- c("method", "model", "equation", "data",
                        "fitted", "residuals", "x0", "extreme",
                        "coefs", "diagnostics", "call")
    expect_equal(sum(names(model) %in% model_contents),
                 length(model_contents))

    ## check data colnames
    data_colnames <- c(fit_sample_name, nirs_channels[2], fitted_name[2])
    expect_equal(sum(names(model$data) %in% data_colnames),
                 length(data_colnames))

    ## length data, residuals, fitted should match
    expect_equal(length(model$residuals), length(model$fitted))
    expect_lte(length(model$residuals), nrow(model$data))
    expect_gt(length(model$residuals), 1)
    expect_lte(length(model$fitted), nrow(model$data))
    expect_gt(length(model$fitted), 1)

    ## expect a dataframe with multiple values
    expect_true(all(sapply(model[c("data", "coefs", "diagnostics")],
                           \(.x) is.data.frame(.x))))
    expect_equal(ncol(model$data), 3)
    expect_gte(ncol(model$coefs), 1)
    expect_gte(ncol(model$diagnostics), 1)

    ## visual check
    model_plot <- plot(model, display_time = FALSE, plot_coefs = TRUE,
                       plot_diagnostics = TRUE, plot_residuals = TRUE)
    expect_s3_class(model_plot, "ggplot")
})
