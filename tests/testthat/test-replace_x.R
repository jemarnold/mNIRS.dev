## replace_outliers ===================================
test_that("replace_outliers works correctly", {
    # devtools::load_all()
    x <- c(1, 2, 3, 16, 5, 6, 7)
    expect_equal(replace_outliers(x, width = 3)[4], median(x))
    expect_equal(replace_outliers(x, width = 3, return = "NA")[4], NA_real_)

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
                 "NAs are not allowed in subscripted assignments")
    expect_true(is.na(replace_outliers(x_na, width = 3, na.rm = TRUE)[4]))

    x_na <- c(1, 2, NA, 35, 5, 6, 7)
    expect_error(replace_outliers(x_na, width = 1, na.rm = FALSE),
                 "NAs are not allowed in subscripted assignments")
    expect_true(is.na(replace_outliers(x_na, width = 3, na.rm = TRUE)[3]))
    expect_equal(replace_outliers(x_na, width = 3, na.rm = TRUE)[4],
                 median(x_na, na.rm = TRUE))
})




test_that("preserve_na and restore_na work correctly", {
    x <- c(1, NA, 3, NA, 5)

    na_info <- preserve_na(x)
    expect_equal(na_info$x_clean, c(1, 3, 5))
    expect_equal(na_info$na_idx, c(FALSE, TRUE, FALSE, TRUE, FALSE))
    expect_equal(na_info$n_orig, 5)

    processed <- na_info$x_clean * 2
    result <- restore_na(processed, na_info)
    expect_equal(result, c(2, NA, 6, NA, 10))
    expect_true(all(is.na(result[c(2, 4)])))
})




# (\(y_length = 1000, n_rep = 50, na_prop = 0.1) { ## benchmark function
#     start_time <- Sys.time()
#     ## create a vector with noise
#     q <- seq_len(y_length)
#     z <- rnorm(1, 0, 2) * q + rnorm(y_length, mean = 0, sd = 20)
#     z[sample(length(z), round(y_length * na_prop))] <- NA
#     ## add random outliers
#     n_outliers <- round(y_length * na_prop/2)
#     outlier_idx <- sample(length(z), n_outliers)
#     outlier_values <- rnorm(n_outliers, mean = 0, sd = sd(z, na.rm = TRUE) * 5)
#     z[outlier_idx] <- z[outlier_idx] + outlier_values
#     x <- z
#     width = 20
#     t0 = 3
#     na.rm = TRUE
#     return = "median"
#
#     f1 <- \() {
#         na_idx <- is.na(x)
#         x_clean <- if (na.rm) {x[!na_idx]} else {x} ## na.omitted vector
#         y_clean <- x_clean ## eventual na.omitted vector with outliers corrected
#         n <- length(x_clean)
#         L <- 1.4826
#         for (i in seq_len(n)) {
#             # Calculate the window bounds, ensuring they stay within vector limits
#             start_idx <- max(1, i - width)
#             end_idx <- min(n, i + width)
#             x0 <- median(x_clean[start_idx:end_idx])
#             S0 <- L * median(abs(x_clean[start_idx:end_idx] - x0))
#             if (abs(x_clean[i] - x0) > t0 * S0) {
#                 y_clean[i] <- if (return == "median") {x0} else {NA_real_}
#             }
#         }
#
#         ## return y_clean to the original x with NAs
#         x[!na_idx] <- y_clean
#         return(x)
#     }
#     f2 <- \() {
#         ## logical whether to handle NAs
#         handle_na <- na.rm & any(is.na(x))
#         if (handle_na) {
#             na_info <- preserve_na(x)
#             x <- na_info$x_clean
#         }
#         n <- length(x)
#         y <- x
#         L <- 1.4826
#
#         ## vectorised window bounds
#         start_idx <- pmax(1, seq_len(n) - width)
#         end_idx <- pmin(n, seq_len(n) + width)
#
#         ## vectorised median & MAD
#         x0 <- vapply(seq_len(n), \(i) {
#             median(x[start_idx[i]:end_idx[i]])}, numeric(1))
#         S0 <- L * vapply(seq_len(n), \(i) {
#             median(abs(x[start_idx[i]:end_idx[i]] - x0[i]))}, numeric(1))
#
#         ## logical outlier positions
#         is_outlier <- abs(x - x0) > t0 * S0
#
#         ## fill outliers with median or NA
#         y[is_outlier] <- if (return == "median") {x0[is_outlier]} else {NA_real_}
#
#         ## return y to original x length with NAs if handled
#         result <- if (handle_na) {restore_na(y, na_info)} else {y}
#         return(result)
#     }
#
#     val1 <- as.vector(replicate(n_rep, f1()))
#     val2 <- as.vector(replicate(n_rep, f2()))
#     MAE <- round(mean(abs(val1 - val2), na.rm = TRUE), 10)
#
#     t1 <- system.time(replicate(n_rep, f1()))["elapsed"]
#     t2 <- system.time(replicate(n_rep, f2()))["elapsed"]
#
#     times <- c(t1/n_rep, t2/n_rep)
#     faster <- which.min(times)
#     fun_names <- c("f1()", "f2()")
#     faster_fun <- fun_names[faster]
#
#     total_time <- Sys.time() - start_time
#
#     cli::cli_alert_success("{.arg {faster_fun}} is faster by ")
#     cli::cli_alert_info("Reps = {.val {n_rep}}")
#     cli::cli_alert_info("Total runtime = {.val {round(as.numeric(total_time, units = 'secs'), 3)}} s")
#
#     data.frame(fun = fun_names,
#                mean = c(mean(val1, na.rm = TRUE), mean(val2, na.rm = TRUE)),
#                MAE = MAE,
#                time = sprintf("%d ms — %d%%",
#                               round(times*1000),
#                               round(c(t1/t2, t2/t1)*100)))
# })()

## replace_invalid ====================================
test_that("replace_invalid works correctly", {
    # devtools::load_all()
    x <- c(1, 2, 3, 16, 5, 6, 7)
    expect_equal(replace_invalid(x, values = 16)[4], NA_real_)
    expect_equal(replace_invalid(x, values = 16, width = 3,
                                 return = "median")[4], median(x))

    ## no invalid
    x_clean <- 1:7
    result_clean <- replace_invalid(x_clean, values = 16, width = 3)
    expect_equal(result_clean, x_clean)

    ## edge cases
    expect_equal(replace_invalid(c(1), values = 16), 1)
    expect_error(replace_invalid(c(1), values = 16, width = 1, return = "median"),
                 "`width` must be half the length")
    expect_equal(sum(replace_invalid(rep(1, 7), values = 16)), 7)
    expect_error(replace_invalid(rep(NA, 7), values = 16),
                 "`x` must be a")

    ## NA handling
    x_na <- c(1, 2, 3, NA, 5, 6, 7)
    expect_equal(replace_invalid(x_na, values = 35), x_na)
    expect_equal(replace_invalid(x_na, values = 35, width = 3, return = "median"),
                 x_na)
    expect_true(is.na(replace_invalid(x_na, values = 35)[4]))

    x_na <- c(1, 2, NA, 35, 5, 6, 7)
    expect_true(all(is.na(replace_invalid(x_na, values = 35)[3:4])))
    expect_equal(replace_invalid(x_na, values = 35, width = 3, return = "median")[4],
                 median(x_na, na.rm = TRUE))
})



# (\(y_length = 1000, n_rep = 100, na_prop = 0.1) { ## benchmark function
#     start_time <- Sys.time()
#     ## create a vector with noise
#     q <- seq_len(y_length)
#     z <- rnorm(1, 0, 2) * q + rnorm(y_length, mean = 0, sd = 20)
#     z[sample(length(z), round(y_length * na_prop))] <- NA
#     ## add random outliers
#     n_outliers <- round(y_length * na_prop/2)
#     outlier_idx <- sample(length(z), n_outliers)
#     z[outlier_idx] <- 100
#     x <- z
#     width = 20
#     return = "median"
#     values = 100
#
#     f1 <- \() {
#         if (return == "median") {
#             y <- x
#             n <- length(x)
#             for (i in 1:n) {
#                 # Calculate the window bounds, ensuring they stay within vector limits
#                 start_idx <- max(1, i - width)
#                 end_idx <- min(n, i + width)
#                 x0 <- median(x[start_idx:end_idx], na.rm = TRUE)
#                 if (x[i] %in% values) {
#                     y[i] <- if (return == "median") {x0} else {NA_real_}
#                 }
#             }
#
#         } else {
#             ## if `return = "NA"` then simply overwrite to `NA`
#             y <- x
#             n <- length(x)
#             for (i in 1:n) {
#                 if (x[i] %in% values) {
#                     y[i] <- NA_real_
#                 }
#             }
#
#         }
#
#         return(y)
#     }
#     f2 <- \() {
#         return <- return == "median" ## into logical
#
#         n <- length(x)
#         y <- x
#
#         if (return) {
#             ## vectorised window bounds
#             start_idx <- pmax(1, seq_len(n) - width)
#             end_idx <- pmin(n, seq_len(n) + width)
#             ## vectorised median & MAD
#             x0 <- vapply(seq_len(n), \(i) {
#                 median(x[start_idx[i]:end_idx[i]], na.rm = TRUE)}, numeric(1))
#         }
#
#         ## fill invalid with median or NA
#         y[y %in% values] <- if (return) {x0[y %in% values]} else {NA_real_}
#
#         return(y)
#     }
#
#     val1 <- as.vector(replicate(n_rep, f1()))
#     val2 <- as.vector(replicate(n_rep, f2()))
#     MAE <- round(mean(abs(val1 - val2), na.rm = TRUE), 10)
#
#     t1 <- system.time(replicate(n_rep, f1()))["elapsed"]
#     t2 <- system.time(replicate(n_rep, f2()))["elapsed"]
#
#     times <- c(t1/n_rep, t2/n_rep)
#     faster <- which.min(times)
#     fun_names <- c("f1()", "f2()")
#     faster_fun <- fun_names[faster]
#
#     total_time <- Sys.time() - start_time
#
#     cli::cli_alert_success("{.arg {faster_fun}} is faster by ")
#     cli::cli_alert_info("Reps = {.val {n_rep}}")
#     cli::cli_alert_info("Total runtime = {.val {round(as.numeric(total_time, units = 'secs'), 3)}} s")
#
#     data.frame(fun = fun_names,
#                mean = c(mean(val1, na.rm = TRUE), mean(val2, na.rm = TRUE)),
#                MAE = MAE,
#                time = sprintf("%d ms — %d%%",
#                               round(times*1000),
#                               round(c(t1/t2, t2/t1)*100)))
# })()

## replace_missing ==========================================
test_that("replace_missing works correctly", {
    # devtools::load_all()
    x <- c(1, 2, 3, 16, 5, 6, 7)
    expect_equal(replace_missing(x, "linear"), x)
    expect_equal(replace_missing(x, "locf"), x)

    x_na <- c(1, 2, 3, NA, 5, 6, 7)
    expect_equal(replace_missing(x_na, "linear")[4], 4)
    expect_equal(replace_missing(x_na, "locf")[4], 3)

    ## leading & trailing NA
    x_na <- c(NA, 2, 3, NA, 5, 6, NA)
    expect_equal(sum(is.na(replace_missing(x_na, "linear", na.rm = FALSE)[c(1, 7)])), 2)
    expect_false(is.na(replace_missing(x_na, "linear", na.rm = FALSE)[4]))
    expect_false(any(is.na(replace_missing(x_na, "linear", na.rm = TRUE))))
    expect_equal(replace_missing(x_na, "linear", na.rm = TRUE)[c(1, 7)], x_na[c(2, 6)])
    expect_equal(replace_missing(x_na, "linear", na.rm = TRUE)[c(4)], 4)

    expect_true(is.na(replace_missing(x_na, "locf", na.rm = FALSE)[1]))
    expect_equal(replace_missing(x_na, "locf", na.rm = TRUE)[c(1, 4, 7)], x_na[c(2, 3, 6)])

    ## maxgap
    x_na_long <- c(1, 2, 3, NA, NA, 6, 7)
    expect_equal(sum(is.na(replace_missing(x_na_long, "linear", maxgap = 1)[4:5])), 2)
    expect_equal(replace_missing(x_na_long, "linear", maxgap = 2)[4:5], c(4, 5))
    expect_equal(sum(is.na(replace_missing(x_na_long, "locf", maxgap = 1)[4:5])), 2)
    expect_equal(replace_missing(x_na_long, "locf", maxgap = 2)[4:5], c(3, 3))

    ## edge cases
    expect_error(replace_missing(rep(NA, 7)),
                 "`x` must be a")
})
