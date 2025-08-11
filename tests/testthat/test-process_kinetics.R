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


# set.seed(13)
# x1 <- seq(-10, 60, by = 2)
# y1 <- 2 + 0.4 * x1 + 0.04 * x1^2 + rnorm(length(x1), 0, 3)
# mydata <- tibble::tibble(xx = x1, yy = y1)
#
# inner <- function(y, x, data) {
#     ## handle data input
#     if (!is.null(data)) {
#         y_exp <- substitute(y) ## symbol from unquoted object name of y
#         y_name <- rlang::as_name(y_exp) ## quoted string name of y
#
#         x_exp <- substitute(x) ## symbol from unquoted object name of x
#         x_name <- if (is.null(x_exp)) {NULL} else {rlang::as_name(x_exp)}
#
#         ## extract y values
#         y_val <- if (y_name %in% names(data)) {
#             data[[y_name]]
#         } else {
#             stop("Column '", y_name, "' not found in data")
#         }
#
#         ## extract or create x values
#         if (is.null(substitute(x))) {
#             x_exp <- substitute(index) ## for x = NULL, name as "index"
#             x_name <- "index"
#             x_val <- seq_along(y_val)
#         } else {
#             if (x_name %in% names(data)) {
#                 x_val <- data[[x_name]]
#             } else {
#                 stop("Column '", x_name, "' not found in data")
#             }
#         }
#     } else if (is.null(data)) {
#         ## handle direct vector input
#         if (is.character(y) && length(y) == 1) {
#             stop("Column name provided but no data supplied")
#         }
#
#         y_exp <- substitute(y) ## symbol from unquoted object name of y
#         y_name <- deparse(y_exp) ## quoted string name of y
#         y_val <- y
#
#         if (is.null(substitute(x))) {
#             x_exp <- substitute(index) ## for x = NULL, name as "index"
#             x_name <- "index"
#             x_val <- seq_along(y_val)
#         } else {
#             if (is.character(x) && length(x) == 1) {
#                 stop("Column name provided but no data supplied")
#             }
#             x_exp <- substitute(x)
#             x_name <- deparse(x_exp)
#             x_val <- x
#         }
#         ## validate lengths
#         if (length(x_val) != length(y_val)) {
#             stop("x and y must have the same length")
#         }
#     }
#
#     # Create output tibble with dynamic names
#     data_out <- tibble::tibble(!!x_name := x_val, !!y_name := y_val)
#
#     return(data_out)
# }
#
# inner(y = y1, x = NULL, data = NULL)
# inner(y = y1, x = x1, data = NULL)
# inner(y = "y1", x = x1, data = NULL) ## Column name provided but no data supplied
# inner(y = y1, x = "x1", data = NULL) ## Column name provided but no data supplied
# inner(y = qq, x = NULL, data = NULL) ## object 'qq' not found
# inner(y = y1, x = qq, data = NULL) ## object 'qq' not found
#
# inner(y = y1, x = x1, data = qq) ## object 'qq' not found
# inner(y = y1, x = x1, data = mydata) ## y1 not found
# inner(y = yy, x = xx, data = mydata)
# inner(y = "yy", x = "xx", data = mydata)
# inner(y = yy, x = "xx", data = mydata)
# inner(y = "yy", x = xx, data = mydata)
# inner(y = yy, x = NULL, data = mydata)
# inner(y = "yy", x = NULL, data = mydata)
#
#
# middle <- function(y, x = NULL, data = NULL) {
#     # Get the data from inner
#     # data_out <- inner(y = {{ y }}, x = {{ x }}, data = data)
#     data_out <- eval(substitute(inner(y = y, x = x, data = data)))
#
#     # Extract components
#     x_name <- names(data_out)[1]
#     y_name <- names(data_out)[2]
#     x_val <- data_out[[1]]
#     y_val <- data_out[[2]]
#
#     return(list(
#         data_out = data_out,
#         x_name = x_name,
#         x_val = x_val,
#         y_name = y_name,
#         y_val = y_val
#     ))
# }
#
# middle(y = y1, x = NULL, data = NULL)
# middle(y = y1, x = x1, data = NULL)
# middle(y = "y1", x = x1, data = NULL) ## Column name provided but no data supplied
# middle(y = y1, x = "x1", data = NULL) ## Column name provided but no data supplied
# middle(y = qq, x = NULL, data = NULL) ## object 'qq' not found
# middle(y = y1, x = qq, data = NULL) ## object 'qq' not found
# middle(y = y1, x = x1, data = qq) ## object 'qq' not found
# middle(y = y1, x = x1, data = mydata) ## y1 not found
# middle(y = yy, x = xx, data = mydata)
# middle(y = "yy", x = "xx", data = mydata)
# middle(y = yy, x = "xx", data = mydata)
# middle(y = "yy", x = xx, data = mydata)
# middle(y = yy, x = NULL, data = mydata)
# middle(y = "yy", x = NULL, data = mydata)
#
#
# outer <- function(y, x = NULL, data = NULL) {
#     # Get the list from middle
#     # result <- middle(y = {{ y }}, x = {{ x }}, data = data)
#     data_out <- eval(substitute(inner(y = y, x = x, data = data)))
#
#     return(data_out)
# }
#
# outer(y = y1, x = NULL, data = NULL)
# outer(y = y1, x = x1, data = NULL)
# outer(y = "y1", x = x1, data = NULL) ## Column name provided but no data supplied
# outer(y = y1, x = "x1", data = NULL) ## Column name provided but no data supplied
# outer(y = qq, x = NULL, data = NULL) ## object 'qq' not found
# outer(y = y1, x = qq, data = NULL) ## object 'qq' not found
# outer(y = y1, x = x1, data = qq) ## object 'qq' not found
# outer(y = y1, x = x1, data = mydata) ## y1 not found
# outer(y = yy, x = xx, data = mydata)
# outer(y = "yy", x = "xx", data = mydata)
# outer(y = yy, x = "xx", data = mydata)
# outer(y = "yy", x = xx, data = mydata)
# outer(y = yy, x = NULL, data = mydata)
# outer(y = "yy", x = NULL, data = mydata)
