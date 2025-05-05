#' Process Kinetics
#'
#'
#'
#' @param x A numeric vector of mNIRS data.
#' @param end_kinetics_width A numeric scalar specifying the number of
#' samples in which to look for a peak or nadir value indicating the kinetics
#' plateau.
#' @param ... Additional arguments (*currently not used*).
#'
#' @details
#' ...
#'
#' @return A list `L` with `L$y` the kinetics time series and `L$x` the
#' raw data...
#'
#' @export
process_kinetics <- function(
        data,
        method = c("monoexponential", "logistic", "half-time", "peak-slope"),
        end_kinetics_width,
        ...
) {

    ## TODO
    ## intake nirs_column as vector
    ## intake idx column as vector?
    ## set end_kinetics_width
    ## choose kinetics models
    ## vector-wise processing
    ## return:
    ## - models
    ## - model coefficients values
    ## - model fit criteria (n_samples, AIC, BIC, r^2, RMSE, ...)
    ## - fitted values
    ## metadata on vector?
    ## list(
    ##     x = x,
    ##     model_monoexp = list(
    ##         table = model_monoexp,
    ##         values = data.frame(A, B, C),
    ##         fit criteria = data.frame(
    ##             n, AIC, BIC, R2, RMSE),
    ##         y = fitted.values),
    ##     model_logistic = list(...),
    ##     ...)


    ## Validation =================================
    args <- list(...)

    ## define & validation: inform `end_kinetics_width`
    if (is.null(end_kinetics_width)) {

        end_kinetics_width <- pmin(pmax(round(nrow(data) * 0.15/15)*15, 15), 30)

        cli::cli_alert_info(paste(
            "{.arg end_kinetics_width} set to {.val {end_kinetics_width}} samples"
        ))

    } else if (!rlang::is_double(end_kinetics_width) |
               !length(end_kinetics_width) == 1) {

        cli::cli_abort(paste(
            "{.arg end_kinetics_width} must be a single {.cls numeric} scalar."))

    }
    #
    ## Processing ===================================

    # x <- dplyr::filter(data, !is.na(fit_index))$VL_HHb
    #
    # max <- zoo::rollapply(
    #     x,
    #     width = 2 * end_kinetics_width + 1,
    #     FUN = max,
    #     align = "center",
    #     partial = TRUE,
    #     na.rm = TRUE) |> print()
    #
    # mean <- mean(x, na.rm = TRUE) |> print()
    #
    # peak <- dplyr::na_if((dplyr::near(x, max) & x > mean) * x, 0) |> print()

    # prepared_kinetics_data <-
        # data |>
        # dplyr::mutate(
        #     ## local peak values with no greater value within 30 samples in
        #     ## either direction
        #     ## TODO stress test centred rolling max window
        #     max = zoo::rollapply(
        #         x,
        #         width = 2 * end_kinetics_width + 1,
        #         FUN = max,
        #         align = "center",
        #         partial = TRUE,
        #         na.rm = TRUE) *
        #         dplyr::if_else(
        #             !is.na(fit_index) & fit_index >= 0, 1, NA),
        #
        #     # ## highest value preserved forward (top to bottom)
        #     # max_right = zoo::rollapplyr(
        #     #     nirs_fit_window,
        #     #     width = end_kinetics_width,
        #     #     FUN = max,
        #     #     partial = TRUE,
        #     #     na.rm = TRUE),
        #     # ## highest value preserved backward (bottom to top)
        #     # max_left = zoo::rollapply(
        #     #     nirs_fit_window,
        #     #     width = end_kinetics_width,
        #     #     FUN = max,
        #     #     align = "left",
        #     #     partial = TRUE,
        #     #     na.rm = TRUE),
        #     ## ensure the peak must be above the mean data within
        #     ## fit_kinetics_window. I think to avoid early peaks?
        #     mean = mean(x, na.rm = TRUE),
        #     peak = dplyr::na_if(
        #         (dplyr::near(x, max) & x > mean) * x, 0),
        #     ## where no local peak exists, use the last fit_kinetics_window value
        #     peak2 = (all(is.na(peak) | !peak) &
        #                  is.na(dplyr::lead(x))) + peak,
        # ) |> print(n=Inf)
        #     ## take the first peak value if multiple exist
        #     first_peak = cumsum(peak2) - ((peak2 == 0) * cumsum(peak2)) == 1,
        #
        #     ## convert Inf or NaN values to NA
        #     dplyr::across(
        #         dplyr::where(is.numeric),
        #         \(.x) dplyr::if_else(
        #             is.infinite(.x) | is.nan(.x), NA_real_, .x)),
        # ) |>
        # suppressWarnings()






    return(list(y = y, x = x))
}
#
# (data <- mNIRS::read_data(
#     file_path = "C:/OneDrive - UBC/Body Position Study/Raw Data/BP01-oxysoft-2025-04-01.xlsx",
#     nirs_columns = c("PS_O2Hb" = "2",
#                      "PS_HHb" = "3",
#                      "VL_O2Hb" = "5",
#                      "VL_HHb" = "6"),
#     sample_column = c("sample" = "1"),
#     event_column = c("label" = "...11"),
#     .keep_all = FALSE))
# # attributes(data)
# # # #
# (data_list <- mNIRS::prepare_kinetics_data(
#     data,
#     nirs_columns = c("PS_HHb", "VL_HHb"),
#     # sample_column = NULL,
#     event_column = "label",
#     # event_index = NULL,
#     # event_sample = NULL,
#     event_label = c("end RP", "end UP", "end stage"),
#     fit_baseline_window = 30,
#     fit_kinetics_window = 180,
#     display_baseline_window = 40,
#     display_kinetics_window = 240,
#     group_kinetics_events = list(c(1, 3, 5), c(2, 4)) #"ensemble"
# ))
# (data <- data_list[[1]] |> dplyr::mutate(x = VL_HHb))
