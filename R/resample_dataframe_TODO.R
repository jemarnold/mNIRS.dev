#' Resample Data
#'
#' Down-samples or up-samples mNIRS dataframe
#'
#' @param data A dataframe containing mNIRS data.
#' @param ... Additional arguments.
#'
#' @details
#' Down-sampling uses
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
resample_dataframe <- function(
        data,
        sample_rate = 1,
        resample_rate = 1,
        resample_by
) {

    metadata <- attributes(data)
    ## validation: check for metadata to ensure `read_data()` has been run
    if (is.null(metadata$nirs_columns)) {
        cli::cli_abort(paste(
            "No {.pkg mNIRS} metadata detected. Data should be extracted with",
            "{.fn read_data} before further processing."
        ))
    } else {
        nirs_columns <- metadata$nirs_columns
    }



    return(y)
}
#

# weighted.mean()
# use cases
# from 10 to 1
# from 0.5 to 1
# by factor of 5
