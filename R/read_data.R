#' Create a dataframe with metadata
#'
#' Learned from https://github.com/fmmattioni/whippr/blob/master/R/tbl.R
#'
#' @param .data A dataframe.
#' @param metadata Metadata passed along with the dataframe.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @keywords internal
create_mnirs_data <- function(.data, metadata) {
    if (!is.data.frame(.data))
        cli::cli_abort("You can only pass a data frame to this function.")

    nirs_data <- tibble::new_tibble(
        x = .data,
        nrow = nrow(.data),
        class = "mNIRS_data",
        read_data = metadata$read_data,
        processed_data = metadata$processed_data,
        data_status = metadata$data_status,
        nirs_device = metadata$nirs_device,
        file_path = metadata$file_path,
        nirs_columns = metadata$nirs_columns,
        sample_column = metadata$sample_column,
        event_column = metadata$event_column,
        event_indices = metadata$event_indices,
        sample_rate = metadata$sample_rate, ## samples per second
        baseline_fit_window = metadata$baseline_fit_window, ## 0-30 sec
        kinetics_fit_window = metadata$kinetics_fit_window, ## 10-300 sec
        baseline_display_window = metadata$baseline_display_window, ## 0-30 sec
        kinetics_display_window = metadata$kinetics_display_window, ## 10-300 sec
        end_kinetics_window = metadata$end_kinetics_window, ## c(10, 15, 30) sec or 1/10 kinetics_window
        fixed_values_removed = metadata$fixed_values_removed, ## c(0, 100)
        outliers_removed = metadata$outliers_removed, ## logical
        missing_index = metadata$missing_index, ## indices of missing data (NA)
        filtered = metadata$filtered, ## filter_method
        shifted_positive = metadata$shifted_positive, ## logical
        normalised = metadata$normalised, ## logical
    )

    tibble::validate_tibble(nirs_data)

    return(nirs_data)
}
#





#' Read data from mNIRS device
#'
#' This function reads and imports mNIRS data from file, and returns a
#' dataframe.
#'
#' @param file_path The local file path including extension (either *".xlsx"*,
#' *".xls"*, or *".csv"*) to import.
#' @param nirs_columns A character vector indicating the mNIRS data columns
#' to import from the target file. Must match exactly. Optionally, a named
#' vector to rename the columns.
#' @param sample_column (optional) A character scalar indicating the name of
#' the time or sample data column. Must match exactly. Default is `NULL`.
#' Optionally, a named scalar to rename the column.
#' @param event_column (optional) A character scalar indicating the name of
#' the event or lap data column. Must match exactly. Default is `NULL`.
#' Optionally, a named scalar to rename the column.
#' @param .keep_all A logical, if `FALSE` (the default) will only include
#' the target data columns. If `TRUE` will include all columns detected from
#' the file.
#' @param ... Additional arguments.
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @export
read_data <- function(
        file_path,
        nirs_columns,
        sample_column = NULL,
        event_column = NULL,
        .keep_all = FALSE,
        ...
) {
    ## validation: check file exists
    if (!file.exists(file_path)) {
        cli::cli_abort(paste(
            "{.arg file_path} = {.file {file_path}} not found.",
            "Check that file exists."
        ))
    }

    ## validation: confirm recognised file types
    if (!stringr::str_ends(file_path, ".xls|.xlsx|.csv")) {
        cli::cli_abort(paste(
            "{.arg file_path} = {.file {file_path}}.",
            "Unrecognised file type. Only {.arg .xls/.xlsx} or",
            "{.arg .csv} currently recognised."
        ))
    }

    ## import from either excel or csv
    ## report error when file is open and cannot be opened by readxl
    if (grepl("xls(x)?", tools::file_ext(file_path))) {

        raw_data_pre <- tryCatch({
            readxl::read_excel(
                path = file_path, col_names = FALSE, col_types = "text",
                n_max = 100) |>
                suppressMessages()
        }, error = \(e) {
            if (stringr::str_detect(e$message, "cannot be opened")) {
                cli::cli_abort(paste(
                    "{e} \n",
                    "{.arg file_path} = {.file {file_path}}",
                    "cannot be opened, likely because the file is in use."
                ))
            } else {stop(e)}
        })

    } else if (grepl("csv", tools::file_ext(file_path))) {

        raw_data_pre <- utils::read.csv(file = file_path, header = FALSE) |>
            tibble::as_tibble()
    }

    ## detect row where nirs_columns exists, assuming this is common header
    ## row for dataframe
    header_row <- which(apply(
        raw_data_pre[1:100, ], 1,
        \(row) all(nirs_columns %in% row)))

    ## validation: nirs_columns must be detected to extract the proper
    ## dataframe
    if (rlang::is_empty(header_row)) {
        cli::cli_abort(paste(
            "{.arg nirs_columns} = `{.val {nirs_columns}}` {?was/were}",
            "not detected in the data."))
    }

    ## return error if nirs_columns string is detected multiple times
    if (length(header_row) > 1) {
        cli::cli_abort(paste(
            "{.arg nirs_columns} = `{.val {nirs_columns}}` {?was/were}",
            "detected at multiple locations. Please ensure that the names",
            "in {.arg nirs_columns} are uniquely identifiable."))
    }

    ## import from either excel or csv
    ## re-read the data at the proper row to extract the dataframe
    if (stringr::str_ends(file_path, ".xls|.xlsx")) {

        raw_data_trimmed <- readxl::read_excel(
            path = file_path, skip = header_row - 1,
            guess_max = 50000, na = c("", "NA")) |>
            suppressMessages()

    } else if (stringr::str_ends(file_path, ".csv")) {

        raw_data_trimmed <- utils::read.csv(
            file = file_path, skip = header_row - 1,
            check.names = FALSE, na.strings = c("", "NA")) |>
            tibble::as_tibble()
    }

    ## rename nirs_columns
    names(nirs_columns) <- if (!is.null(names(nirs_columns))) {
        replace(names(nirs_columns),
                names(nirs_columns) == "",
                nirs_columns[names(nirs_columns) == ""])
    } else {nirs_columns}

    ## rename sample_column
    names(sample_column) <- if (!is.null(names(sample_column))) {
        replace(names(sample_column),
                names(sample_column) == "",
                sample_column[names(sample_column) == ""])
    } else {sample_column}

    ## rename event_column
    names(event_column) <- if (!is.null(names(event_column))) {
        replace(names(event_column),
                names(event_column) == "",
                event_column[names(event_column) == ""])
    } else {event_column}


    ## validation: check that sample_column and event_column
    if (!is.null(sample_column)) {
        if (!sample_column %in% names(raw_data_trimmed)) {
            cli::cli_abort(paste(
                "{.arg sample_column} = `{.val {sample_column}}` not detected.",
                "Column names are case sensitive and should match exactly."))
        }}

    if (!is.null(event_column)) {
        if (!event_column %in% names(raw_data_trimmed)) {
            cli::cli_abort(paste(
                "{.arg event_column} = `{.val {event_column}}` not detected.",
                "Column names are case sensitive and should match exactly."))
        }}

    raw_data_prepared <-
        raw_data_trimmed |>
        ## keep_all selects everything, else only manual columns
        dplyr::select(
            dplyr::any_of(c(
                sample_column,
                event_column,
                nirs_columns)),
            if (.keep_all) dplyr::everything()
        ) |>
        ## rename columns from manual input
        dplyr::rename(
            dplyr::any_of(c(
                nirs_columns,
                sample_column,
                event_column))
        ) |>
        ## drops columns where all(is.na()) or all(.==0) values
        dplyr::select(dplyr::where(~ !all(is.na(.) | . == 0))) |>
        dplyr::mutate(
            ## convert blank values to NA
            dplyr::across(
                dplyr::where(is.numeric),
                ~ ifelse(. %in% c(Inf, -Inf, NaN), NA_real_, .)),
            dplyr::across(
                dplyr::where(is.character),
                ~ ifelse(. %in% c("", "NA"), NA_character_, .)),
        ) |>
        ( \(df) {
            ## drops rows after the first row where all(is.na())
            ## c(..., 0) ensures the last row will be taken when no rows
            ## are all(is.na)
            first_allna_row <- which(
                diff(c(rowSums(is.na(df)) != ncol(df), 0)) != 0)[1]
            dplyr::slice_head(df, n = first_allna_row)
        })() |>
        dplyr::mutate(
            ## convert sample column to unique numeric values in seconds
            ## https://github.com/fmmattioni/whippr/blob/master/R/read-data.R
            ## detects either character or dttm formats
            ## tested on Moxy, PerfPro, Oxysoft, VMPro x2
            ## TODO test on Train.Red, NNOXX, Graspor, Oxysoft csv, ...
            dplyr::across(
                dplyr::any_of(names(sample_column)) &
                    dplyr::where(is.character),
                ~ as.POSIXct(., tryFormats = c(
                    "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%H:%M:%OS"),
                    format = "%H:%M:%OS")),
            ## adds a sequential index column
            dplyr::across(
                dplyr::any_of(names(nirs_columns[1])),
                ~ seq_along(.),
                .names = "index"),
        ) |>
        dplyr::relocate(index)

    sample_vector <- as.numeric(raw_data_prepared[[names(sample_column)]])

    ## validation: soft check whether sample_column has non-sequential or
    ## repeating values
    if (any(c(diff(sample_vector) <= 0, FALSE) | duplicated(sample_vector))) {
        repeated_samples <-
            raw_data_prepared |>
            dplyr::filter(
                c(diff(get(names(sample_column))) <= 0, FALSE) |
                    duplicated(get(names(sample_column)))
            ) |>
            dplyr::pull(index)

        cli::cli_warn(paste(
            "{.arg sample_column} = {.val {names(sample_column)}} has",
            "non-sequential or repeating values. Consider investigating at",
            "sample(s) {repeated_samples}."))
    }

    ## validation: soft check gap in sample_column > 1 hr
    if (any(diff(sample_vector) > 3600)) {
        big_gap <-
            raw_data_prepared |>
            dplyr::filter(
                c(diff(get(names(sample_column))) > 3600, FALSE)
            ) |>
            dplyr::pull(index)

        cli::cli_warn(paste(
            "{.arg sample_column} = {.val {names(sample_column)}} has a gap",
            "greater than 60 minutes. Consider investigating at sample(s)",
            "{big_gap}."))
    }

    metadata <- list(
        file_path = stringr::str_replace_all(file_path, "\\\\", "/"),
        nirs_columns = nirs_columns,
        sample_column = if (!is.null(sample_column)) sample_column else NA,
        event_column = if (!is.null(event_column)) event_column else NA,
        missing_data = any(is.na(raw_data_prepared[names(nirs_columns)])),
        ## TODO detect sample rate intelligently
        # sample_rate = ifelse( ## will be incorrect if `sample_column` is
        ## sample number
        #     "sample" %in% names(raw_data_prepared),
        #     1/mean(head(diff(sample_vector)), na.rm = TRUE), ## samples
        ## per second
        #     NA)
        NULL)

    raw_data <- create_mnirs_data(raw_data_prepared, metadata)
    # attributes(raw_data)

    return(raw_data)
} ## read_data
#


## troubleshooting ===============================
# ## Moxy
# (raw_data <-
#      read_data(
#          file_path = r"(C:\OneDrive - UBC\5-1 Assessments\Processed Data\03-2_2021-08-10-data.xlsx)",
#          nirs_columns = c("smo2_right_VL", "smo2_left_VL"),
#          sample_column = "Time",
#          event_column = NULL) |>
#      dplyr::mutate(event = dplyr::if_else(index %in% c(3009), "reoxy", NA)) |>
#      # dplyr::filter(index >= 2340, index <= 2590) |>
#      dplyr::filter(index >= 2980, index <= 3050) |> ## reoxy
#      dplyr::mutate(index = index - dplyr::first(index))
#  )
# #
# read_data(
#     file_path = r"(C:\OneDrive - UBC\FLIA Clinical Assessments\Raw Data\CSIO-LGC-2024-1126.xlsx)",
#     nirs_columns = c("SmO2" = "SmO2 Live", "SmO2 Averaged", "total[heme]" = "THb"),
#     sample_column = c("Time" = "hh:mm:ss"),
#     event_column = NULL,
#     .keep_all = TRUE)
#
# ## Moxy
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Group Projects\JAData\1619.csv)",
#     nirs_columns = c("SmO2 Live", "SmO2 Averaged", "THb"),
#     sample_column = "hh:mm:ss",
#     event_column = NULL)
# #
# # ## PerfPro
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Group Projects\JAData\Treadmill-VO2max-PerfPro-2024-12-16.xlsx)",
#     nirs_columns = "smo2_left_VL",
#     sample_column = "Time",
#     event_column = NULL) |> attributes()
# #
# # ## Artinis Oxysoft
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_columns = "HHb",
#     sample_column = "time",
#     event_column = "event")
# #
# # ## VMPro
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Group Projects\JAData\DataAverage-VMPro-2023-05-17.xlsx)",
#     nirs_columns = c("SmO2[%]", "SmO2 -  2[%]", "THb[THb]"),
#     sample_column = "Time[hh:mm:ss]",
#     event_column = NULL)
# #
# # ## VMPro
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Group Projects\JAData\MoxyUnit-VMPro-2023-05-17.xlsx)",
#     nirs_columns = c("SmO2[%]", "SmO2 -  2[%]", "THb[THb]"),
#     sample_column = "Time[s]",
#     event_column = NULL)
# # #
#
