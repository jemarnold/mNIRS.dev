#' Create a Dataframe with Metadata
#'
#' Description
#'
#' @param data A dataframe.
#' @param metadata Metadata passed along with the dataframe.
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#' metadata available with `attributes()`.
#'
#' @keywords internal
create_mnirs_data <- function(
        data,
        metadata
) {
    ## from https://github.com/fmmattioni/whippr/blob/master/R/tbl.R
    if (!is.data.frame(data))
        cli::cli_abort("You can only pass a data frame to this function.")

    nirs_data <- tibble::new_tibble(
        data,
        class = "mNIRS.data",
        nirs_device = metadata$nirs_device,
        nirs_columns = metadata$nirs_columns,
        sample_column = metadata$sample_column,
        event_column = metadata$event_column,
        sample_rate = metadata$sample_rate, ## samples per second
        event_sample_list = metadata$event_sample_list, ##
        fit_window = metadata$fit_window, ## c(-baseline, +kinetics)
        display_window = metadata$display_window, ## c(-baseline, +kinetics)
        end_kinetics_window = metadata$end_kinetics_window, ## c(10, 15, 30) sec or 1/10 kinetics_window
    )

    tibble::validate_tibble(nirs_data)

    return(nirs_data)
}
#





#' Read mNIRS Data From File
#'
#' Reads mNIRS data from file, and returns a dataframe.
#'
#' @param file_path The file path including extension (either *".xlsx"*,
#' *".xls"*, or *".csv"*) to import.
#' @param nirs_columns A character vector indicating the mNIRS data columns
#' to import from the target file. Must match exactly. A named character vector
#' can be used to rename columns.
#' @param sample_column *(optional)* A character scalar indicating the name of
#' a time or sample data column. Must match exactly. A named character vector
#' can be used to rename columns. Default is `NULL`.
#' @param event_column *(optional)* A character scalar indicating the name of
#' an event or lap data column. Must match exactly. A named character vector
#' can be used to rename columns. Default is `NULL`.
#' @param .keep_all A logical. `FALSE` (the default) will only include the
#' explicitly indicated data columns. `TRUE` will include all columns detected
#' from the file.
#' @param ... Additional arguments (see *Details*).
#'
#' @details
#' Column names must match exactly. In data files with duplicate column
#' names (such as Train.Red .csv export), the column names will match in order
#' of appearance. You may want to double check the correct columns have been
#' assigned as intended.
#'
#' Additional arguments will accept `sample_rate` as an integer scalar for the
#' recorded sample rate in Hz of the mNIRS file. This is required for certain
#' functions. If not defined explicitly, `sample_rate` will be estimated based
#' on the mean difference between values in the `sample_column`. If
#' `sample_column` is not specified, then `sample_rate` will be set to 1 Hz.
#'
#' If `sample_column` contains decimal or date-time values, `sample_rate`
#' should be correctly estimated in Hz. If `sample_column` contains integer
#' sample numbers rather than time values, then `sample_rate` will be
#' incorrectly estimated to be 1 Hz, and should be defined explicitly.
#'
#' A method has been included to correctly identify `sample_rate` for
#' *Artinis Oxysoft* files exported in English.
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#' metadata available with `attributes()`.
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
    ## TODO add #' @import dplyr & other packages

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

    ## pass through optional arguments
    args <- list(...)

    ## import from either excel or csv
    ## report error when file is open and cannot be opened by readxl
    if (grepl("xls(x)?", tools::file_ext(file_path))) {

        data_pre <- tryCatch({
            readxl::read_excel(
                path = file_path, col_names = FALSE, col_types = "text",
                n_max = 1000) |>
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

        ## detect row where nirs_columns exists, assuming this is common header
        ## row for dataframe
        header_row <- which(apply(
            data_pre[1:1000, ], 1,
            \(row) all(nirs_columns %in% row)))

    } else if (grepl("csv", tools::file_ext(file_path))) {

        all_lines <- readLines(file_path, warn = FALSE)

        header_row <- which(
            Reduce(`&`, lapply(nirs_columns, grepl, x = all_lines)))

        data_pre <- data.table::fread(file_path, fill = Inf) |>
            tibble::as_tibble()
    }

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

        data_trimmed <- readxl::read_excel(
            path = file_path, skip = header_row - 1,
            guess_max = 50000, na = c("", "NA")
        ) |>
            ## drops columns where all NA or 0
            dplyr::select(dplyr::where(\(.x) !all(is.na(.x) | .x == 0))) |>
            ## drops rows where all NA
            dplyr::filter(
                dplyr::if_any(dplyr::everything(), \(.x) !is.na(.x))
            ) |>
            suppressMessages()

    } else if (stringr::str_ends(file_path, ".csv")) {

        data_trimmed <- utils::read.csv(
            file = file_path, skip = header_row - 1,
            check.names = FALSE, na.strings = c("", "NA")
        ) |>
            tibble::as_tibble(.name_repair = "unique") |>
            ## drops empty columns where all NA
            dplyr::select(dplyr::where(\(.x) !all(is.na(.x)))) |>
            ## drops empty rows where all NA
            dplyr::filter(
                dplyr::if_any(dplyr::everything(), \(.x) !is.na(.x))
            ) |>
            suppressMessages()
    }

    ## rename named column vectors
    rename_column_function <- function(colnames) {
        names(colnames) <- if (!is.null(names(colnames))) {
            replace(names(colnames),
                    names(colnames) == "",
                    colnames[names(colnames) == ""])
        } else {colnames}

        return(colnames)
    }

    nirs_columns <- rename_column_function(nirs_columns)
    sample_column <- rename_column_function(sample_column)
    event_column <- rename_column_function(event_column)


    ## validation: check that sample_column and event_column
    if (!is.null(sample_column)) {
        if (!all(sample_column %in% names(data_trimmed))) {
            cli::cli_abort(paste(
                "{.arg sample_column} = `{.val {sample_column}}` not detected.",
                "Column names are case sensitive and should match exactly."))
        }}

    if (!is.null(event_column)) {
        if (!all(event_column %in% names(data_trimmed))) {
            cli::cli_abort(paste(
                "{.arg event_column} = `{.val {event_column}}` not detected.",
                "Column names are case sensitive and should match exactly."))
        }}

    match_columns <- function(name_vector) {
        ## Get the base names (without suffixes) of dataframe columns
        base_names <- gsub("(?<!^)\\.\\.\\.[0-9]+$", "",
                           names(data_trimmed),
                           perl = TRUE)

        ## Initialize result vector
        matched_cols <- character(length(name_vector)) |>
            setNames(names(name_vector))

        ## Track which columns have been matched already
        used_cols <- logical(length(names(data_trimmed)))

        ## For each name in the input vector
        for (i in seq_along(name_vector)) {
            target_name <- name_vector[i]

            ## Find indices of columns with matching base name
            ## which haven't been used yet
            matching_indices <- which(base_names == target_name & !used_cols)

            if (length(matching_indices) > 0) {
                ## Take the first available match
                matched_cols[i] <- names(data_trimmed)[matching_indices[1]]
                ## Mark this column as used
                used_cols[matching_indices[1]] <- TRUE
            } else {
                ## No matching column found
                matched_cols[i] <- NA_character_
            }
        }

        return(matched_cols)
    }

    nirs_columns <- match_columns(nirs_columns)
    sample_column <- match_columns(sample_column)
    event_column <- match_columns(event_column)

    data_prepared <- data_trimmed |>
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
        ## drops empty columns where all NA
        dplyr::select(dplyr::where(\(.x) !all(is.na(.x)))) |>
        dplyr::mutate(
            ## convert blank values to NA
            dplyr::across(
                dplyr::where(is.numeric),
                \(.x) ifelse(.x %in% c(Inf, -Inf, NaN), NA_real_, .x)),
            dplyr::across(
                dplyr::where(is.character),
                \(.x) ifelse(.x %in% c("", "NA"), NA_character_, .x)),
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
            dplyr::across(
                dplyr::any_of(names(sample_column)) &
                    dplyr::where(is.character),
                \(.x) as.POSIXct(.x, tryFormats = c(
                    "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%H:%M:%OS"),
                    format = "%H:%M:%OS")),
            ## adds a sequential index column
            index = dplyr::row_number(),
        ) |>
        dplyr::relocate(index)

    ## Soft check sample values if sample_column is present
    if (!is.null(names(sample_column))) {

        sample_vector <- as.numeric(data_prepared[[names(sample_column)]])

        ## validation: soft check whether sample_column has non-sequential or
        ## repeating values
        if (any(c(diff(sample_vector) <= 0, FALSE) | duplicated(sample_vector))) {
            repeated_samples <- data_prepared |>
                dplyr::filter(
                    c(diff(get(names(sample_column))) <= 0, FALSE) |
                        duplicated(get(names(sample_column)))
                ) |>
                dplyr::pull(index)

            cli::cli_warn(paste(
                "{.arg sample_column} = {.val {names(sample_column)}} has",
                "non-sequential or repeating values. Consider investigating at",
                if (length(repeated_samples) > 5) {
                    paste(
                        "sample(s)",
                        "{paste(head(repeated_samples, 3), collapse = ', ')},",
                        "and {.val {length(tail(repeated_samples, -3))}} more",
                        "samples.")
                } else {
                    "sample(s) {repeated_samples}."
                }))
        }

        ## validation: soft check gap in sample_column > 1 hr
        if (any(diff(sample_vector) > 3600)) {
            big_gap <- data_prepared |>
                dplyr::filter(
                    c(diff(get(names(sample_column))) > 3600, FALSE)
                ) |>
                dplyr::pull(index)

            cli::cli_warn(paste(
                "{.arg sample_column} = {.val {names(sample_column)}} has a gap",
                "greater than 60 minutes. Consider investigating at sample(s)",
                "{big_gap}."))
        }
    }

    ## estimate sample rate
    if ("sample_rate" %in% names(args)) {
        ## return custom input sample rate
        sample_rate <- args$sample_rate

    } else if (
        any(apply(
            data_pre[1:1000, ], 1,
            \(row) all("Export sample rate" %in% row)))
    ) {

        ## manually extract Oxysoft sample rate
        oxysoft_sample_row <- which(apply(
            data_pre[1:1000, ], 1,
            \(row) all("Export sample rate" %in% row)))

        sample_rate <- as.numeric(data_pre[oxysoft_sample_row, 2])

        cli::cli_alert_info(paste(
            "Estimated sample rate is {.val {sample_rate}} Hz.",
            "Overwrite this by re-running with {.arg sample_rate = X}"
        ))

    } else if (exists("sample_vector")) {

        ## TODO sample_rate will be incorrect if `sample_column` is sample number
        ## samples per second
        sample_rate <- head(diff(sample_vector), 100) |>
            mean(na.rm = TRUE) |>
            (\(.x) round((1/.x)/0.5)*0.5)()


        cli::cli_alert_info(paste(
            "Estimated sample rate is {.val {sample_rate}} Hz.",
            "Overwrite this by re-running with {.arg sample_rate = X}"
        ))
    } else {

        sample_rate <- 1

        cli::cli_alert_info(paste(
            "No {.arg sample_column} provided. Sample rate set to 1 Hz.",
            "Overwrite this by re-running with {.arg sample_rate = X}"
        ))
    }

    metadata <- list(
        # TODO detect NIRS device nirs_device = nirs_device,
        nirs_columns = names(nirs_columns),
        sample_column = if (!is.null(sample_column)) names(sample_column) else NA,
        event_column = if (!is.null(event_column)) names(event_column) else NA,
        sample_rate = sample_rate)

    mNIRS_data <- create_mnirs_data(data_prepared, metadata)

    return(mNIRS_data)
} ## read_data
#


## troubleshooting ===============================
## Train.Red
# mNIRS::read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\BP02-TrainRed-2025-05-06.csv)",
#     nirs_columns = c(SmO2_VL = "SmO2 unfiltered",
#                      SmO2_PS = "SmO2 unfiltered"),
#     sample_column = c(time = "Timestamp (seconds passed)"),
#     event_column = NULL,
#     .keep_all = FALSE)

# ## Moxy
# read_data(
#     file_path = r"(C:\OneDrive - UBC\5-1 Assessments\Processed Data\03-2_2021-08-10-data.xlsx)",
#     nirs_columns = c("smo2_right_VL", "smo2_left_VL"),
#     sample_column = "Time",
#     event_column = NULL)
#
# ## Moxy
# read_data(
#     file_path = r"(C:\OneDrive - UBC\JAData\1619.csv)",
#     nirs_columns = c("SmO2 Live", "SmO2 Averaged", "THb"),
#     sample_column = c("time" = "hh:mm:ss"),
#     event_column = NULL)
# #
# # ## PerfPro
# read_data(
#     file_path = r"(C:\OneDrive - UBC\JAData\Treadmill-VO2max-PerfPro-2024-12-16.xlsx)",
#     nirs_columns = "smo2_left_VL",
#     sample_column = "Time",
#     event_column = NULL)
# #
# # ## Artinis Oxysoft
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_columns = c("HHb" = "6", "O2Hb" = "7"),
#     sample_column = c("sample" = "1"),
#     event_column = c("event" = "10"))
# #
# # ## VMPro
# mNIRS::read_data(
#     file_path = r"(C:\OneDrive - UBC\JAData\DataAverage-VMPro-2023-05-17.xlsx)",
#     nirs_columns = c("right_smo2" = "SmO2[%]",
#                      "left_smo2" = "SmO2 -  2[%]",
#                      "thb" = "THb[THb]"),
#     sample_column = c("time" = "Time[hh:mm:ss]"),
#     event_column = NULL,
#     .keep_all = FALSE)
# #
# # ## VMPro
# mNIRS::read_data(
#     file_path = r"(C:\OneDrive - UBC\JAData\MoxyUnit-VMPro-2023-05-17.xlsx)",
#     nirs_columns = c("right_smo2" = "SmO2[%]",
#                      "left_smo2" = "SmO2 -  2[%]",
#                      "thb" = "THb[THb]"),
#     sample_column = c("time" = "Time[hh:mm:ss]"),
#     event_column = NULL)
# # #
#
## Train Red
# mNIRS::read_data(
#     file_path = "C:/OneDrive - UBC/Body Position Study/Raw Data/BP01-Train.Red-2025-04-01.csv",
#     nirs_columns = c("smo2_left" = "SmO2 unfiltered",
#                      # "HHb" = "HHb unfiltered",
#                      # "O2Hb" = "O2HB unfiltered",
#                      "smo2_right" = "SmO2 unfiltered"),
#     sample_column = c("time" = "Timestamp (seconds passed)"),
#     event_column = c("Lap/Event"))
#
## Oxysoft
# mNIRS::read_data(
#     file_path = "C:/OneDrive - UBC/Body Position Study/Raw Data/BP01-oxysoft-2025-04-01.xlsx",
#     nirs_columns = c("PS_O2Hb" = "2",
#                      "PS_HHb" = "3",
#                      "VL_O2Hb" = "5",
#                      "VL_HHb" = "6"),
#     sample_column = c("sample" = "1"),
#     event_column = c("event" = "10", "label" = "...11"),
#     .keep_all = FALSE)
