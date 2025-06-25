#' Create an mNIRS Dataframe with Metadata
#'
#' Manually add class "mNIRS.data" and metadata to an existing dataframe.
#'
#' @param data A dataframe.
#' @param metadata Metadata passed along with the dataframe.
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#'  metadata available with `attributes()`.
#'
#' @examples
#' ## currently implemented metadata
#' metadata <- list(nirs_device = NULL,
#'                  nirs_columns = NULL,
#'                  sample_column = NULL,
#'                  event_column = NULL,
#'                  sample_rate = NULL,
#'                  event_sample_list = NULL,
#'                  fit_window = NULL,
#'                  display_window = NULL,
#'                  end_kinetics_window = NULL)
#'
#' df <- data.frame(A = 1:3,
#'                  B = seq(10, 30, 10),
#'                  C = seq(11, 33, 11))
#' attributes(df)
#'
#' nirs_data <- create_mNIRS_data(
#'     df,
#'     metadata = list(nirs_columns = c("B", "C"),
#'                     sample_column = "A",
#'                     sample_rate = 1)
#' )
#' attributes(nirs_data)
#'
#' @export
create_mNIRS_data <- function(
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
#' Create an mNIRS dataframe imported from file.
#'
#' @param file_path The file path including extension (either *".xlsx"*,
#'  *".xls"*, or *".csv"*) to import.
#' @param nirs_columns A character vector indicating the mNIRS data columns
#'  to import from the target file. Must match exactly. A named character vector
#'  can be used to rename columns (see *Details*).
#' @param sample_column (*Optional*). A character scalar indicating the name of
#'  a time or sample data column. Must match exactly. A named character vector
#'  can be used to rename columns.
#' @param event_column (*Optional*). A character scalar indicating the name of
#'  an event or lap data column. Must match exactly. A named character vector
#'  can be used to rename columns.
#' @param sample_rate (*Optional*). A numeric scalar for the sample rate in Hz.
#'  If not defined explicitly, will be estimated from the file data
#'  (see *Details*).
#' @param .numeric_time A logical. `TRUE` (*default*) will convert
#'  date-time formatted columns to numeric values in seconds. `FALSE` will retain
#'  these columns formatted as date-time in the format of the original file.
#' @param .keep_all A logical. `FALSE` (*default*) will only include the
#'  explicitly indicated data columns. `TRUE` will include all columns detected
#'  from the file.
#' @param .verbose A logical. `TRUE` (*default*) will return warnings and
#'  messages which can be used for data error checking. `FALSE` will silence these
#'  messages. Errors will always be returned.
#'
#' @details
#' Column names are matched to a single row, anywhere in the data file, not
#' necessarily the top row of the file. Column names must match exactly to data
#' file text. If there are duplicate column names, the columns will be matched in
#' order of appearance and appended with `...X`, where `X` is the column number.
#' You may want to double check the correct columns have been assigned as intended.
#'
#' Columns can be renamed in the format `c("new_name" = "original_name")`, where
#' `"original_name"` should match the file text exactly.
#'
#' `sample_rate` is required for certain `{mNIRS}` functions. If it is not
#' defined explicitly, `sample_rate` will be estimated based
#' on the mean difference between values in the `sample_column`. If
#' `sample_column` is not defined, then `sample_rate` will be set to 1 Hz.
#' If `sample_column` in the data file contains integer row numbers, then
#' `sample_rate` will be incorrectly estimated to be 1 Hz, and should be
#' defined explicitly.
#'
#' Columns and rows which are entirely missing (`NA`) are omitted.
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#'  metadata available with `attributes()`.
#'
#' @export
read_data <- function(
        file_path,
        nirs_columns,
        sample_column = NULL,
        event_column = NULL,
        sample_rate = NULL,
        .numeric_time = TRUE,
        .keep_all = FALSE,
        .verbose = TRUE
) {
    ## TODO add #' @import dplyr & other packages

    ## validation: check file exists
    if (!file.exists(file_path)) {
        cli::cli_abort(paste(
            "{.val file_path = {file_path}} not found.",
            "Check that file exists."
        ))
    }

    ## validation: confirm recognised file types
    if (!stringr::str_ends(file_path, ".xls|.xlsx|.csv|.CSV")) {
        cli::cli_abort(paste(
            "{.val file_path = {file_path}}.",
            "Unrecognised file type. Only {.arg .xls/.xlsx} or",
            "{.arg .csv} currently recognised."
        ))
    }

    ## import from either excel or csv
    ## report error when file is open and cannot be accessed by readxl
    if (grepl("xls(x)?$", file_path)) {

        data_pre <- tryCatch({
            readxl::read_excel(
                path = file_path,
                col_names = FALSE,
                col_types = "text",
                n_max = 1000) |>
                suppressMessages()
        }, error = \(.e) {
            if (stringr::str_detect(.e$message, "cannot be opened")) {
                cli::cli_abort(paste(
                    "{e} \n",
                    "{.val file_path = {file_path}}",
                    "cannot be opened, likely because the file is in use."
                ))
            } else {stop(e)}
        })

        ## detect header row where nirs_columns exists
        header_row <- which(apply(data_pre[1:1000, ], 1,
                                  \(.row) all(nirs_columns %in% .row)))

    } else if (grepl("csv$", file_path)) {

        ## read raw lines from csv. Avoids issues with multiple empty rows
        all_lines <- readLines(file_path, warn = FALSE)

        ## detect header row where nirs_columns exists
        header_row <- which(
            Reduce(`&`, lapply(nirs_columns, grepl, x = all_lines)))

        data_pre <- data.table::fread(file_path, fill = Inf) |>
            tibble::as_tibble()
    }

    ## validation: nirs_columns must be detected to extract the proper dataframe
    if (rlang::is_empty(header_row)) {
        cli::cli_abort(paste(
            "{.val nirs_columns = {nirs_columns}} {?was/were}",
            "not detected in the data."))
    }

    ## return error if nirs_columns string is detected at multiple rows
    if (length(header_row) > 1) {
        cli::cli_abort(paste(
            "{.val nirs_columns = {nirs_columns}} {?was/were}",
            "detected at multiple rows. Please ensure that the column names",
            "in the data file are uniquely identifiable."))
    }

    ## import from either excel or csv
    ## re-read the data at the proper row to extract the dataframe
    data_trimmed <- if (stringr::str_ends(file_path, ".xls|.xlsx")) {

        readxl::read_excel(
            path = file_path,
            skip = header_row - 1,
            guess_max = 50000,
            na = c("", "NA")
        ) |>
            suppressMessages()

    } else if (stringr::str_ends(file_path, ".csv")) {

        utils::read.csv(
            file = file_path,
            skip = header_row - 1,
            check.names = FALSE,
            na.strings = c("", "NA")
        )

    } |>
        ## repair duplicate names, append "...x" where x is row_number
        tibble::as_tibble(.name_repair = "unique") |>
        suppressMessages()

    ## rename named or non-named vectors
    rename_vector <- function(vec) {
        names(vec) <- if (!is.null(names(vec))) {
            replace(names(vec),
                    names(vec) == "",
                    vec[names(vec) == ""])
        } else {vec}

        return(vec)
    }

    ## explicitly name column_name vectors
    nirs_columns <- rename_vector(nirs_columns)
    sample_column <- rename_vector(sample_column)
    event_column <- rename_vector(event_column)


    ## validation: check that sample_column exists
    if (!is.null(sample_column)) {
        if (!all(sample_column %in% names(data_trimmed))) {
            cli::cli_abort(paste(
                "{.val sample_column = {sample_column}} not detected.",
                "Column names are case sensitive and should match exactly."))
        }}

    ## validation: check that event_column exists
    if (!is.null(event_column)) {
        if (!all(event_column %in% names(data_trimmed))) {
            cli::cli_abort(paste(
                "{.val event_column = {event_column}} not detected.",
                "Column names are case sensitive and should match exactly."))
        }}

    ## function to match duplicate column names
    ## (generated with code-bot, confirmed manually)
    match_columns <- function(name_vector) {
        ## Get the base names (without suffixes) of dataframe columns
        base_names <- gsub("(?<!^)\\.\\.\\.[0-9]+$", "",
                           names(data_trimmed),
                           perl = TRUE)


        matched_cols <- stats::setNames(character(length(name_vector)),
                                 names(name_vector))

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

    ## match column names to duplicates
    nirs_columns <- match_columns(nirs_columns)
    sample_column <- match_columns(sample_column)
    event_column <- match_columns(event_column)

    data_prepared <- data_trimmed |>
        ## .keep_all selects everything, else only explicitly defined columns
        dplyr::select(
            tidyselect::any_of(c(
                sample_column,
                event_column,
                nirs_columns)),
            if (.keep_all) tidyselect::everything()
        ) |>
        ## rename columns from explicit input
        dplyr::rename(
            tidyselect::any_of(c(
                nirs_columns,
                sample_column,
                event_column))
        ) |>
        ## drops empty columns where all NA
        dplyr::select(tidyselect::where(\(.x) !all(is.na(.x)))) |>
        ## drops empty rows where all NA
        dplyr::filter(
            dplyr::if_any(tidyselect::everything(), \(.x) !is.na(.x))
        ) |>
        ## TODO 2025-06-23 do I need this?
        # ( \(.df) {
        #     ## drops rows after the first row where all NA
        #     ## c(..., 0) ensures the last row will be taken when no rows
        #     ## are all(is.na)
        #     first_allna_row <- which(
        #         diff(c(rowSums(is.na(.df)) != ncol(.df), 0)) != 0)[1]
        #     dplyr::slice_head(.df, n = first_allna_row)
        # })() |>
        dplyr::mutate(
            ## convert dttm format sample column to numeric values in seconds
            ## detects either character or dttm formats
            dplyr::across(
                tidyselect::any_of(names(sample_column)) &
                    tidyselect::where(is.character),
                \(.x) as.POSIXct(.x, tryFormats = c(
                    "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%H:%M:%OS"),
                    format = "%H:%M:%OS")),
            if (.numeric_time) dplyr::across(
                tidyselect::any_of(names(sample_column)) &
                    tidyselect::where(lubridate::is.POSIXct),
                \(.x) lubridate::hour(.x) * 3600 +
                    lubridate::minute(.x) * 60 +
                    lubridate::second(.x)),

            ## round to avoid floating point error
            dplyr::across(tidyselect::where(is.numeric), \(.x) round(.x, 3)),

            ## convert blank values to NA
            dplyr::across(
                tidyselect::where(is.numeric),
                \(.x) ifelse(.x %in% c(Inf, -Inf, NaN), NA_real_, .x)),
            dplyr::across(
                tidyselect::where(is.character),
                \(.x) ifelse(.x %in% c("", "NA"), NA_character_, .x)),
        )

    ## Soft check sample values if sample_column is present
    if (!is.null(names(sample_column))) {

        sample_vector <- as.numeric(data_prepared[[names(sample_column)]])

        ## validation: soft check whether sample_column has non-sequential or
        ## repeating values
        if (
            any(c(diff(sample_vector) <= 0, FALSE) | duplicated(sample_vector))
        ) {
            repeated_samples <- data_prepared |>
                dplyr::filter(
                    c(diff(get(names(sample_column))) <= 0, FALSE) |
                        duplicated(get(names(sample_column)))
                ) |>
                dplyr::pull(tidyselect::any_of(names(sample_column)))

            ## warning message about repeated samples
            if (.verbose) {
                cli::cli_warn(paste(
                    "{.val sample_column = {names(sample_column)}} has",
                    "non-sequential or repeating values. Consider",
                    "investigating at",
                    if (length(repeated_samples) > 5) {
                        paste(
                            "{.arg {names(sample_column)} =",
                            "{paste(head(repeated_samples, 3), collapse = ', ')}},",
                            "and {.val {length(tail(repeated_samples, -3))}} more",
                            "samples.")
                    } else {
                        "{.val {names(sample_column)} = {repeated_samples}}."
                    }))
            }
        }

        ## validation: soft check gap in sample_column > 1 hr
        if (any(diff(sample_vector) > 3600)) {
            big_gap <- data_prepared |>
                dplyr::filter(
                    c(diff(get(names(sample_column))) > 3600, FALSE)
                ) |>
                dplyr::pull(tidyselect::any_of(names(sample_column)))

            if (.verbose) {
                cli::cli_warn(paste(
                    "{.val sample_column = {names(sample_column)}} has a gap",
                    "greater than 60 minutes. Consider investigating at",
                    "{.val {names(sample_column)} = {big_gap}}."))
            }
        }
    }

    ## detect exported sample_rate from Oxysoft (english) file
    oxysoft_sample_row <- apply(data_pre[1:1000, ], 1,
                                \(.row) all("Export sample rate" %in% .row))

    ## estimate sample rate
    if (!is.null(sample_rate) & !is.numeric(sample_rate)) {

        if (.verbose) {
            cli::cli_alert_info(paste(
                "{.arg sample_column} should be a numeric value.",
                "Sample rate set to 1 Hz. Overwrite this with"
                ,"{.arg sample_rate = X}"
            ))
        }

    } else if (is.null(sample_rate) & any(oxysoft_sample_row)) {

        ## manually extract Oxysoft sample rate
        oxysoft_sample_row <- which(oxysoft_sample_row)

        sample_rate <- as.numeric(data_pre[oxysoft_sample_row, 2])

        if (.verbose) {
            cli::cli_alert_info(paste(
                "Oxysoft detected sample rate = {.val {sample_rate}} Hz."
            ))
        }

    } else if (is.null(sample_rate) & !is.null(names(sample_column))) {

        ## sample_rate will be incorrect if `sample_column` is integer
        ## estimate samples per second
        sample_rate <- head(diff(sample_vector), 100) |>
            mean(na.rm = TRUE) |>
            (\(.x) round((1/.x)/0.5)*0.5)()

        if (.verbose) {
            cli::cli_alert_info(paste(
                "Estimated sample rate = {.val {sample_rate}} Hz."
            ))
        }

    } else if (is.null(sample_rate) & is.null(names(sample_column))) {

        sample_rate <- 1

        if (.verbose) {
            cli::cli_alert_info(paste(
                "No {.arg sample_column} provided. Sample rate set to 1 Hz.",
                "Overwrite this with {.arg sample_rate = X}"
            ))
        }
    }

    metadata <- list(
        # TODO detect NIRS device? nirs_device = nirs_device,
        nirs_columns = names(nirs_columns),
        sample_column = names(sample_column),
        event_column = names(event_column),
        sample_rate = sample_rate)

    mNIRS_data <- create_mNIRS_data(data_prepared, metadata)

    return(mNIRS_data)
} ## read_data
#


## file testing ===============================
## Train.Red
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\BP05-TrainRed-2025-05-06.csv)",
#     nirs_columns = c(SmO2_VL = "SmO2 unfiltered",
#                      SmO2_PS = "SmO2 unfiltered"),
#     sample_column = c(time = "Timestamp (seconds passed)"),
#     event_column = NULL,
#     .keep_all = FALSE,
#     .verbose = TRUE)
#
# ## Moxy PerfPro
# read_data(
#     file_path = r"(C:\OneDrive - UBC\5-1 Assessments\Processed Data\03-2_2021-08-10-data.xlsx)",
#     nirs_columns = c("smo2_right_VL", "smo2_left_VL"),
#     sample_column = "Time",
#     event_column = NULL)
#
# ## Moxy
# read_data(
#     file_path = r"(C:\OneDrive - UBC\JAData\1619.csv)",
#     nirs_columns = c(smo2 = "SmO2 Live", thb = "THb"),
#     sample_column = c(time = "hh:mm:ss"),
#     event_column = NULL,
#     .numeric_time = FALSE)
# lubridate::seconds_to_period(50590)
#
#
# #
# # ## Artinis Oxysoft
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_columns = c(HHb = "6", O2Hb = "7"),
#     sample_column = c("sample" = "1"),
#     event_column = c(event = "10"),
#     .keep_all = FALSE)
# #
# # ## VMPro
# read_data(
#     file_path = r"(C:\OneDrive - UBC\JAData\DataAverage-VMPro-2023-05-17.xlsx)",
#     nirs_columns = c(right_smo2 = "SmO2[%]",
#                      left_smo2 = "SmO2 -  2[%]",
#                      thb = "THb[THb]"),
#     sample_column = c(time = "Time[hh:mm:ss]"),
#     event_column = NULL)
# #
# # ## VMPro
# mNIRS::read_data(
#     file_path = r"(C:\OneDrive - UBC\JAData\MoxyUnit-VMPro-2023-05-17.xlsx)",
#     nirs_columns = c("right_smo2" = "SmO2[%]",
#                      "left_smo2" = "SmO2 -  2[%]",
#                      "thb" = "THb[THb]"),
#     sample_column = c("time" = "Time[hh:mm:ss]"),
#     .numeric_time = FALSE,
#     event_column = NULL)
#
