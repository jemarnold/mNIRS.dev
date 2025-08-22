#' Create an mNIRS Dataframe with Metadata
#'
#' Manually add class `"mNIRS.data"` and metadata to an existing dataframe.
#'
#' @param data A dataframe.
#' @param metadata Metadata passed along with the dataframe.
#'  - nirs_device
#'  - nirs_columns
#'  - sample_column
#'  - event_column
#'  - sample_rate
#'  - event_sample_list
#'  - fit_window
#'  - display_window
#'  - end_kinetics_window
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
#' @keywords internal
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
#' Will read files exported from most commercially available mNIRS devices and
#' return a dataframe of class `"mNIRS.data"` with recorded data and metadata.
#'
#' @param file_path The file path including extension (either *".xlsx"*,
#'  *".xls"*, or *".csv"*) to import.
#' @param nirs_columns A character vector indicating the mNIRS data columns
#'  to import from the file. Must match column names in the data file exactly.
#'  A named character vector can be used to rename columns in the form:
#'  `c(new_name = "old_name")` (see *Details*).
#' @param sample_column An *optional* character scalar indicating the time or
#'  sample data column to import from the file. Must match column names in the
#'  data file exactly. A named character vector can be used to rename columns in
#'  the form: `c(new_name = "old_name")` (see *Details*).
#' @param event_column An *optional* character scalar indicating the event or lap
#'  data column to import from the file. Must match column names in the data file
#'  exactly. A named character vector can be used to rename columns in the form:
#'  `c(new_name = "original_name")` (see *Details*).
#' @param sample_rate An *optional* numeric scalar for the sample rate in Hz.
#'  If not defined explicitly, will be estimated from the data (see *Details*).
#' @param numeric_time A logical. `TRUE` (the *default*) will convert
#'  a date-time formatted `sample_column` to numeric values in seconds.
#'  `FALSE` will return `sample_column` in the format of the original file.
#' @param keep_all A logical. `FALSE` (the *default*) will only include the
#'  explicitly specified data columns. `TRUE` will include all columns detected
#'  from the file.
#' @param verbose A logical. `TRUE` (the *default*) will return warnings and
#'  messages which can be used for troubleshooting. `FALSE` will silence these
#'  messages. Errors will always be returned.
#'
#' @details
#' Column names are matched to a single row, anywhere in the data file, not
#' necessarily the top row of the file.
#'
#' Columns can be renamed in the format `c(new_name = "original_name")`, where
#' `"original_name"` should match the column names found in the file exactly.
#'
#' If there are duplicate column names in the file, the columns will be matched
#' in the order in which they appear. You may want to confirm that the correct
#' columns have been assigned as intended.
#'
#' If `sample_column` is not specified, then an `index` column will be added
#' from the row numbers. If the specified `sample_column` contains unequal
#' sampling (i.e., repeated values or unordered samples) a warning will be given
#' suggesting the user confirm the file data manually.
#'
#' When the `sample_column` is provided in date-time format (e.g. `hh:mm:ss`),
#' this can be converted back to numeric values by `numeric_time = TRUE`. In this
#' case, values will be recalculated as starting from `0` at the first sample.
#'
#' `sample_column` will typically represent time values in seconds. However, some
#' NIRS devices export the sample index. This can be converted to time values
#' if the `sample_rate` is known.
#'
#' `sample_rate` is required for certain `{mNIRS}` functions to work properly.
#' If it is not defined explicitly, it will be estimated based on the mean
#' difference between values in the `sample_column`. If `sample_column` is not
#' defined, then `sample_rate` will be set to 1 Hz. If `sample_column` in the
#' data file contains integer row numbers, then `sample_rate` will be
#' incorrectly estimated to be 1 Hz, and should be defined explicitly.
#'
#' Columns and rows which contain entirely missing data (`NA`) are omitted.
#'
#' @return A [tibble][tibble::tibble-package] of class `"mNIRS.data"` with
#'  metadata available with `attributes()`.
#'
#' @export
read_data <- function(
        file_path,
        nirs_columns,
        sample_column = NULL,
        event_column = NULL,
        sample_rate = NULL,
        numeric_time = TRUE,
        keep_all = FALSE,
        verbose = TRUE
) {
    ## TODO add #' @import dplyr & other packages

    ## validation: check file exists
    if (!file.exists(file_path)) {
        cli::cli_abort(
            "{.val file_path = {file_path}} not found. Check that file exists.")
    }

    ## empty to NULL
    sample_column <- if (length(sample_column[sample_column != ""]) > 0) {
        sample_column[sample_column != ""]
    } else {NULL}
    event_column <- if (length(event_column[event_column != ""]) > 0) {
        event_column[event_column != ""]
    } else {NULL}

    ## validation: check file types
    is_excel <- grepl("\\.xls(x)?$", file_path, ignore.case = TRUE)
    is_csv <- grepl("\\.csv$", file_path, ignore.case = TRUE)

    ## import from either excel or csv
    if (is_excel) {
        ## report error when file is open and cannot be accessed by readxl
        data_full <- tryCatch({
            readxl::read_excel(
                path = file_path,
                col_names = FALSE,
                col_types = "text",
                guess_max = 10000,
                .name_repair = "minimal")
        }, error = \(.e) {
            if (grepl("cannot be opened", .e$message)) {
                cli::cli_abort(c(
                    "{e}",
                    "!" = paste("{.val file_path = {file_path}} cannot be opened,",
                                "likely because the file is in use.")))
            } else {stop(.e)}
        })
    } else if (is_csv) {
        ## read raw lines from csv. Avoids issues with multiple empty rows
        all_lines <- readLines(file_path, warn = FALSE)
        row_vectors <- strsplit(all_lines, ",")
        ## pad row length with NA for explcit rectangular
        max_cols <- max(lengths(row_vectors))
        rows_to_pad <- lengths(row_vectors) < max_cols
        row_vectors[rows_to_pad] <- lapply(
            row_vectors[rows_to_pad],
            \(.x) {
                length(.x) <- max_cols;
                .x
            })

        data_full <- as_tibble(do.call(rbind, row_vectors), .name_repair = "minimal")
    } else if (!(is_excel | is_csv)) {
        ## validation: check file types
        cli::cli_abort(c(
            "{.val file_path = {file_path}}.",
            "!" = paste("Unrecognised file type. Only {.arg .xls(x)} or",
                        "{.arg .csv} currently recognised.")))
    }

    ## detect header row where nirs_columns exists
    header_row <- which(apply(
        data_full[1:1000, ], 1,
        \(.row) all(c(nirs_columns, sample_column) %in% .row)
    ))

    ## validation: nirs_columns must be detected to extract the proper dataframe
    if (rlang::is_empty(header_row)) {
        cli::cli_abort(paste(
            "{.val data column names} not detected.",
            "Column names are case sensitive and should match exactly."))
    }

    ## return error if nirs_columns string is detected at multiple rows
    if (length(header_row) > 1) {
        cli::cli_abort(paste(
            "{.val data column names} detected at multiple rows.",
            "Please ensure that column names in the data file are unique."))
    }

    ## extract the data table, and name by header row
    data_table <- data_full[(header_row + 1):nrow(data_full), ] |>
        setNames(data_full[header_row, ])

    ## rename named or non-named vectors as unique
    make_names_unique <- function(x) {
        x_names <- names(x) %||% character(length(x))
        empty_name <- x_names == "" | is.na(x_names)

        if (is.vector(x)) {
            ## rename blank vector names from x
            x <- make.unique(as.character(x))
            x_names[empty_name] <- x
            ## rename desired name with "_" to avoid duplicates later
            unique_names <- make.unique(x_names, sep = "_")

            if(verbose && anyDuplicated(x_names)) {
                dup_names <- x_names[duplicated(x_names)]
                renamed <- unique_names[unique_names != x_names]

                cli::cli_warn(c(
                    "Duplicated input column names detected and renamed:",
                    "i" = "{.val {paste(dup_names, renamed, sep = ' = ')}}",
                    "i" = "Consider revising to unique names"
                ))
            }

            return(setNames(x, unique_names))
        } else if(is.data.frame(x)) {
            ## rename blank dataframe names from index
            x_names[empty_name] <- seq_along(x_names)[empty_name]
            unique_names <- make.unique(x_names)
            return(setNames(x, make.unique(x_names)))
        }
    }

    ## explicitly rename objects
    data_table <- make_names_unique(data_table)
    nirs_columns <- make_names_unique(nirs_columns)
    sample_column <- make_names_unique(sample_column)
    event_column <- make_names_unique(event_column)

    ## desired column names supercede existing dataframe column names
    ## EDGE CASE for when desired column matches existing column and keep_all == TRUE
    col_check <- names(data_table) %in% c(
        names(sample_column)[!sample_column %in% names(sample_column)],
        names(nirs_columns)[!nirs_columns %in% names(nirs_columns)],
        names(event_column)[!event_column %in% names(event_column)])

    names(data_table)[col_check] <- paste0(names(data_table)[col_check], "..1")

    ## validation: check that sample_column exists
    if (is.null(sample_column)) {
        sample_column_was_null <- TRUE
        sample_column <- make_names_unique("index")
        data_table[["index"]] <- 1:nrow(data_table)
        if (verbose) {
            cli::cli_alert_info(paste(
                "No {.arg sample_column} provided. Adding an {.arg index}",
                "column by row number. Provide {.arg sample_column}",
                "to overwrite this."))
        }
    } else if (!is.null(sample_column) && !sample_column %in% names(data_table)) {
        cli::cli_abort(paste(
            "{.arg sample_column} = {.val {sample_column}} not detected.",
            "Column names are case sensitive and should match exactly."))
    }

    ## validation: check that event_column exists
    if (!is.null(event_column) && !event_column %in% names(data_table)) {
        cli::cli_abort(paste(
            "{.arg event_column} = {.val {event_column}} not detected.",
            "Column names are case sensitive and should match exactly."))
    }

    ## helper to test and appropriately return numeric data
    try_numeric <- function(x) {
        numeric_x <- suppressWarnings(as.numeric(x))
        if (!all(is.na(numeric_x)) || all(is.na(x))) {
            return(numeric_x)
        } else {return(x)}
    }

    data_prepared <- data_table |>
        ## select and rename defined columns
        ## keep_all selects everything, else only explicitly defined columns
        dplyr::select(
            {{ sample_column }}, {{ event_column }}, {{ nirs_columns }},
            if (keep_all) dplyr::everything(),
        ) |>
        dplyr::mutate(
            ## appropriately convert character columns to numeric
            dplyr::across(dplyr::where(is.character), \(.x) try_numeric(.x)),
        ) |>
        ## drops empty columns where all NA
        dplyr::select(dplyr::where(\(.x) !all(is.na(.x) | .x == ""))) |>
        ## drops empty rows where all NA
        dplyr::filter(
            dplyr::if_any(dplyr::everything(), \(.x) !is.na(.x) | .x != "")
        ) |>
        dplyr::mutate(
            ## convert dttm or character to dttm "%Y-%m-%d %H:%M:%OS"
            dplyr::across(
                dplyr::any_of(names(sample_column)) & dplyr::where(is.character),
                \(.x) as.POSIXct(.x, tryFormats = c(
                    "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%dT%H:%M:%OS%z",
                    "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                    "%d-%m-%Y %H:%M:%OS", "%d/%m/%Y %H:%M:%OS",
                    "%H:%M:%OS"),
                    optional = TRUE)),

            ## convert dttm format sample column to numeric values in seconds
            if (numeric_time) dplyr::across(
                dplyr::any_of(names(sample_column)) &
                    dplyr::where(\(.x) inherits(.x, "POSIXct")),
                \(.x) {
                    num_time <- as.numeric(format(.x, "%H")) * 3600 +
                        as.numeric(format(.x, "%M")) * 60 +
                        as.numeric(format(.x, "%OS8"))
                    ## restart converted numeric time from zero
                    num_time - dplyr::first(num_time)
                }),

            ## round to avoid floating point error
            dplyr::across(dplyr::where(is.numeric), \(.x) round(.x, 8)),
            dplyr::across(
                dplyr::any_of(names(sample_column)) & dplyr::where(is.numeric),
                \(.x) round(.x, 3)),

            ## convert blank values to NA
            dplyr::across(
                dplyr::everything(),
                \(.x) dplyr::if_else(is.infinite(.x) | is.nan(.x) |
                                         .x %in% c("", "NA"), NA, .x))
        )

    sample_vector <- as.numeric(data_prepared[[names(sample_column)]])
    repeated_samples <- c(diff(sample_vector) <= 0, FALSE) | duplicated(sample_vector)

    ## validation: soft check whether sample_column has
    ## non-sequential or repeating values
    if (verbose && any(repeated_samples)) {
        repeated_samples <- sample_vector[repeated_samples]

        cli::cli_warn(c(paste(
            "{.arg sample_column = {names(sample_column)}} has",
            "non-sequential or repeating values."),
            "i" = paste("Consider investigating at",
                        if (length(repeated_samples) > 5) {
                            paste(
                                "{.arg {names(sample_column)} =",
                                "{paste(head(repeated_samples, 3), collapse = ', ')}},",
                                "and {.val {length(tail(repeated_samples, -3))}} more",
                                "samples.")
                        } else {
                            "{.val {names(sample_column)} = {repeated_samples}}."
                        })))
    }

    ## validation: soft check gap in sample_column > 1 hr
    if (verbose && any(diff(sample_vector) > 3600)) {
        big_gap <- sample_vector[c(diff(sample_vector) > 3600, FALSE)]

        cli::cli_warn(paste(
            "{.val sample_column = {names(sample_column)}} has a gap",
            "greater than 60 minutes. Consider investigating at",
            "{.val {names(sample_column)} = {big_gap}}."))
    }

    ## check for non-NULL, not applicable `sample_rate`
    if (!is.null(sample_rate) & (!is.numeric(sample_rate) || sample_rate <= 0)) {
        sample_rate <- NULL

        if (verbose) {
            cli::cli_alert_info(paste(
                "{.arg sample_rate} = {cli::col_blue('X')} should be defined",
                "explicitly as a numeric value > 0 Hz."))
        }
    }

    ## detect exported sample_rate from Oxysoft (english) file
    oxysoft_sample_row <- apply(
        data_full[1:1000, ], 1, \(.row) all("Export sample rate" %in% .row)
    )

    ## conditions to define/estimate `sample_rate`
    if (!is.null(sample_rate)) {
        sample_rate <- sample_rate
    } else if (any(oxysoft_sample_row)) {
        sample_rate <- as.numeric(data_full[which(oxysoft_sample_row), 2])

        if (verbose) {
            cli::cli_alert_info(paste(
                "Oxysoft detected sample rate = {.val {sample_rate}} Hz.",
                "Overwrite this with {.arg sample_rate} = {cli::col_blue('X')}."))
        }
    } else if (exists("sample_column_was_null")) {
        sample_rate <- 1

        if (verbose) {
            cli::cli_alert_info(paste(
                "No {.arg sample_column} provided. Sample rate set to {.val {1}} Hz.",
                "Overwrite this with {.arg sample_rate} = {cli::col_blue('X')}."))
        }
    } else {
        ## sample_rate will be incorrect if `sample_column` is integer
        ## estimate samples per second to nearest 0.5 Hz
        sample_rate <- head(diff(sample_vector), 100) |>
            mean(na.rm = TRUE) |>
            (\(.x) round((1/.x)/0.5)*0.5)()

        if (verbose) {
            cli::cli_alert_info(paste(
                "Estimated sample rate = {.val {sample_rate}} Hz.",
                "Overwrite this with {.arg sample_rate} = {cli::col_blue('X')}."))
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
}


## file testing ===============================
## Train.Red
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\BP05-TrainRed-2025-05-06.csv)",
#     nirs_columns = c(SmO2_VL = "SmO2 unfiltered",
#                      SmO2_PS = "SmO2 unfiltered"),
#     sample_column = c(time = "Timestamp (seconds passed)"),
#     event_column = NULL,
#     keep_all = FALSE,
#     verbose = TRUE)
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
#     numeric_time = FALSE)
# lubridate::seconds_to_period(50590)
#
#
#
# ## Artinis Oxysoft
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_columns = c(HHb = "6", O2Hb = "7"),
#     sample_column = c("sample" = "1"),
#     event_column = c(event = "10"),
#     keep_all = FALSE, verbose = FALSE)
#
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_columns = c(HHb = "6", O2Hb = "7"),
#     sample_column = c("sample" = "1"),
#     event_column = c(event = "10"),
#     keep_all = FALSE)
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
#     numeric_time = FALSE,
#     event_column = NULL)
#
