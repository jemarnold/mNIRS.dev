#' Create an mNIRS Dataframe with Metadata
#'
#' Manually add class `"mNIRS.data"` and metadata to an existing dataframe.
#'
#' @param data A dataframe.
#' @param metadata Metadata passed along with the dataframe.
#'  - nirs_device
#'  - nirs_channels
#'  - sample_channel
#'  - event_channel
#'  - sample_rate
#'  - event_sample_list
#'  - fit_span
#'  - display_span
#'  - end_kinetics_span
#'
#' @return A [tibble][tibble::tibble-package] of class `mNIRS.data` with
#'  metadata available with `attributes()`.
#'
#' @examples
#' ## currently implemented metadata
#' metadata <- list(nirs_device = NULL,
#'                  nirs_channels = NULL,
#'                  sample_channel = NULL,
#'                  event_channel = NULL,
#'                  sample_rate = NULL,
#'                  event_sample_list = NULL,
#'                  fit_span = NULL,
#'                  display_span = NULL,
#'                  end_kinetics_span = NULL)
#'
#' df <- data.frame(A = 1:3,
#'                  B = seq(10, 30, 10),
#'                  C = seq(11, 33, 11))
#' attributes(df)
#'
#' nirs_data <- create_mNIRS_data(
#'     df,
#'     metadata = list(nirs_channels = c("B", "C"),
#'                     sample_channel = "A",
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
        cli_abort("You can only pass a data frame to this function.")

    nirs_data <- tibble::new_tibble(
        data,
        class = "mNIRS.data",
        nirs_device = metadata$nirs_device,
        nirs_channels = metadata$nirs_channels,
        sample_channel = metadata$sample_channel,
        event_channel = metadata$event_channel,
        sample_rate = metadata$sample_rate, ## samples per second
        event_sample_list = metadata$event_sample_list, ##
        fit_span = metadata$fit_span, ## c(-baseline, +kinetics)
        display_span = metadata$display_span, ## c(-baseline, +kinetics)
        end_kinetics_span = metadata$end_kinetics_span, ## c(10, 15, 30) sec or 1/10 kinetics_window
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
#' @param nirs_channels A character vector indicating the mNIRS data columns
#'  to import from the file. Must match column names in the data file exactly.
#'  A named character vector can be used to rename columns in the form:
#'  `c(new_name = "old_name")` (see *Details*).
#' @param sample_channel An *optional* character scalar indicating the time or
#'  sample data column to import from the file. Must match column names in the
#'  data file exactly. A named character vector can be used to rename columns in
#'  the form: `c(new_name = "old_name")` (see *Details*).
#' @param event_channel An *optional* character scalar indicating the event or lap
#'  data column to import from the file. Must match column names in the data file
#'  exactly. A named character vector can be used to rename columns in the form:
#'  `c(new_name = "original_name")` (see *Details*).
#' @param sample_rate An *optional* numeric scalar for the sample rate in Hz.
#'  If not defined explicitly, will be estimated from the data (see *Details*).
#' @param time_to_numeric A logical. `TRUE` (the *default*) will convert
#'  a date-time formatted `sample_channel` to numeric values in seconds.
#'  `FALSE` will return `sample_channel` in the format of the original file.
#' @param time_from_zero A logical. `FALSE` (the *default*) will return the
#'  original numeric values of `sample_channel`. `TRUE` will re-sample
#'  `sample_channel` to start from zero.
#' @param keep_all A logical. `FALSE` (the *default*) will only include the
#'  explicitly specified data columns. `TRUE` will include all columns detected
#'  from the file.
#' @param verbose A logical. `TRUE` (the *default*) will return warnings and
#'  messages which can be used for troubleshooting. `FALSE` will silence these
#'  messages. Errors will always be returned.
#'
#' @details
#' Channel names are matched to a single row, representing the header row for
#' data columns anywhere in the data file, not necessarily the top row of the file.
#'
#' Channels can be renamed in the format `c(new_name = "original_name")`, where
#' `"original_name"` should match the column names found in the file exactly.
#'
#' If there are duplicate column names in the file, the channel names will be
#' matched in the order in which they appear. You may want to confirm that the
#' correct columns have been assigned to each channel as intended.
#'
#' If `sample_channel` is not specified, then an `index` column will be added
#' from the row numbers. If the specified `sample_channel` contains unequal
#' sampling (i.e., repeated values or unordered samples) a warning will be given
#' suggesting the user confirm the file data manually.
#'
#' When the `sample_channel` is provided in date-time format (e.g. `hh:mm:ss`),
#' this can be converted back to numeric values by `time_to_numeric = TRUE`. In
#' this case, values will be recalculated as starting from `0` at the first sample.
#'
#' `sample_channel` will typically represent time values in seconds. However, some
#' NIRS devices export the sample index. This can be converted to time values
#' if the `sample_rate` is known.
#'
#' `sample_rate` is required for certain `{mNIRS}` functions to work properly.
#' If it is not defined explicitly, it will be estimated based on the mean
#' difference between values in the `sample_channel`. If `sample_channel` is not
#' defined, then `sample_rate` will be set to 1 Hz. If `sample_channel` in the
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
        nirs_channels,
        sample_channel = NULL,
        event_channel = NULL,
        sample_rate = NULL,
        time_to_numeric = TRUE,
        time_from_zero = FALSE,
        keep_all = FALSE,
        verbose = TRUE
) {
    ## TODO add #' @import dplyr & other packages

    ## validation: check file exists
    if (!file.exists(file_path)) {
        cli_abort(
            "{.val file_path = {file_path}} not found. Check that file exists.")
    }

    ## empty to NULL
    sample_channel <- if (length(sample_channel[sample_channel != ""]) > 0) {
        sample_channel[sample_channel != ""]
    } else {NULL}
    event_channel <- if (length(event_channel[event_channel != ""]) > 0) {
        event_channel[event_channel != ""]
    } else {NULL}

    ## validation: check file types
    is_excel <- grepl("\\.xls(x)?$", file_path, ignore.case = TRUE)
    is_csv <- grepl("\\.csv$", file_path, ignore.case = TRUE)

    ## import data_full from either excel or csv ===============================
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
                cli_abort(c(
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
        cli_abort(c(
            "{.val file_path = {file_path}}.",
            "!" = paste("Unrecognised file type. Only {.arg .xls(x)} or",
                        "{.arg .csv} currently recognised.")))
    }

    ## detect header row where nirs_channels exists
    header_row <- which(apply(
        data_full[1:1000, ], 1,
        \(.row) all(c(nirs_channels, sample_channel) %in% .row)
    ))

    ## validation: nirs_channels must be detected to extract the proper dataframe
    if (rlang::is_empty(header_row)) {
        cli_abort(paste(
            "Channel names not detected. Names are case sensitive",
            "and should match exactly."))
    }

    ## return error if nirs_channels string is detected at multiple rows
    if (length(header_row) > 1) {
        cli_abort(paste(
            "Channel names detected at multiple rows.",
            "Please ensure that column names in the data file are unique."))
    }

    ## extract the data_raw, and name by header row ===========================
    data_raw <- data_full[(header_row + 1):nrow(data_full), ] |>
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

                cli_warn(c(
                    "Duplicated input channel names detected and renamed:",
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
    data_raw <- make_names_unique(data_raw)
    nirs_channels <- make_names_unique(nirs_channels)
    sample_channel <- make_names_unique(sample_channel)
    event_channel <- make_names_unique(event_channel)

    ## desired channel names supercede existing dataframe column names
    ## EDGE CASE for when desired channel matches existing column and keep_all == TRUE
    col_check <- names(data_raw) %in% c(
        names(sample_channel)[!sample_channel %in% names(sample_channel)],
        names(nirs_channels)[!nirs_channels %in% names(nirs_channels)],
        names(event_channel)[!event_channel %in% names(event_channel)])
    names(data_raw)[col_check] <- paste0(names(data_raw)[col_check], "..1")

    ## validation: check that sample_channel exists
    if (is.null(sample_channel)) {
        sample_channel_was_null <- TRUE
        sample_channel <- make_names_unique("index")
        data_raw[["index"]] <- 1:nrow(data_raw)
        if (verbose) {
            cli_alert_info(paste(
                "No {.arg sample_channel} provided. Adding an {.arg index}",
                "column by row number. Provide {.arg sample_channel}",
                "to overwrite this."))
        }
    } else if (!is.null(sample_channel) && !sample_channel %in% names(data_raw)) {
        cli_abort(paste(
            "{.arg sample_channel} = {.val {sample_channel}} not detected.",
            "Channel names are case sensitive and should match exactly."))
    }

    ## validation: check that event_channel exists
    if (!is.null(event_channel) && !event_channel %in% names(data_raw)) {
        cli_abort(paste(
            "{.arg event_channel} = {.val {event_channel}} not detected.",
            "Channel names are case sensitive and should match exactly."))
    }

    ## detect mNIRS device ===================================================
    ## returns NULL if not found
    devices <- c("Artinis" = "OxySoft", "Train.Red" = "Train.Red", "Moxy" = "Moxy")
    nirs_device <- names(devices)[match(TRUE, sapply(devices, \(.x) {
        any(grepl(.x, data_full[1:100, ], ignore.case = TRUE))
    }))]

    # if (verbose && !is.null(nirs_device)) {
    #     cli_alert_info(paste(
    #         "{.arg nirs_device} recognised as {.val {nirs_device}}.",
    #         "Overwrite this with {.arg nirs_device} = {.val X}."))
    # }

    ## helper to test and appropriately return numeric data
    try_numeric <- function(x) {
        numeric_x <- suppressWarnings(as.numeric(x))
        if (!all(is.na(numeric_x)) || all(is.na(x))) {
            return(numeric_x)
        } else {return(x)}
    }

    data_prepared <- data_raw |>
        ## select and rename defined columns
        ## keep_all selects everything, else only explicitly defined channels
        dplyr::select(
            {{ sample_channel }}, {{ event_channel }}, {{ nirs_channels }},
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
                dplyr::any_of(names(sample_channel)) & dplyr::where(is.character),
                \(.x) as.POSIXct(.x, tryFormats = c(
                    "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%dT%H:%M:%OS%z",
                    "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                    "%d-%m-%Y %H:%M:%OS", "%d/%m/%Y %H:%M:%OS",
                    "%H:%M:%OS"),
                    optional = TRUE)),

            ## convert dttm format sample channel to numeric values in seconds
            if (time_to_numeric) dplyr::across(
                dplyr::any_of(names(sample_channel)) &
                    dplyr::where(\(.x) inherits(.x, "POSIXct")),
                \(.x) {
                    num_time <- as.numeric(format(.x, "%H")) * 3600 +
                        as.numeric(format(.x, "%M")) * 60 +
                        as.numeric(format(.x, "%OS8"))
                    ## resample converted numeric time to start from zero
                    num_time - dplyr::first(num_time)
                }),
            ## resample time to start from zero
            if (time_from_zero) dplyr::across(
                dplyr::any_of(names(sample_channel)),
                \(.x) .x - dplyr::first(.x)),

            ## round to avoid floating point error
            dplyr::across(dplyr::where(is.numeric), \(.x) round(.x, 8)),
            dplyr::across(
                dplyr::any_of(names(sample_channel)) & dplyr::where(is.numeric),
                \(.x) round(.x, 3)),

            ## convert blank values to NA
            dplyr::across(
                dplyr::everything(),
                \(.x) dplyr::if_else(is.infinite(.x) | is.nan(.x) |
                                         .x %in% c("", "NA"), NA, .x))
        )

    sample_vector <- as.numeric(data_prepared[[names(sample_channel)]])
    repeated_samples <- c(diff(sample_vector) <= 0, FALSE) | duplicated(sample_vector)

    ## validation: soft check whether sample_channel has
    ## non-sequential or repeating values
    if (verbose && any(repeated_samples)) {
        repeated_samples <- sample_vector[repeated_samples]

        cli_warn(c(paste(
            "{.arg sample_channel = {names(sample_channel)}} has",
            "non-sequential or repeating values."),
            "i" = paste("Consider investigating at",
                        if (length(repeated_samples) > 5) {
                            paste(
                                "{.arg {names(sample_channel)} =",
                                "{paste(head(repeated_samples, 3), collapse = ', ')}},",
                                "and {.val {length(tail(repeated_samples, -3))}} more",
                                "samples.")
                        } else {
                            "{.val {names(sample_channel)} = {repeated_samples}}."
                        })))
    }

    ## validation: soft check gap in sample_channel > 1 hr
    if (verbose && any(diff(sample_vector) > 3600)) {
        big_gap <- sample_vector[c(diff(sample_vector) > 3600, FALSE)]

        cli_warn(paste(
            "{.val sample_channel = {names(sample_channel)}} has a gap",
            "greater than 60 minutes. Consider investigating at",
            "{.val {names(sample_channel)} = {big_gap}}."))
    }

    ## check for non-NULL, not applicable `sample_rate`
    if (!is.null(sample_rate) & (!is.numeric(sample_rate) || sample_rate <= 0)) {
        sample_rate <- NULL

        if (verbose) {
            cli_alert_info(paste(
                "{.arg sample_rate} = {.cls X} should be defined",
                "explicitly as a numeric value >{.val {0}} Hz."))
        }
    }

    ## detect exported sample_rate from Oxysoft (english) file
    oxysoft_sample_row <- apply(
        data_full[1:100, ], 1, \(.row) all("Export sample rate" %in% .row)
    )

    ## conditions to define/estimate `sample_rate`
    if (!is.null(sample_rate)) {
        sample_rate <- sample_rate
    } else if (any(oxysoft_sample_row)) {
        sample_rate <- as.numeric(data_full[which(oxysoft_sample_row), 2])
        # data_prepared$time <- data_prepared[[names(sample_channel)]] / sample_rate
        data_prepared <- data_prepared |>
            dplyr::mutate(
                time = .data[[names(sample_channel)]] / sample_rate
            ) |>
            dplyr::relocate(time, .after = dplyr::any_of(sample_channel))

        if (verbose) {
            cli_alert_info(paste(
                "Oxysoft detected sample rate = {.val {sample_rate}} Hz.",
                "Overwrite this with {.arg sample_rate} = {.cls X}."))
            cli_alert_info(paste(
                "{.arg time} column in seconds added from",
                "{.arg {names(sample_channel)}} and {.arg sample_rate}"))
        }
        names(sample_channel) <- "time" ## to add to metadata
    } else if (exists("sample_channel_was_null")) {
        sample_rate <- 1

        if (verbose) {
            cli_alert_info(paste(
                "No {.arg sample_channel} provided. Sample rate set to {.val {1}} Hz.",
                "Overwrite this with {.arg sample_rate} = {.cls X}."))
        }
    } else {
        ## sample_rate will be incorrect if `sample_channel` is integer
        ## estimate samples per second to nearest 0.5 Hz
        sample_rate <- head(diff(sample_vector), 100) |>
            mean(na.rm = TRUE) |>
            (\(.x) round((1/.x)/0.5)*0.5)()

        if (verbose) {
            cli_alert_info(paste(
                "Estimated sample rate = {.val {sample_rate}} Hz.",
                "Overwrite this with {.arg sample_rate} = {.cls X}."))
        }
    }

    metadata <- list(
        nirs_device = nirs_device,
        nirs_channels = names(nirs_channels),
        sample_channel = names(sample_channel),
        event_channel = names(event_channel),
        sample_rate = sample_rate)

    return(create_mNIRS_data(data_prepared, metadata))
}


## file testing ===============================
## Train.Red
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\BP05-TrainRed-2025-05-06.csv)",
#     nirs_channels = c(SmO2_VL = "SmO2 unfiltered",
#                      SmO2_PS = "SmO2 unfiltered"),
#     sample_channel = c(time = "Timestamp (seconds passed)"),
#     event_channel = NULL,
#     keep_all = FALSE,
#     verbose = TRUE)
#
# ## Moxy PerfPro
# read_data(
#     file_path = r"(C:\OneDrive - UBC\5-1 Assessments\Processed Data\03-2_2021-08-10-data.xlsx)",
#     nirs_channels = c("smo2_right_VL", "smo2_left_VL"),
#     sample_channel = "Time",
#     event_channel = NULL)
#
# ## Moxy
# read_data(
#     file_path = r"(C:\OneDrive - UBC\JAData\1619.csv)",
#     nirs_channels = c(smo2 = "SmO2 Live", thb = "THb"),
#     sample_channel = c(time = "hh:mm:ss"),
#     event_channel = NULL,
#     time_to_numeric = FALSE)
# lubridate::seconds_to_period(50590)
#
#
#
# ## Artinis Oxysoft
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_channels = c(HHb = "6", O2Hb = "7"),
#     sample_channel = c("sample" = "1"),
#     event_channel = c(event = "10"),
#     keep_all = FALSE, verbose = FALSE)
#
# read_data(
#     file_path = r"(C:\OneDrive - UBC\Body Position Study\Raw Data\SRLB02-Oxysoft-2024-12-20.xlsx)",
#     nirs_channels = c(HHb = "6", O2Hb = "7"),
#     sample_channel = c("sample" = "1"),
#     event_channel = c(event = "10"),
#     keep_all = FALSE)
# #
# # ## VMPro
# read_data(
#     file_path = r"(C:\OneDrive - UBC\JAData\DataAverage-VMPro-2023-05-17.xlsx)",
#     nirs_channels = c(right_smo2 = "SmO2[%]",
#                      left_smo2 = "SmO2 -  2[%]",
#                      thb = "THb[THb]"),
#     sample_channel = c(time = "Time[hh:mm:ss]"),
#     event_channel = NULL)
# #
# # ## VMPro
# mNIRS::read_data(
#     file_path = r"(C:\OneDrive - UBC\JAData\MoxyUnit-VMPro-2023-05-17.xlsx)",
#     nirs_channels = c("right_smo2" = "SmO2[%]",
#                      "left_smo2" = "SmO2 -  2[%]",
#                      "thb" = "THb[THb]"),
#     sample_channel = c("time" = "Time[hh:mm:ss]"),
#     time_to_numeric = FALSE,
#     event_channel = NULL)
#
