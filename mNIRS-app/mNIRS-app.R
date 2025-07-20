## setup =====================================
suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(DT)
    library(signal)
    library(mNIRS)
    library(tidyverse)
})

options(digits = 5, digits.secs = 3, scipen = 3,
        dplyr.summarise.inform = FALSE,
        tibble.print_min = 20,
        shiny.maxRequestSize = 50*1024^2)

string_to_named_vector <- function(x) {
    noquotes <- gsub('["\\\"]', '', x)
    split_vec <- unlist(strsplit(noquotes, "\\s*,\\s*"))
    split_list <- strsplit(split_vec, "\\s*=\\s*")
    setNames(sapply(split_list, \(.x) trimws(last(.x))),
             sapply(split_list, \(.x) trimws(first(.x))))
}
#
## UI ============================================
ui <- fluidPage(
    tabsetPanel(
        tabPanel(
            "Clean Data", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    # style = "position:fixed;width:inherit;",
                    width = 2,
                    ## Upload file
                    fileInput("upload_file",
                              label = NULL,
                              buttonLabel = "Upload File",
                              accept = c('.xlsx', '.xls', '.csv', '.CSV')),

                    ## Tell it which columns are which
                    textInput("nirs_columns",
                              label = "mNIRS Channel Names\n(accepts multiple)",
                              value = "smo2 = SmO2",
                              placeholder = "new_name = file_name",
                              updateOn = "blur"),
                    textInput("sample_column",
                              label = "Time/Sample Column Name",
                              value = "time = Timestamp (seconds passed)",
                              placeholder = "new_name = file_name",
                              updateOn = "blur"),
                    textInput("event_column",
                              label = "Lap/Event Column Name",
                              placeholder = "new_name = file_name",
                              updateOn = "blur"),

                    numericInput("sample_rate",
                                 label = "Sample Rate (estimated automatically)",
                                 value = 0,
                                 min = 0),

                    numericInput("downsample_rate",
                                 label = "Downsample Rate",
                                 value = 0,
                                 min = 0),

                    ## remove head/tail samples
                    numericInput("slice_head",
                                 label = "Remove Head Samples",
                                 value = 0,
                                 min = 0,
                                 step = 1),

                    numericInput("slice_tail",
                                 label = "Remove Tail Samples",
                                 value = 0,
                                 min = 0,
                                 step = 1),

                    ## Replace invalid values (column wise)
                    textInput("invalid_values",
                              label = "Replace Invalid Values",
                              placeholder = "0, 100, ...",
                              updateOn = "blur"),

                    ## Replace outliers (column wise)
                    checkboxInput("replace_outliers", "Replace Outliers"),

                    ## Replace missing values (column wise)
                    checkboxInput("replace_missing", "Replace Missing Values"),

                    ## reset start time to zero
                    checkboxInput("zero_start_time", "Zero Start Time", value = TRUE),

                    ## Filter/smooth data (column wise)
                    selectInput("filter_method",
                                label = "Digital Filter Method",
                                choices = c("none",
                                            "smooth-spline",
                                            "butterworth",
                                            "moving-average")),
                    uiOutput("filter_method_ui"),

                    ## Shift data (dataframe)
                    checkboxInput("shift_logical", "Shift Data"),
                    uiOutput("shift_data_ui"),

                    ## rescale (dataframe)
                    checkboxInput("rescale_logical", "Rescale Data"),
                    uiOutput("rescale_data_ui"),

                    ## place manual event lines in data
                    textInput("manual_events",
                              label = "Place Event Markers",
                              placeholder = "0, 100, ...",
                              updateOn = "blur"),

                    checkboxInput("keep_all", "Keep all Columns in File Export"),

                    downloadButton("download_data", "Download Data"),
                ),

                mainPanel(
                    width = 10,

                    h4("mNIRS Signals Display"),
                    plotOutput("plot", height = "600px"),

                    h4("mNIRS Data Table"),
                    DT::DTOutput("nirs_table")
                )
            )
        ),

        tabPanel(
            "Process Kinetics", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    width = 2,

                    ## TODO
                    ## prepare kinetics data
                    ## enter sample number or event label
                    # mNIRS::prepare_kinetics_data()
                    ## one mNIRS channel at a time (drop-down menu)

                    ## output: multiplot with each kinetics data & fitted data AND
                    ## tabbed table of coefs & fit criteria
                    ## main tab for each NIRS channel
                    ## multiplot for each kinetics event


                    uiOutput("kinetics_checkbox_ui"),

                    ## numeric sample_column values
                    textInput(
                        "event_sample",
                        label = "Sample Column Values to Detect Kinetics Start",
                        # value = "370",
                        placeholder = "0, 100, ..."),

                    ## character event label
                    textInput(
                        "event_label",
                        label = "Event Labels to Detect Kinetics Start",
                        placeholder = "start event"),

                    ## two-element numeric vector for the fit window
                    numericInput(
                        "fit_baseline_window",
                        label = "Sample Values to Include Before Start of Kinetics",
                        value = 30),
                    numericInput(
                        "fit_kinetics_window",
                        label = "Sample Values to Include After Start of Kinetics",
                        value = 180),

                    ## group events either distinct or ensemble
                    selectInput(
                        "group_events",
                        label = "Group Kinetics Events",
                        choices = c("distinct", "ensemble")),

                    selectInput(
                        "kinetics_method",
                        label = "Kinetics Method",
                        choices = c("Monoexponential", "Half-Recovery Time",
                                    "Peak Slope")),
                    uiOutput("peak_slope_width_ui"),

                ),

                mainPanel(
                    width = 10,

                    h4(code("<under development>")),

                    h4("mNIRS Kinetics Display"),
                    # plotOutput("kinetics_plot", height = "600px"),

                    h4("mNIRS Kinetics Coefficients Table"),
                    DT::DTOutput("kinetics_coefs")
                )
            )
        ),

        tabPanel(
            "Instructions", fluid = TRUE,
            mainPanel(
                width = 10,
                h2("Instructions"),

                h4("Upload File:"),
                p("Upload an ", code(".xlsx"), ", ", code(".xls"), ", or ",
                  code(".csv"), " file containing mNIRS data.
                  At the very least, this file should contain one column with an
                  mNIRS data channel. Files exported directly from common NIRS
                  devices in various formats should work."),

                h4("Channel Names:"),
                p("Enter the column names for the mNIRS data channels (e.g. ",
                  em("SmO2"), ", ", em("HHb"), ", ", em("TSI"), "), Sample column
                (e.g. ", em("Time"), "), and Lap/Event column.
                  Only a single mNIRS data channel column is required. Other columns
                  can be left blank if not required / if they do not exist."),
                p("Data channels can be renamed with the format ",
                  code("new_name = file_column_name"), ". Original column names must
                  match the file contents exactly. Multiple names can be separated
                  with commas, as ",
                  code("new_name1 = file_name1, new_name2 = file_name2"), "."),
                p("For example, from a ", em("Moxy"), " .csv file: ",
                  em("NIRS Channel Names:"), " ", code("smo2_left = SmO2 Live"),
                  "; ", em("Sample Column Name:"), " ", code("time = hh:mm:ss"),
                  "; [", em("Lap/Event column"), " left blank]."),

                h4("Sample Rate:"),
                p("Will be estimated automatically from the data, or can be
                  overwritten explcitly. This is required for certain functions,
                  including downsampling and proper digital filtering. Check the
                  exported file sample times to confirm."),

                h4("Downsample Rate:"),
                p("Can be defined to downsample the data and reduce the number of
                  output samples to improve signal to noise ratio."),

                h4("Remove Head/Tail Samples:"),
                p("Will remove samples from the start and end of the data,
                  respectively. This can be used to omit invalid data at the
                  head/tail of a recording, or to include only a particular
                  selection of the entire data file. "),
                p(code("<under development:
                  interactive graphical method to select portions of data>")),

                h4("Replace Invalid Values:"),
                p("A vector of numeric values can be defined to be explicitly
                  removed and interpolated across, such as ", code("0"), " and ",
                  code("100"), " when mNIRS sensors return invalid values."),

                h4("Replace Outliers:"),
                p("Will replace local outliers using a ", code("Hampel"), " filter."),

                h4("Replace Missing Values:"),
                p("Will interpolate across missing (", code("NA"), ") samples."),

                h4("Zero Start Time:"),
                p("Reset the sample/time column to start from zero, for when
                  selecting a subset of the data."),

                h4("Digital Filter Method:"),
                p("Choose a digital filter method from the drop-down options.
                  The simplest option that works well for many datasets is a
                  non-parametric ", code("smooth-spline"), "."),
                p("A ", code("Butterworth"), " filter can be defined to optimise
                  the signal-to-noise ratio for your particular signal. The most
                  common ", em("Filter Type"), " is 'low-pass'. ",
                  em("Filter Order (n)"), " should be an integer, typically
                  between ", code("[1:8]"), ". ", em("Critical Frequency"),
                  " should be a positive numeric value less than half of the sample
                  rate, in Hz. "),
                p(code("<under development: interactive graphical method to compare
                       digital filter methods>")),

                h4("Shift Data:"),
                p("Will shift mNIRS channels either together (with ",
                  em("Channels to Shift"), " = 'ensemble') or separately (with
                  'distinct') to the value specified by ", em("Value to Shift"),
                  ". ", em("Position to Shift"), " defines which reference values
                  from the data channel(s) to shift. ", em("Samples to Shift"),
                  " defines over how many samples to shift the mean value."),
                p("For example, each mNIRS channel can be shifted so that the first
                  30-sec mean value begins at zero, with the settings: ",
                  em("Value to Shift"), " = ", code("0"), "; ",
                  em("Channels to Shift"), " = 'distinct'; ",
                  em("Position to Shift"), " = 'first'; ", em("Samples to Shift"),
                  " = ", code("[30 * Sample Rate]"), " (equivalent to 30 seconds)."),

                h4("Rescale Data:"),
                p("Will re-scale the mNIRS channels either together (",
                  em("Channels to Rescale"), " = 'ensemble') or separately (with
                  'distinct') to the data range specified by ",
                  em("Rescale Range Minimum/Maximum"), "."),
                p("For example, all present mNIRS channels can be re-scaled so that
                  the range of data are within range of ", code("0-100"),
                  ", preserving the relative scaling of each channel to the other,
                  with the settings: ", em("Rescale Range Minimum"), " = ",
                  code("0"), "; ", em("Rescale Range Maximum"), " = ", code("100"),
                  "; ", em("Channels to Rescale"), " = 'ensemble'."),

                h4("Keep all columns in file export:"),
                p("Will keep all columns present from the original data file when
                  exporting the processed file, along with the mNIRS, sample, and
                  lap/event columns specified explicitly."),

                h4("Download Data:"),
                p("Will open a dialogue box to save an ", code(".xlsx"), " file
                  with the processed data to your system."),

                h4("Clean Data Tab:"),
                p("mNIRS file data will be displayed in a plot and data table, and
                  updated according to manually entered data cleaning parameters"),

                h4("Process kinetics Tab"),
                p(code("<under development>")),
            )
        )
    )

)
#
## server ===========================================
server <- function(input, output, session) {
    ## set delay in case tab-out before full string completion
    nirs_columns_debounced <- debounce(reactive(input$nirs_columns), 2000)
    sample_column_debounced <- debounce(reactive(input$sample_column), 2000)
    event_column_debounced <- debounce(reactive(input$event_column), 2000)


    # Data upload and processing
    raw_data <- reactive({
        req(input$upload_file, nirs_columns_debounced(), sample_column_debounced())

        upload_file <- input$upload_file$datapath

        data <- tryCatch(
            mNIRS::read_data(
                file_path = upload_file,
                nirs_columns = string_to_named_vector(nirs_columns_debounced()),
                sample_column = string_to_named_vector(sample_column_debounced()),
                event_column = string_to_named_vector(event_column_debounced()),
                sample_rate = input$sample_rate,
                numeric_time = TRUE,
                keep_all = input$keep_all,
                verbose = FALSE),
            error = \(e) {
                ## remove CLI formatting from error message
                gsub('\033\\[34m\\"|\\"\033\\[39m', '', e$message)
            })

        validate(need(!inherits(data, "character"), data))

        return(data)
    })


    ## update `sample_rate`
    observe({
        updateNumericInput(session,
                           inputId = "sample_rate",
                           value = attributes(raw_data())$sample_rate)
    })

    ## Create dynamic UI for filter method
    output$filter_method_ui <- renderUI({
        req(raw_data(), input$filter_method,
            nirs_columns_debounced(), sample_column_debounced())

        raw_data <- raw_data()
        sample_rate <- attributes(raw_data)$sample_rate

        ## different UI based on selection
        if (input$filter_method == "butterworth") {
            tagList(
                selectInput(
                    "butter_type",
                    label = "Butterworth Filter Type",
                    choices = c("low", "high", "stop", "pass")),
                numericInput(
                    "n",
                    label = "Filter Order (n)",
                    value = 2,
                    min = 1,
                    max = 10,
                    step = 1),
                # numericInput(
                #   "W",
                #   label = "Fractional Critical Frequency (W)",
                #   value = 0.1,
                #   min = 0,
                #   max = 1,
                #   step = 0.01),
                numericInput(
                    "critical_frequency",
                    label = "Critical Frequency (Hz)",
                    value = 0.1,
                    min = 0,
                    max = sample_rate/2,
                    step = 0.05)
            )
        } else if (input$filter_method == "moving-average") {
            tagList(
                numericInput(
                    "width",
                    label = "width",
                    value = 15,
                    min = 1,
                    step = 1)
            )
        } else {
            NULL
        }
    })

    ## Create dynamic UI for shift option
    output$shift_data_ui <- renderUI({
        req(raw_data(), input$shift_logical,
            nirs_columns_debounced(), sample_column_debounced())

        if (input$shift_logical) {
            tagList(
                numericInput(
                    "shift_value",
                    label = "Value to Shift",
                    value = 0),
                selectInput(
                    "shift_which_cols",
                    label = "Channels to Shift",
                    choices = c("ensemble", "distinct")),
                selectInput(
                    "shift_position",
                    label = "Position to Shift",
                    choices = c("minimum", "maximum", "first")),
                numericInput(
                    "shift_samples",
                    label = "Samples to Shift",
                    value = 1),
            )
        }
    })

    ## Create dynamic UI for rescale option
    output$rescale_data_ui <- renderUI({
        req(raw_data(), input$rescale_logical,
            nirs_columns_debounced(), sample_column_debounced())

        if (input$rescale_logical) {
            tagList(
                numericInput(
                    "rescale_min",
                    label = "Rescale Range Minimum",
                    value = 0),
                numericInput(
                    "rescale_max",
                    label = "Rescale Range Maximum",
                    value = 100),
                selectInput(
                    "rescale_which_cols",
                    label = "Channels to Rescale",
                    choices = c("ensemble", "distinct")),
            )
        }
    })


    ## dynamic UI for process_kinetics kinetics_y
    output$kinetics_checkbox_ui <- renderUI({
        req(raw_data())

        raw_data <- raw_data()
        nirs_columns <- attributes(raw_data)$nirs_columns
        nirs_columns_list <- setNames(c(1:length(nirs_columns)), nirs_columns)

        checkboxGroupInput("kinetics_y",
                           "Select all mNIRS channels that apply",
                           choices = nirs_columns_list,
                           selected = 1:length(nirs_columns))
    })



    ## dynamic UI for process_kinetics peak_slope width
    output$peak_slope_width_ui <- renderUI({
        req(raw_data())

        if (input$kinetics_method == "Peak Slope") {
            tagList(
                numericInput(
                    "peak_slope_width",
                    label = "Peak Slope Width (units of x-axis)",
                    value = 10, min = 2),
            )
        }

    })



    nirs_data <- reactive({
        req(raw_data(), nirs_columns_debounced(), sample_column_debounced())

        raw_data <- raw_data()
        nirs_columns <- attributes(raw_data)$nirs_columns
        sample_column <- attributes(raw_data)$sample_column
        event_column <- attributes(raw_data)$event_column
        sample_rate <- attributes(raw_data)$sample_rate
        invalid_values <- strsplit(input$invalid_values, split = "\\s*,\\s*")[[1]] |>
            as.numeric()
        manual_events <- strsplit(input$manual_events, split = "\\s*,\\s*")[[1]] |>
            as.numeric()

        nirs_data <- raw_data |>
            mNIRS::downsample_data(
                sample_column = sample_column,
                sample_rate = sample_rate,
                downsample_rate = input$downsample_rate,
                verbose = FALSE
            ) |>
            ## remove the head rows
            (\(.df) if (input$slice_head > 0) {
                slice_tail(.df, n = -input$slice_head)
                # filter(.df, .data[[sample_column]] > input$slice_head)
            } else {.df})() |>
            ## remove the tail rows
            (\(.df) if (input$slice_tail > 0) {
                slice_head(.df, n = -input$slice_tail)
                # filter(.df, .data[[sample_column]] < input$slice_tail)
            } else {.df})() |>
            mutate(
                if (input$replace_outliers) {
                    across(
                        any_of(nirs_columns),
                        \(.x) mNIRS::replace_outliers(
                            .x,
                            width = 5 * sample_rate,  ## 5-sec window
                            na.rm = TRUE,
                            return = "median"))
                },
                if (!is.null(invalid_values)) {
                    across(
                        any_of(nirs_columns),
                        \(.x) mNIRS::replace_invalid(
                            .x,
                            values = invalid_values,
                            width = 5 * sample_rate, ## 5-sec window
                            return = "median"))
                },
                if (input$replace_missing) {
                    across(
                        any_of(nirs_columns),
                        \(.x) mNIRS::replace_missing(
                            .x, method = "linear", na.rm = TRUE))
                },
                if (input$filter_method == "smooth-spline") {
                    across(
                        any_of(nirs_columns),
                        \(.x) mNIRS::filter_data(
                            .x, method = input$filter_method))
                } else if (input$filter_method == "butterworth") {
                    req(input$n, input$critical_frequency)

                    across(
                        any_of(nirs_columns),
                        \(.x) mNIRS::filter_data(
                            .x, method = "butterworth",
                            type = input$butter_type,
                            n = input$n,
                            critical_frequency = input$critical_frequency,
                            sample_rate = sample_rate)
                        # \(.x) filtfilt_edges2(
                        #     .x,
                        #     type = input$butter_type,
                        #     n = input$n,
                        #     W = input$critical_frequency / (sample_rate/2))
                    )
                } else if (input$filter_method == "moving-average") {
                    req(input$width)

                    across(
                        any_of(nirs_columns),
                        \(.x) mNIRS::filter_data(
                            .x, method = input$filter_method,
                            width = input$width))
                },
            ) |>
            (\(.df) if (input$shift_logical) {
                req(input$shift_value, input$shift_position,
                    input$shift_which_cols, input$shift_samples)

                if (input$shift_which_cols == "ensemble") {
                    shift_nirs_columns <- nirs_columns
                } else if (input$shift_which_cols == "distinct") {
                    shift_nirs_columns <- as.list(nirs_columns)
                }

                mNIRS::shift_data(
                    data = .df,
                    nirs_columns = shift_nirs_columns,
                    shift_to = input$shift_value,
                    position = input$shift_position,
                    mean_samples = input$shift_samples,
                )
            } else {.df})() |>
            (\(.df) if (input$rescale_logical) {
                req(input$rescale_min, input$rescale_max,
                    input$rescale_which_cols)

                if (input$rescale_which_cols == "ensemble") {
                    rescale_nirs_columns <- nirs_columns
                } else if (input$rescale_which_cols == "distinct") {
                    rescale_nirs_columns <- as.list(nirs_columns)
                }

                mNIRS::rescale_data(
                    data = .df,
                    nirs_columns = rescale_nirs_columns,
                    rescale_range = c(input$rescale_min, input$rescale_max)
                )
            } else {.df})() |>
            mutate(
                ## reset sample/time values to zero
                if (input$zero_start_time) {
                    across(any_of(sample_column), \(.x) .x - first(.x))
                },
                event = if (isTruthy(manual_events) & !isTruthy(event_column)) {
                    case_when(
                        .data[[sample_column]] %in% manual_events ~
                            as.character(.data[[sample_column]]),
                        TRUE ~ NA_character_)
                },
                if (isTruthy(manual_events) & isTruthy(event_column)) {
                    across(any_of(event_column),
                           \(.x) case_when(
                               .data[[sample_column]] %in% manual_events ~
                                   if(is.numeric(.x)) {
                                       .data[[sample_column]]
                                   } else {as.character(.data[[sample_column]])},
                               TRUE ~ .x)
                    )
                },
                across(any_of(nirs_columns), \(.x) round(.x, 2)),
                across(any_of(sample_column),
                       \(.x) round(.x * sample_rate) / sample_rate),
            )

        return(nirs_data)

    })


    output$nirs_table <- DT::renderDT({
        req(raw_data(), nirs_data())

        DT::datatable(
            nirs_data(),
            options = list(
                pageLength = 20,
                scrollX = TRUE,
                searchHighlight = TRUE
            ))
    })



    output$plot <- renderPlot({
        req(raw_data(), nirs_data(),
            nirs_columns_debounced(), sample_column_debounced())

        nirs_data <- nirs_data()
        manual_events <- strsplit(input$manual_events, split = "\\s*,\\s*")[[1]] |>
            as.numeric()

        plot(nirs_data) +
            if (!is.null(manual_events)) {
                geom_vline(xintercept = manual_events, linetype = "dashed")
            } else {NULL}
    })



    output$download_data <- downloadHandler(

        filename = function() {
            paste0("mNIRS_processed_", Sys.time(), ".xlsx")
        },

        content = function(file) {
            writexl::write_xlsx(nirs_data(), path = file)
        }
    )



    kinetics_model_list <- reactive({
        req(nirs_data(), isTruthy(input$event_sample) | isTruthy(input$event_label),
            input$fit_baseline_window, input$fit_kinetics_window)

        nirs_data <- nirs_data()
        nirs_columns <- attributes(nirs_data)$nirs_columns
        sample_column <- attributes(nirs_data)$sample_column
        event_column <- attributes(nirs_data)$event_column
        sample_rate <- attributes(nirs_data)$sample_rate
        event_sample <- strsplit(input$event_sample, split = "\\s*,\\s*")[[1]] |>
            as.numeric()

        data_list <- prepare_kinetics_data(
            nirs_data,
            event_sample = event_sample,
            event_label = input$event_label,
            fit_window = c(input$fit_baseline_window, input$fit_kinetics_window),
            group_events = input$group_events)

        # kinetics_model_list <- purrr::pmap(
        #     expand_grid(.df = data_list, .nirs = nirs_columns),
        #     \(.df, .nirs)
        #     process_kinetics(x = paste0("fit_", sample_column),
        #                      y = .nirs,
        #                      data = .df,
        #                      method = input$kinetics_method,
        #                      width = input$peak_slope_width)
        # )

        return(nirs_data)
    })



    output$kinetics_coefs <- DT::renderDT({
        req(kinetics_model_list())

        coef_data <- kinetics_model_list()#[[1]]

        # coef_data <- purrr::imap(
        #     kinetics_model_list(),
        #     \(.x, idx)
        #     tibble::as_tibble(as.list(c(.x$coefs))) |>
        #         dplyr::mutate(
        #             event = idx,
        #             channel = names(.x$data)[2]
        #         ) |>
        #         dplyr::relocate(event, channel)
        # ) |>
        #     purrr::list_rbind()

        DT::datatable(
            coef_data,
            options = list(
                pageLength = 20,
                scrollX = TRUE,
                searchHighlight = TRUE
            ))
    })


}

shinyApp(ui = ui, server = server)
