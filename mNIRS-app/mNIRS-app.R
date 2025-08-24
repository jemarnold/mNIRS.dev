## setup =====================================
suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(DT)
    library(signal)
    library(mNIRS)
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(ggplot2)
})

# devtools::install_github("jemarnold/mNIRS", force = TRUE)

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
                    textInput("nirs_channels",
                              label = "mNIRS Channel Names\n(accepts multiple)",
                              # value = "smo2_left = SmO2, smo2_right = SmO2",
                              # value = "smo2_left = SmO2 Live, smo2_right = SmO2 Live(2)",
                              placeholder = "new_name = file_column_name",
                              updateOn = "blur"),
                    textInput("sample_channel",
                              label = "Time/Sample Channel Name",
                              # value = "time = Timestamp (seconds passed)",
                              # value = "time = hh:mm:ss",
                              placeholder = "new_name = file_column_name",
                              updateOn = "blur"),
                    textInput("event_channel",
                              label = "Lap/Event Channel Name\n(optional)",
                              placeholder = "new_name = file_column_name",
                              updateOn = "blur"),

                    numericInput("sample_rate",
                                 label = "Sample Rate (estimated automatically)",
                                 value = 0,
                                 min = 0),

                    numericInput("resample_rate",
                                 label = "Re-sample Rate",
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
                    checkboxInput("time_from_zero", "Zero Start Time"),

                    ## display x as h:mm:ss
                    checkboxInput("display_hmmss", "Display Time as (h:mm:ss)"),

                    ## Filter/smooth data (column wise)
                    selectInput("filter_method",
                                label = "Digital Filter Method",
                                choices = c("None",
                                            "Smooth-Spline",
                                            "Butterworth",
                                            "Moving-Average")),
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
                              # value = c("370, 1080"),
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

                    h4("Detect Kinetics Start"),
                    ## numeric sample_channel values
                    textInput(
                        "event_sample",
                        label = "Sample Values",
                        value = NULL,
                        placeholder = "370, 1085, ..."),

                    ## character event label
                    # textInput(
                    #     "event_label",
                    #     label = "Event Labels",
                    #     value = NULL,
                    #     placeholder = "start event"),

                    ## two-element numeric vector for the fit window
                    numericInput(
                        "fit_baseline_window",
                        label = "Sample Values to Include Before Start of Kinetics",
                        value = 30),
                    numericInput(
                        "fit_kinetics_window",
                        label = "Sample Values to Include After Start of Kinetics",
                        value = 180),

                    ## group events either Distinct or Ensemble
                    selectInput(
                        "group_events",
                        label = "Group Kinetics Events",
                        choices = c("Ensemble", "Distinct")),

                    selectInput(
                        "kinetics_method",
                        label = "Analysis Method",
                        choices = c("Peak Slope", "Half-Recovery Time",
                                    "Monoexponential", "Sigmoidal")),
                    uiOutput("peak_slope_span_ui"),

                    ## display x as h:mm:ss
                    checkboxInput("display_kinetics_hmmss",
                                  "Display Time as (mm:ss)", value = TRUE),

                    downloadButton("download_kinetics_data",
                                   "Download Kinetics Data"),

                ),

                mainPanel(
                    width = 10,

                    h4(code("<under development. Currently works for only ONE
                            kinetics event or ENSEMBLE grouping>")),

                    uiOutput("kinetic_event_tabs"),
                    # h4("mNIRS Kinetics Display"),
                    # plotOutput("kinetics_plot", height = "600px"),
                    #
                    # h4("mNIRS Kinetics Coefficients Table"),
                    # DT::DTOutput("kinetics_coefs", width = "50%")
                )
            )
        ),

        tabPanel(
            "Instructions", fluid = TRUE,
            mainPanel(
                width = 10,
                h2("Instructions"),

                hr(),
                h3(em("Clean Data"), "Tab:"),
                p("mNIRS files can be imported to display in a plot and data table,",
                  "and can be cleaned using data processing options."),

                h4("Upload File:"),
                p("Upload an", code(".xlsx"), ",", code(".xls"), ", or",
                  code(".csv"), "file containing mNIRS data.
                  This file should contain at least one column with a
                  mNIRS data channel. Files exported directly from common NIRS
                  devices in various formats should work."),

                h4("Channel Names:"),
                p("Enter the column names for the mNIRS data channels (e.g.",
                  em("SmO2"), ",", em("HHb"), ",", em("TSI"), "), Sample channel
                (e.g.", em("Time"), "), and Lap/Event channel.
                  At least a single mNIRS data channel is required. Other channels
                  can be left blank if not required / if they do not exist."),
                p("Data channels can be renamed with the format",
                  code("new_name = file_column_name"), ". Original column names must
                  match the file contents exactly. Multiple names can be separated
                  with commas, as",
                  code("new_name1 = file_name1, new_name2 = file_name2"), "."),
                p("For example, from a", em("Moxy"), ".csv file:",
                  em("NIRS Channel Names:"), code("smo2_left = SmO2 Live"),
                  ";", em("Sample Channel Name:"), code("time = hh:mm:ss"),
                  "; [", em("Lap/Event Channel Name"), "left blank]."),

                h4("Sample Rate:"),
                p("Will be estimated automatically from the data, or can be
                  overwritten explcitly. This is required for certain functions,
                  including re-sampling and proper digital filtering. Check the
                  exported file sample times to confirm."),

                h4("Re-sample Rate:"),
                p("Re-sample the data to match the sample rate of a different",
                  "data source, or make large, high-frequency data tables faster",
                  "to work with."),

                h4("Remove Head/Tail Samples:"),
                p("Will remove samples from the start and end of the data,
                  respectively. This can be used to omit invalid data at the
                  head/tail of a recording, or to include only a particular
                  selection of the entire data file."),
                p(code("<under development:
                  interactive graphical method to select portions of data>")),

                h4("Replace Invalid Values:"),
                p("A vector of numeric values can be defined to be explicitly
                  removed and interpolated across, such as", code("0"), "and",
                  code("100"), "when mNIRS sensors return invalid values."),

                h4("Replace Outliers:"),
                p("Will replace local outliers using a", code("Hampel"), "filter."),

                h4("Replace Missing Values:"),
                p("Will interpolate across missing (", code("NA"), ") samples."),

                h4("Zero Start Time:"),
                p("Reset the sample/time channel to start from zero, for when
                  selecting a subset of the data."),

                h4("Digital Filter Method:"),
                p("Choose a digital filter method from the drop-down options.
                  The simplest option that works well for many datasets is a
                  non-parametric", code("smooth-spline"), "."),
                p("A", code("Butterworth"), "filter can be defined to optimise
                  the signal-to-noise ratio for your particular signal. The most
                  common", em("Filter Type"), "is 'low-pass'.",
                  em("Filter Order (n)"), "should be an integer, typically
                  between", code("[1:8]"), ".", em("Critical Frequency"),
                  "should be a positive numeric value less than half of the sample
                  rate, in Hz."),
                p(code("<under development: interactive graphical method to compare
                       digital filter methods>")),

                h4("Shift Data:"),
                p("Will shift mNIRS channels either together (with",
                  em("Channels to Shift"), "= 'Ensemble') or separately (with
                  'Distinct') to the value specified by", em("Value to Shift"),
                  ".", em("Position to Shift"), "defines which reference values
                  from the data channel(s) to shift.", em("Samples to Shift"),
                  "defines over how many samples to shift the mean value."),
                p("For example, each mNIRS channel can be shifted so that the first
                  30-sec mean value begins at zero, with the settings:",
                  em("Value to Shift"), "=", code("0"), ";",
                  em("Channels to Shift"), "= 'Distinct';",
                  em("Position to Shift"), "= 'First';", em("Samples to Shift"),
                  "=", code("[30 * Sample Rate]"), "(equivalent to 30 seconds)."),

                h4("Rescale Data:"),
                p("Will re-scale the mNIRS channels either together (",
                  em("Channels to Rescale"), "= 'Ensemble') or separately (with
                  'Distinct') to the data range specified by",
                  em("Rescale Range Minimum/Maximum"), "."),
                p("For example, all present mNIRS channels can be re-scaled so that
                  the range of data are within range of", code("0-100"),
                  ", preserving the relative scaling of each channel to the other,
                  with the settings:", em("Rescale Range Minimum"), "=",
                  code("0"), ";", em("Rescale Range Maximum"), "=", code("100"),
                  ";", em("Channels to Rescale"), "= 'ensemble'."),

                h4("Place Event Markers:"),
                p("Vertical lines can be added to the plot display by sample",
                  "number (value on the x-axis). These can be used to manually",
                  "add event markers, such as the start or end of work intervals,",
                  "or kinetics events."),
                p("Any numeric value will appear in the plot, but only values",
                  "matching existing sample values will appear in the exported",
                  "data. These event markers will be pulled over to the",
                  em("Process Kinetics"), "tab to be used for kinetics events."),

                h4("Keep all columns in file export:"),
                p("Will keep all columns present from the original data file when
                  exporting the processed file, along with the mNIRS, sample, and
                  lap/event columns specified explicitly."),

                h4("Download Data:"),
                p("Will open a dialogue box to save an", code(".xlsx"), "file
                  with the processed data to your system."),





                hr(),
                h3(em("Process Kinetics"), "Tab:"),
                p(code("<under development>")),

                h4("Select mNIRS channels to be processed:"),
                p("Will be automatically populated with named defined in the",
                  em("Clean Data"), "tab. Can be (de-)selected as required."),

                h4("Detect Kinetics Start:"),
                p("Values can be entered manually to indicate the start of",
                  "kinetics events, by either value of the sample channel (in units",
                  "of the x-axis), or by text label in the events channel."),
                p("The range of the kinetics window (defined below) will be",
                  "re-centred on this value as zero (i.e.", em("time = 0"),
                  ")."),
                p(code("<under development. Currently only ONE kinetics event",
                       "can be processed at a time>")),

                h4("Sample Values"),
                p("Values on the x-axis (e.g.", em("time"), "or", em("sample"),
                  "can be added here. Will also be automatically populated with",
                  "values entered in", em("Place Event Markers"), "on the",
                  em("Clean Data"), "tab. e.g.,", code("370, 1085"),
                  "as second values for x-axis =", em("Time.")),

                h4("Event Labels"),
                p(code("<under development>")),

                h4("Sample Values to Include Before Start of Kinetics"),
                p("How many samples (what range of values on the x-axis) should",
                  "be included in the kinetics processing before the start",
                  "of the kinetics event (i.e.", em("time"), "=", code("0"), ")?",
                  "Default is", code("30"), "seconds."),

                h4("Sample Values to Include After End of Kinetics"),
                p("How many samples (what range of values on the x-axis) should",
                  "be included in the kinetics processing after the start",
                  "of the kinetics event (i.e.", em("time"), "=", code("0"), ")?",
                  "Default is", code("180"), "seconds."),

                h4("Group Kinetics Events"),
                p(code("<under development>")),
                p(code("Distinct"), "will plot each",
                  "added kinetics event in a separate tab."),
                p(code("Ensemble"), "will ensemble-",
                  "or time-average all added kinetics events into a single event."),

                h4("Analysis Method"),
                p("Currently, there are two parametric and two non-parametric",
                  "methods for evaluating mNIRS kinetics. Each method will display",
                  "the relevant fitted values on the kinetics plot, and will return",
                  "coefficients with the appropriate", em("x"), "and", em("y"),
                  "values. A table of model diagnostics and goodness-of-fit values",
                  "will be returned", em("(for parametric models only)"),
                  "which can be used to select the better fitting model."),
                p(code("Peak Slope:"), "will return the peak positive or negative",
                  "linear regression slope (depending on the response direction)",
                  "over a window defined by", code("span"), "(default",
                  em("span"), "=", code("10"), "seconds), as a non-parametric",
                  "estimate of the speed of the kinetics event."),
                p(code("Half-Recovery Time:"), "will return the time (or",
                  em("x value"), ") required to recover half of the total mNIRS",
                  em("y value"), "amplitude during the kinetics event, as a",
                  "non-parametric estimate of the time course of the kinetics",
                  "event to 50% completion."),
                p(code("Monoexponential:"), "will return the coefficients of a",
                  "parametric exponential association model fit to the data using",
                  "the least sum of squares approach (with", code("stats::nls()"),
                  "). In particular, the", em("Mean Response Time (MRT)"),
                  "is the sum of the", em("Time Constant (tau)"), "and",
                  em("Time Delay (TD)"), "of the monoexponential function,",
                  "representing the time course of the kinetics event to",
                  "63.2% completion."),
                p(code("Sigmoidal:"), "will return the coefficients of a",
                  "parametric generalised logistic regression model fit to the",
                  "data using the least sum of squares approach (with",
                  code("stats::nls()"), "). In particular, the",
                  em("Mid-response Inflection (xmid)"), "representing the time",
                  "course of the kinetics event to 50% completion."),

                h4("Download Kinetics Data:"),
                p("These coefficients can be downloaded as an", code(".xlsx"),
                  "file to your system."),
            )
        )
    )

)
#
## server ===========================================
server <- function(input, output, session) {
    ## set delay in case tab-out before full string completion
    nirs_channels_debounced <- debounce(reactive(input$nirs_channels), 500)
    sample_channel_debounced <- debounce(reactive(input$sample_channel), 500)
    event_channel_debounced <- debounce(reactive(input$event_channel), 500)


    # Data upload and processing
    raw_data <- reactive({
        req(input$upload_file, nirs_channels_debounced(), sample_channel_debounced())

        upload_file <- input$upload_file$datapath

        data <- tryCatch(
            mNIRS::read_data(
                file_path = upload_file,
                nirs_channels = string_to_named_vector(nirs_channels_debounced()),
                sample_channel = string_to_named_vector(sample_channel_debounced()),
                event_channel = string_to_named_vector(event_channel_debounced()),
                sample_rate = input$sample_rate %||% 0,
                time_to_numeric = TRUE,
                time_from_zero = FALSE,
                keep_all = input$keep_all,
                verbose = FALSE),
            error = \(e) {
                ## remove CLI formatting from error message
                gsub('\033\\[34m\\"|\\"\033\\[39m', '', e$message)
            })

        validate(need(!inherits(data, "character"), data))

        return(data)
    })

    ## Create dynamic UI for filter method
    output$filter_method_ui <- renderUI({
        req(raw_data(), input$filter_method)

        raw_data <- raw_data()
        sample_rate <- attributes(raw_data)$sample_rate

        ## different UI based on selection
        if (input$filter_method == "Butterworth") {
            tagList(
                selectInput(
                    "butter_type",
                    label = "Butterworth Filter Type",
                    choices = c("Low-Pass")), ## , "High-Pass", "Stop-Band", "Pass-Band"
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
                    "fc",
                    label = "Critical Frequency (Hz)",
                    value = 0.1,
                    min = 0,
                    max = sample_rate/2,
                    step = 0.05)
            )
        } else if (input$filter_method == "Moving-Average") {
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

    butter_type <- reactive({
        req(raw_data(), input$filter_method)

        switch(
            input$butter_type,
            "Low-Pass" = "low",
            "High-Pass" = "high",
            "Stop-Band" = "stop",
            "Pass-Band" = "pass")
    })

    ## Create dynamic UI for shift option
    output$shift_data_ui <- renderUI({
        req(raw_data(), input$shift_logical)

        if (input$shift_logical) {
            tagList(
                numericInput(
                    "shift_value",
                    label = "Value to Shift",
                    value = 0),
                selectInput(
                    "shift_which_cols",
                    label = "Channels to Shift",
                    choices = c("Ensemble", "Distinct")),
                selectInput(
                    "shift_position",
                    label = "Position to Shift",
                    choices = c("Minimum", "Maximum", "First")),
                numericInput(
                    "shift_samples",
                    label = "Samples to Shift",
                    value = 1),
            )
        }
    })

    ## Create dynamic UI for rescale option
    output$rescale_data_ui <- renderUI({
        req(raw_data(), input$rescale_logical)

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
                    choices = c("Ensemble", "Distinct")),
            )
        }
    })




    ## update `sample_rate`
    observe({
        updateNumericInput(session,
                           inputId = "sample_rate",
                           value = attributes(raw_data())$sample_rate)
    })



    nirs_data <- reactive({
        req(raw_data())

        raw_data <- raw_data()
        nirs_channels <- attributes(raw_data)$nirs_channels
        sample_channel <- attributes(raw_data)$sample_channel
        event_channel <- attributes(raw_data)$event_channel
        sample_rate <- attributes(raw_data)$sample_rate
        invalid_values <- strsplit(input$invalid_values, split = "\\s*,\\s*")[[1]] |>
            as.numeric()
        manual_events <- strsplit(input$manual_events, split = "\\s*,\\s*")[[1]] |>
            as.numeric()

        validate(need(input$resample_rate,
                      "resample Rate must be provided. Default is `0`"))
        validate(need(input$slice_head,
                      "Remove Head Samples must be provided. Default is `0`"))
        validate(need(input$slice_tail,
                      "Remove Tail Samples must be provided. Default is `0`"))


        nirs_data <- raw_data |>
            mNIRS::resample_data(
                sample_channel = sample_channel,
                sample_rate = sample_rate,
                resample_rate = input$resample_rate,
                verbose = FALSE
            ) |>
            ## remove the head rows
            (\(.df) if (input$slice_head > 0) {
                slice_tail(.df, n = -input$slice_head)
                # filter(.df, .data[[sample_channel]] > input$slice_head)
            } else {.df})() |>
            ## remove the tail rows
            (\(.df) if (input$slice_tail > 0) {
                slice_head(.df, n = -input$slice_tail)
                # filter(.df, .data[[sample_channel]] < input$slice_tail)
            } else {.df})() |>
            mutate(
                if (input$replace_outliers) {
                    across(
                        any_of(nirs_channels),
                        \(.x) mNIRS::replace_outliers(
                            .x,
                            width = 15 * sample_rate,  ## 5-sec window
                            na.rm = TRUE,
                            return = "median"))
                },
                if (!is.null(invalid_values)) {
                    across(
                        any_of(nirs_channels),
                        \(.x) mNIRS::replace_invalid(
                            .x,
                            values = invalid_values,
                            width = 15 * sample_rate, ## 5-sec window
                            return = "median"))
                },
                if (input$replace_missing) {
                    across(
                        any_of(nirs_channels),
                        \(.x) mNIRS::replace_missing(.x, method = "linear"))
                },
                if (input$filter_method == "Smooth-Spline") {
                    # tryCatch(
                    #     ,
                    #     error = \(e) {
                    #         e$message
                    #         ## remove CLI formatting from error message
                    #         # gsub('\033\\[34m\\"|\\"\033\\[39m', '', e$message)
                    #     })
                    across(
                        any_of(nirs_channels),
                        \(.x) mNIRS::filter_data(
                            .x, method = tolower(input$filter_method),
                            verbose = FALSE, na.rm = TRUE))
                } else if (input$filter_method == "Butterworth") {
                    req(input$n, input$fc)

                    across(
                        any_of(nirs_channels),
                        \(.x) mNIRS::filter_data(
                            .x, method = tolower(input$filter_method),
                            type = butter_type(),
                            n = input$n,
                            fc = input$fc,
                            sample_rate = sample_rate,
                            verbose = FALSE,
                            na.rm = TRUE)
                    )
                } else if (input$filter_method == "Moving-Average") {
                    req(input$width)

                    across(
                        any_of(nirs_channels),
                        \(.x) mNIRS::filter_data(
                            .x, method = tolower(input$filter_method),
                            width = input$width,
                            verbose = FALSE,
                            na.rm = TRUE))
                },
            ) |>
            (\(.df) if (input$shift_logical) {
                req(input$shift_value, input$shift_position,
                    input$shift_which_cols, input$shift_samples)

                if (input$shift_which_cols == "Ensemble") {
                    shift_nirs_channels <- nirs_channels
                } else if (input$shift_which_cols == "Distinct") {
                    shift_nirs_channels <- as.list(nirs_channels)
                }

                mNIRS::shift_data(
                    data = .df,
                    nirs_channels = shift_nirs_channels,
                    shift_to = input$shift_value,
                    position = tolower(input$shift_position),
                    mean_samples = input$shift_samples,
                )
            } else {.df})() |>
            (\(.df) if (input$rescale_logical) {
                req(input$rescale_min, input$rescale_max,
                    input$rescale_which_cols)

                if (input$rescale_which_cols == "Ensemble") {
                    rescale_nirs_channels <- nirs_channels
                } else if (input$rescale_which_cols == "Distinct") {
                    rescale_nirs_channels <- as.list(nirs_channels)
                }

                mNIRS::rescale_data(
                    data = .df,
                    nirs_channels = rescale_nirs_channels,
                    rescale_range = c(input$rescale_min, input$rescale_max)
                )
            } else {.df})() |>
            mutate(
                ## reset sample/time values to zero
                if (input$time_from_zero) {
                    across(any_of(sample_channel), \(.x) .x - first(.x))
                },
                ## avoid floating point precision issues
                across(where(is.numeric), \(.x) round(.x, 8)),
                across(any_of(sample_channel), \(.x) round(.x * sample_rate) / sample_rate),
            ) |>
            (\(.df) if (isTruthy(manual_events) & !isTruthy(event_channel)) {
                mutate(
                    .df,
                    event = case_when(
                        .data[[sample_channel]] %in% manual_events ~
                            paste0("event_", as.character(.data[[sample_channel]])),
                        TRUE ~ NA_character_)
                )
            } else if (isTruthy(manual_events) & isTruthy(event_channel)) {
                mutate(
                    .df,
                    across(
                        any_of(event_channel),
                        \(.x) case_when(
                            .data[[sample_channel]] %in% manual_events ~
                                if(is.numeric(.x)) {
                                    .data[[sample_channel]]
                                } else {
                                    paste0("event_", as.character(.data[[sample_channel]]))
                                },
                            TRUE ~ .x)
                    )
                )
            } else .df)() |>
            mutate(
                across(any_of(nirs_channels), \(.x) round(.x, 2)),
                across(any_of(sample_channel), \(.x) round(.x * sample_rate) / sample_rate),
            )

        return(nirs_data)

    })


    output$nirs_table <- DT::renderDT({
        req(nirs_data())

        DT::datatable(
            nirs_data(),
            rownames = FALSE,
            options = list(
                dom = 't',
                pageLength = 20,
                scrollX = TRUE,
                searchHighlight = FALSE
            )
        )
    })



    output$plot <- renderPlot({
        req(nirs_data())

        nirs_data <- nirs_data()
        manual_events <- strsplit(input$manual_events, split = "\\s*,\\s*")[[1]] |>
            as.numeric()

        plot(nirs_data, display_time = input$display_hmmss) +
            theme_mNIRS(base_size = 20, legend.position = "top") +
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




    ## update `event_sample` from `manual_events`
    observe({
        req(nirs_data(), isTruthy(input$manual_events))

        updateTextInput(session,
                        inputId = "event_sample",
                        value = input$manual_events)
    })



    kinetics_method <- reactive({
        req(nirs_data())

        switch(
            input$kinetics_method,
            "Monoexponential" = "monoexponential",
            "Sigmoidal" = "sigmoidal",
            "Half-Recovery Time" = "half_time",
            "Peak Slope" = "peak_slope")
    })


    ## dynamic UI for process_kinetics kinetics_y
    output$kinetics_checkbox_ui <- renderUI({
        req(raw_data())

        raw_data <- raw_data()
        nirs_channels <- attributes(raw_data)$nirs_channels
        # nirs_channels_list <- setNames(c(1:length(nirs_channels)), nirs_channels)

        checkboxGroupInput("kinetics_y",
                           "Select all mNIRS channels to be processed",
                           choices = nirs_channels,
                           selected = nirs_channels)
    })



    ## dynamic UI for process_kinetics peak_slope span
    output$peak_slope_span_ui <- renderUI({
        req(raw_data(), kinetics_method())

        if (input$kinetics_method == "Peak Slope") {
            tagList(
                numericInput(
                    "peak_slope_span",
                    label = "Peak Slope Span (units of x-axis)",
                    value = 10, min = 2),
            )
        }

    })



    kinetics_model_list <- reactive({
        req(nirs_data(),
            isTruthy(input$event_sample),# | isTruthy(input$event_label),
            input$fit_baseline_window, input$fit_kinetics_window,
            kinetics_method())

        nirs_data <- nirs_data()
        nirs_channels <- attributes(nirs_data)$nirs_channels
        sample_channel <- attributes(nirs_data)$sample_channel
        event_channel <- attributes(nirs_data)$event_channel
        sample_rate <- attributes(nirs_data)$sample_rate
        event_sample <- strsplit(input$event_sample, split = "\\s*,\\s*")[[1]] |>
            as.numeric()


        data_list <- prepare_kinetics_data(
            nirs_data,
            event_sample = event_sample,
            # event_label = ifelse(is.null(event_channel), NULL, input$event_label),
            fit_window = c(input$fit_baseline_window, input$fit_kinetics_window),
            group_events = tolower(input$group_events))

        kinetics_model_list <- pmap(
            expand_grid(.df = data_list, .nirs = nirs_channels),
            \(.df, .nirs)
            process_kinetics(x = paste0("fit_", sample_channel),
                             y = .nirs,
                             data = .df,
                             method = kinetics_method(),
                             span = input$peak_slope_span %||% 10)
        )

        return(kinetics_model_list)
    })



    kinetics_display_list <- reactive({
        req(kinetics_model_list())

        nirs_data <- nirs_data()
        kinetics_model_list <- kinetics_model_list()
        sample_channel <- attributes(nirs_data)$sample_channel
        # nirs_channels <- attributes(nirs_data)$nirs_channels
        # nirs_channels <- intersect(input$kinetics_y,
        #                           attributes(nirs_data)$nirs_channels)
        nirs_channels <- input$kinetics_y
        # nirs_fitted <- paste0(nirs_channels, "_fitted")
        fit_sample <- paste0("fit_", sample_channel)

        map(kinetics_model_list, \(.x)
            select(.x$data, matches(c(fit_sample, nirs_channels)))
        ) |>
            (\(.l) split(.l, names(.l)))() |>
            map(\(.l) reduce(.l, full_join, by = fit_sample))
    })



    ## dynamic UI for kinetics_display_list
    output$kinetic_event_tabs <- renderUI({
        req(kinetics_display_list())

        kinetic_event_names <- unique(names(kinetics_display_list()))

        # Create tabPanel list dynamically
        tab_panels <- lapply(kinetic_event_names, \(.n) {
            tabPanel(
                title = .n,

                h4(paste(.n, "Kinetic Event Plot")),
                plotOutput("kinetics_plot", height = "600px"),

                h4(paste(.n, "Kinetic Coefficients Table")),
                DT::DTOutput("kinetics_coefs", width = "auto"),

                h4(paste(.n, "Diagnostics Table")),
                DT::DTOutput("kinetics_diagnostics", width = "auto")
            )
        })

        # Create tabsetPanel with dynamic tabs
        do.call(tabsetPanel, c(tab_panels, id = "main_tabs"))
    })




    output$kinetics_plot <- renderPlot({
        req(kinetics_display_list())

        nirs_data <- nirs_data()
        kinetics_model_list <- kinetics_model_list()
        sample_channel <- attributes(nirs_data)$sample_channel
        sample_rate <- attributes(nirs_data)$sample_rate
        nirs_channels <- input$kinetics_y
        nirs_fitted <- paste0(nirs_channels, "_fitted")
        fit_sample <- paste0("fit_", sample_channel)

        data <- kinetics_display_list()
        coefs <- kinetics_coef_data()

        ## UNDER DEVELOPMENT ONLY DISPLAY ONE KINETICS DATASET
        data <- data[[1]]

        ggplot(data) +
            aes(x = .data[[fit_sample]]) +
            theme_mNIRS(base_size = 20, legend.position = "top") +
            scale_x_continuous(
                name = if (input$display_kinetics_hmmss) {
                    paste(fit_sample, "(mm:ss)")
                } else {waiver()},
                breaks = if (input$display_kinetics_hmmss) {
                    mNIRS:::breaks_timespan(n = 8)
                } else if (rlang::is_installed("scales")) {
                    scales::breaks_pretty(n = 8)
                } else {waiver()},
                labels = if (input$display_kinetics_hmmss) {
                    mNIRS:::format_hmmss
                } else {waiver()},
                expand = expansion(mult = 0.01)) +
            scale_y_continuous(
                name = "mNIRS Signals",
                breaks = if (rlang::is_installed("scales")) {
                    scales::breaks_pretty(n = 6)
                } else {
                    waiver()
                },
                expand = expansion(mult = 0.01)) +
            scale_colour_manual(
                name = NULL,
                aesthetics = c("fill", "colour"),
                breaks = c(nirs_channels, "fitted"),
                values = c(mNIRS_palette(length(nirs_channels)), "black"),
                limits = force) +
            geom_vline(xintercept = 0, linetype = "dotted") +
            map(nirs_channels, \(.x)
                geom_line(aes(y = .data[[.x]], colour = .x), linewidth = 1)
            ) +
            {if (kinetics_method() %in% c("monoexponential")) {
                map(nirs_channels, \(.x) {
                    coef_channel <- coefs[coefs$channel == .x,]
                    nirs_fitted <- paste0(.x, "_fitted")

                    list(
                        geom_line(aes(y = .data[[nirs_fitted]], colour = "fitted"),
                                  linewidth = 0.8, na.rm = TRUE),
                        geom_segment(
                            data = tibble(
                                x = coef_channel$MRT,
                                y = coef_channel$MRT_fitted),
                            aes(x = x, xend = x, y = y, yend = -Inf),
                            arrow = arrow(), linewidth = 1),
                        geom_point(
                            data = tibble(
                                x = coef_channel$MRT,
                                y = coef_channel$MRT_fitted),
                            aes(x = x, y = y, colour = "fitted"),
                            size = 4, shape = 21, stroke = 1)
                    )
                })
            }} +
            {if (kinetics_method() == "sigmoidal") {
                map(nirs_channels, \(.x) {
                    coef_channel <- coefs[coefs$channel == .x,]
                    nirs_fitted <- paste0(.x, "_fitted")

                    list(
                        geom_line(aes(y = .data[[nirs_fitted]], colour = "fitted"),
                                  linewidth = 0.8, na.rm = TRUE),
                        geom_segment(
                            data = tibble(
                                x = coef_channel$xmid,
                                y = coef_channel$xmid_fitted),
                            aes(x = x, xend = x, y = y, yend = -Inf),
                            arrow = arrow(), linewidth = 0.8),
                        geom_point(
                            data = tibble(
                                x = coef_channel$xmid,
                                y = coef_channel$xmid_fitted),
                            aes(x = x, y = y, colour = "fitted"),
                            size = 4, shape = 21, stroke = 1.2)
                    )
                })
            }} +
            {if (kinetics_method() == "half_time") {
                map(nirs_channels, \(.x) {
                    coef_channel <- coefs[coefs$channel == .x,]
                    B_x <- data[[fit_sample]][which(round(data[[.x]], 2) >= coef_channel$B)[1]]
                    half_x <- paste0("half_", fit_sample)


                    list(
                        geom_segment(
                            data = tibble(x = coef_channel[[half_x]],
                                          y = coef_channel$half_value),
                            aes(x = x, xend = x, y = y, yend = -Inf),
                            arrow = arrow(), linewidth = 0.8),
                        geom_point(
                            data = tibble(
                                x = c(0, B_x, coef_channel[[half_x]]),
                                y = c(coef_channel$A, coef_channel$B, coef_channel$half_value)),
                            aes(x = x, y = y, colour = "fitted"),
                            size = 3, shape = 21, stroke = 1),
                        NULL)
                })
            }} +
            {if (kinetics_method() == "peak_slope") {
                map(nirs_channels, \(.x) {
                    coef_channel <- coefs[coefs$channel == .x,]
                    nirs_fitted <- paste0(.x, "_fitted")
                    peak_slope_x <- paste0("peak_slope_", fit_sample)

                    list(
                        geom_line(
                            aes(y = .data[[nirs_fitted]], colour = "fitted"),
                            linewidth = 0.8, na.rm = TRUE),
                        geom_segment(
                            data = tibble(
                                x = coef_channel[[peak_slope_x]],
                                y = coef_channel$peak_slope_fitted),
                            aes(x = x, xend = x, y = y, yend = -Inf),
                            arrow = arrow(), linewidth = 0.8),
                        geom_point(
                            data = tibble(
                                x = coef_channel[[peak_slope_x]],
                                y = coef_channel$peak_slope_fitted),
                            aes(x = x, y = y, colour = "fitted"),
                            size = 3, shape = 21, stroke = 1)
                    )
                })
            }}
    })


    kinetics_coef_data <- reactive({
        req(kinetics_model_list())

        imap(
            kinetics_model_list(),
            \(.x, idx)
            as_tibble(as.list(c(.x$coefs))) |>
                mutate(
                    event = idx,
                    channel = names(.x$data)[2],
                    across(where(is.numeric), \(.x) round(.x, 2))
                ) |>
                relocate(event, channel)
        ) |>
            list_rbind()
    })

    output$kinetics_coefs <- DT::renderDT({
        req(kinetics_coef_data())

        DT::datatable(
            kinetics_coef_data(),
            rownames = FALSE,
            options = list(
                dom = 't',
                pageLength = 20,
                scrollX = TRUE,
                searchHighlight = FALSE
            )
        )
    })

    kinetics_diag_data <- reactive({
        req(kinetics_model_list())

        imap(
            kinetics_model_list(),
            \(.x, idx)
            as_tibble(as.list(c(.x$diagnostics))) |>
                mutate(
                    event = idx,
                    channel = names(.x$data)[2],
                    across(where(is.numeric), \(.x) round(.x, 3))
                ) |>
                relocate(event, channel) |>
                ## TODO 2025-08-15 temp remove R2 until I figure it out
                select(-matches("r2", ignore.case = TRUE))
        ) |>
            list_rbind()
    })

    output$kinetics_diagnostics <- DT::renderDT({
        req(kinetics_diag_data())

        DT::datatable(
            kinetics_diag_data(),
            rownames = FALSE,
            options = list(
                dom = 't',
                pageLength = 20,
                scrollX = TRUE,
                searchHighlight = FALSE
            )
        )
    })



    output$download_kinetics_data <- downloadHandler(

        filename = function() {
            paste0("mNIRS_kinetic_coefs_", Sys.time(), ".xlsx")
        },

        content = function(file) {
            writexl::write_xlsx(list(coefs = kinetics_coef_data(),
                                     diagnostics = kinetics_diag_data()),
                                path = file)
        }
    )

}

shinyApp(ui = ui, server = server)
