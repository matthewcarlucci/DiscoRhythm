fluidPage(
    theme = discotheme,
    fluidRow(
        column(
            6,
            div(
                id = "inputLocked",
                style = "display:none;",
                h4(tags$b(paste0("Input dataset locks after
                                 proceeding beyond this page. ",
                    "Please restart DiscoRhythm to use a new dataset"))),
                actionButton("restartAppInSelectData", "Restart App"),
                br()
                ),
            div(
                id = "inputSection",
                radioButtons("selectInputType",
                    label = addQicon("Input method"),
                    choices = list(
                        "Demo CSV" = "preload",
                        "Upload CSV" = "csv"
                        ),
                    selected = "preload"
                    ),
                conditionalPanel(
                    condition = "input.selectInputType == 'preload'",

                    selectizeInput("preData",
                        label = NULL,
                        "Simulated Rhythmic Transcripts",
                        "Simulated Rhythmic Transcripts"
                        ),
                    a("Demo Dataset Descriptions",
                        href = docsURL,
                        target = "_blank"
                        ),
                    br()
                    ),
                conditionalPanel(
                    condition = "input.selectInputType == 'csv'",

          # File input, example download and main submit button
                    fileInput("inCSV", label = NULL,
                              accept = c(".csv", ".CSV")),
                    p(class = "text-muted",
                        "To continue, upload a CSV file (100Mb max) or select
                      a Demo CSV")
                )
                ),
            br(),
            downloadButton("Example", "Download Example CSV")
            ),
        column(
            6,
            box(tags$style(HTML("
                    input:invalid {
                    background-color: #FFCCCC;
                    }")),
                title = "Analysis Options", solidHeader = FALSE, width = NULL,
                radioButtons("timeType",
                    addQicon("Time Type"),
                    choices = c(
                        "Linear Time" = "linear",
                        "Circular Time" = "circular"),
                        selected = "linear"),
                numericInput("main_per",
                    addQicon("Main Period of Interest", "main_per-label"),
                    value = 24, step=0.001),
                textInput(inputId = "tUnit", value = "ZT",
                    label = "Time Unit (Axis Labels)"),
                textInput(inputId = "obsUnit", value = "Value",
                    label = "Observation Unit (Axis Labels)")
                )
            )
        ),
    hr(),
    fluidRow(
        column(
            12,
            box(
                title = "Sampling Summary",
                collapsed = FALSE,
                collapsible = FALSE,
                fluidRow(
                    column(
                        9,
                        div(
                            style = "overflow-x: scroll;",
                            withSpinner(DTOutput("sampleSummary", height=300))
                            )
                        ),
                    column(
                        3,
                        p(class = "text-muted",
                            "This table summarizes the experimental design
                            extracted from column names. Each column represents
                            one sampling time and the 'Total' row indicates
                            the number of samples for the respective time.
                            The number in brackets shows the number of
                            technical replicates for each biological
                            replicate.
                            Created by DiscoRhythm's ",
                            tags$code("discoDesignSummary()"),
                            " R function."
                          )
                        )
                    ),
                width = NULL
                )
            )
        ),
    fluidRow(
        column(
            12,
            box(
                title = "Inspect Input Data Matrix",
                footer = HTML(paste("Table displaying first
                50 rows of input. Data is checked and cleaned by the
                DiscoRhythm's ", tags$code("discoCheckInput()"),"R function.")),
                collapsed = TRUE,
                collapsible = TRUE,
                withSpinner(DT::dataTableOutput("rawSampleKey")),
                width = NULL
                )
            )
        ),
      fluidRow(
        column(
            12,
            box(
                title = "Inspect Parsed Sample Metadata",
                footer = "Table displaying the metadata read in by DiscoRhythm",
                collapsed = TRUE,
                collapsible = TRUE,
                fluidRow(
                    column(
                        3,
                        p("This sample metadata was extracted from the
                        column names of the input matrix. If this
                        does not look accurate for your dataset
                        see the ",a("user's guide",href=docsURL,
                                    target="_blank"),
                        " for more details on the
                        expected input dataset format.", "Column names were
                        processed by DiscoRhythm's ",
                        tags$code("discoParseMeta()")," R function.",
                            class = "text-muted"
                            ),
                        hr(),
                        p(class = "text-muted",
                          "For best results, the column naming format should be 
                          used, however, the sample metadata may also be 
                          uploaded directly in a separate CSV file below."),
                        fileInput("inMetaCSV",
                                  label = "Sample Metadata Override",
                                  accept = c(".csv", ".CSV"))
                        ),
                    column(
                        8,
                        withSpinner(DT::dataTableOutput("rawMetadata"))
                        )
                    ),
                width = NULL
                )
            )
        ),
    shinyBS::bsTooltip(
        title = paste(
            "Circular Time - e.g. Time-of-day",br(),
            "Linear Time - e.g. Time since experiment start",br(),
            tags$a(href = docsURL,
                   target = "_blank", "More info")
            ),
        id = "timeType .fa",
        placement = "right", trigger = c("hover","click"), options = NULL
        ),
    shinyBS::bsTooltip(
        title = paste0(
            "A hypothesized periodicity for the dataset. ",
            "Sets the default period in some sections."),
        id = "main_per-label .fa",
        placement = "right", trigger = "hover", options = NULL
        ),
    shinyBS::bsTooltip(
        title = paste0("Use one of the preloaded public datasets ",
            "or upload a properly formated CSV. ",
            "Download an example dataset and/or see the ",
            tags$a(href = docsURL,
                target = "_blank", "Docs"), " for more details. ",
            "Note: Once you leave this page ",
            "the chosen input will be locked in."),
        id = "selectInputType .fa",
        placement = "right", trigger = c("hover", "click"), options = NULL
        )
    )
