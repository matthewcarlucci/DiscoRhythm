fluidPage(
    theme = discotheme,
    fluidRow(
        column(
            4,
      # Main table manipulation and selection inputs
            box(
                title = "Options", status = "primary", solidHeader = FALSE,
                p(class = "text-muted",
                    "Select samples to use for further analysis.
                    Samples flagged as outliers in previous steps are
                    deselected by default"),
                actionButton("rowSelectionDeselectPca",
                    "Deselect PCA Outliers",
                    icon = icon("remove-circle", lib = "glyphicon"),
                    width = "220px"),
                actionButton("rowSelectionDeselectCorr",
                    "Deselect Correlation Outliers",
                    icon = icon("remove-circle", lib = "glyphicon"),
                    width = "220px"),
                actionButton("rowSelectionRefresh",
                    "Refresh Selection",
                    icon = icon("refresh", lib = "glyphicon"),
                    width = "220px"),
                actionButton("rowSelectionClear",
                    "Clear Selection",
                    icon = icon("remove-circle", lib = "glyphicon"),
                    width = "220px"),
                actionButton("rowSelectionAll",
                    "Select ALL Samples",
                    icon = icon("ok-circle", lib = "glyphicon"),
                    width = "220px"),
                actionButton("rowSelectionCurrent",
                    "Select Current Page",
                    icon = icon("table"), width = "220px"),
                hr(),
                p(class = "text-muted",
                    "Technical replicates are ignored when
                    considering \"Biological samples\". Outliers refer to
                    \"All samples\"."),
                tableOutput("filterSummary"),
                hr(),
                p("Download data matrix for selected samples only ",
                    class = "text-muted"),
                downloadButton("DataFinal", "CSV"),
                width = NULL
                )
            ),
    # The main table itself
        column(
            8,
            tabBox(
                width = NULL,
                id = "selectionTable",
                tabPanel("Selection Table",
                    width = NULL,
                    DT::dataTableOutput("tableSelectedSamples"),
                    tags$hr(),
                    p(
                        class = "text-muted",
                        "Filtering Data Summary"
                        )
                    ),
                tabPanel(
                    "Raw Distributions",
                    fluidRow(
                        column(10),
                        column(2, downloadButton("dlObsDistribution", "pdf",
                            class = "pull-right"))
                        ),
                    withSpinner(plotOutput("plotObsDistribution")),
                    tags$hr(),
                    p(
                        class = "text-muted",
                        "Distributions for each sample colored by whether
                        they are selected in the Selection Table. Boxes indicate
                        interquartile range while whiskers indicate
                        max/min values."
                        )
                    ),
                tabPanel(
                    "Input and Final data",
                    fluidRow(
                        column(6,
                            p("Original Input sample summary",
                              class="text-muted")
                            ),
                        column(
                            12,
                            div(
                                style = "overflow-x: scroll;",
                                withSpinner(DTOutput("compareSummary",
                                                    height=300))
                                )
                            ), column(12,p(br())),
                        column(6,
                            p("Final Filtered sample summary",
                              class="text-muted")
                            ),
                        column(
                            12,
                            div(
                                style = "overflow-x: scroll;",
                                withSpinner(DTOutput("filterSummaryTable",
                                                    height=300))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
