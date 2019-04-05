fluidPage(
    theme = discotheme,
  # Settings used in PCA
    fluidRow(
        column(
            3,
            box(
                title = "Outlier Options", status = "primary",
                solidHeader = FALSE,
                width = NULL,
                checkboxInput("PCAscale", "Scale Before PCA", TRUE),
                actionButton("reset_pcToCut", icon("redo"),
                    style = "padding:2px; font-size:60%"),
                selectizeInput("pcToCut", "PCs To Use For Outlier Detection:",
                    choices = paste0("PC", seq_len(10)),
                    selected = paste0("PC", 1:4),
                    multiple = TRUE
                    ),
                sliderInput("maxSD", "Standard Deviation Threshold:",
                    min = 1, max = 5, value = 5, step = 0.1,
                    post = HTML("&sigma;")
                    ),
                br(),
        # PCA summary text output
                htmlOutput("pcaSummary"), br(),
                downloadButton("pcaBeforeCSV", label = "Before CSV"),
                downloadButton("pcaAfterCSV", label = "After CSV"),
                hr(),
                p(class="text-muted","Results for this section were generated
                using DiscoRhythm's ",tags$code("discoPCAoutliers()"),
                  " R function.")
                )
            ),
        column(
            9,
            tabBox(
                width = NULL,
                id = "pcaPlotBox",
                tabPanel(
                    "Distributions",
                    fluidRow(
                        column(10),
                        column(
                            2,
                            downloadButton("dlPcaDistsPlot", "pdf",
                                           class = "pull-right")
                            )
                        ),
                    withSpinner(plotlyOutput("pcaPlotDists", height = "600px")),
                    tags$hr(),
                    p(
                        class = "text-muted",
                        "Two dashed lines indicate the upper and lower
                        outlier thresholds based on the selected SD threshold.
                        Every sample outside the dashed lines will be marked
                        as an outlier and shown as a cross. Only PCs selected
                        as 'PCs To Use For Outlier Detection' in the options
                        will be considered when identifying outliers.
                        Samples marked as outliers are automatically flagged
                        in 'Filtering Summary' tab."
                        )
                    ),
                tabPanel(
                    "One Pair",
                    fluidRow(
                        column(
                            width = 6,
                            withSpinner(plotlyOutput("pcaPlotBefore"))
                            ),
                        column(
                            width = 6,
                            withSpinner(plotlyOutput("pcaPlotAfter"))
                            )
                        ),
                    fluidRow(
                        column(
                            width = 2,
                            selectInput("pcA", "X-axis PC:", seq_len(10), 1)
                            ),
                        column(
                            2,
                            selectInput("pcB", "Y-axis PC:", seq_len(10), 2)
                            ),
                        column(
                            6,
                            radioButtons("PCAcolor", "Colour by:",
                                c("No Color" = "No Color", "ID" = "ID",
                                  "Time" = "Time",
                                    "Replicate ID" = "ReplicateID"),
                                inline = TRUE
                                )
                            ),
                        column(
                            2,
                            downloadButton("dlPcaB4After", "pdf",
                                           class = "pull-right")
                            )
                        ),
                    tags$hr(),
                    p(
                        class = "text-muted",
                        "Comparison of two user specified PCs before
                        and after outlier
                        removal to determine the effect of outlier removal."
                        )
                    ),
                tabPanel(
                    "Scree",
                    fluidRow(
                        column(10),
                        column(
                            2,
                            downloadButton("dlPCAscree", "pdf",
                                           class = "pull-right")
                            )
                        ),
                    withSpinner(plotOutput("pcaPlotStats")),
                    tags$hr(),
                    p(
                        class = "text-muted",
                        "Barplot indicating the proportion of variance
                        explained by each PC before and after outlier removal."
                        )
                    ),
                tabPanel(
                    "All Pairs",
                    fluidRow(
                        conditionalPanel("!input.PCApairType",
                            withSpinner(plotOutput("pcaPairPlot"))),
                        conditionalPanel("input.PCApairType",
                            withSpinner(plotOutput("pcaPairPlotAfter"))),
                        column(
                            3,
                            checkboxInput("PCApairType",
                                label = "After Outlier Removal", value = FALSE)
                            ),
                        column(
                            7,
                            actionButton("reset_pairsPCs", icon("redo"),
                                style = "padding:2px; font-size:60%"),
                            selectizeInput(
                                inputId = "pairsPCs",
                                "PCs To Use For plotting pairs:",
                                choices = paste0("PC", 1:10),
                                selected = paste0("PC", 1:4),
                                multiple = TRUE
                                )
                            ),

                        column(
                            2,
                            conditionalPanel(
                                "!input.PCApairType",
                                downloadButton("dlPCApairsBefore", "pdf",
                                               class = "pull-right")
                                ),
                            conditionalPanel(
                                "input.PCApairType",
                                downloadButton("dlPCApairsAfter", "pdf",
                                               class = "pull-right")
                                )
                            )
                        ),
                    tags$hr(),
                    p(
                        class = "text-muted",
                        "Grid of PC scatterplots based on each
                        possible PC pairing.
                        Useful for  identifying outlier clusters of samples.
                        Inspect further in the One Pair tab."
                        )
                    )
                )
            )
        ),
shinyBS::bsTooltip("pcToCut",
    "Backspace to delete. Click to add.",
    placement = "right"
    )
)
