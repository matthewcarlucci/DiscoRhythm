fluidPage(
    theme = discotheme,
    tags$div(
        id = "ANOVA",
        fluidRow(
            div(
                id = "rowReplicateDiv",
                column(
                    3,
                    box(
                        title = "Options", status = "primary",
                        solidHeader = FALSE,
                        width = NULL,
                        radioButtons("aovMethod",
                            addQicon("ANOVA Method:", "aovMethod-label"),
                            choices = c("Equal Variance", "Welch", "None"),
                            selected="None"),
                        splitLayout(
                            cellWidths = c("90%", "10%"),
                            sliderInput("anovaCut", "Significance Cutoff:",
                                value = 0.05, min = 0, max = 1, step = 0.01),
                            actionButton("reset_anovaCut", icon("redo"),
                                style = "padding:2px; font-size:60%")
                            ),
                        checkboxInput("SNRcheck", "Signal to Noise analysis",
                                      TRUE),
                        splitLayout(
                            cellWidths = c("90%", "10%"),
                            sliderInput("anovaFstatCut", "F-Statistic Cutoff:",
                                value = 1, min = 0, max = 10, step = 0.01),
                            actionButton("reset_anovaFstatCut", icon("redo"),
                                style = "padding:2px; font-size:60%")
                            ),
                        radioButtons("avgMethod",
                            addQicon("Combining method:", "avgMethod-label"),
                            choices = c("Median", "Mean", "Random", "None")),
                        numericInput("usrSeed", "Random seed:", 123),
                        hr(),
                        p(class="text-muted","Results for this section were
                        generated using DiscoRhythm's ",
                          tags$code("discoRepAnalysis()")," R function.")
                        )
                    ),
                column(
                    9,
                    tabBox(
                        width = NULL,
                        id = "anovaPlotBox",
                        tabPanel(
                            "P-Value",
                            downloadButton("dlanovaPlot", "pdf"),
                            withSpinner(plotOutput("anovaPlot")),
                            splitLayout(
                                cellWidths = c("30%", "70%"),
                                sliderInput("nBreaks", "Number of Bins:",
                                    value = 50, min = 10, max = 500,step = 5),
                                actionButton("reset_nBreaks", icon("redo"),
                                    style = "padding:2px; font-size:60%")
                                ),
                            tags$p("Proportion of rows which exhibit significant
                                biological signal compared to technical
                                replicates using the selected ANOVA method.
                                Prior to oscillation
                                detection,
                                technical replicates will be averaged.
                                ", class = "text-muted")
                            ),
                        tabPanel(
                            "Signal to Noise",
                            downloadButton("dlFstatPlot", "pdf"),
                            withSpinner(plotOutput("FstatPlot")),
                            tags$p("Distribution of the signal to noise ratio
                                (adjusted for degrees of freedom) in
                                a theoretical F-distribution compared to the
                                observed distribution across the dataset.
                                The magnitude of the F-statistic can be
                                filtered for further analysis to limit analysis
                                to rows exhibiting high biological signal
                                relative to the noise present in replicates.",
                                class = "text-muted")
                            )
                        )
                    )
                )
            )
        ),
    fluidRow(
        div(
            id = "rowRepMatrixDiv",
            box(
                title = "Inspect Final Averaged Data Matrix",
                collapsed = TRUE,
                collapsible = TRUE,
                fluidRow(
                    column(
                        3,
                        p("This table displays the first 50 rows of the final
                            data matrix to be used in 'Main Analysis'.
                            The full CSV may be downloaded below for independent
                            analysis or as input to a new DiscoRhythm session.",
                            br(), downloadButton("regressionData", "CSV"),
                            class = "text-muted"
                            )
                        ),
                    column(
                        8,
                        withSpinner(DT::dataTableOutput("regressionDataTable"))
                        )
                    ),
                width = NULL
                )
            )
        ),

    shinyBS::bsTooltip(
        title = paste0("Selecting \"None\" results in all rows used for ",
        "further analysis"),
        id = "aovMethod-label .fa",
        placement = "right", trigger = "hover", options = NULL
        ),
    shinyBS::bsTooltip(
        title = paste0("Which method to use when combining ",
                    "technical replicates. ",
            "Selecting \"None\" treats technical replicates ",
            "as biological replicates. ",
            "Selecting \"Random\" keeps one randomly selected ",
            "technical replicate. User can set random seed ",
            "for reproducibility"),
        id = "avgMethod-label .fa",
        placement = "right", trigger = "hover", options = NULL
        )
    )
