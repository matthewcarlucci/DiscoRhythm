fluidPage(
    theme = discotheme,
    shinyjs::useShinyjs(),
    # avoiding V8 dep by using functions argument
    shinyjs::extendShinyjs(text = jsCollapseBox,functions = c("collapse")),
    fluidRow(
        div(
            id = "previewRegDataDiv",
            box(
                id = "previewRegData", title = "Submission Settings",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                fluidRow(
                    column(4,
                           checkboxGroupInput("regressionMethod",
                                              "Choose Oscillation Detection Method(s):",
                                              choices = name2id, selected = name2id,
                                              inline = FALSE
                           )
                    ),
                    column(
                        4,
                        numericInput("periodInput",
                                     label = "Enter Period to Detect:",
                                     value = 24),
                        htmlOutput("regressionWarning")
                    ),
                    column(4,
                           p(class="text-muted","Results for this section are
                        generated using DiscoRhythm's ",
                             tags$code("discoODAs()")," R function. Reports
                             are generated using DiscoRhythm's ",
                             tags$code("discoBatch()")," R function.")
                    )
                ),
                hr(),
                h5("Exclusion Criteria Matrix"),
                fluidRow(
                    column(
                        12,
                            column(
                                4, p(class = "text-muted",
                                    "The matrix to the right indicates any
                                    criteria
                                    that may exlcude an oscillation
                                    detection method from use.
                                    Highlighted are excluded algorithms
                                    and 'x' indicates a reason for exclusion."),
                                tags$a(
                                    href = docsURL,
                                    target = "_blank",
                                    "More details"
                                    )
                                ),
                        column(8, withSpinner(
                            tableOutput("exclusionMatrix"))
                        )
                    )
                    ),
                br(),
                box(
                    id = "jobSubmission",
                    width = NULL,
                    collapsible = FALSE,
                    collapsed = FALSE,
                    h5("Job Submission"),
                    fluidRow(
                        column(2,
                               br(),
                               conditionalPanel('!input.byEmail && input.batchReceiveMethod == "Report"',{
                                   downloadButton("reportOutput", "Submit")
                               }),
                               conditionalPanel('!(!input.byEmail && input.batchReceiveMethod == "Report")',{
                                   actionButton("startRegress", " Submit",
                                                icon = icon("play"))
                               })
                        ),
                        column(3,
                               radioButtons("batchReceiveMethod",
                                            label = "Execution Method",
                                            choices = c("Interactive","Report"),inline = TRUE
                               )
                        ),
                    uiOutput("emailUI")
                )),
                width = 12
                )
            )
        ),
    fluidRow(
        div(
            id = "regressionIndividualDiv",
            box(
                id = "regressionIndividualBox",
                width = 12,
                title = "Individual Models",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                fluidRow(
                    column(
                        4,
                        radioButtons("methodToShow",
                            "Algorithm:",
                            "",
                            selected = "",
                            inline = TRUE
                            ),
                        radioButtons("withErrorBar",
                            "Confidence Type:",
                            c("Confidence Band" = "without",
                              "Error Bars" = "with"),
                            selected = "without"
                            ),
                        checkboxGroupInput(
                            "rangeSelection", "Filter By:",
                            c(
                                "Amplitude" = "amplitude",
                                "Acrophase" = "acrophase",
                                "P-value" = "pvalue",
                                "Q-value" = "qvalue",
                                "Mean" = "mean"
                                )
                            ),
                        sliderInput("ampliRange", "Amplitude Range",
                            value = c(0, 5), min = 0, max = 5, step = 0.01),
                        sliderInput("acroRange", "Acrophase Range",
                            value = c(0, 24), min = 0, max = 24, step = 0.01),
                        sliderInput("pvalRange", "P-value Range",
                            value = c(0, 1), min = 0, max = 1, step = 0.01),
                        sliderInput("qvalRange", "Q-value Range",
                            value = c(0, 1), min = 0, max = 1, step = 0.01),
                        sliderInput("meanRange", "Mean Range",
                            value = c(0, 1), min = 0, max = 1, step = 0.01),
                        checkboxInput("useFilterInSummary",
                            "Use Filtered Table in Summary Section", FALSE),
                        a("See the documentation for more information on this
                      section",href=docsURL,target="_blank")
                    ),
                    column(
                        8,
                        fluidRow(
                            column(10),
                            column(2, downloadButton("dlSingleModelTable",
                                "CSV", class = "pull-right"))
                            ),
                        div(withSpinner(DT::dataTableOutput(
                          "tableSelectedSample"))),
                        fluidRow(
                            column(10),
                            column(
                                2,
                                downloadButton("dlWavePlot", "pdf",
                                               class = "pull-right")
                                )
                            ),
                        withSpinner(plotOutput("wavePlot"))
                        )
                    )
                )
            )
        ),
    fluidRow(
        div(
            id = "regressionSummaryDiv",
            box(
                id = "regressionSummaryBox",
                width = 12,
                title = "Summary",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                fluidRow(
                    column(
                        4,
                        selectInput("pqValueSummary",
                            label = "Threshold By:",
                            choices = list("P-Value" = 1, "Q-Value" = 0),
                            selected = 1
                            )
                        ),
                    column(
                        4,
                        splitLayout(
                            cellWidths = c("90%", "10%"),
                            sliderInput("PvaluesNBreaksSummary",
                                        "Number of Bins",
                                min = 10, max = 500, step = 1, value = 50
                                ),
                            actionButton("reset_PvaluesNBreaksSummary",
                                icon("redo"),
                                style = "padding:2px; font-size:60%")
                            )
                        ),
                    column(
                        4,
                        splitLayout(
                            cellWidths = c("90%", "10%"),
                            sliderInput("PQvaluesCutSummary",
                                        "Significance Cutoff",
                                min = 0,
                                max = 1,
                                step = 0.01,
                                value = 0.05
                                ),
                            actionButton("reset_PQvaluesCutSummary",
                                icon("redo"),
                                style = "padding:2px; font-size:60%")
                            )
                        )
                    ),
                tabBox(
                    width = NULL,
                    id = "summaryTab",
                    tabPanel(
                        "P-Value",
                        fluidRow(
                            column(10),
                            column(
                                2,
                                downloadButton("dlAllPQValueHist", "pdf",
                                               class = "pull-right")
                                )
                            ),
                        withSpinner(plotOutput("allPQValueHist")),
                        tags$hr(),
                        p(
                            class = "text-muted",
                            "P-value histogram of each method.
                            The line indicates the set
                            threshold of significance."
                            )
                        ),
                    tabPanel(
                        "Acrophase",
                        fluidRow(
                            column(
                                4,
                                checkboxInput("plotRose", "Rose Plot", TRUE)
                                ),
                            column(6),
                            column(
                                2,
                                downloadButton("dlAllAcroHist", "pdf",
                                               class = "pull-right")
                                )
                            ),
                        withSpinner(plotOutput("allAcroHist")),
                        tags$hr(),
                        p(
                            class = "text-muted",
                            "Acrophases of significant oscillations
                            from each method."
                            )
                        ),
                    tabPanel(
                        "Amplitude",
                        fluidRow(
                            column(10),
                            column(
                                2,
                                downloadButton("dlAllAmpliHist", "pdf",
                                               class = "pull-right")
                                )
                            ),
                        withSpinner(plotOutput("allAmpliHist")),
                        tags$hr(),
                        p(
                            class = "text-muted",
                            "Oscillation amplitudes are estimated by Cosinor and
                            significance obtained from Cosinor F-test results."
                            )
                        ),
                    tabPanel(
                        "Intersection",
                        fluidRow(
                            column(10),
                            column(
                                2,
                                downloadButton("dlAllIntersect", "pdf",
                                               class = "pull-right")
                                )
                            ),
                        withSpinner(plotOutput("allIntersect")),
                        tags$hr(),
                        p(
                            class = "text-muted",
                            "Indicates the overlap of significant oscillations
                            between the detection methods
                            selected to calculate."
                            )
                        )
                    )
                )
            )
        ),
    fluidRow(
        div(
            id = "regressionPairwiseDiv",
            box(
                id = "regressionPairwiseBox",
                width = 12,
                title = "Method Comparison",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                fluidRow(
                    column(
                        4,
                        selectInput("methodsToCompare1",
                            "Choose Two Methods to Compare:",
                            "",
                            selected = ""
                            ),
                        selectInput("methodsToCompare2",
                            NULL,
                            "",
                            selected = ""
                            )
                        ),
                    column(
                        2,
                        selectInput("pqValue",
                            label = "Threshold By:",
                            choices = list("P-Value" = 1, "Q-Value" = 0),
                            selected = 1
                            )
                        ),
                    column(
                        4,
                        splitLayout(
                            cellWidths = c("90%", "10%"),
                            sliderInput("PQvaluesCut", "Significance Cutoff",
                                min = 0,
                                max = 1,
                                step = 0.01,
                                value = 0.05
                                ),
                            actionButton("reset_PQvaluesCut",
                                icon("redo"),
                                style = "padding:2px; font-size:60%")
                            ),
                        splitLayout(
                            cellWidths = c("90%", "10%"),
                            sliderInput("PvaluesNBreaks", "Number of Bins",
                                min = 10, max = 500, step = 1, value = 30
                                ),
                            actionButton("reset_PvaluesNBreaks",
                                icon("redo"),
                                style = "padding:2px; font-size:60%")
                            )
                        ),
                    column(
                        2,
                        radioButtons(
                            "ovMethod", "Overlap Method",
                            c("Both", "Either", "Method 1", "Method 2",
                              "Neither")
                            ),
                        downloadButton("dlOverlaps", "CSV",
                                       class = "pull-right")
                        )
                    ),
                tabBox(
                    width = NULL,
                    id = "pairwiseTab",
                    tabPanel(
                        "Overlap",
                        fluidRow(
                            column(
                                6,
                                br(),
                                withSpinner(tableOutput("metaCompare")),
                                withSpinner(htmlOutput("fisherExactText"))
                                ),
                            column(
                                6,
                                withSpinner(plotOutput("metaVenn"))
                                )
                            )
                        ),
                    tabPanel(
                        "Acrophase",
                        withSpinner(plotOutput("scatterHists")),
                        downloadButton("dlscatterHists", "pdf",
                                       class = "pull-right")
                        )
                    )
                )
            )
        )
    )
