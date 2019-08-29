fluidPage(
    theme = discotheme,
    shinyjs::useShinyjs(),
    fluidRow(
        div(
            id = "overviewDiv",
            box(
                id = "periodDetect",
                title = "Period Detection",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                fluidRow(
                    column(
                        3,
                        uiOutput("PeriodDetectionRange"),
                        p(class="text-muted","See the ",
                          a("documentation",
                            href=paste0(docsURL,"#45_period_detection"),
                            target="_blank")," for more details on how to
                          use period detection to guide your analysis."),
                        hr(),
                        p(class="text-muted","Results for this section were
                        generated using DiscoRhythm's ",
                            tags$code("discoRepAnalysis()")," R function.")
                    ),
                    column(
                        9,
                        fluidRow(
                            column(10),
                            column(
                                2,
                                downloadButton("dlOVperiod", "pdf",
                                               class = "pull-right")
                                )
                            ),
                        withSpinner(plotOutput("OVperiod")),
                        tags$hr(),
                        p(
                            class = "text-muted",
                            "Each period is tested as an independent Cosinor
                            model fit across all rows.
                            A dominant period should present
                            itself by a higher median r-squared of Cosinor fit
                            and row specific periodicities may be seen as 
                            outlier data points."
                            )
                        )
                    )
                ),
            box(
                id = "periodPCA",
                title = "PC Cosinor Fits",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                fluidRow(
                    column(
                        3,
                        uiOutput("PCfitSelectPeriod"),
                        selectizeInput(
                            inputId = "PCfitSelectPCs",
                            "PCs To Plot",
                            choices = paste0("PC", 1:10),
                            selected = paste0("PC", 1:4),
                            multiple = TRUE
                        ),
                        p(class="text-muted",
                          "PCA is executed on the data matrix to obtain
                          the scores for the PC loadings that explain the most
                          biological variation in the dataset. A cosinor model
                          is then fit at the specified period to infer whether 
                          this patterm is rhythmic. See the ",
                          a("documentation",
                            href=paste0(docsURL,"#45_period_detection"),
                            target="_blank")," for more details on how to
                          use this section to guide your analysis."
                          )
                        ),
                    column(
                        9,
                        tabBox(
                            width = NULL,
                            id = "PCfitsTab",
                            tabPanel(
                                "PC Cosinor Fits",
                                fluidRow(
                                    column(10),
                                    column(
                                        2,
                                        downloadButton("dlOVpcaScatter", "pdf",
                                            class = "pull-right")
                                        )
                                    ),
                                withSpinner(plotOutput("OVpcaScatter")),
                                p(
                                    class = "text-muted",
                                    "Cosinor fit lines to up to the
                                    first 10 PCs in the dataset."
                                    )
                                ),
                            tabPanel(
                                "Cosinor Table",
                                fluidRow(
                                    column(10),
                                    column(
                                        2,
                                        downloadButton("dlOVpcaFits", "CSV",
                                            class = "pull-right")
                                        )
                                    ),
                                withSpinner(tableOutput("OVpcaFits"))
                                ),
                            tabPanel(
                                "PC Importance",
                                withSpinner(tableOutput("OVperiodTable")),
                                downloadButton("dlOVperiodTable", "CSV")
                                )
                            )
                        )
                    )
                )
            )
        )
    )
