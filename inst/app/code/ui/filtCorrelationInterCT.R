fluidPage(
    theme = discotheme,
    fluidRow(
        column(
            width = 3,
            box(
                title = "Options",
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = NULL,
                radioButtons(
                    inputId = "corMethod",
                    label = "Correlation Coefficient:",
                    choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                    selected = "pearson"
                    ),
                radioButtons(
                    inputId = "whatToCut",
                    label = "Set Outlier Threshold By:",
                    c("Correlation Value" = "value",
                      "Standard Deviation" = "sd"),
                    "sd"
                    ),
        # Slider to set correlation cutoff
                conditionalPanel(
                    "input.whatToCut == 'value'",
                    sliderInput("corValue", "Outlier Threshold By Value:",
                        value = 0, min = 0, max = 5, step = 0.001
                        )
                    ),
        # Slider to set correlation cutoff
                conditionalPanel(
                    "input.whatToCut != 'value'",
                    sliderInput("corSD",
                                "Outlier Threshold By Standard Deviation:",
                        value = 0, min = 0, max = 5, step = 0.1,
                        post = HTML("&sigma;")
                        )
                    ),
        # Correlation summary text output
                htmlOutput("corSummary"), br(),
                downloadButton("corMatrixCSV", "CSV"),
                hr(),
                p(class="text-muted","Results for this section were generated
                using DiscoRhythm's ",tags$code("discoInterCorOutliers()"),
                  " R function.")
                )
            ),
        column(
            width = 9,
            tabBox(
                width = NULL,
                id = "corTabBox",
                tabPanel(
                    "Outlier Detection",
                    fluidRow(
                        column(10),
                        column(
                            2,
                            downloadButton("dlInterCt", "pdf",
                                           class = "pull-right")
                            )
                        ),
          # Correlation plot
                    withSpinner(plotlyOutput("plotInterCTCor",
                                             height = "600px")),
                    tags$hr(),
                    p(
                        class = "text-muted",
                        "Pairwise correlations are evaluated by the
                        chosen method (Pearson or Spearman) and average
                        correlation computed for each sample as an indication
                        of sample similarity. Samples sufficiently dissimilar
                        from all other samples will be marked as outliers.
                        Outlier cutoff is indicated by the horizontal line
                        and can be set by changing the
                        threshold slider in the Options panel. Every dot
                        below the threshold will be marked as a cross on the
                        plot and flagged as an outlier in
                        'Filtering Summary' tab. Data point
                        color is solely a visual aid."
                        )
                    ),
                tabPanel(
                    "Correlation Heatmap",
          # Correlation heatmap and results in a table format
                    fluidRow(
                        column(
                            2,
                            h5("Number of Clusters:")
                            ),
                        column(
                            4,
                            sliderInput("corNclust",
                                label = NULL,
                                value = 1, min = 1, max = 10, step = 1,
                                ticks = FALSE
                                )
                            ),
                        column(4,
                               checkboxInput("outliersCorShowOutliers",
                                             label = "Include Outliers",
                                             value=FALSE)
                        ),
                        column(
                            2,
                            downloadButton("dlCorHeatmapHTML", "html",
                                           class = "pull-right")
                            )
                        ),
                    withSpinner(plotlyOutput("plotCorrelationHeatmap",
                        height = "800px")),
                    tags$hr(),
                    p(
                        class = "text-muted",
                        "Heatmap of the pairwise correlations between samples.
                        Samples are clustered by complete-linkage
                        and correlation values are indicated by color.
                        Outliers are excluded in the heatmap by default and may
                        be included using the check box."
                        )
                    )
                )
            )
        )
    )
