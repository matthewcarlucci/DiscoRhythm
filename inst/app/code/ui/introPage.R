fluidPage(
    theme = discotheme,
    fluidRow(
        div(
          style="max-width:800px; word-wrap:break-word;",
            id = "introPage",
          h3("Getting Started"),
                                p(
                                    "DiscoRhythm (Discovering Rhythms) provides
                                    tools to explore cyclical temporal data.",
                                    br(),"For a detailed guide, please refer
                                    to the ",
                                    a(href = docsURL, "documentation.",
                                    target = "_blank"), br(), br(),
                                    "The sections on the left are sequentially
                                    executed to first, perform outlier
                                    detection, then, detect dominant
                                    periodicities
                                    in the data, and finally, detect rhythmicity
                                    for each biological feature. Interactive
                                    visualizations of oscillation parameters
                                    (p-value, amplitude, acrophase) will be
                                    available and a final report may be
                                    downloaded to archive the session."),
                                p("An example dataset is available and loaded by
                                default to test the features of the application.
                                  "),
                                p(
                                    "Four methods of oscillation detection are
                                    currently available:"),
                                HTML("<ol><li>",
                                    paste(
                                    a(href = paste0("https://www.ncbi.nlm.nih",
                                    ".gov/pmc/articles/PMC3119870/"),
                                    "JTK_CYCLE", target = "_blank")),"</li>
                                    <li>",
                                    paste(
                                    a(href = paste0("https://www.ncbi.nlm.nih",
                                    ".gov/pubmed/16303799/"),
                                    "Lomb-Scargle", target = "_blank")),"</li>
                                    <li>",
                                    paste(
                                    a(href = paste0("https://www.ncbi.nlm.nih",
                                    ".gov/pmc/articles/PMC3991883/"),
                                    "Cosinor",
                                    target = "_blank")),"</li>
                                    <li>",
                                    paste(
                                    a(href = paste0("https://www.ncbi.nlm.nih",
                                    ".gov/pubmed/20529902/"),
                                    "ARSER", target = "_blank")),"</li>
                                    </ol>"),
                                p(
                                    "Appropriate method(s) will be available
                                    dependent on the experimental
                                    design detected by DiscoRhythm."),
                                p(
                                  "Throughout the app, look for these symbols:",
                                  br(),
                                  "Hover over ", icon("question-circle"),
                                  "'s for instructions.",
                                  br(),
                                  "Click the ", icon("play",
                                                    class = "disco-button"),
                                  " to go to the next section.", br(),
                                  "Click the", icon("redo"),
                                  "'s to reset to default values."
                                ),br(),br(),

        # )
            # ),
            box(
                id = "dataIntro",
                title = "Brief Workflow Procedure Details",
                status = "primary",
                solidHeader = FALSE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                fluidRow(column(12,
        h3("Select Data: Data Upload and Specifications"),
                                p(
                                    "The purpose of this section is to choose
                                    and configure your input dataset. You can
                                    upload
                                    a dataset (in .csv format) or use the
                                    provided example. The example is a small
                                    dataset capable
                                    of quickly generating all results so
                                    it is recommended to use this when
                                    first testing features of the application.",
                                    br(),br(),
                                    "When uploading your own dataset, be sure
                                    to follow the naming convention described
                                    in the ", a(href = docsURL,
                                                "documentation.",
                                                target = "_blank"), "You should
                                    also check the 'Sampling Summary' and
                                    'Data Matrix' tables at the bottom of the
                                    'Select Data' page to make sure your data
                                    is read in correctly."
                ),
        h3("Quality Control: Outliers and Signal-to-noise"),
                                p(
                                    "The purpose of this section is to perform
                                    quality control procedures to filter out
                                    poor quality columns or rows of the data.
                                    The procedures detect systematic
                                    biases in the data and allow identification
                                    of outlier samples.",br(),
                                    "By default no outliers
                                    will be flagged, however, by setting
                                    appropriate thresholds (~3 standard
                                    deviations) outliers may be flagged for
                                    downstream removal.",
                                    br(),
                                    HTML("<ol>
                                    <li>Inter-Sample Correlations:</li>
                                    A measure of similarity between all samples
                                    based on pairwise correlations. If a sample
                                    shows relatively low correlation with the
                                    remaining data it will be flagged.
                                    <li>Principal Components Analysis:</li>
                                    A dimensionality reduction method which
                                    identifies systemic patterns in the data
                                    and can identify outlier samples. This step
                                    is also useful when looking for batch
                                    effects or other major confounders.
                                    <li>Filtering Summary:</li>
                                    Summary of the quality control procedures.
                                    All flagged samples will
                                    be removed by default, however, you can
                                    change this selection manually if you
                                    believe this to be innappropriate.
                                    <li>Row Selection (optional):</li>
                                    Signal-to-noise analysis is used to
                                    identify features that show stronger
                                    biological signal compared to technical
                                    noise. These features are disabled by
                                    default to avoid unintended filtering of
                                    the data. This section
                                    also contains the technical replicate
                                    merging method.
                                    </ol>")),
                                    p(
                                    "The status table in the sidebar contains
                                    relevant summary information regarding
                                    all quality control."
                                ),
        h3("Main Analysis: Period Detection and Feature-wise
            Rhythmicity Characterization"),
                                p(
                                    "In this section you can find the dominant
                                    period in your data and run the main
                                    oscillation detection algorithms.",
                                    br(),
                                    HTML("<ol>
                                        <li>Period Detection:</li>
                                        This section summarizes the strength of
                                        multiple periods across all remaining
                                        features and can
                                        be used to identify relevant periods in
                                        the dataset.
                                        Below this, a fixed period can be fit to
                                        the first 10 principal components of
                                        the dataset which may reveal strong
                                        dataset-wide rhythmicity at the given
                                        period.
                                        <li>Oscillation Detection:</li>
                                        This is the main analysis section,
                                        where multiple oscillation detection
                                        algorithms (ODAs) are used to detect
                                        rhythmicity in the data. All ODAs
                                        applicable to the data
                                        design are selected by default. You
                                        must press Detect Oscillations to
                                        compute using the input parameters
                                        and to update results when changing
                                        input parameters.
                                        </ol>")
                                ),
        h3("Session Archiving"),
        p("All results and visualizations can be re-compiled into a
          report for archiving of the DiscoRhythm session. Download
          of the R data associated with the session is also available.")
                )))
        )
    )
)
