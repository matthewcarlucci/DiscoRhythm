observe({
    req(input$sidebar == "regressionPage" |
        input$sidebar == "overview" |
        input$sidebar == "rowReplicateAnalysis")

    if (length(unique(MetaFinal()$Time)) <= 3) {
        nrows <- nrow(MetaFinal())

        showModal(modalDialog(
            title = "Low sample size detected!",
            HTML(
                paste0(
                    "You currently have very few samples selected (",
                    nrows, "), this might cause errors while computing CS."
                    ),
                "<br/>",
                paste0(
                    "Errors might also be caused by very few unique CTs.
                    Please consider expanding your sample.
                    You can do so by going ",
                    actionButton("reSample",
                        "Here",
                        icon = icon("vcard-o")
                        )
                    )
                ),
            easyClose = FALSE, footer = NULL
            ))
        shinyjs::hide("previewRegDataDiv")
        shinyjs::hide("overviewDiv")
        shinyjs::hide("rowReplicateDiv")
        shinyjs::hide("rowRepMatrixDiv")
    # status$init <- FALSE
    # stop("Not enough data")
    } else {
        status$filtered_inf_design <- DiscoRhythm:::inferFilteredDesign(FilteredSE())
    # status$init <- TRUE
        shinyjs::show("previewRegDataDiv")
        shinyjs::show("overviewDiv")
        shinyjs::show("rowReplicateDiv")
        shinyjs::show("rowRepMatrixDiv")
    }
})

observe({
    req(input$sidebar == "rowReplicateAnalysis")
    validate(need(status$filtered_inf_design, "Tech reps not evaluated."))
    if (!status$filtered_inf_design$with_tech_replicate) {
        showModal(modalDialog(
            title = "No technical replicates detected,
            filtering using ANOVA unavailable.",
            HTML(
                "<br/>",
                paste0(
                    actionButton("noTechRep",
                        "Continue",
                        icon = icon("vcard-o")
                        ),
                    " to period detection and oscillation analysis"
                    )
                ),
            easyClose = FALSE, footer = NULL
            ))
        shinyjs::hide("rowReplicateDiv")
        shinyjs::hide("rowRepMatrixDiv")
    } else {
        shinyjs::show("rowReplicateDiv")
        shinyjs::show("rowRepMatrixDiv")
    }
})

observe(
    if (input$aovMethod != "None") {
        shinyjs::show("anovaCut")
        shinyjs::show("reset_anovaCut")
        shinyjs::show("anovaFstatCut")
        shinyjs::show("reset_anovaFstatCut")
        shinyjs::show("nBreaks")
        shinyjs::show("reset_nBreaks")
        shinyjs::show("SNRcheck")
    } else {
        shinyjs::hide("anovaCut")
        shinyjs::hide("reset_anovaCut")
        shinyjs::hide("anovaFstatCut")
        shinyjs::hide("reset_anovaFstatCut")
        shinyjs::hide("nBreaks")
        shinyjs::hide("reset_nBreaks")
        shinyjs::hide("SNRcheck")
    }
    )

observe(
    if (input$SNRcheck) {
        shinyjs::show("anovaFstatCut")
        shinyjs::show("reset_anovaFstatCut")
    # showTab(inputId = "anovaPlotBox", target = "Signal to Noise")
        updateSliderInput(session, "anovaFstatCut", "F-Statistic Cutoff:",
            value = 1, min = 0, max = 10, step = 0.01)
    } else {
    # hideTab(inputId = "anovaPlotBox", target = "Signal to Noise")
        shinyjs::hide("anovaFstatCut")
        shinyjs::hide("reset_anovaFstatCut")
        updateSliderInput(session, "anovaFstatCut", "F-Statistic Cutoff:",
            value = 0, min = 0, max = 10, step = 0.01)
    }
    )
anovaRes <- reactive({
    validate(need(status$filtered_inf_design, "Tech reps not evaluated."))
    validate(
    # check if there is enough data
        need(length(unique(paste(MetaFinal()$Time,
                                 MetaFinal()$ReplicateID))) >= 2,
            message = "Not enough samples for ANOVA"
            )
        )

    if(input$avgMethod=="Random"){
        DiscoRhythm:::discoShinyHandler({
            message(paste0(
                "Seed (",input$usrSeed,") was set for random number generation.
                Future selection of random samples with this seed will return
                the same results."))
            set.seed(input$usrSeed)
        },"Combining Method", shinySession = session)
    }

    if (status$filtered_inf_design$with_tech_replicate) {
        DiscoRhythm:::discoShinyHandler({
            discoRepAnalysis(FilteredSE(),
                input$aovMethod, input$anovaCut,
                input$anovaFstatCut, input$avgMethod)
        }, "ANOVA", shinySession = session)
    } else {
        DiscoRhythm:::discoShinyHandler({
            discoRepAnalysis(FilteredSE(),
                             aov_method = "None",
                             aov_pcut = 1, aov_Fcut = 0,
                             avg_method = "None")
        }, "ANOVA", shinySession = session)
    }
})

anovaP <- reactive(anovaRes()$allStats$pvalue)

anovaPlot <- reactive({
    plotPvalues(anovaP(), input$nBreaks, input$anovaCut, "ANOVA")
})

output$anovaPlot <- renderPlot({
    validate(
        need(nrow(anovaRes()$se) != 0,
            paste0("No rows pass the significance threshold. ",
                "Consider using a lower cutoff."))
        )
    if (input$aovMethod %in% c("Welch", "Equal Variance") &
        status$filtered_inf_design$with_tech_replicate) {
        anovaPlot()
    } else {
        errorPlot(main = "ANOVA Not In Use")
    }
})

output$dlanovaPlot <- downloadHandler(
    filename = "anovaPlot.pdf",
    content = function(name) {
        if (input$aovMethod %in% c("Welch", "Equal Variance") &
            status$filtered_inf_design$with_tech_replicate) {
            p <- anovaPlot()
        } else {
            p <- errorPlot(main = "ANOVA Not In Use")
        }
    ggsave(name, p, "pdf")}
)

FstatPlot <- reactive({
  ObsVsExpSNR(anovaRes()$allStats, input$anovaFstatCut)
})

output$FstatPlot <- renderPlot({
    if (input$aovMethod %in% c("Welch", "Equal Variance") &
        status$filtered_inf_design$with_tech_replicate & input$SNRcheck) {
        FstatPlot()
    } else {
        errorPlot(main = "Signal to Noise Analysis Not In Use")
    }
})

output$dlFstatPlot <- downloadHandler(
  filename = "FstatPlot.pdf",
  content = function(name) {
    if (input$aovMethod %in% c("Welch", "Equal Variance") &
        status$filtered_inf_design$with_tech_replicate) {
      p <- FstatPlot()
    } else {
      p <- errorPlot(main = "Signal to Noise Analysis Not In Use")
    }
    ggsave(name, p, "pdf")}
)

# Main downstream dataframe, passed to regression models
regressionMeta <- reactive({
    req(FinalSE())
    as.data.frame(colData(FinalSE()))
})
regressionData <- reactive({
    req(FinalSE())
    discoSEtoDF(FinalSE())
})
FinalSE <- reactive({
  req(anovaRes())
  anovaRes()$se
})

output$regressionDataTable <- DT::renderDataTable(
    head(regressionData(), 50),
    rownames = FALSE,
    options = list(scrollX = TRUE, pageLength = 10, autoWidth = TRUE),
    server = FALSE
    )

output$regressionData <- downloadHandler(
    filename <- paste0(status$loadedDatasetName, "_regressionData.csv"),
    content = function(file) {
        write.csv(regressionData(), file, row.names = FALSE)
    }
    )

observeEvent(input$reset_anovaCut, {
    updateSliderInput(session, "anovaCut", "Significance Cutoff:",
        value = 0.05, min = 0, max = 1, step = 0.01)
})
observeEvent(input$reset_anovaFstatCut, {
    updateSliderInput(session, "anovaFstatCut", "F-Statistic Cutoff:",
        value = 1, min = 0, max = 10, step = 0.01)
})
observeEvent(input$reset_nBreaks, {
    updateSliderInput(session, "nBreaks", "Number of Bins:",
        value = 50, min = 10, max = 500)
})

observeEvent(input$noTechRep, {
    updateTabItems(session, "sidebar", "overview")
    removeModal()
})

observe({
    if (input$avgMethod == "Random") {
        shinyjs::show("usrSeed")
    } else {
        shinyjs::hide("usrSeed")
    }
})

observe({
    req(regressionData(), regressionMeta())
    summaryVal$nSamples <- ncol(regressionData()) - 1
    summaryVal$nRows <- nrow(regressionData())
})

observe({
    req(status$filtered_inf_design$with_tech_replicate)
    if (!status$filtered_inf_design$with_tech_replicate) {
        summaryVal$ANOVAstate <- "Unavailable"
        summaryVal$TRmerge <- "None"
    } else if (input$aovMethod == "Equal Variance") {
        summaryVal$ANOVAstate <- "Equal Var"
        summaryVal$TRmerge <- input$avgMethod
    } else {
        summaryVal$ANOVAstate <- input$aovMethod
        summaryVal$TRmerge <- input$avgMethod
    }
})
