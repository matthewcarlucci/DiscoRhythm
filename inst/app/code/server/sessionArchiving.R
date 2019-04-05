output$sessionArchivingUI <- renderUI({
    advanced_usage <- paste(
        h3("Reproducibility and Batch Execution"),
        "The complementary R package to this application may be used to
        batch execute the entire workflow to obtain all results."
        )

    app_state <- paste(h3("App State"),"DiscoRhythm ", verCode)

    HTML(paste(
        '<div style="max-width:500px; word-wrap:break-word;">',
        advanced_usage, br(),br(),
        downloadButton("reportOutput", "Generate HTML Report"),
        "Re-execute analysis from all sections
        (with the current parameter settings) and export an html report.
        This may be useful for archiving results or
        viewing the code necessary to produce all results.",br(),br(),
        downloadButton("downloadData", "Download R Data of Results"),
        "Download the data objects associated with each step of the
        analysis providing easy access to the data for further analysis.",
        br(),br(),
        downloadButton("downloadData", "Download R Data of Inputs"),
        "Download the input dataset and essential parameter
        settings for regenerating the results using the DiscoRhythm R function
        discoBatch().",
        "</div>"
        ))
})

observe({
    if (input$selectInputType == "preload") {
        status$loadedDatasetName <- input$preData
    } else if (input$selectInputType == "csv" & !is.null(input$inCSV$name)) {
        status$loadedDatasetName <- input$inCSV$name
    }
})

# Download internal objects
# Evaluating all reactive objects to export
output$downloadData <- downloadHandler(
    filename = function() {
        paste0("disco", verCode, "_stateexport.Rdata")
    },
    content = function(file) {
        reactives <- Filter(function(x) "reactive" %in% class(get(x)),
            ls(envir = parent.env(environment())))
        rest <- Filter(function(x) !("reactive" %in% class(get(x))),
            ls(envir = parent.env(environment())))
        disco_ro <- lapply(reactives,
            function(x) try(do.call(x, list()), silent = TRUE))
        names(disco_ro) <- reactives
        tmp <- c(as.list(rest), "disco_ro")
        tmp[["file"]] <- file
        do.call(save, tmp)
    }
    )

output$reportOutput <- downloadHandler(
    filename = function() {
        paste0("DiscoRhythm", verCode, "_report_", status$loadedDatasetName,
               ".html")
    },
    content = function(file) {
        setwd(dirname(file))

        # Using the same format and code as the oscillation detection
        # modal
        # Model names
        usedModels <- paste(selectedModels()) %>% replace(., name2id,id2name)[.]
        # Runtime estimation
        nrows <- nrow(Maindata())
        runtime <- sum(nrows / RTconst[selectedModels()])
        showModal(modalDialog(
            title = "Generating Summary Report",
            p("Please be patient, DiscoRhythm is re-computing
                    the workflow using the DiscoRhythm R package command
                    discoBatch() with all current
                    parameter settings in this session to generate
                    an HTML report.",br(),
                "For better performance, consider using a local installation
                of DiscoRhythm.",
                       br(), br(),
                       "discoBatch Execution Summary:",br(),
                       "Number of Samples: ", ncol(Maindata())-1, br(),
                "Number of Rows: ", nrow(Maindata()), br(),
                "Selected Methods: ", paste(usedModels,collapse=", "), br(),
                "Time started: ", format(Sys.time(),usetz = TRUE), br(),
                "Approximate runtime: ",
                paste0(formatC(round(runtime / 60) + 1), " minute(s)"),
                br(),br(),
                "If the app is taking too long,
                consider refreshing the page to start again."
                ),
            easyClose = TRUE, footer = NULL, size="l"
            ))

        DiscoRhythm:::discoShinyHandler({
            do.call(discoBatch,c(list(report=file),discoBatchParams()))
        },"Report Generation",shinySession=session)
    }
    )

# Collecting all inputs needed to run discoBatch()
discoBatchParams <- reactive({
   list(indata = Maindata(),
                outdata = FALSE,
                cor_threshold = input$corSD,
                cor_method = input$corMethod,
                cor_threshType = input$whatToCut,
                pca_threshold = input$maxSD,
                pca_scale = input$PCAscale,
                pca_pcToCut = input$pcToCut,
                aov_method = input$aovMethod,
                aov_pcut = input$anovaCut,
                osc_method = selectedModels(),
                aov_Fcut = input$anovaFstatCut,
                timeType = input$timeType,
                main_per = input$main_per,
                osc_period = input$periodInput,
                ncores = NCORES)
})

output$dlInputs <- downloadHandler(
    filename = function() {
        paste0("DiscoRhythm", verCode, "_inputs_", status$loadedDatasetName,
               ".Rdata")
    },
    content = function(file) {
      save(discoBatchParams(),file = file)
    })

# Summary table rendered for the sidebar
output$summaryTable <- renderTable({
    tmp <- data.frame(
        "Status" = as.character(c(
            summaryVal$nSamplesOri,
            summaryVal$nRowsOri,
            summaryVal$corCutoff,
            summaryVal$pcaCutoff,
            summaryVal$ANOVAstate,
            summaryVal$TRmerge,
            summaryVal$nSamples,
            summaryVal$nRows
            ))
        )
    rownames(tmp) <- c(
        "Samples (Input)",
        "Rows (Input)",
        ifelse(input$whatToCut == "value", "Cor cutoff", "Cor cutoff (SD)"),
        "PCA cutoff (SD)",
        "ANOVA",
        "Rep Merge Method",
        ifelse(input$avgMethod!="None",
            "Samples (Combined)","Samples (Filtered)"),
        "Rows (Filtered)"
        )
    xtable::xtable(na.exclude(tmp))
}, rownames = TRUE, striped = FALSE)

output$dataName <- renderText({
        status$loadedDatasetName
})

output$sessionInfo <- renderText({
  capture.output(sessionInfo())
})
