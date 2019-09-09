output$sessionArchivingUI <- renderUI({
    app_state <- paste(h3("App State"),"DiscoRhythm ", verCode)

    HTML(paste(
        '<div style="max-width:800px; word-wrap:break-word;">',
        h4("Batch Execution",
           downloadButton("reportOutput", "Generate Batch Results")),
        "Execute a batch version of the analysis with the current parameter 
        settings.",
        "If the input dataset is large, this may be a preffered mode
        of execution over the interactive rhythm detection section which 
        could time out before performing a manual download of results. 
        Results from the batch execution mode will 
        be automatically downloaded/saved upon completion.",
        "A zip file will be produced containing:",
        HTML("<ol><li>An HTML report</li>
              <li>A CSV file for each rhythm detection algorithm's results</li>
              <li>R data of inputs </li></ol>"),
        "Batch execution is the recommended method for archiving results as
        the inputs, outputs, and all software versions used will be archived
        in one location.",
        br(),br(),
        h4("Application Inputs",
           downloadButton("dlInputs", "Download R Data of Inputs")),
        "Simply download the current imported dataset and parameter
        settings such that results can later be (re-)generated with the
        DiscoRhythm R package.",
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

output$reportOutput <- downloadHandler(
    filename = function() {
        paste0("DiscoRhythm", verCode, "_batchResults_",
               tools::file_path_sans_ext(status$loadedDatasetName),
               ".zip")
    },
    content = function(file) {
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
                of DiscoRhythm (if not already).",
                       br(), br(),
                       "discoBatch Execution Summary:",br(),
                       "Number of Samples: ", ncol(Maindata())-1, br(),
                "Number of Rows: ", nrow(Maindata()), br(),
                "Selected Methods: ", paste(usedModels,collapse=", "), br(),
                "Time started: ", format(Sys.time(),usetz = TRUE), br(),
                "Approximate runtime: ",
                paste0(formatC(round(runtime / 60) + 1), " minute(s)"),
                br(),br(),
                "If the application is taking much longer than expected,
                consider refreshing the page to start again."
                ),
            easyClose = TRUE, footer = NULL, size="l"
            ))
        
        tdir <- paste0(tempfile(),"_batch_results")
        report_file <- paste0(tdir,"/discorhythm_report.html")
        DiscoRhythm:::discoShinyHandler({
            tmpODAres <- do.call(discoBatch,c(list(report=report_file),
                                              discoBatchParams()))
        },"Report Generation",shinySession=session)
        
        # exports
        usedODAs <- names(tmpODAres)
        lapply(usedODAs,function(name){
          outfile=paste0(tdir,"/",name,".csv")
          write.csv(tmpODAres[[name]],file=outfile)
        })
        
        # input parameters
        saveRDS(discoBatchParams(),file=paste0(tdir,"/discorhythm_inputs.RDS"))
        
        zip::zipr(file,dir(tdir,full.names = TRUE))
    }
    )

# Collecting all inputs needed to run discoBatch()
discoBatchParams <- reactive({
   list(indata = Maindata(),
                outdata = TRUE,
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
                avg_method = input$avgMethod,
                timeType = input$timeType,
                main_per = input$main_per,
                osc_period = input$periodInput,
                ncores = NCORES)
})

output$dlInputs <- downloadHandler(
    filename = function() {
        paste0("DiscoRhythm", verCode, "_inputs_", status$loadedDatasetName,
               ".RDS")
    },
    content = function(file) {
      saveRDS(discoBatchParams(),file = file)
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
        )[seq_len(nrow(tmp))]
    xtable::xtable(na.exclude(tmp))
}, rownames = TRUE, striped = FALSE)

output$dataName <- renderText({
        status$loadedDatasetName
})

output$sessionInfo <- renderText({
  capture.output(sessionInfo())
})
