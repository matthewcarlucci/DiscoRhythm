observe({
    if (input$selectInputType == "preload") {
        status$loadedDatasetName <- input$preData
    } else if (input$selectInputType == "csv" & !is.null(input$inCSV$name)) {
        status$loadedDatasetName <- input$inCSV$name
    }
})

# Used to generate the local report file with appropriate shiny alerts
prepareReport <- function(file){
   showModal(modalDialog(
            title = "Generating Summary Report",
            p("Please be patient, DiscoRhythm is re-computing
                    the workflow using the DiscoRhythm R package command
                    discoBatch() with the current
                    parameter settings in this session to generate
                    a zip archive containing an HTML report."
            ),
            ODAmodalText(),
            easyClose = TRUE, footer = NULL, size="l"
            ))
        
        tdir <- paste0(tempfile(),"_batch_results")
        report_file <- paste0(tdir,"/discorhythm_report.html")
        DiscoRhythm:::discoShinyHandler({
            tmpODAres <- do.call(discoBatch,c(list(indata=rawData(),
                                                   report=report_file),
                                              discoBatchParams()))
        },"Report Generation",shinySession=session)
        
        # exports
        usedODAs <- names(tmpODAres)
        lapply(usedODAs,function(name){
          # Add dataset name since excel can't open CSVs with same name
          outfile = paste0(tdir,"/",name,"_",
                           tools::file_path_sans_ext(status$loadedDatasetName),
                           ".csv")
          tmpdata <- tmpODAres[[name]]
          
          # Creating the same output as output$dlSingleModelTable
          res <- data.frame(IDs = rownames(tmpdata),
                             acrophase = tmpdata$acrophase,
                             amplitude = tmpdata$amplitude,
                             pvalue = tmpdata$pvalue,
                             qvalue = tmpdata$qvalue,
                             Mean = rowMean()
          )
          write.csv(res,file=outfile,row.names = FALSE) 
          
        })
        
        # input parameters
        saveRDS(discoBatchParams(),file=paste0(tdir,"/discorhythm_inputs.RDS"))
        
        removeModal()
        
        zip::zipr(file,dir(tdir,full.names = TRUE))
}

get_zipname <- function(){
  paste0("DiscoRhythm", verCode, "_",
                   tools::file_path_sans_ext(status$loadedDatasetName),
                   format(Sys.time(), "_%Y%m%d_%H%M%S"),".zip")
}

# Prepare report results for download
output$reportOutput <- downloadHandler(
    filename = get_zipname, #function
    content = prepareReport #function
)

# Prepare report results for email
observeEvent(input$startRegress,{
    req(input$batchReceiveMethod == "Report")
  
    outfile <- paste0(tempdir(),"/",get_zipname())
    
    # In the off chance a file with the same name exists, modify the name
    while(file.exists(outfile)) outfile <- gsub('.zip','_dup.zip',outfile)
    file.create(outfile)
    
    prepareReport(outfile)
    
    if(file.exists(sender_creds_file) & input$byEmail){
      DiscoRhythm:::discoShinyHandler({ 
        
        sendEmail(recipients=input$emailAddress,
                  subject = "DiscoRhythm Report",
                  body = paste0("Your DiscoRhythm results ",
                  "are attached. The session will time out ",
                  "after 30 minutes of inactivity."),
                  attach.files = outfile)
        
        message("An email was sent to ",
                input$emailAddress,
                " with a report attatchment")
        
      },"Report Job Email",shinySession = session)
    }
    
})

# Collecting all inputs needed to run discoBatch()
discoBatchParams <- reactive({
   list(outdata = TRUE,
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

summaryTable <- reactive({
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
    
    return(tmp)
})

# Summary table rendered for the sidebar
output$summaryTable <- renderTable({
    xtable::xtable(na.exclude(summaryTable()))
}, rownames = TRUE, striped = FALSE)

output$dataName <- renderText({
        status$loadedDatasetName
})

output$sessionInfo <- renderText({
  capture.output(sessionInfo())
})
