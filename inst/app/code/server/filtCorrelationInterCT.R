########################################
# Render Average Correlation Plot
########################################
interCTPlot <- reactive({
    plotAvgCor(Metadata(), meanCor(), corrThreshold(), input$tUnit)
})
output$plotInterCTCor <- renderPlotly({
    gg <- plotly::ggplotly(interCTPlot())
  # Remove additional info from point to display
    for (i in seq_len(length(gg$x$data))) {
        gg$x$data[[i]]$text <- gsub("<.*", "", gsub(".*ID: ", "",
            gg$x$data[[i]]$text))
    }
    gg %>% layout(yaxis = list(automargin=TRUE))
})
output$dlInterCt <- downloadHandler(
    filename <- "CorrelationIntraCT.pdf",
    content = function(file) {
        ggsave(file, plot = interCTPlot(), device = "pdf")
    }
    )

########################################
# Get correlations or return error if incorrect sample selection
########################################

# Return Correlation Matrix
getCorrelation <- reactive({
    validate(
        need(!is.null(Maindata()) & nrow(Maindata()) >= 2,
            message = "Please select at least two samples
            in 'Filtering summary' tab"
            )
        )
    cor(Maindata()[, -1], method = tolower(input$corMethod))
})
output$corMatrixCSV <- downloadHandler(
    filename <- "correlation.csv",
    content = function(file) {
        write.csv(getCorrelation(), file)
    }
    )

####################
# Heatmap
####################
CorrelationHeatmapPlot <- reactive({
    if(input$outliersCorShowOutliers){
        mat <- getCorrelation()
        p <- plotHeatMCor(mat = mat, k = input$corNclust,
                          row_side_colors=data.frame(
                            "flagged_outlier"=!outliersCorkept()
                          ))
    }else{
        mat <- getCorrelation()[outliersCorkept(), outliersCorkept()]
        p <- plotHeatMCor(mat = mat, k = input$corNclust)
    }
    return(p)
})
output$plotCorrelationHeatmap <- renderPlotly({
    CorrelationHeatmapPlot() # reactive object
})
output$dlCorHeatmapHTML <- downloadHandler(
    filename = "Correlation_Heatmap.html",
    content = function(file) {
        htmlwidgets::saveWidget(CorrelationHeatmapPlot(), file)
    }
    )

########################################
# Slider: select cutoff based on value
########################################

meanCor <- reactive({
    corMat <- getCorrelation()
    # Remove Diagonal of the correlation matrix which used to be all 1
    diag(corMat) <- NA
    rowMeans(corMat, na.rm = TRUE)
})

observe({
    minCor <- min(meanCor())
    maxCor <- max(meanCor())
    meanCor <- mean(meanCor())
    sdCor <- sd(meanCor())
  # Update filter slider
    updateSliderInput(session, "corValue",
        min = round((minCor), 2) - 0.01,
        max = round(maxCor, 2),
        value = round(minCor, 2) - 0.01
        )
  # 5 sd or value of most extremem outlier
    updateSliderInput(session, "corSD",
        min = 0,
        max = max(5, ceiling((meanCor - minCor) / sdCor)),
        value = ceiling((meanCor - minCor) / sdCor)
        )
})

corrThreshold <- reactive({
    if (input$whatToCut == "value") {
        threshold <- input$corValue
    } else {
        threshold <-  mean(meanCor()) - input$corSD * sd(meanCor())
    }
    threshold
})
########################################
# Send filtered sample information to Filtering summary
########################################

outliersCorkept <- reactive({
    !(meanCor() <= corrThreshold())
})

output$corSummary <- renderText({
    if (input$whatToCut == "value") {
        txt <- paste(as.numeric(corrThreshold()))
    } else {
        txt <- paste0("Mean ", ifelse(input$corSD >= 0, "+ ", "- "),
            abs(input$corSD), "&sigma; = ", formatC(corrThreshold()))
    }
    paste0(
        "Threshold:  ",
        "<b>", txt, "</b>", "<br/><br>",
        "Number of outliers found: ",
        "<b>", sum(!outliersCorkept()), "</b>"
        )
})

observe({
  # req(Maindata(), Metadata())
  # req(getCorrelation())
    req(!is.na(summaryVal$nSamplesOri))
    summaryVal$corCutoff <- formatC(ifelse(input$whatToCut == "value",
        input$corValue, input$corSD))
  # corrThreshold())
})
