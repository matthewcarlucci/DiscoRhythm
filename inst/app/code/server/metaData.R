########################################
# Render main data table using outlier info
########################################
output$tableSelectedSamples <- DT::renderDataTable({
    d <- data.frame(Metadata())
    outliers <- c()
    d$`Outlier PCA` <- "yes"
    d$`Outlier PCA`[which(outliersPCAkept())] <- "no"
    d$`Outlier Correlation` <- "yes"
    d$`Outlier Correlation`[which(outliersCorkept())] <- "no"
    intsct <- intersect(
        which(outliersPCAkept()),
        which(outliersCorkept())
        )

    res <- DT::datatable(d,
        class = "cell-border stripe",
        rownames = FALSE, filter = "top",
        options = list(pageLength = 300, autoWidth = TRUE),
        selection = list(mode = "multiple", selected = intsct, target = "row")
        )
    res <- formatStyle(res, "Outlier PCA",
        backgroundColor = styleEqual(
            c("yes"),
            c(colors$outlier)
            )
        )
    res <- formatStyle(res, "Outlier Correlation",
        backgroundColor = styleEqual(
            c("yes"),
            c(colors$outlier)
            )
        )
    res
}, server = FALSE)
########################################
# Extract selected samples from table
# Define button logic for selection
########################################
proxy <- dataTableProxy("tableSelectedSamples")
observeEvent(input$rowSelectionDeselectPca, {
    proxy %>% selectRows(intersect(input$tableSelectedSamples_rows_selected,
        which(outliersPCAkept())))
})
observeEvent(input$rowSelectionDeselectCorr, {
    proxy %>% selectRows(intersect(input$tableSelectedSamples_rows_selected,
        which(outliersCorkept())))
})
observeEvent(input$rowSelectionRefresh, {
    intsct <- intersect(
        which(outliersPCAkept()),
        which(outliersCorkept())
        )
    proxy %>% selectRows(intsct)
})
observeEvent(input$rowSelectionClear, {
    proxy %>% selectRows(NULL)
})
observeEvent(input$rowSelectionCurrent, {
    proxy %>% selectRows(input$tableSelectedSamples_rows_current)
})
observeEvent(input$rowSelectionAll, {
    proxy %>% selectRows(input$tableSelectedSamples_rows_all)
})
selectedSamples <- reactive({
    req(!is.null(Metadata()))
  # If nothing has been selected yet, use everything
    if (is.null(input$tableSelectedSamples_rows_selected) &
        is.null(input$tableSelectedSamples_rows_all)) {
        seq_len(nrow(Metadata()))
    } else {
        intersect(input$tableSelectedSamples_rows_selected,
            input$tableSelectedSamples_rows_all)
    }
})
########################################
# Create object using selected samples
########################################

# Generate the filtered Major Data
# 1 is the id column
DataFinal <- reactive({
    req(!is.null(selectedSamples()))
    data <- Maindata()[, c(1, selectedSamples() + 1)]
    data
})

# Generate the filtered Meta Data
MetaFinal <- reactive({
    req(!is.null(selectedSamples()))
    df <- Metadata()[selectedSamples(), ]
    df
})

FilteredSE <- reactive({
  discoDFtoSE(DataFinal(),MetaFinal())
})

output$DataFinal <- downloadHandler(
    filename <- paste0(status$loadedDatasetName, "_DataFinal.csv"),
    content = function(file) {
        write.csv(DataFinal(), file, row.names = FALSE)
    }
    )

########################################
# Render plots for selected sample
########################################
obsDistributionPlot <- reactive({
    plotObsDistribution(Maindata()[,-1], selectedSamples(),ylab=input$obsUnit)
})
output$plotObsDistribution <- renderPlot({
    obsDistributionPlot()
})
output$dlObsDistribution <- downloadHandler(
    filename = "sample_distributions.pdf",
    content = function(file) {
        pdf(file, width = 6, height = 6)
        plotObsDistribution(Maindata()[,-1], selectedSamples(),
                            ylab=input$obsUnit)
        dev.off()
    }
    )
########################################
# Add summary for sample filtering
########################################
output$filterSummary <- renderTable({
    tmp <- data.table::data.table(
        as.character(c(
            nrow(Metadata()), # total samples
            sum(!outliersCorkept()), # N cor outliers
            sum(!outliersPCAkept()), # N PCA outliers
            length(union(which(!outliersPCAkept()),which(!outliersCorkept()))),
            length(selectedSamples()) # N selected samples
            ))
        )
    rownames(tmp) <- c("Total samples",
        "Correlation outliers", "PCA outliers", "Total outliers",
        "Selected samples")
    xtable::xtable(na.exclude(tmp))
}, rownames = TRUE, striped = FALSE, colnames = FALSE
)

output$filterSummaryTable <- renderDT({
    req(!is.null(MetaFinal()))
    DT::datatable(discoDesignSummary(MetaFinal()),
        options = list(dom='t',
            ordering=FALSE),
        selection='none') %>%
    formatStyle(" ",target="row",
        backgroundColor = styleEqual(c("Total"), colors$discoMain2))
}, rownames = TRUE, striped = TRUE,server=FALSE
)


