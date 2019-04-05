########################################
# Update PC to remove to visualize
########################################
# Max number of PCs
npcs <- reactive({
  # req(!is.null(Maindata()))
    nr <- nrow(Maindata())
    nc <- ncol(Maindata())

  # The max #PCs can be calculated as
    min(nc - 1, nr)
})

observe({
    if (npcs() < 10) {
        updateSelectInput(session, "pcA", choices = 1:npcs(), selected = 1)
        updateSelectInput(session, "pcB", choices = 1:npcs(), selected = 2)
        updateSelectizeInput(session, "pcToCut",
            choices = paste0("PC", 1:npcs()),
            selected = paste0("PC", 1:min(4, npcs()))
            )
        updateSelectizeInput(session,
            inputId = "pairsPCs",
            choices = paste0("PC", 1:npcs()),
            selected = paste0("PC", 1:npcs())
            )
    }
})

########################################
# Perform PCA
########################################
pcaBefore <- reactive({
    outdata <- discoPCA(Maindata()[, -1], input$PCAscale)

  # Test if a max sd=5 is large enough for the slider
  # such that there are no outliers by default
    x <- outdata$x
    sdVec <- matrixStats::rowSds(t(x[, input$pcToCut, drop = FALSE]))
    meanVec <- colMeans(x[, input$pcToCut, drop = FALSE])
    meanMat <- matrix(rep(meanVec, nrow(x)), nrow = nrow(x), byrow = TRUE)
    sdMat <- matrix(rep(sdVec, nrow(x)), nrow = nrow(x), byrow = TRUE)
    msd <- max(abs(x[, input$pcToCut, drop = FALSE] - meanMat) / sdMat)
    if (msd > 5) {
        updateSliderInput(session, "maxSD", "SD Threshold:",
            min = 1, max = ceiling(msd), value = ceiling(msd), step = 0.1
            )
    }

    outdata
})

outliersPCAkept <- reactive({
    DiscoRhythm:::discoShinyHandler(
        DiscoRhythm:::discoPCAgetOutliers(pcaBefore()$x,
                                          as.numeric(input$maxSD),
                                          input$pcToCut),
        "PCA",
        shinySession = session
        )
})

npcs <- reactive({
  # req(!is.null(Maindata()))
    nr <- nrow(Maindata())
    nc <- ncol(Maindata())

  # The max #PCs can be calculated as
    min(nc - 1, nr)
})

pcaAfter <- reactive({
    DiscoRhythm:::discoShinyHandler({
        if (sum(outliersPCAkept()) < 2) {
            stop(
                "Insufficient number of samples to perform PCA
                after removing outliers"
                )
        }

        afterData <- Maindata()[, -1][, outliersPCAkept()]
    # Flag rows with NAs
        rowToKeep <- apply(afterData, 1, function(x) !any(is.na(as.numeric(x))))
    # Flag rows with constant values
        rowToKeep[rowToKeep] <- !apply(
            afterData[rowToKeep, ], 1,
            function(x) max(as.numeric(x)) == min(as.numeric(x))
            )
        if (sum(!rowToKeep) != 0) {
            afterData <- afterData[rowToKeep, ]
            warning(
                paste0("Deleted ",
                    sum(!rowToKeep),
                    " rows since they were constant
                    across samples or contained ",
                    "missing values"))
        }
        discoPCA(afterData, input$PCAscale)
    }, "PCA", shinySession = session)
})

########################################
# PCA summary
########################################
output$pcaSummary <- renderText({
    paste0(
        "Number of outliers found: ",
        "<b>", sum(!outliersPCAkept()), "</b>"
        )
})

########################################
# Render plots
########################################

########################################
# Distributions
########################################
PcaDistsPlot <- reactive({
    plotPCAdists(pcaBefore(),
        as.numeric(input$maxSD),
        pcToUse = input$pcToCut,
        npcs = 10
        )
})
output$pcaPlotDists <- renderPlotly({
    ggplotly(PcaDistsPlot())
})
output$dlPcaDistsPlot <- downloadHandler(
    filename <- "pca_distributions.pdf",
    content = function(file) {
        ggsave(file, plot = PcaDistsPlot(), device = "pdf")
    }
    )

########################################
# Scree
########################################
output$pcaBeforeCSV <- downloadHandler(
    filename <- "pca_before_outlier_removal.csv",
    content = function(file) {
        write.csv(pcaBefore()$table, file)
    }
    )
output$pcaAfterCSV <- downloadHandler(
    filename <- "pca_after_outlier_removal.csv",
    content = function(file) {
        write.csv(pcaAfter()$table, file)
    }
    )
pcaStatsPlot <- reactive({
    if (sum(outliersPCAkept()) < 2) {
        p <- errorPlot(main = "0 or 1 Samples\nRemaining", textcol = "black")
    } else {
        p <- plotPCAstats(pcaBefore()$table, pcaAfter()$table,
            pcToUse=input$pcToCut, npcs=10)
    }
    p
})
# Variance explained
output$pcaPlotStats <- renderPlot({
    pcaStatsPlot()
})
output$dlPCAscree <- downloadHandler(
    filename <- "pca_scree.pdf",
    content = function(file) {
        p <- pcaStatsPlot()
        ggsave(file, plot = p, device = "pdf")
    }
    )

########################################
# One Pair
########################################
pcaPlotBefore <- reactive({
    plotPCAWithShape(
        pcaBefore()$x,
        Metadata(),
        input$PCAcolor,
        input$pcA,
        input$pcB,
        outliersPCAkept(),
        "Before Outlier Removal"
        )
})

pcaPlotAfter <- reactive({
    if (sum(!outliersPCAkept()) == 0) {
        p <- errorPlot(main = "No Outliers", textcol = "black")
    } else if (sum(outliersPCAkept()) < 2) {
        p <- errorPlot(main = "0 or 1 Samples\nRemaining", textcol = "black")
    } else {
        p <- plotPCAWithShape(
            pcaAfter()$x,
            Metadata(),
            input$PCAcolor,
            input$pcA,
            input$pcB,
            NULL,
            "After Outlier Removal"
            )
    }
    p
})

# Original PCA
output$pcaPlotBefore <- renderPlotly({
    ggplotly(pcaPlotBefore())
})

# PCA with outliers removed
output$pcaPlotAfter <- renderPlotly({
    ggplotly(pcaPlotAfter())
})


output$dlPcaB4After <- downloadHandler(
    filename <- "pca_Before_and_After.pdf",
    content = function(file) {
        p <- grid.arrange(pcaPlotBefore(), pcaPlotAfter(), ncol = 2)
        ggsave(file, plot = p, device = "pdf")
    }
    )

########################################
# All Pairs
########################################


# Before Outlier Removal
output$pcaPairPlot <- renderPlot({
    plotPCAPairs(pcaBefore()$x,input$pairsPCs)
})
output$dlPCApairsBefore <- downloadHandler(
    filename = "pca_pairs_before.pdf",
    content = function(file) {
        ggsave(file, plotPCAPairs(pcaBefore()$x,input$pairsPCs),
                "pdf", width = 6, height = 6)
    }
    )

# After Outlier Removal
output$pcaPairPlotAfter <- renderPlot({
    plotPCAPairsAfter(pcaAfter()$x,input$pairsPCs,!outliersPCAkept())
})
output$dlPCApairsAfter <- downloadHandler(
    filename = "pca_pairs_after.pdf",
    content = function(file) {
        ggsave(file,
            plotPCAPairsAfter(pcaAfter()$x,input$pairsPCs,!outliersPCAkept()),
            "pdf",
            width = 6, height = 6)
    }
    )

observeEvent(input$reset_pairsPCs, {
    updateSelectizeInput(session, "pairsPCs", "PCs To Use For plotting pairs:",
        choices = paste0("PC", seq_len(10)),
        selected = paste0("PC", seq_len(10))
        )
})

observeEvent(input$reset_pcToCut, {
    updateSelectizeInput(session, "pcToCut",
                         "PCs To Use For Outlier Detection:",
        choices = paste0("PC", seq_len(10)),
        selected = paste0("PC", seq_len(4))
        )
})

## Hide the Color By Tech Rep button.
observe({
    status$raw_inf_design <- DiscoRhythm:::inferFilteredDesign(
      Maindata(), Metadata())
})

observe({
    req(input$sidebar == "pca")
    if (!status$raw_inf_design$with_tech_replicate) {
        updateRadioButtons(session, "PCAcolor", "Colour by:",
            c("No Color" = "No Color", "ID" = "ID", "Time" = "Time"),
            inline = TRUE
            )
    } else {
        updateRadioButtons(session, "PCAcolor", "Colour by:",
            c("No Color" = "No Color", "ID" = "ID", "Time" = "Time",
                "Replicate ID" = "ReplicateID"),
            inline = TRUE
            )
    }
})

observe({
  # req(Maindata(), Metadata())
  # req(pcaAfter())
    req(!is.na(summaryVal$nSamplesOri))
    summaryVal$pcaCutoff <- input$maxSD
})
