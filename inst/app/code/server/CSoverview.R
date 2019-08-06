observe({
    req(input$sidebar == "regressionPage" | input$sidebar == "overview")
    if (nrow(regressionData()) == 0) {
        showModal(modalDialog(
            title = "No rows selected!",
            HTML(
                paste0("You currently have no data rows selected."),
                "<br/>",
                paste0(
                    "Unable to continue analysis, consider ",
                    "using a lower significance cutoff in  ",
                    actionButton("reSampleRow",
                        "Row Selection",
                        icon = icon("vcard-o")
                        )
                    )
                ),
            easyClose = FALSE
            ))


        shinyjs::hide("previewRegDataDiv")
        shinyjs::hide("overviewDiv")
    } else {
        shinyjs::show("previewRegDataDiv")
        shinyjs::show("overviewDiv")
    }
})


OVpca <- reactive(DiscoRhythm:::discoPCA(FinalSE()))

### Period detection
output$PeriodDetectionRange <- renderUI({
  # Circular time can only detect harmonics of the base-cycle period
  # Linear time can detect a continuous range of periods

    csect <- input$timeType == "circular"
    trainper <- as.numeric(input$main_per)
    rng <- diff(range(regressionMeta()$Time))
    sampint <- min(unique(diff(sort(unique(regressionMeta()$Time)))))

    if (csect) {
    # Only test periods larger than twice the sampling interval
        tests <- (trainper / c(1:8))[
        which(as.numeric(trainper / c(1:8)) > sampint * 2)]
        selectInput(
            inputId = "OVperiodSlider",
            label = "Periods To Test:",
            multiple = TRUE,
            choices = tests,
            selected = tests
            )
    } else {
        list(
            sliderInput("OVperiodSlider",
                label = "Range of Periods:",
                min = sampint * 3,
                max = rng + sampint,
                value = c(sampint*3, rng + sampint)
                ),
            p("12 periods will be tested, spaced evenly across the frequency
                domain within the selected range.", class = "text-muted")
            )
    }
})

PeriodRes <- reactive({
    req(!is.null(input$OVperiodSlider) & nrow(regressionData()) != 0)

    DiscoRhythm:::discoShinyHandler({
        discoPeriodDetection(
            FinalSE(),
            input$timeType,
            input$main_per,
            input$OVperiodSlider
            )
    }, section = "Period Detection", shinySession = session)
})



output$OVperiodTable <- renderTable(OVpca()$table)
output$dlOVperiodTable <- downloadHandler(
    filename <- "periodogram.csv",
    content = function(file) {
        write.csv(OVpca()$table, file)
    }
    )

OVperiodPlot <- reactive(plotPeriodDetect(PeriodRes(), input$tUnit))
output$OVperiod <- renderPlot(OVperiodPlot())
output$dlOVperiod <- downloadHandler(
    filename = "periodogram.pdf",
    content = function(file) {
        ggsave(file, OVperiodPlot())
    }
    )


########################################
# Single period fits
########################################
output$PCfitSelectPeriod <- renderUI({
  # Circular time can only detect harmonics of the base-cycle period
  # Linear time can detect a continuous range of periods

    csect <- input$timeType == "relative"
    trainper <- as.numeric(input$main_per)
    rng <- diff(range(regressionMeta()$Time))
    sampint <- min(unique(diff(sort(unique(regressionMeta()$Time)))))

    if (csect) {
    # Only test periods larger than twice the sampling interval
        tests <- (trainper / c(1:8))[
        which(as.numeric(trainper / c(1:8)) > sampint * 2)]
        selectInput(
            inputId = "OVperiodSelect",
            label = "Period",
            choices = tests,
            selected = trainper
            )
    } else {
        sliderInput("OVperiodSelect",
            label = "Period",
            min = sampint,
            max = rng + sampint,
            value = trainper
            )
    }
})

OVpcaFits <- reactive({
    npc <- ifelse(ncol(OVpca()$x) > 10, 10, ncol(OVpca()$x))
    per <- as.numeric(input$OVperiodSelect)
    se <- discoDFtoSE(data.frame("PC"=1:ncol(OVpca()$x),t(OVpca()$x)),
                      regressionMeta())
    res <- discoODAs(se,period = per)$CS[seq_len(npc), ]
    res <- data.frame("PC" = paste0("PC", seq_len(npc)), res)
    res
})
output$OVpcaFits <- renderTable(OVpcaFits())
output$dlOVpcaFits <- downloadHandler(
    filename = "PCfits.csv",
    content = function(file) write.csv(OVpcaFits(), file)
    )

OVpcaScatter <- reactive({
    req(input$OVperiodSelect)
    plotOVpcaScatter(OVpca(), regressionMeta(), input$OVperiodSelect,
                     PCsToUse = input$PCfitSelectPCs)
})
output$OVpcaScatter <- renderPlot(OVpcaScatter())
output$dlOVpcaScatter <- downloadHandler(
    filename = "PCfits.pdf",
    content = function(file) {
        ggsave(file, OVpcaScatter(), "pdf")
    }
    )
