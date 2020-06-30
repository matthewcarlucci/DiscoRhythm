shinyjs::hide(id = "regressionIndividualDiv")
shinyjs::hide(id = "regressionSummaryDiv")
shinyjs::hide(id = "regressionPairwiseDiv")
shinyjs::hide(id = "tableSelectedSample")
shinyjs::hide(id = "regressionRowCompDiv")

#######################################
########## Individual Model  #########
#####################################

rowMean <- reactive({
  rowMeans(regressionData()[, -1], na.rm = FALSE)
})

observe({
    req(input$methodToShow %in% c("JTK", "ARS", "LS", "CS"))

    amp <- discoODAres()[[input$methodToShow]]$amplitude
  # Update Mean Range slider
    minMean <- round(min(rowMean(), na.rm = TRUE), 3) - 0.01
    maxMean <- round(max(rowMean(), na.rm = TRUE), 3) + 0.01
    updateSliderInput(session, "meanRange", min = minMean, max = maxMean,
        value = c(min(rowMean()), max(rowMean())))

    minamp <- round(min(amp, na.rm = TRUE), 3)
    maxamp <- round(max(amp, na.rm = TRUE), 3) + 0.001

    updateSliderInput(session, "ampliRange", min = minamp, max = maxamp,
        value = c(minamp, maxamp))

    shinyjs::show(id = "tableSelectedSample")
})

observe({
    shinyjs::hide("pvalRange")
    shinyjs::hide("qvalRange")
    shinyjs::hide("acroRange")
    shinyjs::hide("ampliRange")
    shinyjs::hide("meanRange")

    if ("amplitude" %in% input$rangeSelection) {
        shinyjs::show("ampliRange")
    }
    if ("acrophase" %in% input$rangeSelection) {
        shinyjs::show("acroRange")
    }
    if ("pvalue" %in% input$rangeSelection) {
        shinyjs::show("pvalRange")
    }
    if ("qvalue" %in% input$rangeSelection) {
        shinyjs::show("qvalRange")
    }
    if ("mean" %in% input$rangeSelection) {
        shinyjs::show("meanRange")
    }
})

singleModelTable <- reactive({
  tmpdata <- discoODAres()[[input$methodToShow]]
  data <- data.frame(IDs = rownames(tmpdata),
                     acrophase = tmpdata$acrophase,
                     amplitude = tmpdata$amplitude,
                     pvalue = tmpdata$pvalue,
                     qvalue = tmpdata$qvalue,
                     Mean = rowMean()
  )
  data
})

output$tableSelectedSample <- DT::renderDataTable({
    ids <- singleModelTable()[individualrowID(), 1]
    dt <- as.data.frame(lapply(singleModelTable()[individualrowID(), ],
        function(X) as.numeric(formatC(X))))
    validate(
        need(length(ids) > 0, "No Significant Rows at Selected Threshold!")
        )
    dt$IDs <- ids
  
    datatable(dt,
        options = list(
            searching = TRUE,
            pageLength = 5,
            lengthMenu = c(5, 10, 15, 20), scrollX = TRUE
            ),
        selection = list(mode = "single", selected = 1, target = "row")
        )
},server = FALSE)
output$dlSingleModelTable <- downloadHandler(
    filename = function() {
        paste0(input$methodToShow, ".csv")
    },
    content = function(file) {
        write.csv(singleModelTable()[individualrowID(), ], file,
                  row.names = FALSE)
    }
    )

# Gives the id of the rows after filtering
individualrowID <- reactive({
    id <- rep(TRUE, nrow(regressionData()))

    if ("amplitude" %in% input$rangeSelection) {
        id <- id & (singleModelTable()$amplitude >= input$ampliRange[1]) &
        (singleModelTable()$amplitude <= input$ampliRange[2])
    }
    if ("acrophase" %in% input$rangeSelection) {
        id <- id & (singleModelTable()$acrophase >= input$acroRange[1]) &
        (singleModelTable()$acrophase <= input$acroRange[2])
    }
    if ("pvalue" %in% input$rangeSelection) {
        id <- id & (singleModelTable()$pvalue >= input$pvalRange[1]) &
        (singleModelTable()$pvalue <= input$pvalRange[2])
    }
    if ("qvalue" %in% input$rangeSelection) {
        id <- id & (singleModelTable()$qvalue >= input$qvalRange[1]) &
        (singleModelTable()$qvalue <= input$qvalRange[2])
    }
    if ("mean" %in% input$rangeSelection) {
        id <- id & (singleModelTable()$Mean >= input$meanRange[1]) &
        (singleModelTable()$Mean <= input$meanRange[2])
    }
    which(id)
})

IndivRowName <- reactive({
    singleModelTable()$IDs[individualrowID()[
    input$tableSelectedSample_rows_selected]]
})
IndivWavePlot <- reactive({
    id <- individualrowID()[input$tableSelectedSample_rows_selected]
    if (!is.null(id) & length(id) != 0) {
        row <- singleModelTable()[id, ]

        data <- regressionData()[id, -1]

        plotRowFit(as.numeric(regressionMeta()$Time),
            as.numeric(data),
            input$methodToShow == "CS",
            withErrorBar = input$withErrorBar == "with",
            per = input$periodInput,
            tunit = input$tUnit,
            ylab = input$obsUnit
            )+ggtitle(paste0(regressionData()[id, 1]))
    }
})
output$wavePlot <- renderPlot({
    IndivWavePlot()
})
output$dlWavePlot <- downloadHandler(
    filename = function(name) {
        paste0(IndivRowName(), ".pdf")
    },
    content = function(file) {
        ggsave(file, IndivWavePlot(), "pdf")
    }
    )

#######################################
############ Summary  ################
#####################################
observe({
    
    # Must check for reactive outside of S4 dispatch (nrow generic) since error
    # attributes are lost (see https://stackoverflow.com/questions/62327810)
    req(discoODAres())
    req(nrow(discoODAres()[[1]]) >= 1)
    
    models <- selectedModels()
    pvalue <- input$pqValueSummary == "1"
    amplilist <- list()
    interlist <- list()

  # Background to use for all summary plots, one of:
  # All rows OR rows selected in "Individual Models"
    bg <- reactive({
        if (input$useFilterInSummary) {
            individualrowID()
        } else {
            seq_len(nrow(regressionData()))
        }
    })

  # Foreground to use for each method
    fg <- reactive({
        idx <- list()
        for (mcod in models) {
            tmp <- which(rowsAfterVar(mcod, input$PQvaluesCutSummary,
                input$pqValueSummary == "1"))
            idx[[mcod]] <- tmp[tmp %in% bg()]
        }
        idx
    })

    allPQValueHist <- reactive({
        plotPQValueHist(discoODAres(),
            bg = bg(),
            input$PvaluesNBreaksSummary,
            input$PQvaluesCutSummary,
            id2name
            )
    })

    allAcroHist <- reactive({
        plotAllAcroHist(discoODAres(), fg(), input$PvaluesNBreaksSummary,
            input$periodInput, input$plotRose, id2name)
    })

    output$allPQValueHist <- renderPlot({
        allPQValueHist()
    })
    output$dlAllPQValueHist <- downloadHandler(
        filename = "pqvalues.pdf",
        content = function(name) {
            ggsave(name, allPQValueHist(), "pdf",
                   width = (length(discoODAres()) - 1) * 4,
                   height = 4)
        }
        )

    output$allAcroHist <- renderPlot({
        allAcroHist()
    })
    output$dlAllAcroHist <- downloadHandler(
        filename = "acrophases.pdf",
        content = function(name) {
            ggsave(name, allAcroHist(), "pdf",
                   width = (length(discoODAres()) - 1) * 4, height = 4)
        }
        )

    if ("CS" %in% models) {
        allAmpliHist <- reactive({
            plotAmpliHist(discoODAres(), bg(), fg()[["CS"]],
                input$PvaluesNBreaksSummary, input$obsUnit)
        })
        output$allAmpliHist <- renderPlot({
            allAmpliHist()
        })
        output$dlAllAmpliHist <- downloadHandler(
            filename = "amplitudes.pdf",
            content = function(name) {
                ggsave(name, allAmpliHist(), "pdf")
            }
            )

        showTab(inputId = "summaryTab", target = "Amplitude")
        shinyjs::show("allAmpliHist")
    } else {
        hideTab(inputId = "summaryTab", target = "Amplitude")
        shinyjs::hide("allAmpliHist")
    }

    if (length(fg()) > 1 & sum(vapply(fg(), length, numeric(1)) >= 1) > 1) {
        showTab(inputId = "summaryTab", target = "Intersection")
        output$allIntersect <- renderPlot({
            allIntersect(fg())
        })
        output$dlAllIntersect <- downloadHandler(
            filename = "intersect_upset.pdf",
            content = function(name) {
                ggsave(name, allIntersect(fg()), "pdf")
            }
            )
    } else {
    # errorPlot("Not enough intersecting results!")
        hideTab(inputId = "summaryTab", target = "Intersection")
    }
})

#######################################
######## Pairwise Comparison #########
#####################################
shinyjs::hide("plotRosePairwise")
shinyjs::hide("metaCompare")
shinyjs::hide("fisherExactText")
shinyjs::hide("metaVenn")
shinyjs::hide("metaAcroHists")
shinyjs::hide("scatterHists")
shinyjs::hide("pairwiseTab")
shinyjs::hide(id = "regressionIndividualDiv")
shinyjs::hide(id = "regressionPairwiseDiv")
shinyjs::hide(id = "regressionSummaryDiv")

observe({
    req(length(discoODAres()) > 0)
    shinyjs::show(id = "regressionIndividualDiv")
    shinyjs::show(id = "regressionRowCompDiv")
    shinyjs::show(id = "regressionSummaryDiv")
    if (length(selectedModels()) >= 2) {
        shinyjs::show(id = "regressionPairwiseDiv")
    } else {
        shinyjs::hide(id = "regressionPairwiseDiv")
    }
})

observe({
  req(length(discoODAres()) >= 2)
  req(input$methodsToCompare1 != "")
  shinyjs::show("plotRosePairwise")
  shinyjs::show("metaCompare")
  shinyjs::show("fisherExactText")
  shinyjs::show("metaVenn")
  shinyjs::show("metaAcroHists")
  shinyjs::show("scatterHists")
  shinyjs::show("pairwiseTab")

    var1 <- rowsAfterVar(input$methodsToCompare1,
        input$PQvaluesCut, input$pqValue == "1")
    var2 <- rowsAfterVar(input$methodsToCompare2,
        input$PQvaluesCut, input$pqValue == "1")
  # Number of bins to compare
    method1 <- input$methodsToCompare1
    method2 <- input$methodsToCompare2

    if (length(unique(var1)) < 2 | length(unique(var2)) < 2) {
        fisher <- list(p.value=NA,
                       estimate=NA)
    } else {
        fisher <- DiscoRhythm:::fisherExact(var1, var2)
    }
    output$metaCompare <- renderTable({
        plt <- plotFisherExact(var1, var2, method1, method2)
    }, include.rownames = TRUE)

    output$fisherExactText <- renderText({
        paste0(
            "<b>Odds ratio:</b> ", fisher$estimate["odds ratio"]
            )
    })

    metaVenn <- reactive({
        vennFisherExact(var1, var2, method1, method2)
    })

    output$metaVenn <- renderPlot({
        if (!any(var1) & !any(var2)) {
            errorPlot("No significant rows")
        } else {
          metaVenn()
        }
    })

    ### Acrophase plot
    pvs1 <- discoODAres()[[method1]]$pvalue
    pvs2 <- discoODAres()[[method2]]$pvalue
    if (input$pqValue == 0) {
        pvs1 <- p.adjust(pvs1, method = "BH")
        pvs2 <- p.adjust(pvs2, method = "BH")
    }

    ovidx <- reactive({
      if (input$ovMethod == "Both") {
        idx <- (pvs1 < input$PQvaluesCut) & (pvs2 < input$PQvaluesCut)
      } else if (input$ovMethod == "Either") {
        idx <- (pvs1 < input$PQvaluesCut) | (pvs2 < input$PQvaluesCut)
      } else if (input$ovMethod == "Method 1") {
        idx <- (pvs1 < input$PQvaluesCut)
      } else if (input$ovMethod == "Method 2") {
        idx <- (pvs2 < input$PQvaluesCut)
      } else if (input$ovMethod == "Neither") {
        idx <- (pvs1 > input$PQvaluesCut) & (pvs2 > input$PQvaluesCut)
      }
      idx
    })
    idx <- ovidx()

    phase1 <- discoODAres()[[method1]]$acrophase[idx]
    phase2 <- discoODAres()[[method2]]$acrophase[idx]

    scatterHists <- reactive({
        acro <- data.frame(phase1, phase2)
        marginal <- ggplot(acro, aes(x = phase1, y = phase2)) +
            geom_point() +
            geom_abline(slope = 1, intercept = 0) +
            xlab(paste0(id2name[method1], " Acrophase")) +
            ylab(paste0(id2name[method2], " Acrophase")) +
            xlim(c(0, input$periodInput)) +
            ylim(c(0, input$periodInput)) +
            sharedtheme()
        ggExtra::ggMarginal(marginal, type = "histogram",
                bins = input$PvaluesNBreaks)
    })

    output$scatterHists <- renderPlot({
        if (!any(var1) | !any(var2)) {
            errorPlot("No significant rows")
        } else {
            scatterHists()
        }
    })

    output$dlscatterHists <- downloadHandler(
        filename = "scatterHists.pdf",
        content = function(name) {
          if (!any(var1) | !any(var2)) {
            p <- errorPlot("No significant rows")
          } else {
            p <- scatterHists()
          }
            ggsave(name, p, "pdf")})

})

output$dlOverlaps <- downloadHandler(
    filename = function() {
        paste0(input$methodsToCompare1,"_",input$methodsToCompare1,
               "_",input$ovMethod,".csv")
    },
    content = function(file) {
        write.csv(discoODAres()[[input$methodsToCompare1]][ovidx(),], file,
                  row.names = FALSE)
    }
    )

### Display additional inputs only when Acrophase tab is selected
observe({
    if (req(input$pairwiseTab) == "Acrophase") {
        shinyjs::show("PvaluesNBreaks")
        shinyjs::show("reset_PvaluesNBreaks")
        shinyjs::show("ovMethod")
    } else {
        shinyjs::hide("PvaluesNBreaks")
        shinyjs::hide("reset_PvaluesNBreaks")
        shinyjs::hide("ovMethod")
    }
})
### Gets logical of significant rows for the "name" method
rowsAfterVar <- function(name, threshold = 0.01, unadj_pvalue = TRUE) {
    pvs <- discoODAres()[[name]]$pvalue
    if (unadj_pvalue) {
        rows <- (pvs <= threshold & (pvs != "NaN"))
    } else {
        rows <- (p.adjust(pvs, method = "BH") <= threshold & (pvs != "NaN"))
    }
    rows
}

### Slider value reset buttons
observeEvent(input$reset_PvaluesNBreaksSummary, {
    updateSliderInput(session, "PvaluesNBreaksSummary", "Number of Bins",
        min = 10, max = 500, step = 1, value = 50
        )
})
observeEvent(input$reset_PQvaluesCutSummary, {
    updateSliderInput(session, "PQvaluesCutSummary", "Significance Cutoff",
        min = 0, max = 1, step = 0.01, value = 0.05
        )
})
observeEvent(input$reset_PQvaluesCut, {
    updateSliderInput(session, "PQvaluesCut", "Significance Cutoff",
        min = 0, max = 1, step = 0.01, value = 0.05
        )
})
observeEvent(input$reset_PvaluesNBreaks, {
    updateSliderInput(session, "PvaluesNBreaks", "Number of Bins",
        min = 10, max = 500, step = 1, value = 30
        )
})
