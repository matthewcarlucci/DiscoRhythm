################################
######### Pre-Analysis #########
# Evaluate the set of appropraite algorithms for the dataset
# Provide other warnings for settings and inputs
################################

shinyjs::hide("avgReminder")
shinyjs::hide("regressionMethod")
shinyjs::hide("previewRegDataDiv")

# Gathering the experimental design for oscillation detection
observe({
    req(FinalSE())
    status$osc_inf_design <- DiscoRhythm:::inferOscDesign(FinalSE())
})
observe({
    status$osc_input_design <- list(
        "circular_t" = input$timeType == "circular",
        "main_per" = input$main_per
        )
})

# Validation of algorithms and input period given the study design
observe({
  # These req are necessary to prevent pre-emptive evaluation for some reason
    req(regressionMeta())
    req(input$sidebar == "regressionPage")

    DiscoRhythm:::discoShinyHandler({
        status$osc_badPeriod <- DiscoRhythm:::checkPeriod(regressionMeta()$Time,
            input$periodInput)
        if ("JTK" %in% input$regressionMethod) {
            status$osc_badJTKperiod <- DiscoRhythm:::checkJTKperiod(
              regressionMeta()$Time,
                input$periodInput)
        } else {
      # User doesn't need to see warnings if not using JTK
            status$osc_badJTKperiod <- suppressWarnings({
                DiscoRhythm:::checkJTKperiod(regressionMeta()$Time,
                                             input$periodInput)
            })
        }
    }, "Period Validation", shinySession = session)
})
observe({
    req(!is.null(status$osc_inf_design))
  # For an unknown reason this obs executes in rowSelection
    req(input$sidebar == "regressionPage")

    status$osc_validMethods <- DiscoRhythm:::checkODAs(
        status$osc_inf_design,
        status$osc_input_design$circular_t,
        status$osc_badPeriod,
        status$osc_badJTKperiod
        )

    if (any(!(input$regressionMethod %in% status$osc_validMethods))) {
        currentInputs <- input$regressionMethod
        newInputs <- currentInputs[currentInputs %in% status$osc_validMethods]
        updateCheckboxGroupInput(session,
            "regressionMethod",
            choices = name2id,
            selected = name2id[name2id %in% newInputs],
            inline = FALSE
            )
    }
})

# Disable invalid methods
observe({
  # req(status$osc_validMethods)
    shinyjs::show("regressionMethod")
  # Update the available regression methods
    updateCheckboxGroupInput(session,
        "regressionMethod",
        choices = name2id,
        selected = name2id[name2id %in% status$osc_validMethods],
        inline = FALSE
        )
})

# This is a function that returns the exclusion criteria matrix
# function() is used instead of renderTable() for a reason I cannot remember
output$exclusionMatrix <- reactive({
    mat <- discoODAexclusionMatrix

    critpresent <- which(c(
        status$osc_inf_design$missing_value,
        status$osc_inf_design$with_replicate,
        status$osc_inf_design$non_integer_interval,
        status$osc_inf_design$uneven_interval,
        status$osc_input_design$circular_t,
        status$osc_badPeriod,
        status$osc_badJTKperiod
        ))

    if(all(!critpresent)){
      ret <- matrix()
    } else {
      exODA <- which(!apply(mat[,critpresent,drop=FALSE],1,all))
      # convert TRUE/FALSE to ""/"x"
      mat2 <- apply(mat, 2,
                    function(x) c("TRUE" = "", "FALSE" = "x")[as.character(x)])
      rownames(mat2) <- id2name[rownames(mat)]
      ret <- t(mat2)[critpresent,,drop=FALSE] %>%
        knitr::kable("html",align='c') %>%
        kableExtra::kable_styling(c("striped", "bordered")) %>%
        kableExtra::column_spec(exODA+1,background = colors$sig,
                            include_thead = TRUE)

    }
    return(ret)
})

# Switch the page if user press the button on "Low sample size detected!" modal
observeEvent(input$reSample, {
    updateTabItems(session, "sidebar", "metadata")
    removeModal()
})

# Switch the page if user press the button on "No rows selected!" modal
observeEvent(input$reSampleRow, {
    updateTabItems(session, "sidebar", "rowReplicateAnalysis")
    removeModal()
})

# Force selection of at least one method
observe({
    req(input$sidebar == "regressionPage" |
        input$sidebar == "overview" |
        input$sidebar == "rowReplicateAnalysis")

    if (sum(status$osc_validMethods %in% input$regressionMethod) == 0) {
        shinyjs::disable("startRegress")
    } else {
        shinyjs::enable("startRegress")
    }
})


# Return runtime estimate
output$regressionWarning <- renderText({
  # Runtime estimate
    nrows <- ifelse(status$filtered_inf_design$with_tech_replicate & 
                      input$aovMethod!="None",
        sum(anovaP() <= input$anovaCut),
        nrow(DataFinal())
        )
    runtime <- sum(nrows / RTconst[selectedModels()])

  # Execution messages/warnings
    txt <- h4("Execution Summary")
    txt <- paste0(
        txt,
        tags$li(tags$b(paste0("Approximate runtime: ",
            formatC(round(runtime / 60) + 1), " minutes"))))

    txt
})
