selectedModels <- reactive({
    methods <- input$regressionMethod
    methods[methods %in% status$osc_validMethods]
})

discoODAres <- eventReactive(input$startRegress, {
  # please see regressionPlot.R
    validate(
        need(input$periodInput > 0, "Has to be bigger than 0")
        )
  # js$collapse("previewRegData") # I don't think users like this

    if (length(selectedModels()) >= 2) {
        updateSelectInput(session,
            "methodsToCompare1",
            choices = name2id[name2id %in% selectedModels()]
            )
        updateSelectInput(session,
            "methodsToCompare2",
            choices = name2id[name2id %in% selectedModels()],
            selected = name2id[name2id %in% selectedModels()][2]
            )
    }

    odares <- DiscoRhythm:::discoShinyHandler({
        discoODAs(
            regressionData(),
            regressionMeta(),
            method = selectedModels(),
            circular_t = status$osc_input_design$circular_t,
            period = input$periodInput,
            ncores = NCORES
            )
    },
    section = "Oscillation Detection",
    shinySession = session
    )

    updateRadioButtons(session,
        "methodToShow",
        choices = name2id[name2id %in% selectedModels()]
        )

    if (status$osc_inf_design$with_replicate) {
        shinyjs::disable("withErrorBars")
    } else {
        shinyjs::enable("withErrorBars")
    }

    odares
})

observeEvent(input$startRegress,{
  # Model names
    usedModels <- paste(selectedModels()) %>%
    replace(., name2id,id2name)[.]
  # Runtime estimation
    nrows <- ifelse(status$filtered_inf_design$with_tech_replicate &
        input$useAnova,
        sum(anovaP() <= input$anovaCut),
        nrow(DataFinal())
        )
    runtime <- sum(nrows / RTconst[selectedModels()])
    showModal(modalDialog(
        title = "Oscillation Detection Running",
        HTML(
            paste0("Please be patient, this might take a few minutes
                depending on your sample size. ",
                "For better performance, consider using a local installation
                of DiscoRhythm.",
                br(), br(),
                "Analysis summary:",br(),
                "Number of Samples: ", nrow(regressionMeta()), br(),
                "Number of Rows: ", nrow(regressionData()), br(),
                "Selected Methods: ", paste(usedModels,collapse=", "), br(),
                "Time started: ", format(Sys.time(),usetz = TRUE), br(),
                "Approximate runtime: ",
                paste0(formatC(round(runtime / 60) + 1), " minute(s)"),
                br(),br(),
                "If the app is taking too long,
                consider refreshing the page to start again."
                )),
        easyClose = FALSE, footer = NULL, size="l"
        ))
})

observe({
    req(input$methodToShow %in% name2id)
    req(!is.null(discoODAres()))
    removeModal()
})
