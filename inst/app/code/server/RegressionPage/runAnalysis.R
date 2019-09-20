# Used for both report and notification emails
sendEmail <- function(recipients, ...){
  
  sender_creds <- readRDS(sender_creds_file)
  sender <- paste0("DiscoRhythm <",sender_creds$email,">")
  
  mailR::send.mail(from = sender,
                   to = recipients,
                   ... ,
                   html=FALSE,
                   smtp = list(host.name = "smtp.gmail.com", port = 465, 
                               user.name = sender_creds$email,            
                               passwd = sender_creds$passwd, 
                               ssl = TRUE),
                   authenticate = TRUE,
                   send = TRUE)
}

selectedModels <- reactive({
    methods <- input$regressionMethod
    methods[methods %in% status$osc_validMethods]
})

discoODAres <- eventReactive(input$startRegress, {
    req(input$batchReceiveMethod == "Interactive")
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
        discoODAs(FinalSE(),
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

    # Notification for ready interactive session
    if(!is.null(input$byEmail)){
      if(file.exists(sender_creds_file) & input$byEmail){
        DiscoRhythm:::discoShinyHandler({
 
          sendEmail(recipients = input$emailAddress,
                    subject = "DiscoRhythm Session Information",
                    body = paste0("Your results are ready for viewing. ",
                                  "The session will time out ",
                  "after 30 minutes of inactivity.")
          )
          
        },"Email Notification",shinySession = session)
      }
    }

    odares
})

# Waiting for interactive results pop-up
observeEvent(input$startRegress,{
    req(input$batchReceiveMethod == "Interactive")
    showModal(modalDialog(
      title = "Oscillation Detection Running",
      p("Please be patient, this might take a few minutes
                depending on your sample size. "),
      ODAmodalText(),
      easyClose = FALSE, footer = NULL, size="l"
    ))
})

# Remove modal when interactive oscillation detection finishes
observe({
    req(input$methodToShow %in% name2id)
    req(!is.null(discoODAres()))
    removeModal()
})

ODAmodalText <- reactive({
  usedModels <- paste(selectedModels()) %>% replace(., name2id,id2name)[.]
  # Runtime estimation
  nrows <- ifelse(status$filtered_inf_design$with_tech_replicate &
                    input$aovMethod!="None",
                  sum(anovaP() <= input$anovaCut),
                  nrow(DataFinal())
  )
  runtime <- sum(nrows / RTconst[selectedModels()])
  
  list(
       tags$b("Do not close this tab, or the session will be terminated and
                   results will be lost."),
       br(),
       h4("Execution Summary"),br(),
       "Number of Samples: ", ncol(Maindata())-1, br(),
       "Number of Rows: ", nrow(Maindata()), br(),
       "Selected Methods: ", paste(usedModels,collapse=", "), br(),
       "Time started: ", format(Sys.time(),usetz = TRUE), br(),
       "Approximate runtime: ",
       paste0(formatC(round(runtime / 60) + 1), " minute(s)"),
       br(),br(),
       "If the application is taking much longer than expected,
                consider refreshing the page to start again.
      For better performance, consider using a local installation
                of DiscoRhythm (if not already)."
  )
})

