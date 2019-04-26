##### DiscoRhythm Shiny Application ###################
# Calls on the DiscoRhythm R package referencing
# functions both:
#   1) Exported (referenced directly)
#   2) Internal (referenced as DiscoRhythm:::)
#################################################

########################################
# LOAD PACKAGES
########################################
# Needed for startup
library(parallel)
library(gridExtra)
library(DT)
library(plotly)
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinydashboard)
library(SummarizedExperiment)
library(DiscoRhythm)

########################################
# DEFINED FUNCTIONS AND OPTIONS
########################################

source("code/server/plots.R")
options(shiny.maxRequestSize = 100 * 1024^2, warn = -1)
options(spinner.color.background = "#F5F5F5")
options(spinner.color = colors$discoMain)

# Add the question mark icon to an input title
addQicon <- function(title = "", id = NULL) {
    tags$span(id = id, HTML(paste(title, icon("question-circle",
        class = "text-muted"))))
}

########################################
# GLOBAL VARIABLES
########################################
docsURL <- "https://bioconductor.org/packages/3.9/bioc/vignettes/DiscoRhythm/inst/doc/disco_workflow_vignette.html"
verCode <- packageVersion("DiscoRhythm")

# Method code to full name
id2name <- discoODAid2name
name2id <- names(id2name)
names(name2id) <- id2name

jsRestartApp <- "shinyjs.reset = function() {history.go(0)}"
jsShinyBusy <- "$('html').hasClass('shiny-busy')"
jsShinyNotBusy <- "!$('html').hasClass('shiny-busy')"
jsCollapseBox <- "shinyjs.collapse = function(boxid) {
    $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}"

# aprox. rows/second for each method
RTconst <- c("JTK" = 100, "ARS" = 75, "LS" = 50, "CS" = 700)

discotheme <- NULL

# argument passed from discoApp or set outside call to app.R
if(exists(".discorhythm_ncores")){
  NCORES <- .discorhythm_ncores
} else {
  NCORES <- 1
}

########################################
# USER INTERFACE
########################################

# Dashboard title is what the browser tab will be named
ui <- dashboardPage(
    title = "DiscoRhythm",
  ####################
  # Header
  ####################
    dashboardHeader(
        title = div(
      # What happens when R server is NOT busy
            conditionalPanel(
                jsShinyNotBusy,
        # Sun Icon
                tags$a(
                    style = "",
                    tags$p(
                        style = "font-size:25px;display: inline-block;",
                        class = "disco",
                        tags$img(
                            src = "disco_32.png",
                            style = "position: absolute; left: 5px;
                            padding: 8px;",
                            srcset = "disco_64.png 2x",
                            alt = "DiscoRhythm",
                            span("DiscoRhythm",style="padding-left:20px")
                            )
                        ),
                    href = docsURL,
                    target = "_blank"
                    )
                ),
      # What happens when R server is Busy/Working
            conditionalPanel(
                jsShinyBusy,
        # Icon spins
                tags$a(
                    class = "disco",
                    div(style="width: 100%; overflow: hidden;",
                      div("DiscoRhythm",class="disco",style="
                            font-size:25px;
                            position: absolute; left: 42px;
                            display: inline-block;
                            margin-left: 10px"),
                        div(class="mainloader",style="width: 32px;
                            margin-top: 9px;margin-left: 0px;
                            margin-bottom: 9px;border-left-width: 8px;
                            float: left")
                    ),
                    href = docsURL,
                    target = "_blank"
                    )
                )
            ),
        tags$li(
            style = "position: absolute; left: 40px; padding: 0px;",
            class = "dropdown",
            actionLink(
                inputId = "continue",
                label = HTML(paste(icon("play", class = "disco-button"), ""))
                )
            ),
        tags$li(
            class = "dropdown",
            "Loaded Dataset:",
                verbatimTextOutput("dataName"),
                tags$head(tags$style("#dataName{font-size:12px;
                                      margin-bottom: 5px;
                                      margin-right: 5px;
                max-height: 45px; background: ghostwhite; padding: 2px;}"))
            )
        ),

  ####################
  # Sidebar
  ####################
  # List items (parts of analysis) in the sidebar
    dashboardSidebar(sidebarMenu(
    # We need this to update/jump
    # when "Run analyis" in uploadData is clicked
        id = "sidebar",
        tags$head(
            tags$style(HTML("
                .sidebar { height: 90vh; overflow-y: auto; }
                "))
            ),
        tags$head(tags$style(".inactiveLink {
            pointer-events: none;
            cursor: default;
        }")),
        shinyjs::useShinyjs(),
    # menuItem is the title for the section
    # tabName corresponds to code in code/ui/*R
        menuItem("Introduction",
             tabName = "introPage",
             icon = icon("info-circle")
        ),
        hr(class="sidebarsplitter"),
        menuItem("Select Data",
            tabName = "selectData",
            icon = icon("cloud-upload")
            ),
        hr(class="sidebarsplitter"),
        div(style = "padding-left: 15px;color: var(--neutral2);",
            h4("Quality Control")),
        menuItem("Inter-Sample Correlations",
            tabName = "filtCorrelationInterCT",
            icon = icon("compress")
            ),
        menuItem("PCA",
            tabName = "pca",
            icon = icon("arrows")
            ),
        menuItem("Filtering Summary",
            tabName = "metadata",
            icon = icon("edit", lib = "glyphicon")
            ),
        menuItem("Row Selection",
            tabName = "rowReplicateAnalysis",
            icon = icon("filter")
            ),
        hr(class="sidebarsplitter"),
        div(style = "padding-left: 15px;color: var(--neutral2);",
            h4("Main Analysis")),
        menuSubItem("Period Detection", "overview",
            icon = icon("signal")),
        menuSubItem("Oscillation Detection", "regressionPage",
            icon = icon("clock-o")),
        hr(class="sidebarsplitter"),
        menuItem("Session Archiving",
            tabName = "sessionArchiving",
            icon = icon("cloud-download-alt")
            ),
        hr(class="sidebarsplitter"),
        tableOutput("summaryTable"),
        p(paste0("DiscoRhythm v", verCode)),
    # Restart App (avoiding V8 dep by using functions arg)
        shinyjs::extendShinyjs(text = jsRestartApp,functions = c("reset"))
        )),

  # Where to find code for each tabItem
    dashboardBody(
        includeCSS("www/custom_styles.css"), # custom color themes
        HTML(paste0('<link rel="stylesheet" href="https://use.fontawesome.com',
            '/releases/v5.3.1/css/all.css">')),
        HTML(paste0('<link rel="stylesheet" href="https://use.fontawesome.com',
            '/releases/v5.3.1/css/v4-shims.css">')),
    # fixed dashboard header/sidebar
        tags$script(HTML("$('body').addClass('fixed');")),
        tabItems(
            tabItem("introPage",
                source("code/ui/introPage.R", TRUE)$value),
            tabItem("selectData",
                source("code/ui/selectData.R", TRUE)$value),
            tabItem("filtCorrelationInterCT",
                source("code/ui/filtCorrelationInterCT.R", TRUE)$value),
            tabItem("pca",
                source("code/ui/PCA.R", TRUE)$value),
            tabItem("metadata",
                source("code/ui/metaData.R", TRUE)$value),
            tabItem("rowReplicateAnalysis",
                source("code/ui/rowReplicateAnalysis.R", TRUE)$value),
            tabItem("overview",
                source("code/ui/CSoverview.R", TRUE)$value),
            tabItem("regressionPage",
                source("code/ui/regressionPlot.R", TRUE)$value),
            tabItem("sessionArchiving",
                source("code/ui/sessionArchiving.R", TRUE)$value)
            )
        )
    )


########################################
# SERVER
########################################

server <- function(input, output, session) {

  # List code for analysis (aka server)
  # Order matters!
    source("code/server/selectData.R", TRUE)
    source("code/server/filtCorrelationInterCT.R", TRUE)
    source("code/server/PCA.R", TRUE)
    source("code/server/metaData.R", TRUE)
    source("code/server/rowReplicateAnalysis.R", TRUE)
    source("code/server/CSoverview.R", TRUE)
    source("code/server/RegressionPage/regression.R", TRUE)
    source("code/server/sessionArchiving.R", TRUE)

  # Stores important values on the analysis status
    status <- reactiveValues()
    summaryVal <- reactiveValues()
  ##### Continue logic
  # Would be better if this was all client side
    observeEvent(input$continue, {
        if (input$sidebar == "introPage") {
            updateTabItems(session, "sidebar", "selectData")
        } else if (input$sidebar == "selectData") {
            if (is.null(input$inCSV$name) & input$selectInputType == "csv") {
                showModal(modalDialog(
                    title = "Input Data Required",
                    "Please upload a CSV or choose a demo CSV to continue",
                    easyClose = TRUE
                    ))
            } else if (hideQc()) {
                runjs(paste0('$("ul.sidebar-menu").',
                    'find("a[href=\'#shiny-tab-filtSamp\']").hide()'))
                runjs(paste0('$("ul.sidebar-menu").',
                    'find("ul[data-expanded=\'QualityControl\']").hide()'))
                updateTabItems(session, "sidebar", "regressionPage")
            } else {
                runjs(paste0('$("ul.sidebar-menu").',
                    'find("a[href=\'#shiny-tab-filtSamp\']").show()'))
                runjs(
                    paste0('$("ul.sidebar-menu").',
                        'find("ul[data-expanded=\'QualityControl\']").show()'))
                updateTabItems(session, "sidebar", "filtCorrelationInterCT")
            }
        } else if (input$sidebar == "filtCorrelationInterCT") {
            updateTabItems(session, "sidebar", "pca")
        } else if (input$sidebar == "pca") {
            updateTabItems(session, "sidebar", "metadata")
        } else if (input$sidebar == "metadata") {
            updateTabItems(session, "sidebar", "rowReplicateAnalysis")
        } else if (input$sidebar == "rowReplicateAnalysis") {
            updateTabItems(session, "sidebar", "overview")
        } else if (input$sidebar == "overview") {
            updateTabItems(session, "sidebar", "regressionPage")
        } else if (input$sidebar == "regressionPage") {
          updateTabItems(session, "sidebar", "sessionArchiving")
        } else if (input$sidebar == "sessionArchiving") {
            showNotification(type = "warning", duration = 4,
                HTML("<h4>No next step available.</h4>"))
        }
    })

  ### Downstream packages
    observe({
        req(input$sidebar != "selectData")
        library(UpSetR)
        library(reshape2)
        library(tools)
        library(VennDiagram)
        library(broom)
        library(heatmaply)
    })
    
}

########################################
# RUN APP
########################################

shinyApp(ui, server)
