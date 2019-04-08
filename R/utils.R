#' Data formatting for DiscoRhythm
#'
#' Functions to convert between usage of
#' data.frame or SummarizedExperiment data formats.
#'
#' @name discoDFtoSE
NULL

#' @rdname discoDFtoSE
#'
#'
#' @inheritParams discoInterCorOutliers
#' @export
#' 
#' @return a SummarizedExperiment object with colData containing a 
#' DiscoRhythm format Metadata DataFrame. 
#' 
#' @importFrom SummarizedExperiment SummarizedExperiment assay
#' @importFrom S4Vectors DataFrame
discoDFtoSE <- function(Maindata,Metadata=NULL){
    if(methods::is(Maindata,"SummarizedExperiment")){
        warning("Input is already a SummarizedExperiment")
        return(Maindata)
    }
    counts <- as.matrix(Maindata[,-1])
    rownames(counts) <- Maindata[,1]
    colnames(counts) <- NULL
    if(is.null(Metadata)){
        Metadata <- discoParseMeta(colnames(Maindata)[-1])
    }
    
    se <- SummarizedExperiment(assays=list(counts=counts),
                               colData=DataFrame(Metadata))
    
    return(se)
}

#' @rdname discoDFtoSE
#' 
#' @param se SummarizedExperiment with \code{!is.null(rownames(se))}.
#' 
#' @export
#' 
#' @return a DiscoRhythm format Maindata data.frame.
#' 
#' @importFrom SummarizedExperiment assay
#' @importFrom BiocGenerics rownames
discoSEtoDF <- function(se){
    return(data.frame("IDs"=rownames(se),assay(se)))
}

#' Handle Error/Warning messages appropriately with
#' shiny notifications for warnings and
#' pop-ups for errors
#'
#' @keywords internal
#' @return output from expr
discoShinyHandler <- function(expr,
    section = "Execution",
    shinySession = NULL) {
    myWarnings <- NULL
    myMessages <- NULL
    results <- tryCatch({
        withCallingHandlers({
            expr
        },
        message = function(msg) {
            myMessages <<- c(myMessages, list(msg))
        },
        warning = function(msg) {
            myWarnings <<- c(myWarnings, list(msg))
            invokeRestart("muffleWarning")
        }
        )
    },
    error = function(msg) {
        shiny::showModal(shiny::modalDialog(
            title = paste0("Error in ", section),
            shiny::tags$b(msg$message), shiny::tags$br(),
            "R function: ",
            msg$call, shiny::tags$br(),
            "Contact the author if you believe this to be a bug",
            easyClose = TRUE, footer = NULL
            ))
    }
    )

    if (!is.null(myMessages) & !is.null(shinySession)) {
        for (i in seq_along(myMessages)) {
            shiny::showNotification(
                type = "message",
                shiny::tags$h4("Message"),
                myMessages[[i]]$message,
                session = shinySession, duration = 10
                )
        }
    }
    if (!is.null(myWarnings) & !is.null(shinySession)) {

        warns <- utils::head(myWarnings)
        # Limit to 5 warnings
        if(length(myWarnings)>5){
            warns[[6]]$message <- paste0("There were ", length(myWarnings)-5,
                                    " other warnings.")
        }

        for (i in seq_along(warns)) {
            shiny::showNotification(
                type = "warning",
                shiny::tags$h4(paste0("Warning in ", section)),
                warns[[i]]$message,
                session = shinySession, duration = 40
                )
        }
    }

    return(results)
}
