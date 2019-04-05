#' Generate Experiment Metadata
#'
#' Parses the sample metadata from a vector of sample names (often column names
#' of a Maindata format data.frame).
#'
#' @param samplenames character, a list of sample names following the
#' DiscoRhythm naming convention (<prefix><Time>_<UniqueID>_<ReplicateID>).
#' @param shinySession shiny session object for use only by the DiscoRhythm
#' shiny app \code{discoApp()} to update the axis labels using the time
#' value prefix.
#'
#' @details The regular expression used to obtain metadata is
#' "^([[:alpha:]]*)(\\-?[0-9]+[\\.]?[0-9]*)\\_?
#' ([[:alnum:]\\.]*)\\_?([[:alnum:]\\.]*)$"
#'
#' Where each () will be used to construct the final metadata data.frame
#'
#' @return a data.frame containing 3 columns of metadata. ID = unique sample
#' identity. Time = sample colleciton time. ReplicateID = Identifier where Time
#' + ReplicateID indicates a biological sample ID.
#'
#' @export
#'
#' @examples
#'
#' discoParseMeta(c("CT24_AD_1","CT24_AS_1","CT24_AE_2","CT24_AW_2",
#' "CT26_AB_1","CT26_AC_1","CT26_BB_2","CT26_BC_2"))
#'
discoParseMeta <- function(samplenames, shinySession = NULL) {

    # master regular expression to extract metadata
    myregex <- paste0("^([[:alpha:]]*)(\\-?[0-9]+[\\.]?[0-9]*)",
        "\\_?([[:alnum:]\\.]*)\\_?([[:alnum:]\\.]*)$")

    if (!all(grepl(myregex, samplenames))) {
        stop(c("Unable to parse sample names. ",
            "Please use time values only, or use the same structure ",
            "as provided example files to indicate replicates."))
    }

    meta <- data.frame(
        "name" = samplenames,
        "prefix" = gsub(myregex, "\\1", samplenames),
        "Time" = gsub(myregex, "\\2", samplenames),
        "unique_id" = gsub(myregex, "\\3", samplenames),
        "replicate_id" = gsub(myregex, "\\4", samplenames)
        )

    # If technical replicates were not explicitly identified
    if (any(meta$replicate_id == "")) {
        # Add dummy techrep ID and sample ID
        meta$replicate_id <- seq_along(meta$Time)
        message(c("Missing replicate information for some samples. ",
            "Assuming there are no technical replicates in this dataset."))
    }

    if(any(duplicated(meta$name))){
        message("Some samples cannot be uniquely identified. New unique
        identifiers have been created for all samples.")
        meta$unique_id <- paste0("uid",seq_along(meta$Time))
        samplenames <- paste0(meta$prefix,meta$Time,
                                "_",meta$unique_id,"_",meta$replicate_id)
    }

    # If executed in the shiny app, update the axis labels using the parsed
    # prefix
    timeName <- unique(meta$prefix)
    if (!is.null(shinySession) & length(timeName) == 1 & timeName != "") {
        message(
            paste0("Using the detected column name prefix (",
                timeName,
                ") as the time unit")
            )
        shiny::updateTextInput(shinySession, inputId = "tUnit",
                                value = timeName)
    }

    return(data.frame(
        "ID" = samplenames,
        "Time" = as.numeric(as.character(meta$Time)),
        "ReplicateID" = as.character(meta$replicate_id)
        ))
}
