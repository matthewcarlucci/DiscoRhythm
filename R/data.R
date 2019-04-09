#' Mapping Identifiers to Full Names
#'
#' A small named vector mapping oscillation detection algorithm names
#' to a convenient identifier.
#'
#' @format A named vector, length 4
#' \describe{
#'   \item{names(discoODAid2name)}{Identifier}
#'   \item{as.vector(discoODAid2name)}{Full names}
#' }
"discoODAid2name"

#' Algorithm Exclusion Matrix
#'
#' A small matrix indicating which algorithms should be excluded given certain
#' experimental designs and data types.
#'
#' @examples
#'
#'
#' # Code used to generate discoODAexclusionMatrix
#'
#' itemNames <- c(
#'  "missing_value",
#'   "with_bio_replicate",
#'   "non_integer_interval",
#'   "uneven_interval",
#'   "circular_t",
#'   "invalidPeriod",
#'   "invalidJTKperiod"
#' )
#'
#' # Creating requirements matrix, first assuming all methods are valid
#' # Then appying exclusion criteria of MetaCycle plus CS criteria
#' mat <- matrix(TRUE, nrow = 4, ncol = length(itemNames))
#' rownames(mat) <- c("CS", "JTK", "LS", "ARS")
#' colnames(mat) <- itemNames
#'
#' # Exclusion criteria from MetaCycle v1.1, i.e. can algorithm handle XXX
#' mat[c("ARS", "JTK"), c("non_integer_interval", "uneven_interval")] <- FALSE
#' mat["ARS", "with_bio_replicate"] <- FALSE
#' mat["ARS", "missing_value"] <- FALSE
#' mat["JTK", "invalidJTKperiod"] <- FALSE
#'
#' # Additional exclusion criteria
#' mat["ARS", "circular_t"] <- FALSE
#' mat["CS", "missing_value"] <- FALSE
#' mat[c("CS", "JTK", "ARS", "LS"), "invalidPeriod"] <- FALSE
#'
#' discoODAexclusionMatrix <- mat
#'
"discoODAexclusionMatrix"

#' Read in the DiscoRhythm Simulated dataset
#'
#' A convenience function to get the simulated circadian transcriptomic
#' system data file used
#' in DiscoRhythm for various demonstrations and tests.
#'
#' @param as_se logical, indicates if example data should be returned as a
#' SummarizedExperiment or data.frame.
#'
#' @return The simulated demo dataset used in the DiscoRhythm
#' web application as a data.frame or SummarizedExperiment.
#'
#' @export
#'
#' @examples
#'
#' indata <- discoGetSimu()
#'
discoGetSimu <- function(as_se=FALSE){
    indata <- utils::read.csv(system.file("extdata",
                        "Simphony_Example.csv",
                        package = "DiscoRhythm", mustWork = TRUE))
    if(as_se) indata <- discoDFtoSE(indata)
    return(indata)
}
