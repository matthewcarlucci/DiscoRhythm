
#' Import Data for DiscoRhythm Analysis
#'
#' Performs various checks and cleaning operations on the input data.
#'
#' Rows containing NA's or all constant values are removed.
#' If matrix values are character it will be attempted to convert them
#' to numeric.
#' If input is not a matrix it will be converted using
#' \code{as.matrix()}.
#' User will be warned if row IDs contain duplicate entries.
#'
#' @inheritParams discoInterCorOutliers
#' @param n_min_sample numeric value specifying minimal number of samples
#' needed to perform analysis.
#'
#' @return SummarizedExperiment checked for errors and
#' modified as needed
#'
#' @export
#'
#' @examples
#' se <- discoGetSimu(TRUE)
#' se_clean <- discoCheckInput(se)
#'
#' @importFrom SummarizedExperiment SummarizedExperiment colData
discoCheckInput <- function(se, n_min_sample = 3) {
    
    if (!methods::is(se,"SummarizedExperiment")) {
        stop("Input must be a SummarizedExperiment.")
    }
    
    dat <- assay(se)

    # Check enough data is available for analysis
    if (ncol(se) <= n_min_sample) {
        stop(paste(
            "More than", n_min_sample,
            "samples are needed to perform analysis."
        ))
    }
    
    if (anyDuplicated(rownames(se))) {
        warning("Please consider deduplicating row IDs before continuing.")
    }

    # If values are not read as numeric coerce to numeric
    if (!all(vapply(dat, function(x) is.numeric(x), logical(1)))) {
        warning("Data was not read as numeric, attempting to coerce to numeric")
        colNames <- colnames(dat)
        dat <- as.matrix(data.frame(lapply(dat, function(x) as.numeric(x))))
        colnames(dat) <- colNames
    }
    
    # Remove constant rows or rows with NAs
    # Flag rows with NAs
    rowToKeep <- apply(dat, 1, function(x) {!any(is.na(x))})
    # Flag rows with constant values
    rowToKeep[rowToKeep] <- apply(
        dat[rowToKeep, ], 1,
        function(x) {!(max(x) == min(x))}
    )
    if (sum(!rowToKeep) != 0) {
        dat <- dat[rowToKeep, ]
        warning(paste(
            "Deleted", sum(!rowToKeep),
            "rows since they were constant across samples or",
            "contained missing values."
        ))
    }
    if (nrow(dat) == 0) {
        stop("Deleted all rows, no result to return.")
    }
    
    ret <- SummarizedExperiment(assays = dat,
                                colData = colData(se)
    )
    
    return(ret)
}

#' Summarize the experimental design
#'
#' Using sample times and biological sample Ids, constructs a summary
#' table of the number of total samples at each timepoint and additionally
#' summarizes the number of replicates for each biological sample.
#'
#' @inheritParams discoInterCorOutliers
#' @inheritParams discoDFtoSE
#'
#' @seealso discoParseMeta
#'
#' @export
#'
#' @return A table where the first row summarizes the number of datapoints for
#' each timepoint and other cells indicate the number of technical replicates
#' for a given biological sample.
#'
#' @examples
#' # import example data
#' Metadata <- SummarizedExperiment::colData(discoGetSimu(TRUE))
#' # Summarize the experiment design
#' discoDesignSummary(Metadata)
#'
discoDesignSummary <- function(Metadata) {
    bioRep <- data.frame(
        ID = paste("Biological Sample", Metadata$ReplicateID),
        ZT = Metadata$Time,
        Rep = Metadata$ReplicateID
    )
    bioRep <- bioRep[order(bioRep$ZT,bioRep$Rep), ]
    bioRep <- reshape2::melt(table(bioRep$ID, bioRep$ZT))
    bioRep <- bioRep[bioRep$value != 0,]
    tmp <- matrix(
        nrow = nrow(bioRep),
        ncol = length(unique(bioRep$Var2)),
        dimnames = list(rep("Samples", nrow(bioRep)), unique(bioRep$Var2))
    )
    for (i in unique(bioRep$Var2)) {
        ids <- paste0(
            bioRep[bioRep$Var2 == i, ]$Var1,
            " (", bioRep[bioRep$Var2 == i, ]$value, ")"
        )
        tmp[seq_along(ids) + 1, as.character(i)] <- ids
        tmp[1, as.character(i)] <- sum(bioRep[bioRep$Var2 == i, ]$value)
    }
    rownames(tmp)[1] <- "Total"
    rownames(tmp)[-1] <- ""
    keep <- max(apply(tmp, 2, function(X) sum(!is.na(X))))
    final <- tmp[seq_len(keep), ]
    final[is.na(final)] <- ""
    return(final)
}
