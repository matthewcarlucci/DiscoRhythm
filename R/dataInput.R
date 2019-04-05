
#' Import Data for DiscoRhythm Analysis
#'
#' Performs various checks and cleaning operations on the input data.
#'
#' Rows containing NA's or all constant values are removed.
#' If matrix values are character it will be attempted to convert them
#' to numeric.
#' If input is not a data.frame it will be converted using
#' \code{as.data.frame()}.
#'
#' @inheritParams discoInterCorOutliers
#'
#' @return data.frame in Maindata format (checked for errors and
#' modified as needed)
#'
#' @export
#'
#' @examples
#' Maindata <- discoGetSimu()
#' Maindata_clean <- discoCheckInput(Maindata)
#'
discoCheckInput <- function(Maindata) {

    # Check input is a data.frame
    if (!methods::is(Maindata,"data.frame")) {
        warning("Input data is not a data.frame, attempting to convert")
        Maindata <- as.data.frame(Maindata)
    }

    # Check enough data is available for analysis
    if (ncol(Maindata) <= 3) {
        stop("More than 3 samples are needed to perform analysis")
    }

    ## Remove constant rows or rows with NAs
    # Flag rows with NAs
    rowToKeep <- apply(Maindata[, -1], 1, function(x)
        !any(is.na(as.numeric(x))))
    # Flag rows with constant values
    rowToKeep[rowToKeep] <- !apply(
        Maindata[rowToKeep, -1], 1,
        function(x) max(as.numeric(x)) == min(as.numeric(x))
        )
    if (sum(!rowToKeep) != 0) {
        Maindata <- Maindata[rowToKeep, ]
        warning(
            paste0("Deleted ", sum(!rowToKeep),
                " rows since they were constant across samples ",
                "or contained missing values"))
    }

    # If values are not read as numeric coerce to numeric
    if (!all(vapply(Maindata[, -1], function(x) is.numeric(x), logical(1)))) {
        warning("Data was not read as numeric, attempting to coerce to numeric")
        colNames <- colnames(Maindata)
        Maindata <- cbind(Maindata[, 1],
            data.frame(lapply(Maindata[, -1], function(x) as.numeric(x))))
        colnames(Maindata) <- colNames
    }

    return(Maindata)
}

#' Summarize the experimental design
#'
#' Using sample times and biological sample Ids, constructs a summary
#' table of the number of total samples at each timepoint and additionally
#' summarizes the number of replicates for each biological sample.
#'
#' @inheritParams discoInterCorOutliers
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
#' Metadata <- discoParseMeta(colnames(discoGetSimu())[-1])
#' discoDesignSummary(Metadata)
#'
discoDesignSummary <- function(Metadata) {
        bioRep <- data.frame(ID=paste0("Biological Sample ",
                                        Metadata$ReplicateID),
                            ZT=Metadata$Time,
                            Rep=Metadata$ReplicateID)
        bioRep <- bioRep[order(bioRep$ZT,bioRep$Rep),]
        bioRep <- reshape2::melt(table(bioRep$ID, bioRep$ZT))
        bioRep <- bioRep[bioRep$value != 0,]
        tmp <- matrix(nrow = nrow(bioRep),ncol = length(unique(bioRep$Var2)),
                    dimnames = list(rep("Samples", nrow(bioRep)),
                                    unique(bioRep$Var2)))
        for (i in unique(bioRep$Var2)) {
            ids <- paste0(bioRep[bioRep$Var2==i,]$Var1,
                            " (",bioRep[bioRep$Var2==i,]$value,")")
            tmp[seq_along(ids)+1,as.character(i)] <- ids
            tmp[1,as.character(i)] <- sum(bioRep[bioRep$Var2==i,]$value)
        }
        rownames(tmp)[1] <- "Total"
        rownames(tmp)[-1] <- ""
        keep <- max(apply(tmp, 2, function(X) sum(!is.na(X))))
        final <- tmp[seq_len(keep),]
        final[is.na(final)] <- ""
    return(final)
}
