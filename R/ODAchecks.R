#' Get Valid Oscillation Detection Algorithms
#'
#' @rdname discoODAs
#'
#' @inheritParams discoODAs
#'
#' @keywords internal
#'
#'
# Wrapper for detectDesign(), checkPeriod(), checkODAs()
# Given the input data, models of interest and period of interest,
#  output which models are valid
# Used by discoODAs

discoGetODAs <- function(se,method=NULL, period, circular_t=FALSE) {

    # Gather necessary info to determine valid ODA methods
    design <- inferOscDesign(se)
    invalidPeriod <- checkPeriod(se$Time, period)

    if ("JTK" %in% method | is.null(method)) {
        invalidJTKperiod <- checkJTKperiod(se$Time, period)
    } else {
    # Value doesn't matter in this case
        invalidJTKperiod <- TRUE
    }

    validModels <- checkODAs(
        infer_design = design,
        circular_t,
        invalidPeriod = invalidPeriod,
        invalidJTKperiod = invalidJTKperiod
        )

    # Use all valid methods if none are specified
    # Filter for only valid methods otherwise
    if (is.null(method)) {
        outmethods <- validModels
    } else {
        if (!all(method %in% validModels) | is.null(method)) {
            warning("Not all selected models are valid,
                    selecting valid methods only")
            outmethods <- method[method %in% validModels]
        } else{
            outmethods <- method
        }
    }

    if(length(outmethods)==0){
        warning("No methods returned from getDiscoRhythmODAs")
    }
    return(outmethods)
}


#' DiscoRhythm Experimental Design
#'
#' Infers the experimental design from various input data
#'
#' Characteristics of the experiment sampling are gathered to determine which
#' oscillation deteciton algorithms are suitable.
#'
#' @inheritParams discoInterCorOutliers
#' @keywords internal
#' @return list with inferred experimental design features needed to perform
#' replicate analysis and merging in discoRepAnalysis.

inferFilteredDesign <- function(se) {
    res <- list()

    mat <- assay(se)
    res$missing_value <- (any(is.na(mat))) | any(is.nan(as.matrix(mat)))

    res$with_tech_replicate <- any(duplicated(paste(se$Time,
                                                    se$ReplicateID)))

    return(res)
}

#' @keywords internal
# Infer experimental design of se
inferOscDesign <- function(se) {

    if(!methods::is(se,"SummarizedExperiment")){
        stop("Input must be a SummarizedExperiment.")
    }
    
    # Setting MetaCycle variables
    EXPM <- assay(se)
    timepoints <- sort(se$Time)
    uni_timepoints <- unique(timepoints)

    ###### Code chunk copied verbatim from MetaCycle v1.1
    #### meta2d() in R/meta2dMainF.R
    #### extract key features of input data, including:
    #### with/without non-integer interval,
    #### even/uneven sampling,
    #### with/without missing values,
    #### with/without replicates
    MISSING_VALUE <- WITH_REPLICATE <- FALSE
    non_integerInterval <- uneven_interval <- FALSE
    if (!all(round(diff(uni_timepoints)) == diff(uni_timepoints))) {
        non_integerInterval <- TRUE
    }
    if (length(unique(diff(uni_timepoints))) > 1) {
        uneven_interval <- TRUE
    }
    if ((!all(!is.na(EXPM))) | (!all(!is.nan(EXPM)))) {
        MISSING_VALUE <- TRUE
    }
    if (length(timepoints) != length(uni_timepoints)) {
        WITH_REPLICATE <- TRUE
    }

    ################################################

    res <- list()
    res$missing_value <- MISSING_VALUE
    res$with_replicate <- WITH_REPLICATE
    res$non_integer_interval <- non_integerInterval
    res$uneven_interval <- uneven_interval

    return(res)
}

# Given the experimental design, return valid algorithms
#' @keywords internal
#' @seealso discoODAexclusionMatrix
checkODAs <- function(infer_design, circular_t,
    invalidPeriod, invalidJTKperiod,
    output = "methods") {
    crit <- as.logical(
        c(unlist(infer_design), circular_t, invalidPeriod, invalidJTKperiod)
        )

    # When checkODAs is evaluated too early in shiny, crit will be empty
    if (length(crit) != ncol(DiscoRhythm::discoODAexclusionMatrix)) {
        warning("Experimental design vector is not the correct length")
        return(NULL)
    }

    names(crit) <- colnames(DiscoRhythm::discoODAexclusionMatrix)

    # Possible outputs:
    # 1) Exclusion criteria present in the dataset
    # 2) Valid algorithm names (default)
    if (output != "methods") {
        return(crit)
    } else {
        return(names(which(apply(
            DiscoRhythm::discoODAexclusionMatrix[, crit, drop = FALSE],
            1, all))))
    }
}

#' Validate Detection Period
#'
#' @param time numeric vector of sample collection times.
#' @param period hypothesized period.
#'
#' @keywords internal
#' @return logical indicating whether the period is suitable for testing given
#' the sampling times of the dataset.
# Given sample collection times check ability to test the period of interest
# FALSE indicates no algorithms can test this period
# Conditions are: There must be 3 unique time%%period values
# Conditions for specific algorithms are evaluated in separate functions
checkPeriod <- function(time, period) {
    if (length(unique(time %% period)) <= 2) {
        warning(c("Sample times modulo period must have ",
            "at least 3 unique values to continue"))
        invalidPeriod <- TRUE
    } else {
        invalidPeriod <- FALSE
    }

    return(invalidPeriod)
}


#' Validate Detection Period for JTK Cycle
#'
#' @keywords internal
#' @return logical stating whether the period is appropriate for JTK Cycle
#' for this dataset.
checkJTKperiod <- function(time, period) {

    # For now period ranges are not used in DiscoRhythm
    minper <- maxper <- period
    releaseNote <- TRUE
    JTKtime <- sort(unique(time))
    invalidJTKperiod <- FALSE

    ##### Code chunk copied verbatim from MetaCycle v1.1 runJTK() in R/JTKv3.1.R
    ## Minor modifications made to errors/warnings
    ## for better compatibility with DiscoRhythm
    uni_JTKtime <- unique(JTKtime)
    freq <- uni_JTKtime[2] - uni_JTKtime[1]
    data_endtime <- length(uni_JTKtime) * freq
    if ((data_endtime >= maxper) & (round(maxper / freq) >= 2)) {
        if (round(minper / freq) >= 2) {
            perTK <- seq(round(minper / freq), round(maxper / freq), by = 1)
        } else {
            perTK <- seq(2, round(maxper / freq), by = 1)
        }
    } else if ((data_endtime < maxper) &
        (data_endtime >= minper) &
        (round(data_endtime / freq) >= 2)) {
        if (round(minper / freq) >= 2) {
            perTK <- seq(round(minper / freq), round(data_endtime / freq),
                        by = 1)
        } else {
            perTK <- seq(2, round(data_endtime / freq), by = 1)
        }
    } else {
        warning(c(
            "The input 'minper' and 'maxper' is out of the range ",
            "that JTK can detect. ",
            "If hope to use JTK for this analysis, please reset the 'minper' ",
            "and 'maxper' between ", 2 * freq, " and ", data_endtime, ".\n"
            ))
        invalidJTKperiod <- TRUE
        return(invalidJTKperiod)
    }
    if ((min(perTK) * freq != minper) & (releaseNote)) {
        warning(c("the input 'minper' is not suitable for JTK,
                it was reset as ",
            min(perTK) * freq, "\n"))
    }
    if ((max(perTK) * freq != maxper) & (releaseNote)) {
        warning(c("the input 'maxper' is not suitable for JTK,
                    it was reset as ",
            max(perTK) * freq, "\n"))
    }

    ############################################################################

    return(invalidJTKperiod)
}


