#' Quality Control for DiscoRhythm
#'
#' Functions for executing outlier detection and row filtering procedures
#' prior to rhythmicity analysis.
#'
#' @name discoQC
NULL

#' Perform PCA
#'
#' Calculates PCA results from \code{prcomp} with error handling and outputs
#' suitable for the DiscoRhythm workflow.
#'
#' @param npcs numeric, maximum number of principal components to return.
#'
#' @inheritParams discoPCAoutliers
#'
#' @return output from \link[stats]{prcomp} with an added table summary
#'
#' @export
#'
#' @examples
#' se <- discoGetSimu(TRUE)
#' pca <- discoPCA(se)
#'
discoPCA <- function(se, scale = TRUE, npcs = 10) {
    # Unit checks
    foo <- dim(se)
    if (foo[2] < 2) {
        stop("Not enough samples for PCA.")
    }
    if (foo[1] < 2) {
        stop("Not enough rows for PCA.")
    }
    if (!methods::is(se, "SummarizedExperiment")) {
        stop("Input must be a SummarizedExperiment.")
    }
    if (any(c(!is.numeric(npcs), length(npcs) != 1, npcs <= 0))) {
        stop("Number of princimap components should be single positive numeric value.")
    }


    dat <- assay(se)
    
    res <- stats::prcomp(t(dat),
                center = TRUE,
                scale. = scale,
                rank. = npcs
                )

    npcs <- min(npcs, ncol(res$x))
    x <- t(summary(res)$importance[, seq_len(npcs)])
    x <- data.frame(PC = rownames(x), x)
    res$table <- x
    rownames(res$x) <- se$ID
    
    return(res)
}

#' Internal function for applying SD cutoff to PCA results
#' Returns a logical indicating which samples are not outliers
#' @keywords internal
#' @return logical indicating which samples are outliers in PCA
discoPCAgetOutliers <- function(x, SDfactor = 3, pcToCut = seq_len(4)) {
    sdVec <- matrixStats::rowSds(t(x[, pcToCut, drop = FALSE])) * SDfactor
    meanVec <- colMeans(x[, pcToCut, drop = FALSE])
    meanMat <- matrix(rep(meanVec, nrow(x)), nrow = nrow(x), byrow = TRUE)
    sdMat <- matrix(rep(sdVec, nrow(x)), nrow = nrow(x), byrow = TRUE)
    isoutlier <- apply(abs(x[, pcToCut,
                            drop = FALSE] - meanMat) > sdMat, 1, any)
    return(!isoutlier)
}

#' @rdname discoQC
#'
#' @inheritParams discoInterCorOutliers
#' @param scale logical, whether or not to scale the data prior to PCA, see
#' \link[stats]{prcomp} for more details.
#' @param pcToCut numeric, which PCs to use for outlier detection. It is
#' recomended to select the first X PCs based on which PCs explain a
#' significant amount of variance in the data.
#'
#' @return list containing PCA results and the detected outliers
#'
#' @export
#'
#' @examples
#' se <- discoGetSimu(TRUE)
#' PCAres <- discoPCAoutliers(se)
#'
discoPCAoutliers <- function(se, threshold = 3,
    scale = TRUE, pcToCut = seq_len(4)) {
    if(!methods::is(se,"SummarizedExperiment")){
        stop("Input must be a SummarizedExperiment.")
    }
    
    res <- discoPCA(se, scale = scale)
    res$outliers <- !discoPCAgetOutliers(res$x, threshold, pcToCut)
    res
}

#' @rdname discoQC
#'
#' @param se SummarizedExperiment, the main data object used by DiscoRhythm 
#' expected to contain se$ID, se$ReplicateID, se$Time sample metadata and
#' non-null rownames. See the vignette for more details.
#' @param cor_method character, method of pairwise correlation
#' (see \link[stats]{cor}'s "method" argument for all options).
#' @param threshold numeric, a threshold determining which samples are
#' outliers (for discoInterCorOutliers, in units of thresh_type, for
#' discoPCAoutliers in units of standard deviations).
#' @param thresh_type character indicating threshold type (either standard
#' deviations below the mean, or an absolution correlation value). One of:
#' "sd" or "value".
#'
#' @return A list of 3 objects:
#' 1) outliers - named logical indicating if the sample is an outlier
#' 2) meanCor - mean of all pairwise correlations for a given sample
#' 3) corMat - Matrix of all pairwise correlation values
#'
#' @export
#'
#' @examples
#' CorRes <- discoInterCorOutliers(se)
#'
discoInterCorOutliers <- function(se,
                                    cor_method = c("pearson",
                                                   "kendall",
                                                   "spearman"),
                                    threshold = 3,
                                    thresh_type = c("sd","value")) {

    cor_method = match.arg(cor_method)
    thresh_type = match.arg(thresh_type)
    
    if(!methods::is(se,"SummarizedExperiment")){
        stop("Input must be a SummarizedExperiment.")
    }
    
    dat <- assay(se)

    mat <- stats::cor(dat, method = cor_method)

    # Remove Diagonal (all 1's) to avoid inflated correlation
    corMat <- mat
    diag(corMat) <- NA

    meanCor <- rowMeans(corMat, na.rm = TRUE)

    if (thresh_type == "sd") {
        cutval <- (mean(meanCor) - threshold * stats::sd(meanCor))
    } else if (thresh_type == "value") {
        cutval <- threshold
    }
    outliers <- meanCor <= cutval

    res <- list(outliers = outliers,
                meanCor = meanCor,
                corMat = corMat, 
                threshold = cutval)
    res
}

################## Row Replicate Analysis #######################

# Average technical replicates
#' @keywords internal
averageTech <- function(se, method = c("Median","Mean","Random","None")) {
    
    method <- match.arg(method)
    
    if(!methods::is(se,"SummarizedExperiment")){
        stop("Input must be a SummarizedExperiment.")
    }
    
    bioID <- paste0(se$Time,"_",se$ReplicateID)
    mat <- assay(se)
    
    # Data matrix
    res <- lapply(unique(bioID), function(I) {
        sidx <- which(bioID == I)
        if (length(sidx) >= 2) {
        if (method == "Median") {
            tmp <- as.data.frame(matrixStats::rowMedians(mat[, sidx]))
        } else if (method == "Mean") {
            tmp <- as.data.frame(matrixStats::rowMeans2(mat[, sidx]))
            } else if (method == "Random") {
                tmp <- as.data.frame((mat[, sample(sidx, 1)]))
            } else if (method == "None") {
                tmp <- as.data.frame(mat[, sidx])
            }
        } else {
            tmp <- as.data.frame(mat[, sidx])
        }
        
        # Following the original input data naming scheme:
        # <prefix><time>_<unique_id>_<replicate_id>
        # so output can be used as shiny input
        if (method != "None") {
            colnames(tmp) <- paste(se$Time[sidx - 1], "A",
                se$ReplicateID[sidx - 1], sep = "_")[1]
        } else {
            colnames(tmp) <- paste(se$Time[sidx - 1], "A",
                se$ReplicateID[sidx - 1], sep = "_")
        }
        tmp
    })

    res2 <- do.call(data.frame, res)
    regDat <- data.frame("ID" = as.character(rownames(se)), res2)

    # Metadata
    res <- lapply(unique(bioID), function(I) {
        sidx <- which(bioID == I)
        if (length(sidx) >= 2) {
            if (method != "None") {
                tmp <- colData(se)[sidx, ][1, ]
            } else {
                tmp <- colData(se)[sidx, ]
            }
        } else {
            tmp <- colData(se)[sidx, ]
        }
        tmp
    })

    res2 <- do.call(rbind, res)
    regMet <- res2[, -1]

    outse <- discoDFtoSE(regDat,regMet)
    
    return(list("se" = outse))
}

#' @rdname discoQC
#'
#' @inheritParams discoBatch
#' @inheritParams discoInterCorOutliers
#'
#' @export
#'
#' @examples
#' ANOVAres <- discoRepAnalysis(se)
#'
discoRepAnalysis <- function(se,
                                aov_method = c("Equal Variance","Welch","None"),
                                aov_pcut = 0.05,
                                aov_Fcut = 0,
                                avg_method = c("Median","Mean","Random","None")
) {
    
    aov_method=match.arg(aov_method)
    avg_method=match.arg(avg_method)
    
    if(!methods::is(se,"SummarizedExperiment")){
        stop("Input must be a SummarizedExperiment.")
    }
    
    Metadata <- colData(se)
    mat <- assay(se)
    
    bioID <- paste0(se$Time,"_",se$ReplicateID)

    noAOV <- !(aov_method %in% c("Welch","Equal Variance"))
    norep <- !any(duplicated(bioID)) & !noAOV
    if(norep) warning("No replicates present for discoRepAnalysis,
        can't perform ANOVA")
    # Return the same for aov_method="None" and for
    # the case of no replicates
    if(norep | noAOV){
        nr <- nrow(se)
        allStats <- list(statistic = rep(100, nr),
            pvalue = rep(0, nrow(se)),
            df.between = rep(0, nr),
            df.within = rep(0, nr),
            estimate = rep(0, nr))
    } else if (aov_method == "Welch") {
        allStats <- matrixTests::row_oneway_welch(mat, bioID)
    } else if (aov_method == "Equal Variance") {
        allStats <- matrixTests::row_oneway_equalvar(mat, bioID)
    }
    idx <- (allStats$pvalue <= aov_pcut) & (allStats$statistic > aov_Fcut)
    res <- averageTech(se[idx, ], avg_method)
    res$aovP <- allStats$pvalue
    res$allStats <- allStats
    
    return(res)
}
