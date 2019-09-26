#' Cosinor
#'
#' Fixed period cosinor ("harmonic regression") on each row of a matrix
#'
#' Fits a cosinor model to each row of a matrix.
#'
#' @param x    - numeric data matrix
#' @param zts  - numeric vector of length ncol(data) representing time points
#'               for each data column
#' @param per  - period of oscillations (default=24)
#'
#' @return data frame with the following estimated statistics:
#' \itemize{
#'         \item acrophase - acrophases
#'         \item amplitude - amplitudes
#'         \item Rsq - r-squared values
#'         \item pvalue - p-values
#'         \item mesor - intercept coefficient
#'         \item sincoef - sine coefficient
#'         \item coscoef - cosine coefficient
#'}
#' @examples
#' \dontrun{
#' tmpData <- matrix(rnorm(24 * 1000), ncol = 24)
#' tmpData[sample(length(tmpData), nrow(tmpData))] <- NA
#' lmCSmat(tmpData, 1:24, 24)
#' }
#' @author Karolis Koncevičius
#'
#' @keywords internal
lmCSmat <- function(x, zts, per = 24) {
  Y <- as.matrix(x)

  res <- lmCSmatNoNA(Y[NULL,], zts, per)
  res[1:nrow(Y),] <- NA

  obsinds  <- t(!is.na(Y))
  obstypes <- unique(obsinds, MARGIN = 2)
  obstypes <- obstypes[,colSums(obstypes) > 2, drop=FALSE]

  for(i in seq_len(ncol(obstypes))) {
    inds <- colMeans(obsinds == obstypes[,i]) == 1
    res[inds,] <- lmCSmatNoNA(Y[inds,obstypes[,i],drop=FALSE], zts[obstypes[,i]], per)
  }

  res
}

#' Cosinor Without NA Values
#'
#' Fixed period cosinor on each row of a matrix with no missing values.
#'
#' Fits a cosinor model to each row of a matrix that has no NA values.
#'
#' @param x    - numeric data matrix
#' @param zts  - numeric vector of length ncol(data) representing time points
#'               for each data column
#' @param per  - period of oscillations (default=24)
#'
#' @return data frame with the following estimated statistics:
#' \itemize{
#'         \item acrophase - acrophases
#'         \item amplitude - amplitudes
#'         \item Rsq - r-squared values
#'         \item pvalue - p-values
#'         \item mesor - intercept coefficient
#'         \item sincoef - sine coefficient
#'         \item coscoef - cosine coefficient
#'}
#' @examples
#' \dontrun{
#' tmpData <- matrix(rnorm(24 * 1000), ncol = 24)
#' lmCSmat(tmpData, 1:24, 24)
#' }
#' @author Karolis Koncevičius
#'
#' @keywords internal
lmCSmatNoNA <- function(x, zts, per = 24) {
    Y <- as.matrix(x)

    x1 <- sin(2 * pi * zts / per)
    x2 <- cos(2 * pi * zts / per)
    x0 <- rep(1, dim(Y)[2])
    X <- cbind(x0, x1, x2)

    betas <- solve(t(X) %*% X) %*% t(X) %*% t(Y)

    acrophases <- sincos2acr(betas[2, ], betas[3, ], per)
    amplitudes <- sincos2amp(betas[2, ], betas[3, ])

    fits <- t(X %*% betas)

    SStot <- rowSums((Y - rowMeans(Y))^2)
    SSres <- rowSums((fits - Y)^2)
    Rsqs <- 1 - (SSres / SStot)

    SSmod <- SStot - SSres
    DFres <- ncol(x) - 3
    DFmod <- 2
    MSres <- SSres / DFres
    MSmod <- SSmod / DFmod
    Fstatistic <- MSmod / MSres

    pval <- stats::pf(Fstatistic, DFmod, DFres, lower.tail = FALSE)

    data.frame(
        acrophase = acrophases, amplitude = amplitudes, Rsq = Rsqs,
        pvalue = pval,
        mesor = betas[1, ], sincoef = betas[2, ], coscoef = betas[3, ]
        )
}

#' Extract key values from stats::fisher.test results
#'
#' Set p-values of 0 to < 2.2e-16 and reformat odds ratio using formatC
#'
#' @keywords internal
#' @return modified output of \link[stats]{fisher.test}
fisherExact <- function(var1, var2) {
    res <- stats::fisher.test(var1,var2)
    if (res["p.value"] == 0) {
        res["p.value"] <- "< 2.2e-16"
    } else {
        res["p.value"] <- formatC(as.numeric(unlist(res["p.value"])))
    }
    res$estimate["odds ratio"] <- formatC(unlist(res$estimate["odds ratio"]))
    res
}
