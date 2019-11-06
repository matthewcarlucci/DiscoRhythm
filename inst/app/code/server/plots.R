##### ggplot Theme #####

sharedtheme <- function() {
    theme_bw() + theme(text = element_text(size = 10))
}

colors <- list(
    "discoMain" = "#FDB813",
    "discoMain2" = "#FA7D2B",
    "discoSec" = "#847dd1",
    "discoSec2" = "#7a74c1",
    "neutral" = "#808285",
    "neutral2" = "#515356",
    "neutral-light" = "#c8cace",
  # "sig"="#BE1E2D",
    "shading" = "#DCDDDE",
    "qualRamp" = rainbow
    )
colors$sig <- colors$discoMain2
colors$outlier <- colors$sig
colors$highval <- colors$discoMain
colors$lowval <- colors$discoSec
# colors$quantRamp <- colorRampPalette(c(colors$lowval,colors$highval))
colors$quantRamp <- viridis::viridis

##### Inter-Sample Correlation #####

plotHeatMCor <- function(mat, k, ...) {
    diag(mat) <- NA
    heatmaply::heatmaply(mat,
        colors = colors$quantRamp(20),
        k_row = k,
        k_col = k,
        ...
        )
}

plotAvgCor <- function(Metadata, meanCor, corCut = 0.95, tUnit = "CT") {
    library(ggplot2)

    set.seed(666)
    df <- as.data.frame(meanCor)
    colnames(df) <- "MeanCor"
    df$ID <- rownames(df)
    df$toRemove <- df$MeanCor < corCut
    df$Time <- Metadata$Time

    tmp <- sort(unique(Metadata$Time))
    tmp <- factor(tmp[seq(1, length(tmp), 2)])
  # make 2 neighbor times different color
    df$colorF <- 3
    df$colorF[df$toRemove == FALSE & df$Time %in% tmp] <- 1
    df$colorF[df$toRemove == FALSE & !df$Time %in% tmp] <- 2

    ggplot(df, aes(Time, MeanCor)) +
    # geom_violin(fill = colors$neutral) +
    geom_jitter(aes(
        color = factor(colorF),
        alpha = toRemove,
        shape = toRemove,
        group = ID
        ),
    size = 2, alpha = 0.8, width = 0.25
    ) +
    labs(
        x = paste0("Sample Time (", tUnit, ")"),
        y = "Average Inter-Sample Correlation"
        ) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4)) +
    scale_alpha_manual(values = c(0.95, 0.3)) +
    scale_color_manual(values = c(colors$neutral, colors$neutral2,
        colors$outlier)) +
    sharedtheme() +
    theme(legend.position = "none") +
    geom_hline(yintercept = corCut, color = colors$outlier)
}


#####  Filtering Summary #####

plotObsDistribution <- function(data, selectedSamples, ylab = "Value") {
    selcol <- colors$discoMain2
    unselcol <- colors$neutral2

    col <- rep(unselcol, ncol(data))
    col[selectedSamples] <- selcol
    boxplot(data, col = col, las = 2, ylab = ylab)
    legend("bottomright",
        inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n",
        legend = c("TRUE", "FALSE"), pch = 15,
        col = c(selcol, unselcol), title = "Selected"
        )
}

#####  PCA #####

plotPCAdists <- function(pca, SDfactor, pcToUse = "", npcs = 10) {
    suppressPackageStartupMessages({library(dplyr)})
    set.seed(1) # For consistent jitter in ggplot

    npcs <- min(npcs, ncol(pca$rotation))

    df <- reshape2::melt(pca$x[, 1:npcs])
    df2 <- df
    df2$usePC = df2$Var2 %in% pcToUse
    df2 <- df2 %>%
        group_by(Var2) %>%
        mutate(SD = sd(value)) %>%
        mutate(outlier = abs(value - mean(value)) >= SD * SDfactor) %>%
        mutate(PCn = as.numeric(substr(Var2, 3, 7)))

    p <- df2 %>%
    ggplot(aes(Var2, value, alpha = usePC)) +
    labs(x = "", y = "PC Score") +
    scale_alpha_manual(values = c(0.5, 1)) +
    geom_hline(yintercept = 0) +
    geom_violin(fill = colors$neutral2)

  # MC: I think this causes errors if not split like this
    if(any(df2$outlier)){
        p <- p + geom_point(
            data = df2 %>% subset(outlier), aes(group = Var1),
            position = position_jitter(width = 0.25), shape = 4, size = 2,
            color = colors$outlier
            )
    }

    p <- p +
    geom_line(data = df2, aes(PCn, SD * SDfactor), linetype = 3,
        color = colors$outlier, alpha = 1) +
    geom_line(data = df2, aes(PCn, -SD * SDfactor), linetype = 3,
        color = colors$outlier, alpha = 1) +
    sharedtheme() +
    theme(legend.position = "none")
    return(p)
}


plotPCAWithShape <- function(x, Metadata, col, pcA = 1, pcB = 2,
    outids = NULL, plotTitle = "") {
    KEY <- rownames(x)
    COLOR <- as.factor(Metadata[which(Metadata$ID %in% KEY), col])
    da <- data.frame(x[, c(as.numeric(pcA), as.numeric(pcB))])
    da$isoutlier <- FALSE
    if (!is.null(outids)) da$isoutlier[outids] <- TRUE
    if (pcA == pcB) warning("X and Y are the same principal component")
    data <- da 
    colnames(data) <- c("A", "B", "isoutlier")

    withcolor <- length(COLOR) != 0

    if (!withcolor) {
        p <- ggplot(data, aes(A, B, key = KEY)) +
        geom_point(aes(shape = isoutlier), size = 2, alpha = 0.8,
            color = colors$neutral)
    } else {
        p <- ggplot(data, aes(A, B, color = COLOR, key = KEY)) +
        geom_point(aes(shape = isoutlier), size = 2, alpha = 0.8)
    }
    p <- p +
    labs(
        x = paste("PC", as.numeric(pcA)),
        y = paste("PC", as.numeric(pcB)),
        color = NULL,
        title = plotTitle
        ) +
    sharedtheme() +
    theme(legend.position = "none") +
    scale_shape_manual(values = c("TRUE"=4,"FALSE"=16))
    if (withcolor) {
        p <- p + scale_color_manual(
          values=colors$qualRamp(length(unique(COLOR))))
    }
    p
}

plotPCAstats <- function(pcaBeforeTable, pcaAfterTable, pcToUse="", npcs=10) {
    usePC <- rep(paste0("PC",seq_len(npcs)),2)

    plotdf <- rbind(
        data.frame(pcaAfterTable, status = "After"),
        data.frame(pcaBeforeTable, status = "Before")
        ) %>%
    reshape2::melt(c("PC", "status")) %>%
    subset(variable == "Proportion.of.Variance") %>%
    mutate(status = factor(status, levels = c("Before", "After"))) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(usePC=usePC %in% pcToUse)

    plotdf %>%
    ggplot(aes(PC, value, fill = status, alpha=usePC)) +
    geom_bar(
        color = "black",
        stat = "identity", position = "dodge"
        ) +
    labs(
        x = "", y = "Proportion of Variance",
        title = "",
        fill = ""
        ) +
    scale_alpha_manual(
        values = c(0.2,1)
        )+
    scale_fill_manual(
        values = c(colors$neutral2, colors$outlier),
        limits = c("Before", "After")
        ) +
    scale_y_continuous(labels = scales::percent) +
    sharedtheme() +
    scale_x_discrete(limits = paste0("PC", seq_len(10)))
}

##### Before Outlier Removal
plotPCAPairs <- function(x,pairsPC) {
    x2 <- x[,pairsPC]
pairs(x2, col=adjustcolor(colors$neutral2, 0.5), pch=20, lwd=0, gap=0, asp=1,
      panel=function(...) {grid(nx=5, ny=5, lty=1, col="grey90");
        points(...)},
      font.labels=2, oma=c(1,1,3,1), xaxt="n", yaxt="n"
      )
}

##### After Outlier Removal
plotPCAPairsAfter <- function(x,pairsPC,outliers) {
    if (sum(outliers) == 0) {
        p <- errorPlot(main = "No Outliers\nTry reducing threshold",
            textcol = "black")
    } else if (sum(!outliers) < 2) {
        p <- errorPlot(
          main = "0 or 1 Samples Remaining\nTry increasing threshold",
            textcol = "black")
    } else {
        if (!all(pairsPC %in% colnames(x))) {
            pairsPC <- pairsPC[pairsPC %in% colnames(x)]
        }
      x2 <- x[,pairsPC]
      p <- pairs(x2, col=adjustcolor(colors$neutral2, 0.5), pch=20, lwd=0,
                 gap=0, asp=1,
      panel=function(...) {grid(nx=5, ny=5, lty=1, col="grey90");
        points(...)},
      font.labels=2, oma=c(1,1,3,1), xaxt="n", yaxt="n"
      )
    }
    p
}

#####  Row Selection #####

ObsVsExpSNR <- function(allStats, anovaFstatCut) {
    perSig <- formatC(mean(allStats$statistic > anovaFstatCut) * 100)
    nSig <- sum(allStats$statistic > anovaFstatCut)
    myTitle <- paste0("Signal to Noise ratio, ", perSig,
        "% Remaining (n=", nSig, ")")
    tmpdf <- data.frame(statistic = stats::rf(1e3, allStats$df.between[1],
        allStats$df.within[1]))
    plyr::rbind.fill(
        data.frame(tmpdf, Distribution = "Theoretical"),
        data.frame(allStats, Distribution = "Observed")
        ) %>%
    mutate(Distribution = factor(Distribution,
        levels = c("Observed", "Theoretical"))) %>%
    ggplot(aes(Distribution, log10(statistic), fill = Distribution)) +
    sharedtheme() +
    guides(fill = FALSE) +
    ggtitle(myTitle) +
    geom_violin(alpha = 0.7, color = colors$neutral2) +
    geom_hline(yintercept = log10(anovaFstatCut)) +
    scale_fill_manual(values = c("Observed" = colors$sig,
        "Theoretical" = colors$neutral))
}

##### Period Detection #####

plotPeriodDetect <- function(dat, tUnit = "hours") {
    dat %>%
    ggplot(aes(as.factor(Var2), value)) +
    sharedtheme() +
    ylab("Cosinor R-squared") +
    geom_boxplot(fill = colors$`neutral-light`,
        outlier.colour = colors$neutral2) +
    xlab("Cosinor Period")
}

plotOVpcaScatter <- function(OVpca, regressionMeta, OVperiodSelect,
    tUnit = "CT", PCsToUse=NULL) {
    time <- regressionMeta$Time
    
    if(is.null(PCsToUse)){
      PCsToUse <- ifelse(ncol(OVpca$x) > 10, 1:10, 1:ncol(OVpca$x))  
    } else{
      PCsToUse <- as.numeric(substr(PCsToUse,3,20))
    }
    
    per <- as.numeric(OVperiodSelect)

  # Shade by fit period/2
  # rects <- data.frame(xmin = per / 2, xmax = per, ymin = -Inf, ymax = Inf)

  # Make plotting data.frame
    plotdf <- reshape2::melt(data.frame("time"=regressionMeta$Time,
        OVpca$x[,PCsToUse]),id.var="time")
    colnames(plotdf) <- c("time","PC","value")
    plotdf$nPC <- as.numeric(gsub("PC","",plotdf$PC))
    plotdf$PC <- reorder(plotdf$PC, plotdf$nPC)

    p <- ggplot(plotdf,aes(time, value)) +
    # geom_rect(data = rects, aes(xmin = xmin, ymin = ymin, xmax = xmax,
    #     ymax = ymax),inherit.aes = FALSE, alpha = 0.2) +
    sharedtheme() +
    geom_point(color = colors$neutral) +
    labs(x = sprintf("Time (%s)",tUnit), y = "PC Score") +
    geom_smooth(method = "lm",
        formula = y ~ sin(x / per * 2 * pi) + cos(x / per * 2 * pi),
        color = colors$neutral2) +
    facet_wrap(~PC)

    return(p)
}

#####  Oscillation Detection #####

plotPvalues <- function(pv, nBin = 80, cutSignificant = 0.05, title = "") {
  perSig <- formatC(mean(pv < cutSignificant) * 100)
    nSig <- formatC(sum(pv < cutSignificant))
    p <- ggplot(data.frame(pv), aes(pv)) +
    geom_histogram(color = colors$neutral2, fill = colors$neutral,
        breaks = 0:nBin / nBin) +
    sharedtheme() +
    labs(
        x = "p-value", y = "Number of Observations",
        title = paste0(title, ", \n", perSig, "% Significant (", formatC(nSig),
                       "/",formatC(length(pv)), ")")
        ) +
    geom_vline(aes(xintercept = as.numeric(cutSignificant)),
        colour = colors$sig)
    p
}
plotPQValueHist <- function(discoODAres, bg, nbreaks, cutoff, id2name) {
    plist <- list()
    for (mcod in names(discoODAres)) {
        plist[[mcod]] <- plotPvalues(
            discoODAres[[mcod]]$pvalue[bg],
            nbreaks,
            cutoff,
            id2name[mcod]
            )
    }
  # arrange plots
    plist[["ncol"]] <- length(discoODAres)
    do.call(gridExtra::grid.arrange, plist)
}

plotAcroHist <- function(vec, binNum, name = "", rose = TRUE, per = 24) {
    numbins <- as.numeric(binNum)
    if (!rose) {
    # convert vector to dataframe
        vec <- data.frame(acrophase = vec)
        p <- ggplot(vec, aes(x = acrophase)) +
        geom_histogram(data = vec, position = "identity", bins = numbins,
            color = colors$neutral2, fill = colors$neutral) +
        scale_x_discrete("", limits = c(0, per)) +
        sharedtheme()
    } else {
    
    # value for centering the bin intervals (i.e. half bin width)  
    offset <- per / numbins / 2
    
    # round acro to nearest bin
        tmp <- floor((vec%%per) / per * numbins) / numbins * per
        cnts <- table(tmp)
        plotdf <- data.frame(
            "acrophase" = as.numeric(names(cnts)),
            "Count" = as.numeric(cnts)
            )
        
    # fill in zeros
    poss_vals <- seq(numbins)/numbins*per-offset*2
    zero_idx <- which(!(poss_vals %in% plotdf$acrophase))
    plotdf <- rbind(plotdf,data.frame("acrophase"=poss_vals[zero_idx],
                                      "Count"=0))

    # find limits and labels for the y axis
        ymax <- max(cnts)
        int <- pretty(c(0, ymax))[2]
        breaks <- if (ymax %/% int > 5) {
            seq(int * 2, ymax, int * 2)
        } else {
            seq(int, ymax, int)
        }
        labels <- formatC(breaks, format = "d", big.mark = ",")
        mcnt <- sum(cnts) / numbins

    # draw
        p <-
          ggplot() +
        geom_rect(data = data.frame(1), ymin = mcnt, ymax = Inf, xmin = 0,
            xmax = per, fill = colors$discoMain, alpha = 0.2) +
        geom_rect(data = data.frame(1), ymin = 0, ymax = mcnt, xmin = 0,
            xmax = per, fill = colors$discoSec, alpha = 0.2) +
        geom_bar(data = plotdf, aes(x = acrophase + offset, y = Count),
            stat = "identity", colour = colors$neutral2,
            position=position_identity(),
            fill = colors$neutral,width = offset) +
        geom_hline(yintercept = c(0, breaks), colour = colors$neutral,
            size = 0.2) +
        scale_x_continuous(name = "", limits = c(0, per),
            breaks = seq(0, per, length.out = 5)) +
        scale_y_continuous(name = "Count", limits = c(-ymax * 0.5, NA),
            breaks = c(0, as.numeric(labels))) +
        annotate("text", x = 0, y = -ymax * 0.5, label = "Acrophase",
            size = 2.5) +
        sharedtheme() +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            aspect.ratio = 1
            ) +
        coord_polar()
    }
    p + ggtitle(name)
}
plotAllAcroHist <- function(discoODAres, fg, nbreaks, per,
    asrose = TRUE, id2name) {
    acrolist <- list()
    for (mcod in names(discoODAres)) {
        id <- fg[[mcod]]
        thismethod <- id2name[mcod]
        thisphase <- discoODAres[[mcod]]$acrophase[id]
        if (length(thisphase) > 1) {
            acrolist[[mcod]] <- plotAcroHist(
                thisphase,
                nbreaks,
                thismethod,
                asrose,
                per
                )
        } else {
      # error plot
            acrolist[[mcod]] <- errorPlot("Not Enough Significant Data") +
            ggtitle(thismethod)
        }
    }
    acrolist[["ncol"]] <- length(discoODAres)
    do.call(gridExtra::grid.arrange, acrolist)
}


plotAmpliHist <- function(discoODAres, bg, fg, binNum, obsUnit = "") {

  if(is.null(discoODAres[["CS"]])){
   return(errorPlot(main = "No Cosinor Results\n
                    No Amplitudes Available"))
  }

    tmp <- data.frame(
        amplitude = discoODAres[["CS"]]$amplitude,
        SigOrNot = "Not Significant", stringsAsFactors = FALSE
        )

    if (length(fg) > 0) {
        tmp$SigOrNot[fg] <- "Significant"
        vec <- tmp[bg, ]
        sigMean <- mean(vec$amplitude[vec$SigOrNot == "Significant"])
    } else {
        vec <- tmp[bg, ]
        sigMean <- NULL
    }

  # Binning the data manually so plot annotation can use count directly
    nfact <- max(vec$amplitude) - min(vec$amplitude)
    plotdf <- vec %>%
    mutate(bin = ceiling((amplitude - min(amplitude)) / nfact * binNum)) %>%
    group_by(bin, SigOrNot) %>%
    summarise(N = dplyr::n(), value = (bin * nfact)[1] / binNum)

    gg <- ggplot(plotdf, aes(x = value, y = N, fill = SigOrNot)) +
    geom_bar(stat = "identity") +
    ggtitle(paste("Amplitudes Estimated by", "OLS/Cosinor")) +
    labs(fill = "", x = paste0("Amplitude (", obsUnit, ")")) +
    scale_fill_manual(values = c("Significant" = colors$sig,
        "Not Significant" = colors$neutral)) +
    sharedtheme()

    if (!is.null(sigMean)) {
        gg <- gg +
        geom_segment(
            x = sigMean, xend = sigMean, y = 0, yend = max(plotdf$N) * 0.8,
            colour = colors$sig, linetype = 2, size = 0.5, alpha = 0.1
            ) +
        annotate(
            geom = "text", x = as.numeric(sigMean), y = max(plotdf$N) * 0.87,
            label = paste0("Mean\n", formatC(sigMean)), color = colors$sig
            )
    }
    gg
}

allIntersect <- function(fg) {
    if(sum(sapply(fg,length)>0)<2) {
        return(errorPlot("Multiple methods needed\n for intersection"))
    }
    library(UpSetR)
    upset(fromList(fg), order.by = "freq")
}

##### Fisher's Exact #####

plotFisherExact <- function(var1, var2, var1Name, var2Name) {

    col1 <- c(sum(var1 & var2), sum(var1 & !var2),
        sum(var1 & var2) + sum(var1 & !var2))
    col2 <- c(sum(!var1 & var2), sum(!var1 & !var2),
        sum(!var1 & var2) + sum(!var1 & !var2))
    col3 <- col1 + col2
    col3[3] <- 0
    rowNames <- c(
        paste(var2Name, "Significant"),
        paste(var2Name, "Non-Significant"),
        "Sum"
        )
    colNames <- c(
        paste(var1Name, "Significant"),
        paste(var1Name, "Non-Significant"),
        "Sum"
        )
    df <- data.frame(col1, col2, col3)
    df[3, 3] <- ""
    rownames(df) <- rowNames
    colnames(df) <- colNames
    xtable::xtable(df)
  # grid.table(df,rows = rowNames, cols = colNames)
}

vennFisherExact <- function(var1, var2, var1Name, var2Name) {
    a <- sum(var1)
    b <- sum(var2)
    c <- sum(var1 & var2)
    grid.draw(draw.pairwise.venn(a, b, c,
        category = c(paste("significant", var1Name),
            paste("significant", var2Name)),
        lty = rep("blank", 2),
        fill = c(colors$neutral, colors$neutral2), alpha = rep(0.25, 2),
        cat.pos = c(0, 0), cat.dist = rep(0.025, 2)
        ))
}

##### Individual Models #####

plotRowFit <- function(times, y, CS = TRUE, withErrorBar = TRUE, per = 24,
    tunit = "CT", ylab = "Value") {
    se <- function(x) {
        return(sd(x) / sqrt(length(x)))
    }
    # rects <- data.frame(xmin = per / 2, xmax = per, ymin = -Inf, ymax = Inf)

    if (withErrorBar) {
        plotdf <- data.frame(times = times, y) %>%
        group_by(times) %>%
        summarise(vals = mean(y), se = se(y))
    } else {
        plotdf <- data.frame(times = times, vals = y)
    }

    p <- ggplot(plotdf, aes(times, vals))

    if (CS) {
        p <- p + geom_smooth(
            method = "lm",
            formula = y ~ sin(x / per * 2 * pi) + cos(x / per * 2 * pi),
            se = !withErrorBar, fullrange = TRUE, color = colors$neutral2
            )
    } else {
        p <- p + geom_smooth(se = !withErrorBar, fullrange = TRUE,
                             method="loess",formula=y~x,
            color = colors$neutral2)
    }
    p <- p +
    # geom_rect(data = rects,
    #     aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    #     inherit.aes = FALSE, alpha = 0.2) +
    geom_point(data = plotdf, aes(y = vals, x = times), inherit.aes = FALSE,
        color = colors$neutral) +
    sharedtheme() +
    labs(y = ylab, x = paste0("Sample Time (", tunit, ")"))

    if (withErrorBar) {
        p + geom_errorbar(data = plotdf, aes(ymin = vals - se * 1.96,
            ymax = vals + se * 1.96),
        color = colors$neutral)
    } else {
        p
    }
}

#####  Error Plot #####

errorPlot <- function(main = "ERROR", top = "", bottom = "", textcol = "red") {
    df <- data.frame(
        x = c(1, 1, 1),
        y = c(1.5, 1, 0.5),
        text = c(top, main, bottom)
        )
    ggplot(df, aes(x, y)) +
    scale_y_discrete("", limits = 0:24) +
    labs(x = "") +
    geom_text(aes(label = text), colour = textcol, vjust = "inward",
        hjust = "inward", size = c(8, 8, 8)) +
    sharedtheme()+
    theme(
        legend.position = "none",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )
}
