######################################################################
# Intended for use by discoBatch or through the DiscoRhythm_report.Rmd
# Includes all R code for the DiscoRhythm data processing
# Expects all arguments to discoBatch in the environment
#####################################################################

# Preprocess inputs
Maindata <- discoCheckInput(indata)
Metadata <- discoParseMeta(colnames(Maindata)[-1])

# Intersample correlations
CorRes <- discoInterCorOutliers(Maindata,Metadata,cor_method,
                                cor_threshold,cor_threshType)

# PCA for outlier detection
PCAres <- discoPCAoutliers(Maindata,pca_threshold,pca_scale,pca_pcToCut)
PCAresAfter <- discoPCA(Maindata[,-1][,!PCAres$outliers])

# Removing the outliers from the main data.frame and metadata data.frame
DataFinal <- data.frame(Maindata[,1],
                        Maindata[,-1][,!PCAres$outliers & !CorRes$outliers])
MetaFinal <- Metadata[!PCAres$outliers & !CorRes$outliers,]

# Running ANOVA and merging replicates
ANOVAres <- discoRepAnalysis(DataFinal,MetaFinal, aov_method,
                             aov_pcut, aov_Fcut, avg_method)

# Data to be used for Period Detection and Oscillation Detection
regressionMeta <- ANOVAres$regMet
regressionData <- ANOVAres$regDat

# Perform PCA on the final dataset
OVpca <- discoPCA(regressionData[,-1])

# Period Detection
PeriodRes <- discoPeriodDetection(regressionData,
                     regressionMeta,
                     timeType,
                     main_per)

# Oscillation Detection
discoODAres <- discoODAs(regressionData,regressionMeta,
                        circular_t = timeType=="circular",
                        period=osc_period,
                        osc_method,ncores)
