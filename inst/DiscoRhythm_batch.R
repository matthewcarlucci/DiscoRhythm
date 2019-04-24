######################################################################
# Intended for use by discoBatch or through the DiscoRhythm_report.Rmd
# Includes all R code for the DiscoRhythm data processing
# Expects all arguments to discoBatch in the environment
#####################################################################

library(DiscoRhythm)

# Preprocess inputs
selectDataSE <- discoCheckInput(discoDFtoSE(indata))

# Intersample correlations
CorRes <- discoInterCorOutliers(selectDataSE,cor_method,
                                cor_threshold,cor_threshType)

# PCA for outlier detection
PCAres <- discoPCAoutliers(selectDataSE,pca_threshold,pca_scale,pca_pcToCut)
PCAresAfter <- discoPCA(selectDataSE[,!PCAres$outliers])

# Removing the outliers from the main data.frame and metadata data.frame
FilteredSE <- selectDataSE[,!PCAres$outliers & !CorRes$outliers]

# Running ANOVA and merging replicates
ANOVAres <- discoRepAnalysis(FilteredSE, aov_method,
                             aov_pcut, aov_Fcut, avg_method)

# Data to be used for Period Detection and Oscillation Detection
FinalSE <- ANOVAres$se

# Perform PCA on the final dataset
OVpca <- discoPCA(FinalSE)

# Period Detection
PeriodRes <- discoPeriodDetection(FinalSE,
                     timeType,
                     main_per)

# Oscillation Detection
discoODAres <- discoODAs(FinalSE,
                        circular_t = timeType=="circular",
                        period=osc_period,
                        osc_method,ncores)
