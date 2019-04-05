### Generates the Simphony_Example.csv example data
## Dataset generated using the following package:
## https://github.com/hugheylab/simphony
## Contains 400 features
## 54 samples (6 timepoints x 3 biological samples x 3 technical replicates).
## 50% rhythmic features, sampling interval of 4.
## Dominant period set to 24.
library(simphony)
set.seed(44)

# 150 non-rhythmic and 150 rhythmic with 18hr phase
featureGroups <- data.frame(fracFeatures = c(0.5, 0.5), amp = c(0, 2),
                            phase=c(0,12))
simData <- simphony(featureGroups, nFeatures = 300, interval = 4, nReps = 3,
                    timeRange = c(0,22),family = 'gaussian')

# 100 Random phase rows
rosc <- data.frame()
for(i in seq_len(100)){
  featureGroups <- data.frame(fracFeatures = c(1), amp = c(2),
                              phase=runif(1)*24)
  tmpsim <- simphony(featureGroups,nFeatures = 1, interval = 4, nReps = 3,
                      timeRange = c(0,22),family = 'gaussian')
  rosc <- rbind(rosc,tmpsim$abundData)
}

simData$abundData <- rbind(simData$abundData,rosc)

# Generate technical replicates
newdat <- t(apply(simData$abundData,1,
                  function(x) x + rnorm(length(x),sd = 3)))
newdat <- cbind(newdat,
                t(apply(simData$abundData,1,
                        function(x) x + rnorm(length(x),sd = 3))))
newdat <- cbind(newdat,
                t(apply(simData$abundData,1,
                        function(x) x + rnorm(length(x),sd = 3))))

newmeta <- rbind(simData$sampleMetadata,
                 simData$sampleMetadata,
                 simData$sampleMetadata)

# Generate inter-gene variation
newdat <- newdat + rnorm(nrow(newdat),sd=3)

# Add two outliers
newdat[,6] <- newdat[,6] + rnorm(nrow(newdat),sd=3)
newdat[,12] <- newdat[,12] + rnorm(nrow(newdat),sd=3)

# Sample Ids
colnames(newdat) <- paste0("CT", newmeta$time, "_",
    seq_along(newmeta$time), "_",
    rep(toupper(letters)[seq_len(nrow(newmeta)/3)],3))

# Label rows by rhythmic/non-rhythmic
isosc <- c(simData$featureMetadata$amp0>0,rep(TRUE,100))
rid <- paste0(c("TRUE"="RhythmGene_Id",
                "FALSE"="nonRhythmGene_Id")[as.character(isosc)],
seq_along(isosc))
outData <- data.frame(IDs=rid, newdat)

# Save data as a .csv
write.csv(outData, "Simphony_Example.csv", row.names=FALSE)

