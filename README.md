<img src="inst/app/www/disco_64.png"> DiscoRhythm - Discovering Rhythmicity
====================================

**An R Package for Discovering Rhythmicity in Biological Data with an Interactive 
Web Interface**

## Getting Started

DiscoRhythm web application [link](https://disco.camh.ca/apps/disco/) (Note: install DiscoRhythm for improved performance)

Web application guide [link](https://disco.camh.ca/apps/vignettes/disco_workflow_vignette.html)

See [DiscoRhythm on Bioconductor](http://bioconductor.org/packages/release/bioc/html/DiscoRhythm.html) for more details.

### R Package Installation

```
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("DiscoRhythm")
```

Or install the latest version from GitHub by:

```
library(devtools)
install_github("matthewcarlucci/DiscoRhythm", build_vignettes=TRUE)
```

