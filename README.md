<img src="inst/app/www/disco_64.png"> DiscoRhythm - Discovering Rhythmicity
====================================

**An R Package for Discovering Rhythmicity in Biological Data with an Interactive 
Web Interface**

## Getting Started

[DiscoRhythm public server](https://mcarlucci.shinyapps.io/discorhythm/)

See the [tutorial](https://bioconductor.org/packages/3.9/bioc/vignettes/DiscoRhythm/inst/doc/disco_workflow_vignette.html) for details on usage of the web application and R package.

See [DiscoRhythm on Bioconductor](https://bioconductor.org/packages/3.9/bioc/html/DiscoRhythm.html) for more details.

### R Package Installation

For local usage of DiscoRhythm, install the R package from Bioconductor:

```
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("DiscoRhythm")
```

Or install the latest version from GitHub:

```
library(devtools)
install_github("matthewcarlucci/DiscoRhythm", build_vignettes=TRUE)
```

### Usage with Docker

If [docker](https://docs.docker.com/install/) is installed, the 
[DiscoRhythm container on Docker Hub](https://hub.docker.com/r/mcarlucci/discorhythm) 
can be used to run the DiscoRhythm web application. 
