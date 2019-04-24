<img src="inst/app/www/disco_64.png"> DiscoRhythm - Discovering Rhythmicity
====================================

**An R Package for Discovering Rhythmicity in Biological Data with an Interactive 
Web Interface**

## Getting Started

Access the DiscoRhythm web application through the [public server](https://disco.camh.ca/apps/disco/). Install DiscoRhythm locally for improved performance.

See the [tutorial](https://bioconductor.org/packages/3.9/bioc/vignettes/DiscoRhythm/inst/doc/disco_workflow_vignette.html) for details on usage of the web application and R package.

See [DiscoRhythm on Bioconductor](https://bioconductor.org/packages/3.9/bioc/html/DiscoRhythm.html) for more details.

### R Package Installation

To install the DiscoRhythm R package from Bioconductor:

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

### DiscoRhythm with Docker

```
docker run --rm -p 3838:3838 mcarlucci/discorhythm
```

The web application should then be available at `localhost:3838/discorhythm`
