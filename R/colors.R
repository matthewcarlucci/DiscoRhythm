#' Color pallete used by DiscoRhythm for plotting.
#' This palette is duplicated in inst/app/www/custom_styles.css for
#' application to the shiny app. 
#' 
#' @keywords internal
discoColors <- list(
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
discoColors$sig <- discoColors$discoMain2
discoColors$outlier <- discoColors$sig
discoColors$highval <- discoColors$discoMain
discoColors$lowval <- discoColors$discoSec
# discoColors$quantRamp <- colorRampPalette(c(discoColors$lowval,discoColors$highval))
discoColors$quantRamp <- viridis::viridis

#' Common theme elements in DiscoRhythm plots
#' 
#' @keywords internal
theme_disco <- function() {
  theme_bw() + theme(text = element_text(size = 10))
}