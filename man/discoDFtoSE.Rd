% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{discoDFtoSE}
\alias{discoDFtoSE}
\alias{discoSEtoDF}
\title{Data formatting for DiscoRhythm}
\usage{
discoDFtoSE(Maindata, Metadata = NULL, shinySession = NULL)

discoSEtoDF(se)
}
\arguments{
\item{Maindata}{data.frame with the first column containing row IDs and all
subsequent columns containing experimental values. Columns should follow the
expected naming format described in the vignette.}

\item{Metadata}{data.frame of sample data, usually generated by
using \code{discoParseMeta} on the column names of the Maindata data.frame. 
If \code{is.null(Metadata)} and Maindata is provided as input, Metadata will 
be generated from Maindata.}

\item{shinySession}{shiny session object for use only by the DiscoRhythm
shiny app \code{discoApp()} to update the axis labels using the time
value prefix.}

\item{se}{SummarizedExperiment, the main data object used by DiscoRhythm 
expected to contain se$ID, se$ReplicateID, se$Time sample metadata and
non-null rownames. See the vignette for more details.}
}
\value{
discoDFtoSE returns a SummarizedExperiment object with colData 
containing sample metadata.

discoSEtoDF returns a DiscoRhythm format data.frame.
}
\description{
Functions to import a data.frame (from the format expected by the 
web application \code{discoApp()}) as a SummarizedExperiment object or
to export a SummarizedExperiment for use with the web application.
}
\examples{

df <- discoGetSimu()
se <- discoDFtoSE(df)

df <- discoSEtoDF(se) 
}
