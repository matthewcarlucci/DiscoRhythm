% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataInput.R
\name{discoCheckInput}
\alias{discoCheckInput}
\title{Import Data for DiscoRhythm Analysis}
\usage{
discoCheckInput(se, n_min_sample = 3)
}
\arguments{
\item{se}{SummarizedExperiment, the main data object used by DiscoRhythm 
expected to contain se$ID, se$ReplicateID, se$Time sample metadata and
non-null rownames. See the vignette for more details.}

\item{n_min_sample}{numeric value specifying minimal number of samples
needed to perform analysis.}
}
\value{
SummarizedExperiment checked for errors and
modified as needed
}
\description{
Performs various checks and cleaning operations on the input data.
}
\details{
Rows containing NA's or all constant values are removed.
If matrix values are character it will be attempted to convert them
to numeric.
If input is not a matrix it will be converted using
\code{as.matrix()}.
User will be warned if row IDs contain duplicate entries.
}
\examples{
se <- discoGetSimu(TRUE)
se_clean <- discoCheckInput(se)

}
