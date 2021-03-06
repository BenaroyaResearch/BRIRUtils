% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Validate.R
\name{validateLibraryFrame}
\alias{validateLibraryFrame}
\title{Validate Library names/locations in a Dataframe}
\usage{
validateLibraryFrame(counts, design, metrics = NULL, summary = NULL,
  countsField = "colnames", designField = "libraryId",
  metricsField = "libraryId", summaryField = "libraryId")
}
\arguments{
\item{counts}{counts dataframe}

\item{design}{design dataframe}

\item{metrics}{metrics dataframe (optional)}

\item{summary}{summary dataframe (optional)}

\item{countsField}{counts library field - usually column names}

\item{designField}{design library field - either the row names, or a column in the dataframe}

\item{metricsField}{metrics library field - either the row names, or a column in the dataframe}

\item{summaryField}{metrics library field - either the row names, or a column in the dataframe}
}
\value{
boolean - side effect of stopping if names are not aligned.
}
\description{
validate standard RNA-seq data: counts, design, metrics, and summary files.
}
\examples{
\donttest{
counts  <- read.csv(file="data/P239-1_CCLKLANXX_180806_combined_counts.csv", row.names=1)
counts  <- counts[,order(colnames(counts))]
design  <- read.csv(file="data/P239-1 Final Adjusted Annotation.csv", stringsAsFactors = F)
libpattern <- "lib[0-9]+"
colnames(counts)  <- str_extract(colnames(counts), libpattern)
validateLibraryFrame(counts, design)
}
}
\author{
Scott R Presnell, \email{SPresnell@benaroyaresearch.org}
}
