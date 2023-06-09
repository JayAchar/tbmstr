% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_dst.R
\name{prepare_dst}
\alias{prepare_dst}
\title{Prepare DST data for analysis}
\usage{
prepare_dst(dst, baseline, purpose = c("quality"))
}
\arguments{
\item{dst}{Data frame of study DST data.}

\item{baseline}{Data frame of baseline study data.}

\item{purpose}{String variable to represent the puprpose of data preparation
Allowed values are:
* quality}
}
\value{
A data frame merged between \code{baseline} and \code{dst} with selected
variables
}
\description{
Prepare DST data for analysis
}
