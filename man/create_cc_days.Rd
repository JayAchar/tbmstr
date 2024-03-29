% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_cc_days.R
\name{create_cc_days}
\alias{create_cc_days}
\title{Calculate time to culture conversion}
\usage{
create_cc_days(
  trtstdat = NULL,
  convdat = NULL,
  baseline,
  myco,
  lab = FALSE,
  culture_type = "culq",
  var_suffix = "lq"
)
}
\arguments{
\item{trtstdat}{date vector representing the treatment initation date}

\item{convdat}{date vector representing the study team's recorded culture
conversion date.}

\item{baseline}{data frame of baseline participant characteristics}

\item{myco}{data frame of participant mycobacteriology results}

\item{lab}{boolean to signifiy whether lab results should be used to
calculate culture conversion dates}

\item{culture_type}{string vector to define valid culture types}

\item{var_suffix}{string suffix for returned variable}
}
\value{
numeric vector representing the number of days to culture conversion
}
\description{
The mSTR study data contains information about each sputum result
and the study team's calculation for culture conversion.
This function compares the treatment start date with the study team's
recorded culture conversion date to generate a numerical result representing
the number of days until culture conversion.
}
