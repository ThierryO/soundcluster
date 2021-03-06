% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_soundpulse.R
\name{read_soundpulse}
\alias{read_soundpulse}
\title{Read the soundPulse objects from rds files}
\usage{
read_soundpulse(path, recursive = TRUE)
}
\arguments{
\item{path}{a character vector of full path names; the default
    corresponds to the working directory, \code{\link{getwd}()}.  Tilde
    expansion (see \code{\link{path.expand}}) is performed.  Missing
    values will be ignored.}

\item{recursive}{logical.  Should the listing recurse into directories?}
}
\description{
Read the soundPulse objects from rds files
}
