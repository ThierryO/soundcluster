% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbind.R
\name{rbind.soundPulse}
\alias{rbind.soundPulse}
\title{Combine soundPulse objects}
\usage{
\method{rbind}{soundPulse}(..., deparse.level = 1)
}
\arguments{
\item{...}{(generalized) vectors or matrices.  These can be given as named
    arguments.  Other \R objects may be coerced as appropriate, or S4
    methods may be used: see sections \sQuote{Details} and
    \sQuote{Value}.  (For the \code{"data.frame"} method of \code{cbind}
    these can be further arguments to \code{\link{data.frame}} such as
    \code{stringsAsFactors}.)}

\item{deparse.level}{integer controlling the construction of labels in
    the case of non-matrix-like arguments (for the default method):\cr
    \code{deparse.level = 0} constructs no labels; the default,\cr
    \code{deparse.level = 1 or 2} constructs labels from the argument
    names, see the \sQuote{Value} section below.}
}
\value{
a new soundPulse object
}
\description{
Combine soundPulse objects
}
