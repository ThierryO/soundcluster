% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wav2rds.R
\name{wav2rds}
\alias{wav2rds}
\title{Extract the pulses in all files}
\usage{
wav2rds(path = ".", recursive = TRUE, existing = c("skip", "append",
  "overwrite"), threshold_amplitude = 10, min_peak_amplitude = 30,
  dimensions = 32, channel = c("left", "right"), te_factor = 1,
  max_length = 5, window_ms = 1, overlap = 0.9)
}
\arguments{
\item{path}{the name of a file or a directory}

\item{recursive}{logical.  Should the listing recurse into directories?}

\item{existing}{what to do with existing rds files. Options are \code{skip}, \code{append} and \code{overwrite}.}

\item{threshold_amplitude}{relevant regions have an amplitude above the \code{threshold_amplitude}. Defaults to 10 dB.}

\item{min_peak_amplitude}{the maximum amplitude in a relevant region must be above \code{min_peak_amplitude}. Defaults to 30 dB.}

\item{dimensions}{the number of rows and columns used to resample the shape. Must be a single number and a power of 2. Will be altered to the next power of 2.}

\item{channel}{Select the left or the right channel}

\item{te_factor}{The factor to which the original sound was slowed down prior
to recording}

\item{max_length}{Maximum length of the recording to use in seconds. If the
recording is longer, the last part is ignored.}

\item{window_ms}{The size of the window in microseconds. Default to 1.}

\item{overlap}{The overlap of two windows. Defaults to 0.9.}
}
\description{
Read the wav files in a directory, extract the pulses and store them in one rds file per wav file.
}
