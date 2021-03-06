% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sound_pyramid.R
\name{sound_pyramid}
\alias{sound_pyramid}
\alias{sound_pyramid.soundPulse}
\alias{sound_pyramid.soundDatabase}
\title{Prepare pulse parameters based on generic pulse properties and the pyramids of the shape}
\usage{
sound_pyramid(x, ...)

\method{sound_pyramid}{soundPulse}(x, end_frequency = c(10, Inf), ...)

\method{sound_pyramid}{soundDatabase}(x, spectrogram, n, ...)
}
\arguments{
\item{x}{the object}

\item{...}{further arguments used by some methods}

\item{end_frequency}{a vector of length 2 defining the lower and upper boundary for the end frequency of relevant pulses}

\item{spectrogram}{an optional spectrogram id}

\item{n}{the optimal sample size. If \code{n} is not set, then the entire set will be returned}
}
\description{
\itemize{
\item \code{duration}: the length of the pulse in ms
\item \code{peak_time}: the relative position of the peak in the pulse
\item \code{peak_frequency}: the frequency of the peak in kHz
\item \code{frequency_range}: the difference between highest and lowest frequency in the pulse (in kHz)
\item \code{start_frequency}: the relative distance between the peak frequency and the lowest frequency in the pulse. Defined as the difference devided by the frequency range.
\item \code{peak_amplitude}: the amplitude of the peak in dB. 0 dB is the median loudness
\item \code{QXXXX}: a set of pyramid values describing the shape of the pulse. The number of pyramid values depends on the total number of pulses and the dimensions of the shape
}
}
