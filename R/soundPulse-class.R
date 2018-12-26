#' The soundPulse class
#'
#' Holds pulses and their metadata.
#' @section Slots:
#'   \describe{
#'    \item{\code{Pulse}}{A data.frame with fingerprint, spectrogram, peak_time, start_time, end_time, peak_frequency, start_frequency, end_frequency, peak_amplitude, start_amplitude, select_amplitude and shape}
#'    \item{\code{Recording}}{A data.frame with fingerprint, filename, timestamp, sample_rate, te_factor, left_channel}
#'    \item{\code{Spectrogram}}{A data.frame with fingerprint, window_ms, window_n, overlap, recording}
#'   }
#' @name soundPulse-class
#' @rdname soundPulse-class
#' @exportClass soundPulse
#' @aliases soundPulse-class
#' @importFrom methods setClass
#' @docType class
#' @include soundSpectrogramMeta-class.R
setClass(
  "soundPulse",
  representation = representation(
    Pulse = "data.frame"
  ),
  contains = "soundSpectrogramMeta"
)
