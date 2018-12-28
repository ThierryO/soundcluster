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

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name noNA
setValidity(
  "soundPulse",
  function(object){
    assert_that(
      has_name(object@Pulse, "fingerprint"),
      has_name(object@Pulse, "spectrogram"),
      has_name(object@Pulse, "peak_time"),
      has_name(object@Pulse, "start_time"),
      has_name(object@Pulse, "end_time"),
      has_name(object@Pulse, "peak_frequency"),
      has_name(object@Pulse, "start_frequency"),
      has_name(object@Pulse, "end_frequency"),
      has_name(object@Pulse, "peak_amplitude"),
      has_name(object@Pulse, "start_amplitude"),
      has_name(object@Pulse, "select_amplitude"),
      has_name(object@Pulse, "shape")
    )
    assert_that(
      inherits(object@Pulse$fingerprint, "character"),
      inherits(object@Pulse$spectrogram, "character"),
      inherits(object@Pulse$peak_time, "numeric"),
      inherits(object@Pulse$start_time, "numeric"),
      inherits(object@Pulse$end_time, "numeric"),
      inherits(object@Pulse$peak_frequency, "numeric"),
      inherits(object@Pulse$start_frequency, "numeric"),
      inherits(object@Pulse$end_frequency, "numeric"),
      inherits(object@Pulse$peak_amplitude, "numeric"),
      inherits(object@Pulse$start_amplitude, "numeric"),
      inherits(object@Pulse$select_amplitude, "numeric"),
      inherits(object@Pulse$shape, "list"),
      noNA(object@Pulse)
    )
    if (anyDuplicated(object@Pulse$fingerprint) > 0) {
      stop("Duplicated pulse fingerprint")
    }
    assert_that(
      all(object@Pulse$start_time >= 0),
      all(object@Pulse$start_time <= object@Pulse$peak_time),
      all(object@Pulse$peak_time <= object@Pulse$end_time),
      all(object@Pulse$start_frequency >= 0),
      all(object@Pulse$start_frequency <= object@Pulse$peak_frequency),
      all(object@Pulse$peak_frequency <= object@Pulse$end_frequency),
      all(object@Pulse$start_amplitude <= object@Pulse$select_amplitude),
      all(object@Pulse$select_amplitude <= object@Pulse$peak_amplitude)
    )

    if (!all(object@Pulse$spectrogram %in% object@Spectrogram$fingerprint)) {
      stop("Each pulse must have a matching spectrogram")
    }

    return(TRUE)
  }
)
