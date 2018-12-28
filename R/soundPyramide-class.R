#' The soundPyramide class
#'
#' Holds pyramides of pulses and their metadata.
#' @section Slots:
#' \describe{
#'   \item{\code{Pulse}}{A data.frame with `fingerprint` and its `spectrogram`}
#'   \item{\code{Pyramide}}{A matrix with pyramide values}
#'   \item{\code{Scaling}}{A matrix with `center` and `sd` for used to center each variable in `Pyramide`}
#'   \item{\code{Recording}}{A data.frame with fingerprint, filename, timestamp, sample_rate, te_factor, left_channel}
#'   \item{\code{Spectrogram}}{A data.frame with fingerprint, window_ms, window_n, overlap, recording}
#' }
#' @name soundPyramide-class
#' @rdname soundPyramide-class
#' @exportClass soundPyramide
#' @aliases soundPyramide-class
#' @importFrom methods setClass
#' @docType class
#' @include soundSpectrogramMeta-class.R
setClass(
  "soundPyramide",
  representation = representation(
    Pulse = "data.frame",
    Pyramide = "matrix",
    Scaling = "matrix"
  ),
  contains = "soundSpectrogramMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name noNA
setValidity(
  "soundPyramide",
  function(object){
    assert_that(
      all(
        c(
          "duration", "peak_time", "peak_frequency", "start_frequency",
          "frequency_range", "peak_amplitude"
        ) %in%
          colnames(object@Pyramide)
      ),
      is.numeric(object@Pyramide),
      noNA(object@Pyramide)
    )

    assert_that(
      has_name(object@Pulse, "fingerprint"),
      has_name(object@Pulse, "spectrogram"),
      all(object@Pulse$fingerprint %in% rownames(object@Pyramide)),
      all(object@Pulse$spectrogram %in% object@Spectrogram$fingerprint)
    )

    assert_that(
      is.numeric(object@Scaling),
      noNA(object@Scaling),
      all(c("center", "sd") %in% colnames(object@Scaling)),
      all(colnames(object@Pyramide) %in% rownames(object@Scaling)),
      object@Scaling["duration", "center"] > 0,
      object@Scaling["peak_time", "center"] >= 0,
      object@Scaling["peak_time", "center"] <= 1,
      object@Scaling["peak_frequency", "center"] > 0,
      object@Scaling["start_frequency", "center"] >= 0,
      object@Scaling["start_frequency", "center"] <= 1,
      object@Scaling["frequency_range", "center"] > 0,
      object@Scaling["peak_amplitude", "center"] > 0,
      all(object@Scaling[, "sd"] > 0)
    )

    return(TRUE)
  }
)
