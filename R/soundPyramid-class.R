#' The soundPyramid class
#'
#' Holds pyramids of pulses and their metadata.
#' @section Slots:
#' \describe{
#'   \item{\code{Pulse}}{A data.frame with `fingerprint` and its `spectrogram`}
#'   \item{\code{Pyramid}}{A matrix with pyramid values}
#'   \item{\code{Scaling}}{A matrix with `center` and `sd` for used to center each variable in `Pyramid`}
#'   \item{\code{Recording}}{A data.frame with fingerprint, filename, timestamp, sample_rate, te_factor, left_channel}
#'   \item{\code{Spectrogram}}{A data.frame with fingerprint, window_ms, overlap, recording}
#' }
#' @name soundPyramid-class
#' @rdname soundPyramid-class
#' @exportClass soundPyramid
#' @aliases soundPyramid-class
#' @importFrom methods setClass
#' @docType class
#' @include soundSpectrogramMeta-class.R
setClass(
  "soundPyramid",
  representation = representation(
    Pulse = "data.frame",
    Pyramid = "matrix",
    Scaling = "matrix"
  ),
  contains = "soundSpectrogramMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name noNA
setValidity(
  "soundPyramid",
  function(object){
    assert_that(
      all(
        c(
          "duration", "peak_time", "peak_frequency", "start_frequency",
          "frequency_range", "peak_amplitude", "amplitude_range"
        ) %in%
          colnames(object@Pyramid)
      ),
      is.numeric(object@Pyramid),
      noNA(object@Pyramid)
    )

    assert_that(
      has_name(object@Pulse, "fingerprint"),
      has_name(object@Pulse, "spectrogram"),
      all(object@Pulse$fingerprint %in% rownames(object@Pyramid)),
      all(object@Pulse$spectrogram %in% object@Spectrogram$fingerprint)
    )

    assert_that(
      is.numeric(object@Scaling),
      noNA(object@Scaling),
      all(c("center", "sd") %in% colnames(object@Scaling)),
      all(colnames(object@Pyramid) %in% rownames(object@Scaling)),
      object@Scaling["duration", "center"] > 0,
      object@Scaling["peak_time", "center"] >= 0,
      object@Scaling["peak_time", "center"] <= 1,
      object@Scaling["peak_frequency", "center"] > 0,
      object@Scaling["start_frequency", "center"] >= 0,
      object@Scaling["start_frequency", "center"] <= 1,
      object@Scaling["frequency_range", "center"] > 0,
      object@Scaling["peak_amplitude", "center"] > 0,
      object@Scaling["amplitude_range", "center"] > 0,
      all(object@Scaling[, "sd"] > 0)
    )

    return(TRUE)
  }
)
