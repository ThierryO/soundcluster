#' The soundSpectrogramMeta class
#'
#' It holds metadata on sound recordings
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{A data.frame with fingerprint, filename, timestamp, sample_rate, te_factor, left_channel}
#'    \item{\code{Spectrogram}}{A data.frame with fingerprint, window_ms, overlap, recording}
#'   }
#' @name soundSpectrogramMeta-class
#' @rdname soundSpectrogramMeta-class
#' @exportClass soundSpectrogramMeta
#' @aliases soundSpectrogramMeta-class
#' @importFrom methods setClass
#' @docType class
#' @include soundWavMeta-class.R
setClass(
  "soundSpectrogramMeta",
  representation = representation(
    Spectrogram = "data.frame"
  ),
  contains = "soundWavMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name noNA
setValidity(
  "soundSpectrogramMeta",
  function(object){
    assert_that(
      has_name(object@Spectrogram, "fingerprint"),
      has_name(object@Spectrogram, "window_ms"),
      has_name(object@Spectrogram, "overlap"),
      has_name(object@Spectrogram, "recording")
    )
    assert_that(
      inherits(object@Spectrogram$fingerprint, "character"),
      inherits(object@Spectrogram$window_ms, "numeric"),
      inherits(object@Spectrogram$overlap, "numeric"),
      inherits(object@Spectrogram$recording, "character"),
      noNA(object@Spectrogram)
    )
    if (anyDuplicated(object@Spectrogram$fingerprint) > 0) {
      stop("Duplicated spectrogram fingerprint")
    }
    if (any(object@Spectrogram$window_ms <= 0)) {
      stop("spectrogram windows_ms must be strict positive")
    }
    if (any(object@Spectrogram$overlap <= 0)) {
      stop("spectrogram overlap must be strict positive")
    }
    if (any(object@Spectrogram$overlap >= 1)) {
      stop("spectrogram overlap must be smaller than 1")
    }

    if (!all(object@Spectrogram$recording %in% object@Recording$fingerprint)) {
      stop("Each spectrogram must have a matching recording")
    }

    return(TRUE)
  }
)
