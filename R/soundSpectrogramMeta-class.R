#' The soundSpectrogramMeta class
#'
#' It holds metadata on sound recordings
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{A data.frame with fingerprint, filename, timestamp, sample_rate, te_factor, left_channel}
#'    \item{\code{Spectrogram}}{A data.frame with fingerprint, window_ms, window_n, overlap, recording}
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
#' @importFrom assertthat assert_that has_name
setValidity(
  "soundSpectrogramMeta",
  function(object){
    assert_that(has_name(object@Spectrogram, "fingerprint"))
    assert_that(has_name(object@Spectrogram, "window_ms"))
    assert_that(has_name(object@Spectrogram, "window_n"))
    assert_that(has_name(object@Spectrogram, "overlap"))
    assert_that(has_name(object@Spectrogram, "recording"))

    return(TRUE)
  }
)
