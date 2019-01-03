#' The soundSpectrogram class
#'
#' Holds a spectrogram and it's metadata.
#' @section Slots:
#'   \describe{
#'    \item{\code{SpecGram}}{The actual spectrogram}
#'    \item{\code{Recording}}{A data.frame with fingerprint, filename, timestamp, sample_rate, te_factor, left_channel}
#'    \item{\code{Spectrogram}}{A data.frame with fingerprint, window_ms, overlap, recording}
#'   }
#' @name soundSpectrogram-class
#' @rdname soundSpectrogram-class
#' @exportClass soundSpectrogram
#' @aliases soundSpectrogram-class
#' @importFrom methods setClass
#' @docType class
#' @include soundSpectrogramMeta-class.R
#' @include import_S3_classes.R
setClass(
  "soundSpectrogram",
  representation = representation(
    SpecGram = "specgram"
  ),
  contains = "soundSpectrogramMeta"
)


#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
setValidity(
  "soundSpectrogram",
  function(object){
    assert_that(
      nrow(object@Spectrogram) == 1
    )

    return(TRUE)
  }
)
