#' The soundWavMeta class
#'
#' It holds metadata on recordings
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{A data.frame with ID, Fingerprint, Filename, Timestamp, SampleRate, SampleRate, TEFactor, LeftChannel}
#'   }
#' @name soundWavMeta-class
#' @rdname soundWavMeta-class
#' @exportClass soundWavMeta
#' @aliases soundWavMeta-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "soundWavMeta",
  representation = representation(
    Recording = "data.frame"
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
setValidity(
  "soundWavMeta",
  function(object){
    assert_that(
      has_name(object@Recording, "id"),
      has_name(object@Recording, "fingerprint"),
      has_name(object@Recording, "filename"),
      has_name(object@Recording, "timestamp"),
      has_name(object@Recording, "sample_rate"),
      has_name(object@Recording, "te_factor"),
      has_name(object@Recording, "left_channel")
    )

    return(TRUE)
  }
)
