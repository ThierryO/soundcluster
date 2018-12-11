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
      has_name(object@Recording, "ID"),
      has_name(object@Recording, "Fingerprint"),
      has_name(object@Recording, "Filename"),
      has_name(object@Recording, "Timestamp"),
      has_name(object@Recording, "SampleRate"),
      has_name(object@Recording, "TEFactor"),
      has_name(object@Recording, "LeftChannel")
    )

    return(TRUE)
  }
)
