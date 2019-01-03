#' The soundWavMeta class
#'
#' It holds metadata on recordings
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{A data.frame with id, fingerprint, filename, timestamp, sample_rate, te_factor, left_channel}
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
#' @importFrom assertthat assert_that has_name noNA
setValidity(
  "soundWavMeta",
  function(object){
    assert_that(
      has_name(object@Recording, "fingerprint"),
      has_name(object@Recording, "filename"),
      has_name(object@Recording, "timestamp"),
      has_name(object@Recording, "duration"),
      has_name(object@Recording, "total_duration"),
      has_name(object@Recording, "sample_rate"),
      has_name(object@Recording, "te_factor"),
      has_name(object@Recording, "left_channel")
    )
    assert_that(
      inherits(object@Recording$fingerprint, "character"),
      inherits(object@Recording$filename, "character"),
      inherits(object@Recording$timestamp, "POSIXct"),
      inherits(object@Recording$sample_rate, "numeric"),
      inherits(object@Recording$duration, "numeric"),
      inherits(object@Recording$total_duration, "numeric"),
      inherits(object@Recording$te_factor, "numeric"),
      inherits(object@Recording$left_channel, "logical"),
      noNA(object@Recording)
    )
    if (anyDuplicated(object@Recording$fingerprint) > 0) {
      stop("Duplicated recording fingerprint")
    }
    if (any(object@Recording$sample_rate <= 0)) {
      stop("recording sample_rate must be strict positive")
    }
    if (any(object@Recording$te_factor <= 0)) {
      stop("recording te_factor must be strict positive")
    }

    return(TRUE)
  }
)
