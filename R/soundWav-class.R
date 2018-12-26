#' The soundWav class
#'
#' Holds the relevant content of wav files
#' @section Slots:
#'   \describe{
#'    \item{\code{Recording}}{The metadata on the recording}
#'    \item{\code{Values}}{The actual recorded values}
#'   }
#' @name soundWav-class
#' @rdname soundWav-class
#' @exportClass soundWav
#' @aliases soundWav-class
#' @importFrom methods setClass
#' @docType class
#' @include soundWavMeta-class.R
setClass(
  "soundWav",
  representation = representation(
    Values = "integer"
  ),
  contains = "soundWavMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that
setValidity(
  "soundWav",
  function(object){
    assert_that(is.vector(object@Values))
    assert_that(nrow(object@Recording) == 1)
    return(TRUE)
  }
)
