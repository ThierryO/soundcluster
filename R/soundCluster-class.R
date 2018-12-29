#' The soundCluster class
#'
#' Holds a cluster model.
#' @section Slots:
#'   \describe{
#'    \item{\code{Network}}{A self organising map}
#'    \item{\code{Scaling}}{The scaling applied to the raw data}
#'   }
#' @name soundCluster-class
#' @rdname soundCluster-class
#' @exportClass soundCluster
#' @aliases soundCluster-class
#' @importFrom methods setClass
#' @docType class
#' @include import_S3_classes.R
setClass(
  "soundCluster",
  representation = representation(
    Network = "kohonen",
    Scaling = "matrix"
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name noNA
setValidity(
  "soundCluster",
  function(object){
    assert_that(
      is.numeric(object@Scaling),
      noNA(object@Scaling),
      all(c("center", "sd") %in% colnames(object@Scaling)),
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
