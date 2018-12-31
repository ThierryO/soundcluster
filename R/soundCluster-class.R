#' The soundCluster class
#'
#' Holds a cluster model.
#' @section Slots:
#'   \describe{
#'     \item{\code{Network}}{A self organising map}
#'     \item{\code{Pulse}}{A data.frame with `fingerprint` and its `spectrogram`}
#'     \item{\code{Scaling}}{A matrix with `center` and `sd` for used to center each variable in `Pyramid`}
#'     \item{\code{Recording}}{A data.frame with fingerprint, filename, timestamp, sample_rate, te_factor, left_channel}
#'     \item{\code{Spectrogram}}{A data.frame with fingerprint, window_ms, window_n, overlap, recording}
#'   }
#' @name soundCluster-class
#' @rdname soundCluster-class
#' @exportClass soundCluster
#' @aliases soundCluster-class
#' @importFrom methods setClass
#' @docType class
#' @include import_S3_classes.R
#' @include soundSpectrogramMeta-class.R
setClass(
  "soundCluster",
  representation = representation(
    Network = "kohonen",
    Pulse = "data.frame",
    Scaling = "matrix"
  ),
  contains = "soundSpectrogramMeta"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name noNA
setValidity(
  "soundCluster",
  function(object){
    assert_that(
      has_name(object@Pulse, "fingerprint"),
      has_name(object@Pulse, "spectrogram"),
      all(object@Pulse$fingerprint %in% rownames(object@Network$data[[1]])),
      all(object@Pulse$spectrogram %in% object@Spectrogram$fingerprint)
    )

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
