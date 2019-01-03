#' The soundDatabase class
#'
#' It holds a connection to a soundDatabase
#' @section Slots:
#'   \describe{
#'    \item{\code{Connection}}{An open SQLiteConnection to the soundDatabase}
#'   }
#' @name soundDatabase-class
#' @rdname soundDatabase-class
#' @exportClass soundDatabase
#' @aliases soundDatabase-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "soundDatabase",
  representation = representation(
    Connection = "SQLiteConnection"
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that
#' @importFrom RSQLite dbListTables dbListFields
setValidity(
  "soundDatabase",
  function(object){
    assert_that(
      all(
        c("device", "pulse", "pyramid", "recording", "spectrogram") %in%
          dbListTables(object@Connection)
      ),
      all(
        c("id", "make", "model", "serial", "sample_rate", "te_factor",
          "left_channel") %in%
          dbListFields(object@Connection, "device")
      ),
      all(
        c("id", "fingerprint", "spectrogram", "peak_time", "peak_frequency",
          "peak_amplitude", "start_time", "start_frequency", "start_amplitude",
          "end_time", "end_frequency", "select_amplitude") %in%
          dbListFields(object@Connection, "pulse")
      ),
      all(
        c("pulse", "quadrant", "value") %in%
          dbListFields(object@Connection, "pyramid")
      ),
      all(
        c("id", "fingerprint", "timestamp", "duration", "total_duration",
          "device", "filename") %in%
          dbListFields(object@Connection, "recording")
      ),
      all(
        c("id", "fingerprint", "recording", "window_ms", "overlap") %in%
          dbListFields(object@Connection, "spectrogram")
      )
    )
    return(TRUE)
  }
)
