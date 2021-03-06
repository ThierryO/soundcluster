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
    Connection = "Pool"
  )
)

#' @importFrom methods setValidity
#' @importFrom pool poolCheckout poolReturn
#' @importFrom assertthat assert_that
#' @importFrom RSQLite dbListTables dbListFields
setValidity(
  "soundDatabase",
  function(object){
    connection <- poolCheckout(object@Connection)
    assert_that(
      inherits(connection, "SQLiteConnection"),
      all(
        c("device", "pulse", "pyramid", "recording", "spectrogram") %in%
          dbListTables(connection)
      ),
      all(
        c("id", "make", "model", "serial", "sample_rate", "te_factor",
          "left_channel") %in%
          dbListFields(connection, "device")
      ),
      all(
        c("id", "fingerprint", "spectrogram", "peak_time", "peak_frequency",
          "peak_amplitude", "start_time", "start_frequency", "start_amplitude",
          "end_time", "end_frequency", "select_amplitude") %in%
          dbListFields(connection, "pulse")
      ),
      all(
        c("pulse", "quadrant", "value") %in%
          dbListFields(connection, "pyramid")
      ),
      all(
        c("id", "fingerprint", "timestamp", "duration", "total_duration",
          "device", "filename") %in%
          dbListFields(connection, "recording")
      ),
      all(
        c("id", "fingerprint", "recording", "window_ms", "overlap") %in%
          dbListFields(connection, "spectrogram")
      )
    )
    poolReturn(connection)
    return(TRUE)
  }
)
