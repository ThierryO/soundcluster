#' create a soundWav object
#' @param filename The name of the file
#' @param timestamp the timestamp of the file
#' @param channel Select the left or the right channel
#' @param te_factor The factor to which the original sound was slowed down prior
#'    to recording
#' @param sample_rate the realtime sample rate
#' @param values the recorded values
#' @export
#' @importFrom digest sha1
#' @importFrom methods new
sound_wav <- function(
  filename,
  timestamp,
  channel = c("left", "right"),
  te_factor = 1,
  sample_rate,
  values
) {
  channel <- match.arg(channel)
  fingerprint <- sha1(
    list(
      sample_rate = sample_rate,
      te_factor = te_factor,
      values = values
    )
  )
  recording <- data.frame(
    id = 1,
    fingerprint = fingerprint,
    filename = filename,
    timestamp = timestamp,
    sample_rate = sample_rate,
    te_factor = te_factor,
    left_channel = channel == "left",
    stringsAsFactors = FALSE
  )
  new(
    "soundWav",
    Recording = recording,
    Values = values
  )
}
