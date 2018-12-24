#' create a soundWav object
#' @param filename The name of the file
#' @param channel Select the left or the right channel
#' @param te_factor The factor to which the original sound was slowed down prior
#'    to recording
#' @param max_length Maximum length of the recording to use in seconds. If the
#' recording is longer, the last part is ignored.
#' @examples
#' wav <- sound_wav(
#'   system.file("demo.wav", package = "soundcluster"),
#'   te_factor = 10,
#'   max_length = 0.1
#' )
#' @export
#' @importFrom assertthat assert_that is.string is.number
#' @importFrom digest sha1
#' @importFrom methods new
#' @importFrom tuneR readWave
#' @importFrom utils file_test
sound_wav <- function(
  filename,
  channel = c("left", "right"),
  te_factor = 1,
  max_length = 5
) {
  assert_that(
    is.string(filename),
    is.number(te_factor),
    te_factor > 0,
    is.number(max_length),
    max_length > 0
  )
  if (!file_test("-f", filename)) {
    stop(filename, " does not exist")
  }
  channel <- match.arg(channel)

  header <- readWave(filename, header = TRUE)
  sample_rate <- header$sample.rate * te_factor
  raw_data <- readWave(
    filename = filename,
    from = 1,
    to = pmin(header$samples, sample_rate * max_length),
    units = "samples"
  )
  if (channel == "left") {
    values <- raw_data@left
  } else {
    values <- raw_data@right
  }
  if (length(values) == 0) {
    stop("No data in selected channel")
  }

  fingerprint <- sha1(
    list(
      sample_rate = sample_rate,
      te_factor = te_factor,
      values = values
    )
  )
  recording <- data.frame(
    fingerprint = fingerprint,
    filename = filename,
    timestamp = file.info(filename)$mtime,
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
