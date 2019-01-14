#' Convert the output of read_wav to a spectrogram
#' @param wav An object as created by \code{\link{sound_wav}}
#' @param window_ms The size of the window in microseconds. Default to 1.
#' @param overlap The overlap of two windows. Defaults to 0.9.
#' @param frequency_range the range of frequencies to use in Hz. Frequencies below the minimum or above the maximum are removed from the spectrogram. Defaults to 10000 (10 kHz) and 130000 (130 kHz).
#' @importFrom assertthat assert_that has_name is.number
#' @importFrom signal specgram
#' @importFrom stats median
#' @importFrom methods validObject
#' @export
#' @examples
#' wav <- sound_wav(
#'   system.file("demo.wav", package = "soundcluster"),
#'   te_factor = 10,
#'   max_length = 0.1
#' )
#' sound_spectrogram(wav)
sound_spectrogram <- function(
  wav, window_ms = 1, overlap = 0.9, frequency_range = c(10000, 130000)
){
  assert_that(
    inherits(wav, "soundWav"),
    is.number(window_ms),
    window_ms > 0,
    is.number(overlap),
    overlap > 0,
    overlap < 1,
    inherits(frequency_range, "numeric"),
    noNA(frequency_range),
    length(frequency_range) >= 2
  )
  validObject(wav)

  window_n <- next_power_2(wav@Recording$sample_rate * window_ms / 1000)
  spectrogram <- specgram(
    x = wav@Values,
    n = window_n,
    Fs = wav@Recording$sample_rate,
    overlap = ceiling(overlap * window_n)
  )
  relevant_frequency <- min(frequency_range) <= spectrogram$f &
    spectrogram$f <= max(frequency_range)
  spectrogram$S <- spectrogram$S[relevant_frequency, ]
  spectrogram$f <- spectrogram$f[relevant_frequency]
  spectrogram$S <- 20 * log10(abs(spectrogram$S))
  spectrogram$S <- spectrogram$S - median(spectrogram$S)
  fingerprint <- sha1(
    list(
      recording = wav@Recording$fingerprint,
      window_ms = window_ms,
      overlap = overlap,
      frequency_range = frequency_range
    )
  )
  spectrogram_meta <- data.frame(
    fingerprint = fingerprint,
    window_ms = window_ms,
    overlap = overlap,
    recording = wav@Recording$fingerprint,
    min_frequency = min(frequency_range),
    max_frequency = max(frequency_range),
    stringsAsFactors = FALSE
  )
  new(
    "soundSpectrogram",
    Recording = wav@Recording,
    Spectrogram = spectrogram_meta,
    SpecGram = spectrogram
  )
}
