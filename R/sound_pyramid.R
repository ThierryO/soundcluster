#' Prepare apulse parameters based on generic pulse properties and the pyramids of the shape
#'
#' - `duration`: the length of the pulse in ms
#' - `peak_time`: the relative position of the peak in the pulse
#' - `peak_frequency`: the frequency of the peak in kHz
#' - `frequency_range`: the difference between highest and lowest frequency in the pulse (in kHz)
#' - `start_frequency`: the relative distance between the peak frequency and the lowest frequency in the pulse. Defined as the difference devided by the frequency range.
#' - `peak_amplitude`: the amplitude of the peak in dB. 0 dB is the median loudness
#' - `PXXXX`: a set of pyramid values describing the shape of the pulse. The number of pyramid values depends on the total number of pulses and the dimensions of the shape
#'
#' @param sound_pulse a soundPulse object
#' @param end_frequency a vector of length 2 defining the lower and upper boundary for the end frequency of relevant pulses
#' @importFrom assertthat assert_that
#' @importFrom methods validObject
#' @importFrom parallel mclapply detectCores
#' @importFrom stats sd
#' @export
sound_pyramid <- function(sound_pulse, end_frequency = c(10, Inf)) {
  assert_that(
    inherits(sound_pulse, "soundPulse"),
    is.numeric(end_frequency),
    length(end_frequency) == 2
  )
  validObject(sound_pulse)

  pulse <- sound_pulse@Pulse
  relevant <-
    min(end_frequency) <= pulse$end_frequency &
    pulse$end_frequency <= max(end_frequency)
  pulse <- pulse[relevant, ]
  cd <- cbind(
    duration = pulse$end_time - pulse$start_time,
    peak_time = pulse$peak_time - pulse$start_time,
    peak_frequency = pulse$peak_frequency,
    frequency_range = pulse$end_frequency - pulse$start_frequency,
    start_frequency = pulse$peak_frequency - pulse$start_frequency,
    peak_amplitude = pulse$peak_amplitude,
    amplitude_range = pulse$peak_amplitude - pulse$select_amplitude
  )
  cd[, "peak_time"] <- cd[, "peak_time"] / cd[, "duration"]
  cd[, "start_frequency"] <- cd[, "start_frequency"] / cd[, "frequency_range"]

  pyramid <- mclapply(
    X = pulse$shape,
    FUN = soundcluster:::shape2pyramid,
    mc.cores = detectCores()
  )
  for (i in seq_along(pyramid)) {
    quadrant <- pyramid[[i]]$quadrant
    pyramid[[i]] <- pyramid[[i]]$value
    names(pyramid[[i]]) <- sprintf("Q%05i", quadrant)
  }
  pyramid <- do.call(rbind, pyramid)

  scaling <- cbind(
    center = colMeans(cd),
    sd = apply(cd, 2, sd)
  )
  scaling <- cbind(
    center = c(colMeans(cd), colMeans(pyramid)),
    sd = c(apply(cd, 2, sd), apply(pyramid, 2, sd))
  )
  cd <- apply(cd, 2, scale, center = TRUE, scale = TRUE)
  pyramid <- apply(pyramid, 2, scale, center = TRUE, scale = TRUE)
  rownames(cd) <- pulse$fingerprint
  rownames(pyramid) <- pulse$fingerprint

  new(
    "soundPyramid",
    Pyramid = pyramid,
    PulseMeta = cd,
    Pulse = pulse[, c("fingerprint", "spectrogram")],
    Scaling = scaling,
    Spectrogram = sound_pulse@Spectrogram,
    Recording = sound_pulse@Recording
  )
}
