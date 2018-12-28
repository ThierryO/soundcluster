#' Prepare apulse parameters based on generic pulse properties and the pyramides of the shape
#'
#' - `duration`: the length of the pulse in ms
#' - `peak_time`: the relative position of the peak in the pulse
#' - `peak_frequency`: the frequency of the peak in kHz
#' - `frequency_range`: the difference between highest and lowest frequency in the pulse (in kHz)
#' - `start_frequency`: the relative distance between the peak frequency and the lowest frequency in the pulse. Defined as the difference devided by the frequency range.
#' - `peak_amplitude`: the amplitude of the peak in dB. 0 dB is the median loudness
#' - `PXXXX`: a set of pyramide values describing the shape of the pulse. The number of pyramide values depends on the total number of pulses and the dimensions of the shape
#'
#' @param sound_pulse a soundPulse object
#' @importFrom assertthat assert_that
#' @importFrom stats sd
#' @export
sound_pyramide <- function(sound_pulse) {
  assert_that(inherits(sound_pulse, "soundPulse"))

  pulse <- sound_pulse@Pulse
  cd <- cbind(
    duration = pulse$end_time - pulse$start_time,
    peak_time = pulse$peak_time - pulse$start_time,
    peak_frequency = pulse$peak_frequency,
    frequency_range = pulse$end_frequency - pulse$start_frequency,
    start_frequency = pulse$peak_frequency - pulse$start_frequency,
    peak_amplitude = pulse$peak_amplitude
  )
  cd[, "peak_time"] <- cd[, "peak_time"] / cd[, "duration"]
  cd[, "start_frequency"] <- cd[, "start_frequency"] / cd[, "frequency_range"]
  depth <- pmax(0, floor(log(nrow(cd) / 20 - ncol(cd) - 1, base = 4)))
  cd <- cbind(cd, t(sapply(sound_pulse@Pulse$shape, pyramide, depth = depth)))

  scaling <- cbind(
    center = colMeans(cd),
    sd = apply(cd, 2, sd)
  )
  cd <- apply(cd, 2, scale, center = TRUE, scale = TRUE)
  rownames(cd) <- pulse$fingerprint

  new(
    "soundPyramide",
    Pyramide = cd,
    Pulse = pulse[, c("fingerprint", "spectrogram")],
    Scaling = scaling,
    Spectrogram = sound_pulse@Spectrogram,
    Recording = sound_pulse@Recording
  )
}

#' create a vector of pyramide values
#'
#' This function is a recursive function. If `depth == 0`, it returns the mean value of the matrix. If `depth > 0`, it will split the matrix into four quandrants and calculate `pyramide(quandrant, depth = depth -1)` on each quandrant. The output vector is a special concatenation of the vectors for each quandrant. The first element is the mean of the first elements of each of the quandrant vectors. This is equal to the overall mean of the matrix. The next three elements contain the difference between the overall mean and the mean of the last three quandrant. The difference of the first quandrant can be derived for the mean and the three differences. The last elements are the rest of the values for each quandrant.
#' @param x a square matrix. The dimensions must be a power of 2.
#' @param depth the maximal depth to use. Must be between `0` and `log2(ncol(x))`
#' @noRd
#' @importFrom utils tail
pyramide <- function(x, depth = log2(ncol(x))) {
  if (depth == 0 || ncol(x) == 1) {
    y <- mean(x)
    names(y) <- "P"
    return(y)
  }
  set <- 1:(ncol(x) / 2)
  s0 <- pyramide(x[set, set], depth = depth - 1)
  names(s0) <- paste0(names(s0), "0")
  s1 <- pyramide(x[set + ncol(x) / 2, set], depth = depth - 1)
  names(s1) <- paste0(names(s1), "1")
  s2 <- pyramide(x[set, set + ncol(x) / 2], depth = depth - 1)
  names(s2) <- paste0(names(s2), "2")
  s3 <- pyramide(x[set + ncol(x) / 2, set + ncol(x) / 2], depth = depth - 1)
  names(s3) <- paste0(names(s3), "3")
  y <- c(s0[1], s1[1], s2[1], s3[1])
  y <- c(
    mean(y), tail(y, -1) - mean(y),
    tail(s0, -1), tail(s1, -1), tail(s2, -1), tail(s3, -1)
  )
  names(y)[1] <- names(s0)[1]
  y[order(names(y))]
}
