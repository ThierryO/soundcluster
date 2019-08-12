#' Extract the pulses in all files
#'
#' Read the wav files in a directory, extract the pulses and store them in one rds file per wav file.
#' @param path the name of a file or a directory
#' @param existing what to do with existing rds files. Options are `skip`, `append` and `overwrite`.
#' @inheritParams base::list.files
#' @inheritParams sound_wav
#' @inheritParams sound_spectrogram
#' @inheritParams extract_full_pulse
#' @export
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom utils flush.console
wav2rds <- function(
  path = ".",
  recursive = TRUE,
  existing = c("skip", "append", "overwrite"),
  threshold_amplitude = 10,
  min_peak_amplitude = 30,
  dimensions = 32,
  channel = c("left", "right"),
  te_factor = 1,
  max_length = 5,
  window_ms = 1,
  overlap = 0.9
) {
  assert_that(
    is.string(path),
    is.flag(recursive)
  )
  channel <- match.arg(channel)
  existing <- match.arg(existing)

  if (file_test("-f", path)) {
    message(path)
    flush.console()
    target <- gsub("(.*)\\..*?$", "\\1.rds", path)
    if (existing == "skip" && file_test("-f", target)) {
      return(NULL)
    }
    if (existing == "append" && file_test("-f", target)) {
      done <- readRDS(target)
      if (threshold_amplitude %in% done@Pulse$select_amplitude) {
        return(NULL)
      }
    }
    wav <- sound_wav(
      filename = path,
      channel = channel,
      te_factor = te_factor,
      max_length = max_length
    )
    spectrogram <- sound_spectrogram(
      wav = wav, window_ms = window_ms, overlap = overlap
    )
    pulses <- extract_full_pulse(
      spectrogram = spectrogram,
      threshold_amplitude = threshold_amplitude,
      min_peak_amplitude = min_peak_amplitude,
      dimensions = dimensions
    )
    if (existing == "append" && file_test("-f", target)) {
      pulses <- rbind(done, pulses)
    }
    saveRDS(pulses, file = target)
    return(TRUE)
  }
  if (!file_test("-d", path)) {
    stop("path must be either an existing file or an existing directory")
  }
  wavs <- list.files(
    path = path,
    pattern = "\\.[Ww][Aa][Vv]$",
    recursive = recursive,
    full.names = TRUE
  )
  if (existing == "skip") {
    rds <- gsub("\\.[Ww][Aa][Vv]$", ".rds", wavs)
    wavs <- wavs[!file.exists(rds)]
  }
  if (requireNamespace("littler", quietly = TRUE)) {
    cmds <- sprintf(
      "r -e 'soundcluster::wav2rds(
      path = \"%s\", threshold_amplitude = %f, min_peak_amplitude = %f,
      dimensions = %i, channel = \"%s\", te_factor = %f, max_length = %f,
      window_ms = %f, overlap = %f, existing = \"%s\")'",
      wavs, threshold_amplitude, min_peak_amplitude, dimensions, channel,
      te_factor, max_length, window_ms, overlap, existing
    )
    lapply(cmds, system)
  } else {
    lapply(
      wavs,
      wav2rds,
      existing = existing,
      threshold_amplitude = threshold_amplitude,
      min_peak_amplitude = min_peak_amplitude,
      dimensions = dimensions,
      channel = channel,
      te_factor = te_factor,
      max_length = max_length,
      window_ms = window_ms,
      overlap = overlap
    )
  }
  return(TRUE)
}
