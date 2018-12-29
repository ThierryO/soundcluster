#' Combine soundPulse objects
#'
#' @return a new soundPulse object
#' @inheritParams base::rbind
#' @importFrom methods slot
#' @export
rbind.soundPulse <- function(..., deparse.level = 1) {
  dots <- list(...)
  pulse <- do.call(
    rbind,
    c(
      lapply(dots, slot, "Pulse"),
      make.row.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  spectrogram <- do.call(
    rbind,
    c(
      lapply(dots, slot, "Spectrogram"),
      make.row.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  recording <- do.call(
    rbind,
    c(
      lapply(dots, slot, "Recording"),
      make.row.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  new(
    "soundPulse",
    Pulse = unique(pulse),
    Spectrogram = unique(spectrogram),
    Recording = unique(recording)
  )
}
