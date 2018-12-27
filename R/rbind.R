#' Combine soundPulse objects
#'
#' @return a new soundPulse object
#' @inheritParams base::rbind
#' @importFrom methods slot
#' @export
rbind.soundPulse <- function(..., deparse.level = 1) {
  dots <- list(...)
  new(
    "soundPulse",
    Pulse = do.call(
      rbind,
      c(
        lapply(dots, slot, "Pulse"),
        make.row.names = FALSE,
        stringsAsFactors = FALSE
      )
    ),
    Spectrogram = do.call(
      rbind,
      c(
        lapply(dots, slot, "Spectrogram"),
        make.row.names = FALSE,
        stringsAsFactors = FALSE
      )
    ),
    Recording = do.call(
      rbind,
      c(
        lapply(dots, slot, "Recording"),
        make.row.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  )
}
