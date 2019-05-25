#' Extract boundary points for each pulse
#'
#' @param spectrogram a `soundSpectrogram` object
#' @param threshold_amplitude relevant regions have an amplitude above the `threshold_amplitude`. Defaults to 10 dB.
#' @param min_peak_amplitude the maximum amplitude in a relevant region must be above `min_peak_amplitude`. Defaults to 30 dB.
#' @param amplitude_step the step size in which the threshold amplitude is increased. Defaults to 5 dB.
#' @export
#' @importFrom assertthat assert_that is.number
#' @importFrom raster raster
extract_boundary_pulse <- function(
  spectrogram,
  threshold_amplitude = 10,
  min_peak_amplitude = 30,
  amplitude_step = 5
) {
  assert_that(
    inherits(spectrogram, "soundSpectrogram"),
    is.number(threshold_amplitude),
    is.number(min_peak_amplitude),
    is.number(amplitude_step),
    min_peak_amplitude > threshold_amplitude,
    amplitude_step > 0
  )
  spectrogram_raster <- raster(
    spectrogram@SpecGram$S[rev(seq_len(nrow(spectrogram@SpecGram$S))), ],
    xmn = min(spectrogram@SpecGram$t) * 1000,
    xmx = max(spectrogram@SpecGram$t) * 1000,
    ymn = min(spectrogram@SpecGram$f) / 1000,
    ymx = max(spectrogram@SpecGram$f) / 1000
  )
  names(spectrogram_raster) <- "dB"

  pulses <- extract_boundary_points(
    spectrogram_raster = spectrogram_raster,
    threshold_amplitude = threshold_amplitude,
    amplitude_step = amplitude_step
  )
  unique(pulses[, 1:2])
}

#' @importFrom raster clump zonal colSums rowSums extent crop rowColFromCell xFromCol yFromRow
extract_boundary_points <- function(
  spectrogram_raster,
  threshold_amplitude = 5,
  min_peak_amplitude = 20,
  amplitude_step = 5,
  parent = c(parent_time = NA, parent_freq = NA)
) {
  min_peak_amplitude <- max(
    min_peak_amplitude, threshold_amplitude + amplitude_step
  )
  relevant <- clump(spectrogram_raster >= threshold_amplitude)
  peak <- zonal(spectrogram_raster, relevant, "max")
  to_do <- min_peak_amplitude <= peak[, "max"]

  if (!any(to_do)) {
    return(matrix(numeric(0), ncol = 18))
  }

  pulses <- lapply(
    peak[to_do, "zone"],
    function(this_clump) {
      local <- relevant$clumps == this_clump
      cols <- range(which(colSums(local, na.rm = TRUE) > 0))
      rows <- range(which(rowSums(local, na.rm = TRUE) > 0))
      local_ext <- extent(local, rows[1], rows[2], cols[1], cols[2])
      clump <- crop(local, local_ext)
      clump[clump == 0] <- NA
      detail <- crop(spectrogram_raster, local_ext)
      detail <- detail * clump
      peak_cell <- which.max(detail)
      peak_rc <- rowColFromCell(detail, peak_cell)

      x <- xFromCol(
        detail,
        c(
          1, ncol(detail),
          peak_rc[, "col"],
          range(which(!is.na(detail[peak_rc[, "row"], ]))),
          floor(median(which(!is.na(detail[1, ])))),
          ceiling(median(which(!is.na(detail[nrow(detail), ]))))
        )
      )
      names(x) <- c(
        "min_time", "max_time", "peak_time", "min_time_peak_freq",
        "max_time_peak_freq", "median_time_max_freq", "median_time_min_freq"
      )
      y <- yFromRow(
        detail,
        c(
          1, nrow(detail),
          peak_rc[, "row"],
          range(which(!is.na(detail[, peak_rc[, "col"]]))),
          floor(median(which(!is.na(detail[, 1])))),
          ceiling(median(which(!is.na(detail[, ncol(detail)]))))
        )
      )
      names(y) <- c(
        "max_freq", "min_freq", "peak_freq", "max_freq_peak_time",
        "min_freq_peak_time", "median_freq_min_time", "median_freq_max_time"
      )
      rbind(
        c(
          parent, x, y, min_amp = threshold_amplitude,
          max_amp = detail$layer[peak_cell]
        ),
        extract_boundary_points(
          spectrogram_raster = detail,
          threshold_amplitude = threshold_amplitude + amplitude_step,
          min_peak_amplitude = min_peak_amplitude,
          amplitude_step = amplitude_step,
          parent = c(
            parent_time = unname(x["peak_time"]),
            parent_freq = unname(y["peak_freq"])
          )
        )
      )
    }
  )
  do.call(rbind, pulses)
}
