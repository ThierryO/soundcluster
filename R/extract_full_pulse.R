#' Extract full pulses of a spectrogram
#' @param spectrogram a `soundSpectrogram` object
#' @param threshold_amplitude relevant regions have an amplitude above the `threshold_amplitude`. Defaults to 10 dB.
#' @param min_peak_amplitude the maximum amplitude in a relevant region must be above `min_peak_amplitude`. Defaults to 30 dB.
#' @param dimensions the number of rows and columns used to resample the shape. Must be a single number and a power of 2. Will be altered to the next power of 2.
#' @export
#' @importFrom assertthat assert_that is.number is.count
#' @importFrom raster raster clump zonal extent crop xyFromCell xmin xmax ymin ymax cellStats resample
extract_full_pulse <- function(
  spectrogram,
  threshold_amplitude = 10,
  min_peak_amplitude = 30,
  dimensions = 32
) {
  assert_that(
    inherits(spectrogram, "soundSpectrogram"),
    is.number(threshold_amplitude),
    is.number(min_peak_amplitude),
    min_peak_amplitude > threshold_amplitude,
    is.count(dimensions)
  )
  dimensions <- next_power_2(dimensions)

  spectrogram_raster <- raster(
    spectrogram@SpecGram$S[rev(seq_len(nrow(spectrogram@SpecGram$S))), ],
    xmn = min(spectrogram@SpecGram$t) * 1000,
    xmx = max(spectrogram@SpecGram$t) * 1000,
    ymn = min(spectrogram@SpecGram$f) / 1000,
    ymx = max(spectrogram@SpecGram$f) / 1000
  )
  names(spectrogram_raster) <- "dB"

  relevant <- clump(spectrogram_raster >= threshold_amplitude)
  peak <- zonal(spectrogram_raster, relevant, max)
  lapply(
    peak[peak[, "value"] >= min_peak_amplitude, "zone"],
    function(this_clump) {
      local <- relevant$clumps == this_clump
      cols <- range(which(colSums(local, na.rm = TRUE) > 0))
      rows <- range(which(rowSums(local, na.rm = TRUE) > 0))
      local <- extent(local, rows[1], rows[2], cols[1], cols[2])
      unscaled <- crop(spectrogram_raster, local)
      scaled <- raster(ext = local, nrows = 32, ncols = 32, crs = NULL)
      local_peak <- xyFromCell(unscaled, which.max(unscaled))
      meta <- data.frame(
        spectrogram = spectrogram@Spectrogram$fingerprint,
        peak_time = local_peak[, "x"],
        start_time = xmin(local),
        end_time = xmax(local),
        peak_frequency = local_peak[, "y"],
        start_frequency = ymin(local),
        end_frequency = ymax(local),
        peak_amplitude = cellStats(unscaled, max),
        start_amplitude = cellStats(unscaled, min),
        select_amplitude = threshold_amplitude,
        stringsAsFactors = FALSE
      )
      (unscaled - meta$start_amplitude) /
        (meta$peak_amplitude - meta$start_amplitude) -> unscaled
      meta$shape <- list(as.matrix(resample(unscaled, scaled)))
      return(meta)
    }
  ) -> pulses
  do.call(rbind, pulses)
}
