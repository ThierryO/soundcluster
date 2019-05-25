#' Extract full pulses of a spectrogram
#' @param spectrogram a `soundSpectrogram` object
#' @param threshold_amplitude relevant regions have an amplitude above the `threshold_amplitude`. Defaults to 10 dB.
#' @param min_peak_amplitude the maximum amplitude in a relevant region must be above `min_peak_amplitude`. Defaults to 30 dB.
#' @param max_peak_amplitude ignore regions where the maximum amplitude is above `max_peak_amplitude`.
#' @param dimensions the number of rows and columns used to resample the shape. Must be a single number and a power of 2. Will be altered to the next power of 2.
#' @export
#' @importFrom assertthat assert_that is.number is.count
#' @importFrom raster raster clump zonal extent crop xyFromCell xmin xmax ymin ymax cellStats resample colSums rowSums which.max as.matrix overlay
#' @importFrom digest sha1
#' @importFrom stats rnorm qnorm
#' @examples
#' wav <- sound_wav(
#'   system.file("demo.wav", package = "soundcluster"),
#'   te_factor = 10,
#'   max_length = 0.1
#' )
#' spectrogram <- sound_spectrogram(wav)
#' extract_full_pulse(spectrogram, min_peak_amplitude = 20, dimensions = 16)
extract_full_pulse <- function(
  spectrogram,
  threshold_amplitude = 10,
  min_peak_amplitude = 30,
  max_peak_amplitude = Inf,
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
  peak <- zonal(spectrogram_raster, relevant, "max")
  to_do <- min_peak_amplitude <= peak[, "max"]
  to_do <- to_do & peak[, "max"] < max_peak_amplitude
  if (!any(to_do)) {
    pulse <- data.frame(
      fingerprint = character(0),
      spectrogram = character(0),
      peak_time = numeric(0),
      start_time = numeric(0),
      end_time = numeric(0),
      peak_frequency = numeric(0),
      start_frequency = numeric(0),
      end_frequency = numeric(0),
      peak_amplitude = numeric(0),
      start_amplitude = numeric(0),
      select_amplitude = numeric(0),
      stringsAsFactors = FALSE
    )
    pulse$shape <- list()
    return(
      new(
        "soundPulse",
        Pulse = pulse,
        Spectrogram = spectrogram@Spectrogram,
        Recording = spectrogram@Recording
      )
    )
  }
  lapply(
    peak[to_do, "zone"],
    function(this_clump) {
      local <- relevant$clumps == this_clump
      cols <- range(which(colSums(local, na.rm = TRUE) > 0))
      rows <- range(which(rowSums(local, na.rm = TRUE) > 0))
      local_ext <- extent(local, rows[1], rows[2], cols[1], cols[2])
      clump <- crop(local, local_ext)
      detail <- crop(spectrogram_raster, local_ext)
      unscaled <- overlay(
        detail,
        clump,
        fun = function(x, y) {
          ifelse(
            is.na(y) | y == 1,
            x,
            rnorm(length(x), sd = threshold_amplitude / qnorm(0.999))
          )
        }
      )
      scaled <- raster(
        ext = local_ext, nrows = dimensions, ncols = dimensions, crs = NULL
      )
      local_peak <- xyFromCell(unscaled, which.max(unscaled))
      fingerprint <- sha1(
        list(
          spectrogram = spectrogram@Spectrogram$fingerprint,
          dimensions = dimensions,
          peak_time = local_peak[, "x"],
          peak_frequency = local_peak[, "y"],
          select_amplitude = threshold_amplitude
        )
      )
      meta <- data.frame(
        fingerprint = fingerprint,
        spectrogram = spectrogram@Spectrogram$fingerprint,
        peak_time = local_peak[, "x"],
        start_time = xmin(local_ext),
        end_time = xmax(local_ext),
        peak_frequency = local_peak[, "y"],
        start_frequency = ymin(local_ext),
        end_frequency = ymax(local_ext),
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
  pulses <- do.call(rbind, pulses)
  sp <- spectrogram@Spectrogram
  sp <- sp[sp$fingerprint %in% pulses$spectrogram, ]
  rec <- spectrogram@Recording
  rec <- rec[rec$fingerprint %in% sp$recording, ]
  new(
    "soundPulse",
    Pulse = pulses,
    Spectrogram = sp,
    Recording = rec
  )
}
