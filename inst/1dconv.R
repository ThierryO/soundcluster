library(soundcluster)
library(raster)
library(Matrix)

prepare_conv_1d <- function(
  path, channel = "left", te_factor = 1, max_length = 30, window_ms = 1,
  overlap = 0.9, frequency_range = c(14e3, 142e3), amplitude_range = c(10, 70),
  resolution = c(0.25, 2)
) {
  to_do <- list.files(path, pattern = "wav$", full.names = TRUE,
                      recursive = TRUE, ignore.case = TRUE)
  done <- list.files(path, pattern = "_1dconv.rds$", full.names = TRUE,
                     recursive = TRUE, ignore.case = TRUE)
  to_do <- to_do[!gsub("\\.wav$", "_1dconv.rds", to_do, ignore.case = TRUE) %in%
    done]
  junk <- vapply(
    to_do,
    function(
      filename, channel, te_factor, max_length, window_ms, overlap,
      frequency_range, amplitude_range
    ) {
      message(filename)
      wav <- sound_wav(filename = filename, channel = channel,
                       te_factor = te_factor, max_length = max_length)
      spectrogram <- sound_spectrogram(
        wav = wav, window_ms = window_ms, overlap = overlap,
        frequency_range = frequency_range + c(-1, 1) * 4e3
      )
      spectrogram_raster <- raster(
        spectrogram@SpecGram$S[rev(seq_len(nrow(spectrogram@SpecGram$S))), ],
        xmn = min(spectrogram@SpecGram$t) * 1000,
        xmx = max(spectrogram@SpecGram$t) * 1000,
        ymn = min(spectrogram@SpecGram$f) / 1000,
        ymx = max(spectrogram@SpecGram$f) / 1000
      )
      names(spectrogram_raster) <- "dB"
      spectrogram_raster <- clamp(
        spectrogram_raster, lower = min(amplitude_range),
        upper = max(amplitude_range)
      )
      spectrogram_raster <- (spectrogram_raster - min(amplitude_range)) /
        diff(range(amplitude_range))
      spectrogram_raster <- aggregate(
        spectrogram_raster,
        fact = round(resolution / res(spectrogram_raster))
      )
      target <- raster(
        xmn = 0,
        xmx = round(bbox(spectrogram_raster)["s1", "max"] / resolution[1]) *
          resolution[1],
        ymn = min(frequency_range) / 1e3, ymx = max(frequency_range) / 1e3,
        res = resolution
      )
      x1 <- resample(spectrogram_raster, target)
      output <- Matrix(round(as.matrix(x1) * 255))
      attr(output, "spectrogram_meta") <- new(
        "soundSpectrogramMeta",
        Spectrogram = spectrogram@Spectrogram,
        Recording = spectrogram@Recording
      )
      attr(output, "amplitude_range") <- range(amplitude_range)
      attr(output, "bbox") <- bbox(x1)
      attr(output, "resolution") <- res(x1)
      saveRDS(
        output,
        file = gsub("\\.wav$", "_1dconv.rds", filename, ignore.case = TRUE)
      )
      return(TRUE)
    },
    logical(1),
    channel = channel, te_factor = te_factor, max_length = max_length,
    window_ms = window_ms, overlap = overlap, frequency_range = frequency_range,
    amplitude_range = amplitude_range
  )
}

prepare_conv_1d("~/opnames/kasteel_beersel/20170806")
prepare_conv_1d("~/opnames/provinciaal_domein_huizingen/20170525/RPA03")
prepare_conv_1d("~/opnames/provinciaal_domein_huizingen/20170525/D240x", channel = "right", te_factor = 10)
prepare_conv_1d("~/opnames/provinciaal_domein_huizingen/20170623/RPA03")
prepare_conv_1d("~/opnames/provinciaal_domein_huizingen/20170623/D240x", channel = "right", te_factor = 10)
prepare_conv_1d("~/opnames/provinciaal_domein_huizingen/20170710")
prepare_conv_1d("~/opnames/provinciaal_domein_huizingen/20170722")
prepare_conv_1d("~/opnames/provinciaal_domein_huizingen/20170819")
prepare_conv_1d("~/opnames/provinciaal_domein_huizingen/20170916")
prepare_conv_1d("~/opnames/thuis")
prepare_conv_1d("~/opnames/sint-rochus")
prepare_conv_1d("~/opnames/Webbekomsbroek", channel = "right", te_factor = 10)
prepare_conv_1d("~/opnames/oostduinkerke", channel = "right", te_factor = 10)
prepare_conv_1d("~/opnames/france-compte", channel = "right", te_factor = 10)
prepare_conv_1d("~/opnames/margam", channel = "right", te_factor = 10)

prepare_conv_1d("~/opnames/lembeek-malakoff")
prepare_conv_1d("~/opnames/hallerbos")
prepare_conv_1d("~/opnames/halle_centrum")
prepare_conv_1d("~/opnames/gooik_lombergbos")
prepare_conv_1d("~/opnames/gooik_kesterheide")
prepare_conv_1d("~/opnames/citadel_diest")
prepare_conv_1d("~/opnames/normandie", channel = "right", te_factor = 10)
prepare_conv_1d("~/opnames/Liedekerke", channel = "right", te_factor = 10)
prepare_conv_1d("~/opnames/Leerbeek", channel = "right", te_factor = 10)
prepare_conv_1d("~/opnames/lac_leman", channel = "right", te_factor = 10)
prepare_conv_1d("~/opnames/kesterbeek", channel = "right", te_factor = 10)
prepare_conv_1d("~/opnames/heikruis - bos ter rijst", channel = "right", te_factor = 10)
