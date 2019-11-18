#' Convert WAV files into 1D convulotion raw data
#' @param db A soundclusterDatabase object.
#' Create it with \code{\link{connect_db}}.
#' @param path the path to the wav files zou want to handle.
#' Note that it will recursively look into the path.
#' @param location the location where you recorded the wav files.
#' @param make the make of the device.
#' @param model the model of the device.
#' @param serial the optional serial number of the device.
#' @param amplitude_range The relevant range in relative amplitude.
#' First we rescale the raw amplitude by substracting the median amplitude of
#' the recording.
#' Then we clamp the relative values to the amplitude range.
#' Defaults to `c(10, 70)`.
#' @param threshold the threshold in relative dB that defines a sound.
#' Defaults to `20`.
#' @param resolution required resolution for the 1D convolution.
#' Time in milliseconds and frequency in kHz.
#' Default to `c(0.25, 2)`
#' @inheritParams sound_wav
#' @inheritParams sound_spectrogram
#' @return invisible NULL
#' @importFrom pool poolCheckout poolReturn
#' @importFrom RSQLite dbQuoteString dbGetQuery dbWriteTable dbQuoteLiteral
#' @importFrom tuneR readWave
#' @importFrom raster raster clamp resample res bbox
#' @importMethodsFrom raster aggregate
#' @importFrom Matrix Matrix
#' @export
wav_1dconv <- function(
  db, path, location, make, model, serial,
  channel = c("left", "right"), te_factor = 1, max_length = 30,
  window_ms = 1, overlap = 0.9, frequency_range = c(14e3, 142e3),
  amplitude_range = c(10, 70), resolution = c(0.25, 2), threshold = 20
) {
  channel <- match.arg(channel)
  connection <- poolCheckout(db@Connection)
  on.exit(poolReturn(connection), add = TRUE)
  files <- list.files(path, pattern = ".wav$", full.names = TRUE,
                      recursive = TRUE, ignore.case = TRUE)
  sql_device <- sprintf(
    "SELECT id, te_factor, left_channel
    FROM device
    WHERE make = %s AND model = %s%s",
    dbQuoteString(connection, make),
    dbQuoteString(connection, model),
    ifelse(
      missing(serial),
      "",
      paste(" AND serial = ", dbQuoteString(connection, serial))
    )
  )
  device <- dbGetQuery(conn = connection, sql_device)
  if (nrow(device) == 0) {
    header <- readWave(filename = files[1], header = TRUE)
    dbWriteTable(
      connection,
      name = "device",
      value = data.frame(
        make = make,
        model = model,
        serial = ifelse(missing(serial), NA, serial),
        sample_rate = header$sample.rate * te_factor,
        te_factor = te_factor,
        left_channel = as.integer(channel == "left"),
        stringsAsFactors = FALSE
      ),
      append = TRUE
    )
    device <- dbGetQuery(conn = connection, sql_device)
  }
  te_factor <- device$te_factor
  channel <- c("right", "left")[device$left_channel + 1]
  sql_location <- sprintf(
    "SELECT id FROM location WHERE location = %s",
    dbQuoteString(connection, location)
  )
  location_id <- dbGetQuery(connection, sql_location)$id
  if (length(location_id) == 0) {
    dbWriteTable(
      conn = connection,
      name = "location",
      value = data.frame(
        location = location,
        stringsAsFactors = FALSE
      ),
      append = TRUE
    )
    location_id <- dbGetQuery(connection, sql_location)$id
  }
  junk <- vapply(
    files,
    function(file) {
      message(file)
      wav <- sound_wav(file, channel = channel, te_factor = te_factor,
                       max_length = max_length)
      n_chunk <- dbGetQuery(
        connection,
        sprintf(
          "SELECT c.id
          FROM recording AS r
          INNER JOIN spectrogram AS s ON r.id = s.recording
          INNER JOIN chunk_set AS cs ON s.id = cs.spectrogram
          INNER JOIN chunk AS c ON cs.id = c.chunk_set
          WHERE r.filename = %s AND location = %s AND time_resolution = %s AND
          frequency_resolution = %s AND frequency_min = %s AND frequency_max = %s
          AND amplitude_min = %s AND amplitude_max = %s AND
          amplitude_threshold = %s
          LIMIT 1
          ",
          dbQuoteString(connection, file),
          dbQuoteLiteral(connection, location_id),
          dbQuoteLiteral(connection, resolution[1]),
          dbQuoteLiteral(connection, resolution[2]),
          dbQuoteLiteral(connection, frequency_range[1] / 1e3),
          dbQuoteLiteral(connection, frequency_range[2] / 1e3),
          dbQuoteLiteral(connection, amplitude_range[1]),
          dbQuoteLiteral(connection, amplitude_range[2]),
          dbQuoteLiteral(connection, threshold)
        )
      )
      if (nrow(n_chunk)) {
        return(FALSE)
      }
      sql_recording <- sprintf(
        "SELECT id FROM recording WHERE filename = %s",
        dbQuoteString(connection, file)
      )
      recording_id <- dbGetQuery(connection, sql_recording)$id
      if (length(recording_id) == 0) {
        dbWriteTable(
          connection,
          name = "recording",
          value = data.frame(
            fingerprint = attr(wav, "Recording")$fingerprint,
            location = location_id,
            timestamp = attr(wav, "Recording")$timestamp,
            duration = attr(wav, "Recording")$duration,
            total_duration = attr(wav, "Recording")$total_duration,
            device = device$id,
            filename = attr(wav, "Recording")$filename,
            stringsAsFactors = FALSE
          ),
          append = TRUE
        )
        recording_id <- dbGetQuery(connection, sql_recording)$id
      }
      spectrogram <- sound_spectrogram(
        wav = wav, window_ms = window_ms, overlap = overlap,
        frequency_range = frequency_range + c(-1, 1) * 4e3
      )
      sql_spectrogram <- sprintf(
        "SELECT id FROM spectrogram WHERE recording = %s",
        dbQuoteLiteral(connection, recording_id)
      )
      spectrogram_id <- dbGetQuery(connection, sql_spectrogram)$id
      if (length(spectrogram_id) == 0) {
        dbWriteTable(
          connection,
          name = "spectrogram",
          value = data.frame(
            fingerprint = attr(spectrogram, "Spectrogram")$fingerprint,
            recording = recording_id,
            window_ms = window_ms,
            overlap = overlap,
            min_frequency = attr(spectrogram, "Spectrogram")$min_frequency,
            max_frequency = attr(spectrogram, "Spectrogram")$max_frequency,
            stringsAsFactors = FALSE
          ),
          append = TRUE
        )
        spectrogram_id <- dbGetQuery(connection, sql_spectrogram)$id
      }
      sql_chunk_set <- sprintf(
        "SELECT id
        FROM chunk_set
        WHERE spectrogram = %s AND time_resolution = %s AND
        frequency_resolution = %s AND frequency_min = %s AND frequency_max = %s
        AND amplitude_min = %s AND amplitude_max = %s AND amplitude_threshold = %s
        ",
        dbQuoteLiteral(connection, spectrogram_id),
        dbQuoteLiteral(connection, resolution[1]),
        dbQuoteLiteral(connection, resolution[2]),
        dbQuoteLiteral(connection, frequency_range[1] / 1e3),
        dbQuoteLiteral(connection, frequency_range[2] / 1e3),
        dbQuoteLiteral(connection, amplitude_range[1]),
        dbQuoteLiteral(connection, amplitude_range[2]),
        dbQuoteLiteral(connection, threshold)
      )
      chunk_set_id <- dbGetQuery(connection, sql_chunk_set)$id
      if (length(chunk_set_id) == 0) {
        dbWriteTable(
          connection,
          name = "chunk_set",
          value = data.frame(
            spectrogram = spectrogram_id,
            time_resolution = resolution[1],
            frequency_min = frequency_range[1] / 1e3,
            frequency_max = frequency_range[2] / 1e3,
            frequency_resolution = resolution[2],
            amplitude_min = amplitude_range[1],
            amplitude_max = amplitude_range[2],
            amplitude_threshold = threshold
          ),
          append = TRUE
        )
        chunk_set_id <- dbGetQuery(connection, sql_chunk_set)$id
      }
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
      spectrogram_raster <- resample(spectrogram_raster, target)
      spectrogram_raster <- as.integer(spectrogram_raster * 255)
      spectrogram_raster <- Matrix(as.matrix(spectrogram_raster))
      signal <- which(
        apply(spectrogram_raster, 2, max) >=
          (threshold - amplitude_range[1]) / diff(range(amplitude_range))
      )
      if (length(signal) == 0) {
        return(FALSE)
      }
      start <- signal[c(1, which(diff(signal) > 1) + 1)]
      end <- signal[c(which(diff(signal) > 1), length(signal))]
      dbWriteTable(
        connection,
        name = "chunk",
        value = data.frame(
          chunk_set = chunk_set_id,
          start = start,
          width = end - start + 1
        ),
        append = TRUE
      )
      output <- lapply(
        seq_along(start),
        function(i) {
          spectrogram_raster[, start[i]:end[i], drop = FALSE]
        }
      )
      sql_chunks <- sprintf(
        "SELECT id FROM chunk WHERE chunk_set = %s ORDER BY start",
        dbQuoteLiteral(connection, chunk_set_id)
      )
      names(output) <- dbGetQuery(connection, sql_chunks)$id
      attr(output, "spectrogram_meta") <- new(
        "soundSpectrogramMeta",
        Spectrogram = spectrogram@Spectrogram,
        Recording = spectrogram@Recording
      )
      attr(output, "amplitude_range") <- range(amplitude_range)
      attr(output, "amplitude_threshold") <- threshold
      attr(output, "frequency_range") <- frequency_range
      attr(output, "resolution") <- resolution
      saveRDS(
        output,
        file = gsub("\\.wav$", "_1dconv.rds", file, ignore.case = TRUE)
      )
      return(TRUE)
    },
    logical(1)
  )
  return(invisible(NULL))
}
