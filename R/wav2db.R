#' Extract pulses and store them in a soundcluster database
#'
#' @param db A connection to a soundcluster database
#' @param path the name of a file or a directory
#' @param make the manufacturer of the device
#' @param model the model of the device
#' @param serial the optional serial number of the device
#' @param existing what to do with existing spectrograms. "append" will add new pulses. "skip" will skip the recording.
#' @inheritParams base::list.files
#' @inheritParams sound_wav
#' @inheritParams wav2spectrogram
#' @inheritParams extract_full_pulse
#' @export
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @importFrom utils file_test flush.console
#' @importFrom parallel mclapply detectCores
#' @importFrom RSQLite dbQuoteString dbGetQuery dbWriteTable dbSendQuery dbClearResult dbRemoveTable dbQuoteLiteral
wav2db <- function(
  db, path, recursive = TRUE, make, model, serial = NA_character_,
  te_factor = 1, channel = c("left", "right"), max_length = 30, window_ms = 1,
  overlap = 0.9, threshold_amplitude = 10, min_peak_amplitude = 30,
  dimensions = 32, existing = c("append", "skip")
) {
  assert_that(
    inherits(db, "soundDatabase"),
    is.string(path),
    is.flag(recursive),
    is.string(make),
    noNA(make),
    is.string(model),
    noNA(model),
    is.string(serial)
  )
  channel <- match.arg(channel)
  existing <- match.arg(existing)

  if (file_test("-f", path)) {
    message(path)
    flush.console()
    available <- dbGetQuery(
      db@Connection,
      sprintf(
        "SELECT
          r.fingerprint, r.timestamp,
          d.serial, d.sample_rate,
          s.window_ms, min(p.peak_amplitude) AS max_peak_amplitude
        FROM  device AS d
        INNER JOIN recording AS r ON r.device = d.id
        INNER JOIN spectrogram AS s ON s.recording = r.id
        INNER JOIN pulse AS p ON p.spectrogram = s.id
        WHERE r.filename = %s AND
          d.make = %s AND d.model = %s AND d.te_factor = %s AND
          s.window_ms = %s AND s.overlap = %s AND
          p.select_amplitude = %s AND
          p.peak_amplitude >= %s
        GROUP BY
          r.fingerprint, r.timestamp, d.serial, d.sample_rate, s.window_ms",
        dbQuoteString(db@Connection, path),
        dbQuoteString(db@Connection, make),
        dbQuoteString(db@Connection, model),
        dbQuoteLiteral(db@Connection, te_factor),
        dbQuoteLiteral(db@Connection, window_ms),
        dbQuoteLiteral(db@Connection, overlap),
        dbQuoteLiteral(db@Connection, threshold_amplitude),
        dbQuoteLiteral(db@Connection, min_peak_amplitude)
      )
    )
    if (nrow(available) > 0) {
      if (existing == "skip" && isTRUE(all.equal(serial, available$serial))) {
        return(TRUE)
      } else {
        max_peak_amplitude <- available$max_peak_amplitude
      }
    } else {
      max_peak_amplitude <- Inf
    }
    wav <- sound_wav(
      filename = path,
      channel = channel,
      te_factor = te_factor,
      max_length = max_length
    )
    spectrogram <- wav2spectrogram(
      wav = wav, window_ms = window_ms, overlap = overlap
    )
    rm(wav)
    junk <- gc(FALSE)
    pulses <- extract_full_pulse(
      spectrogram = spectrogram,
      threshold_amplitude = threshold_amplitude,
      min_peak_amplitude = min_peak_amplitude,
      max_peak_amplitude = max_peak_amplitude,
      dimensions = dimensions
    )
    rm(spectrogram)
    junk <- gc(FALSE)
    pyramid <- mclapply(
      X = pulses@Pulse$shape,
      FUN = shape2pyramid,
      mc.cores = detectCores()
    )
    for (i in seq_along(pyramid)) {
      pyramid[[i]]$pulse <- pulses@Pulse$fingerprint[i]
    }
    pyramid <- do.call(rbind, pyramid)

    if (is.na(serial)) {
      sql <- sprintf(
        "SELECT id
        FROM device
        WHERE make = %s AND model = %s AND serial IS NULL",
        dbQuoteString(db@Connection, make),
        dbQuoteString(db@Connection, model)
      )
    } else {
      sql <- sprintf(
        "SELECT id
        FROM device
        WHERE make = %s AND model = %s AND serial = %s",
        dbQuoteString(db@Connection, make),
        dbQuoteString(db@Connection, model),
        dbQuoteString(db@Connection, serial)
      )
    }
    device_id <- dbGetQuery(conn = db@Connection, sql)
    if (nrow(device_id) == 0) {
      dbWriteTable(
        conn = db@Connection,
        name = "device",
        value = data.frame(
          make = make,
          model = model,
          serial = serial,
          sample_rate = pulses@Recording$sample_rate,
          te_factor = te_factor,
          left_channel = as.integer(channel == "left"),
          stringsAsFactors = FALSE
        ),
        append = TRUE
      )
      device_id <- dbGetQuery(conn = db@Connection, sql)
    }

    recording_id <- dbGetQuery(
      db@Connection,
      sprintf(
        "SELECT id FROM recording WHERE fingerprint = %s",
        dbQuoteString(db@Connection, pulses@Recording$fingerprint)
      )
    )
    if (nrow(recording_id) == 0) {
      dbWriteTable(
        db@Connection,
        name = "recording",
        value = data.frame(
          fingerprint = pulses@Recording$fingerprint,
          timestamp = as.integer(pulses@Recording$timestamp),
          duration = pulses@Recording$duration,
          total_duration = pulses@Recording$total_duration,
          device = device_id$id,
          filename = pulses@Recording$filename,
          stringsAsFactors = FALSE
        ),
        append = TRUE
      )
      recording_id <- dbGetQuery(
        db@Connection,
        sprintf(
          "SELECT id FROM recording WHERE fingerprint = %s",
          dbQuoteString(db@Connection, pulses@Recording$fingerprint)
        )
      )
    }

    spectrogram_id <- dbGetQuery(
      db@Connection,
      sprintf(
        "SELECT id FROM spectrogram WHERE fingerprint = %s",
        dbQuoteString(db@Connection, pulses@Spectrogram$fingerprint)
      )
    )
    if (nrow(spectrogram_id) == 0) {
      dbWriteTable(
        conn = db@Connection,
        name = "spectrogram",
        value = data.frame(
          fingerprint = pulses@Spectrogram$fingerprint,
          recording = recording_id$id,
          window_ms = window_ms,
          overlap = overlap,
          stringsAsFactors = FALSE
        ),
        append = TRUE
      )
      spectrogram_id <- dbGetQuery(
        db@Connection,
        sprintf(
          "SELECT id FROM spectrogram WHERE fingerprint = %s",
          dbQuoteString(db@Connection, pulses@Spectrogram$fingerprint)
        )
      )
    }

    if (nrow(pulses@Pulse) > 0) {
      dbWriteTable(
        conn = db@Connection,
        name = "pulse",
        value = data.frame(
          fingerprint = pulses@Pulse$fingerprint,
          spectrogram = spectrogram_id$id,
          peak_time = pulses@Pulse$peak_time,
          peak_frequency = pulses@Pulse$peak_frequency,
          peak_amplitude = pulses@Pulse$peak_amplitude,
          start_time = pulses@Pulse$start_time,
          start_frequency = pulses@Pulse$start_frequency,
          start_amplitude = pulses@Pulse$start_amplitude,
          end_time = pulses@Pulse$end_time,
          end_frequency = pulses@Pulse$end_frequency,
          select_amplitude = pulses@Pulse$select_amplitude,
          stringsAsFactors = FALSE
        ),
        append = TRUE
      )

      dbWriteTable(
        conn = db@Connection,
        name = "staging_pyramid",
        value = pyramid
      )
      res <- dbSendQuery(
        db@Connection,
        "INSERT INTO pyramid (pulse, quadrant, value)
        SELECT p.id AS pulse, sp.quadrant, sp.value
        FROM staging_pyramid AS sp
        INNER JOIN pulse AS p ON sp.pulse = p.fingerprint"
      )
      dbClearResult(res)
      dbRemoveTable(conn = db@Connection, name = "staging_pyramid")
    }

    return(TRUE)
  } else if (!file_test("-d", path)) {
    stop("path must be an existing file or directory")
  }
  wavs <- list.files(
    path = path,
    pattern = "\\.[Ww][Aa][Vv]$",
    recursive = recursive,
    full.names = TRUE
  )
  lapply(
    sample(wavs),
    wav2db,
    db = db,
    make = make,
    model = model,
    serial = serial,
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

#' @importFrom utils tail
#' @importFrom digest sha1
shape2pyramid <- function(x) {
  if (ncol(x) == 2) {
    return(
      data.frame(
        quadrant = 0:3,
        value = c(mean(x), mean(x) - tail(as.vector(x), -1)),
        stringsAsFactors = FALSE
      )
    )
  }
  set <- 1:(ncol(x) / 2)
  s0 <- shape2pyramid(x[set, set])
  s1 <- shape2pyramid(x[set + ncol(x) / 2, set])
  s2 <- shape2pyramid(x[set, set + ncol(x) / 2])
  s3 <- shape2pyramid(x[set + ncol(x) / 2, set + ncol(x) / 2])
  s0$quadrant <- s0$quadrant * 4
  s1$quadrant <- s1$quadrant * 4 + 1
  s2$quadrant <- s2$quadrant * 4 + 2
  s3$quadrant <- s3$quadrant * 4 + 3
  z <- rbind(s0, s1, s2, s3)
  y <- z[z$quadrant < 4, ]
  y <- data.frame(
    quadrant = 0:3,
    value = c(mean(y$value), mean(y$value) - tail(y$value, -1)),
    stringsAsFactors = FALSE
  )
  rbind(y, z[z$quadrant >= 4, ])
}
