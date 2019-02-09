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
#' @inheritParams sound_spectrogram
#' @inheritParams extract_full_pulse
#' @export
wav2db <- function(
  db, path, recursive = TRUE, make, model, serial = NA_character_,
  te_factor = 1, channel = c("left", "right"), max_length = 30, window_ms = 1,
  overlap = 0.9, threshold_amplitude = 10, min_peak_amplitude = 30,
  dimensions = 32, existing = c("append", "skip"),
  frequency_range = c(10000, 130000)
) {
  UseMethod("wav2db", db)
}

#' @export
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @importFrom utils file_test flush.console
#' @importFrom parallel mclapply detectCores
#' @importFrom pool poolCheckout poolReturn
#' @importFrom RSQLite dbQuoteString dbGetQuery dbWriteTable dbSendQuery dbClearResult dbRemoveTable dbQuoteLiteral
wav2db.soundDatabase <- function(
  db, path, recursive = TRUE, make, model, serial = NA_character_,
  te_factor = 1, channel = c("left", "right"), max_length = 30, window_ms = 1,
  overlap = 0.9, threshold_amplitude = 10, min_peak_amplitude = 30,
  dimensions = 32, existing = c("append", "skip"),
  frequency_range = c(10000, 130000)
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
    connection <- poolCheckout(db@Connection)
    available <- dbGetQuery(
      connection,
      sprintf(
        "SELECT
          r.fingerprint, r.timestamp,
          d.serial, d.sample_rate,
          s.window_ms, s.min_frequency, s.max_frequency,
          min(p.peak_amplitude) AS max_peak_amplitude
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
          r.fingerprint, r.timestamp, d.serial, d.sample_rate,
          s.window_ms, s.min_frequency, s.max_frequency",
        dbQuoteString(connection, path),
        dbQuoteString(connection, make),
        dbQuoteString(connection, model),
        dbQuoteLiteral(connection, te_factor),
        dbQuoteLiteral(connection, window_ms),
        dbQuoteLiteral(connection, overlap),
        dbQuoteLiteral(connection, threshold_amplitude),
        dbQuoteLiteral(connection, min_peak_amplitude)
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
    spectrogram <- sound_spectrogram(
      wav = wav, window_ms = window_ms, overlap = overlap,
      frequency_range = frequency_range
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
        dbQuoteString(connection, make),
        dbQuoteString(connection, model)
      )
    } else {
      sql <- sprintf(
        "SELECT id
        FROM device
        WHERE make = %s AND model = %s AND serial = %s",
        dbQuoteString(connection, make),
        dbQuoteString(connection, model),
        dbQuoteString(connection, serial)
      )
    }
    device_id <- dbGetQuery(conn = connection, sql)
    if (nrow(device_id) == 0) {
      dbWriteTable(
        conn = connection,
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
      device_id <- dbGetQuery(conn = connection, sql)
    }

    recording_id <- dbGetQuery(
      connection,
      sprintf(
        "SELECT id FROM recording WHERE fingerprint = %s",
        dbQuoteString(connection, pulses@Recording$fingerprint)
      )
    )
    if (nrow(recording_id) == 0) {
      dbWriteTable(
        connection,
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
        connection,
        sprintf(
          "SELECT id FROM recording WHERE fingerprint = %s",
          dbQuoteString(connection, pulses@Recording$fingerprint)
        )
      )
    }

    spectrogram_id <- dbGetQuery(
      connection,
      sprintf(
        "SELECT id FROM spectrogram WHERE fingerprint = %s",
        dbQuoteString(connection, pulses@Spectrogram$fingerprint)
      )
    )
    if (nrow(spectrogram_id) == 0) {
      dbWriteTable(
        conn = connection,
        name = "spectrogram",
        value = data.frame(
          fingerprint = pulses@Spectrogram$fingerprint,
          recording = recording_id$id,
          window_ms = window_ms,
          overlap = overlap,
          min_frequency = min(frequency_range),
          max_frequency = max(frequency_range),
          stringsAsFactors = FALSE
        ),
        append = TRUE
      )
      spectrogram_id <- dbGetQuery(
        connection,
        sprintf(
          "SELECT id FROM spectrogram WHERE fingerprint = %s",
          dbQuoteString(connection, pulses@Spectrogram$fingerprint)
        )
      )
    }

    if (nrow(pulses@Pulse) > 0) {
      dbWriteTable(
        conn = connection,
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
        conn = connection,
        name = "staging_pyramid",
        value = pyramid
      )
      res <- dbSendQuery(
        connection,
        "INSERT INTO pyramid (pulse, quadrant, value)
        SELECT p.id AS pulse, sp.quadrant, sp.value
        FROM staging_pyramid AS sp
        INNER JOIN pulse AS p ON sp.pulse = p.fingerprint"
      )
      dbClearResult(res)
      dbRemoveTable(conn = connection, name = "staging_pyramid")
    }

    poolReturn(connection)
    return(invisible(path))
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
    overlap = overlap,
    existing = existing,
    frequency_range = frequency_range
  )
  return(invisible(wavs))
}

#' @export
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom utils file_test
#' @importFrom pool poolClose
wav2db.character <- function(
  db, path, recursive = TRUE, make, model, serial = NA_character_,
  te_factor = 1, channel = c("left", "right"), max_length = 30, window_ms = 1,
  overlap = 0.9, threshold_amplitude = 10, min_peak_amplitude = 30,
  dimensions = 32, existing = c("append", "skip"),
  frequency_range = c(10000, 130000)
) {
  assert_that(
    is.string(path)
  )
  channel <- match.arg(channel)
  existing <- match.arg(existing)
  if (file_test("-f", path)) {
    x <- connect_db(path = db)
    wav2db(
      db = x,
      path = path,
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
      overlap = overlap,
      frequency_range = frequency_range
    )
    poolClose(x)
    return(invisible(path))
  } else if (!file_test("-d", path)) {
    stop("path must be an existing file or directory")
  }
  assert_that(is.flag(recursive))
  wavs <- list.files(
    path = path,
    pattern = "\\.[Ww][Aa][Vv]$",
    recursive = recursive,
    full.names = TRUE
  )
  if (requireNamespace("littler", quietly = TRUE)) {
    cmds <- sprintf(
      "r -e 'soundcluster::wav2db(
      path = \"%s\", db = \"%s\", threshold_amplitude = %f,
      min_peak_amplitude = %f, dimensions = %i, channel = \"%s\",
      te_factor = %f, max_length = %f, window_ms = %f, overlap = %f,
      existing = \"%s\", frequency_range = c(%f, %f), make = \"%s\",
      model = \"%s\"%s)'",
      sample(wavs), db, threshold_amplitude, min_peak_amplitude, dimensions,
      channel, te_factor, max_length, window_ms, overlap, existing,
      min(frequency_range), max(frequency_range), make, model,
      ifelse(is.na(serial), "", paste0(", serial = \"", serial, "\""))
    )
    lapply(cmds, system)
  } else {
    x <- connect_db(path = db)
    lapply(
      sample(wavs),
      wav2db,
      db = x,
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
      overlap = overlap,
      existing = existing,
      frequency_range = frequency_range
    )
    poolClose(x)
  }
  return(invisible(wavs))
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
