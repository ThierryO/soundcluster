#' Prepare pulse parameters based on generic pulse properties and the pyramids of the shape
#'
#' - `duration`: the length of the pulse in ms
#' - `peak_time`: the relative position of the peak in the pulse
#' - `peak_frequency`: the frequency of the peak in kHz
#' - `frequency_range`: the difference between highest and lowest frequency in the pulse (in kHz)
#' - `start_frequency`: the relative distance between the peak frequency and the lowest frequency in the pulse. Defined as the difference devided by the frequency range.
#' - `peak_amplitude`: the amplitude of the peak in dB. 0 dB is the median loudness
#' - `QXXXX`: a set of pyramid values describing the shape of the pulse. The number of pyramid values depends on the total number of pulses and the dimensions of the shape
#' @param x the object
#' @param ... further arguments used by some methods
#' @export
sound_pyramid <- function(x, ...) {
  UseMethod("sound_pyramid", x)
}

#' @rdname sound_pyramid
#' @param end_frequency a vector of length 2 defining the lower and upper boundary for the end frequency of relevant pulses
#' @importFrom assertthat assert_that
#' @importFrom methods validObject
#' @importFrom parallel mclapply detectCores
#' @importFrom stats sd
#' @export
sound_pyramid.soundPulse <- function(x, end_frequency = c(10, Inf), ...) {
  assert_that(
    inherits(x, "soundPulse"),
    is.numeric(end_frequency),
    length(end_frequency) == 2
  )
  validObject(x)

  pulse <- x@Pulse
  relevant <-
    min(end_frequency) <= pulse$end_frequency &
    pulse$end_frequency <= max(end_frequency)
  pulse <- pulse[relevant, ]
  cd <- cbind(
    duration = pulse$end_time - pulse$start_time,
    peak_time = pulse$peak_time - pulse$start_time,
    peak_frequency = pulse$peak_frequency,
    frequency_range = pulse$end_frequency - pulse$start_frequency,
    start_frequency = pulse$peak_frequency - pulse$start_frequency,
    peak_amplitude = pulse$peak_amplitude,
    amplitude_range = pulse$peak_amplitude - pulse$select_amplitude
  )
  cd[, "peak_time"] <- cd[, "peak_time"] / cd[, "duration"]
  cd[, "start_frequency"] <- cd[, "start_frequency"] / cd[, "frequency_range"]

  pyramid <- mclapply(
    X = pulse$shape,
    FUN = shape2pyramid,
    mc.cores = detectCores()
  )
  for (i in seq_along(pyramid)) {
    quadrant <- pyramid[[i]]$quadrant
    pyramid[[i]] <- pyramid[[i]]$value
    names(pyramid[[i]]) <- sprintf("Q%05i", quadrant)
  }
  pyramid <- do.call(rbind, pyramid)

  scaling <- cbind(
    center = colMeans(cd),
    sd = apply(cd, 2, sd)
  )
  scaling <- cbind(
    center = c(colMeans(cd), colMeans(pyramid)),
    sd = c(apply(cd, 2, sd), apply(pyramid, 2, sd))
  )
  cd <- apply(cd, 2, scale, center = TRUE, scale = TRUE)
  pyramid <- apply(pyramid, 2, scale, center = TRUE, scale = TRUE)
  rownames(cd) <- pulse$fingerprint
  rownames(pyramid) <- pulse$fingerprint

  new(
    "soundPyramid",
    Pyramid = pyramid,
    PulseMeta = cd,
    Pulse = pulse[, c("fingerprint", "spectrogram")],
    Scaling = scaling,
    Spectrogram = x@Spectrogram,
    Recording = x@Recording
  )
}

#' @rdname sound_pyramid
#' @param spectrogram an optional spectrogram id
#' @param n the optimal sample size. If `n` is not set, then the entire set will be returned
#' @importFrom methods validObject
#' @importFrom assertthat assert_that is.count
#' @importFrom RSQLite dbGetQuery dbQuoteLiteral
#' @importFrom stats sd
#' @export
sound_pyramid.soundDatabase <- function(x, spectrogram, n, ...) {
  validObject(x)

  if (missing(n)) {
    sample_sql <- ""
  } else {
    sample_sql <- sprintf(
      "ORDER BY random() LIMIT %s",
      dbQuoteLiteral(x@Connection, n)
    )
  }
  if (missing(spectrogram)) {
    where_sql <- ""
  } else {
    assert_that(is.count(spectrogram))
    where_sql <- sprintf(
      "spectrogram = %s",
      dbQuoteLiteral(x@Connection, spectrogram)
    )
  }
  where_sql <- paste(where_sql, collapse = " AND ")
  if (nchar(where_sql) > 0) {
    where_sql <- paste("WHERE", where_sql)
  }

  staging_table <- paste0("sample_", sha1(Sys.time()))
  staging_sql <- sprintf(
    "CREATE TEMPORARY TABLE %s AS
    SELECT id FROM pulse
    %s %s",
    staging_table,
    where_sql,
    sample_sql
  )
  res <- dbSendQuery(x@Connection, staging_sql)
  dbClearResult(res)

  sql <- sprintf(
    "SELECT
      fingerprint,
      peak_frequency,
      end_time - start_time AS duration,
      end_frequency - start_frequency AS frequency_range,
      (peak_time - start_time) / (end_time - start_time) AS peak_time,
      (peak_frequency - start_frequency) / (end_frequency - start_frequency) AS
        start_frequency,
      peak_amplitude,
      peak_amplitude - select_amplitude AS amplitude_range
    FROM pulse AS p INNER JOIN temp.%s AS sp ON p.id = sp.id
    ORDER BY p.id",
    staging_table
  )
  pulses <- dbGetQuery(x@Connection, sql)
  fingerprint <- pulses$fingerprint
  pulses$fingerprint <- NULL

  max_var <- pmax(1, 4 ^ floor(log(nrow(pulses), base = 4)))
  pyramid <- matrix(
    NA_real_,
    ncol = max_var, nrow = nrow(pulses),
    dimnames = list(fingerprint, sprintf("Q%05i", seq_len(max_var) - 1))
  )
  i <- 1
  while (i <= max_var) {
    sql <- sprintf(
      "SELECT value
      FROM pyramid AS py INNER JOIN %s AS sp ON py.pulse = sp.id
      WHERE py.quadrant = %s
      ORDER BY py.pulse",
      staging_table,
      dbQuoteLiteral(x@Connection, i - 1)
    )
    extra <- dbGetQuery(x@Connection, sql)
    if (nrow(extra) < nrow(pulses)) {
      pyramid <- pyramid[, seq_len(i - 1), drop = FALSE]
      break
    }
    pyramid[, i] <- extra$value
    i <- i + 1
  }

  scaling <- cbind(
    center = c(colMeans(pulses), colMeans(pyramid)),
    sd = c(apply(pulses, 2, sd), apply(pyramid, 2, sd))
  )
  pulses <- apply(pulses, 2, scale, center = TRUE, scale = TRUE)
  pyramid <- apply(pyramid, 2, scale, center = TRUE, scale = TRUE)
  rownames(pulses) <- fingerprint
  rownames(pyramid) <- fingerprint

  sql <- sprintf(
    "SELECT p.fingerprint, s.fingerprint AS spectrogram
    FROM pulse AS p
    INNER JOIN spectrogram AS s ON p.spectrogram = s.id
    INNER JOIN %s AS sp ON p.id = sp.id",
    staging_table
  )
  pulse_spectrogram <- dbGetQuery(x@Connection, sql)

  sql <- sprintf(
    "WITH cte_spectrogram AS (
      SELECT s.id
      FROM spectrogram AS s
      INNER JOIN pulse AS p ON s.id = p.spectrogram
      INNER JOIN %s AS sp ON p.id = sp.id
      GROUP BY s.id
    )

    SELECT
      s.fingerprint, s.window_ms, s.overlap, s.min_frequency, s.max_frequency,
      r.fingerprint AS recording
    FROM cte_spectrogram AS cs
    INNER JOIN spectrogram AS s ON cs.id = s.id
    INNER JOIN recording AS r ON s.recording = r.id",
    staging_table
  )
  spectrogram_meta <- dbGetQuery(x@Connection, sql)

  sql <- sprintf(
    "WITH cte_spectrogram AS (
      SELECT s.id
      FROM spectrogram AS s
      INNER JOIN pulse AS p ON s.id = p.spectrogram
      INNER JOIN %s AS sp ON p.id = sp.id
      GROUP BY s.id
    ),
    cte_recording AS (
      SELECT r.id
      FROM cte_spectrogram AS cs
      INNER JOIN recording AS r ON cs.id = r.id
      GROUP BY r.id
    )

    SELECT
      r.fingerprint, r.filename, r.timestamp, r.duration, r.total_duration,
      d.sample_rate, d.te_factor, d.left_channel
    FROM cte_recording AS cr
    INNER JOIN recording AS r ON cr.id = r.id
    INNER JOIN device AS d ON r.device = d.id",
    staging_table
  )
  recording <- dbGetQuery(x@Connection, sql)
  recording$timestamp <- as.POSIXct(recording$timestamp, origin = "1970-01-01")
  recording$left_channel <- as.logical(recording$left_channel)

  new(
    "soundPyramid",
    Pyramid = pyramid,
    PulseMeta = pulses,
    Pulse = pulse_spectrogram,
    Scaling = scaling,
    Spectrogram = spectrogram_meta,
    Recording = recording
  )
}
