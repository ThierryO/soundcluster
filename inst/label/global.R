library(soundcluster)
pool <- connect_db(path = Sys.getenv("LABEL_PATH"))@Connection
onStop(function() {
  poolClose(pool)
})

find_start <- function(data, input) {
  start <- mean(
    c(
      data$pulse$start_time[data$current_pulse],
      data$pulse$end_time[data$current_pulse]
    )
  ) - 0.5 * input$timeinterval
  start <- pmin(
    pmax(0, start),
    data$maximum - input$timeinterval
  )
  floor(start)
}

set_dropdown <- function(session, pool) {
    behaviour <- dbGetQuery(
      pool,
      "SELECT id, name FROM behaviour ORDER BY name"
    )
    updateSelectInput(
      session,
      "behaviour",
      choices = c(
        setNames(behaviour$id, behaviour$name),
        "[no behaviour]" = "0", "[new behaviour]" = "-1"
      ),
      selected = "0"
    )

    species <- dbGetQuery(
      pool,
      "SELECT id, name FROM species ORDER BY name"
    )
    updateSelectInput(
      session,
      "species",
      choices = c(
        setNames(species$id, species$name),
        "[no species]" = "0", "[new species]" = "-1"
      ),
      selected = "0"
    )
    updateSelectInput(
      session,
      "species_parent",
      choices = c(setNames(species$id, species$name), "[no parent]" = "0"),
      selected = "0"
    )

    class <- dbGetQuery(
      pool,
      "SELECT id, abbreviation
      FROM class ORDER BY abbreviation"
    )
    updateSelectInput(
      session,
      "class_id",
      choices = c(
        setNames(class$id, class$abbreviation),
        "[no class]" = "0", "[new class]" = "-1"
      )
    )
}

set_pulses <- function(session, pool, data, input) {
  connection <- poolCheckout(pool)
  on.exit(poolReturn(connection))
  meta <- dbGetQuery(
    connection,
    sprintf(
      "SELECT
        s.fingerprint, s.window_ms, s.overlap,
        r.duration, r.filename, d.te_factor,
        CASE WHEN d.left_channel = 1 THEN 'left' ELSE 'right' END AS channel
      FROM spectrogram AS s
      INNER JOIN recording AS r ON s.recording = r.id
      INNER JOIN device AS d ON r.device = d.id
      WHERE s.id = %s",
      dbQuoteLiteral(connection, data$current_spectrogram)
    )
  )
  data$pulse <- read_pulse(input = input, data = data, connection = connection)
  max_freq <- ceiling(max(c(data$pulse$end_frequency, 150)))
  updateSliderInput(
    session, "frequency", value = c(0, max_freq), max = max_freq
  )
  updateSliderInput(
    session, "timeinterval",
    value = max_freq * as.numeric(input$aspect) * 1.35
  )
  data$title <- meta$filename
  to_do <- which(data$pulse$class == 0)
  data$current_pulse <- ifelse(length(to_do) == 0, 1, min(to_do))

  wav <- sound_wav(
    filename = meta$filename, channel = meta$channel,
    te_factor = meta$te_factor, max_length = meta$duration
  )
  sonogram <- sound_spectrogram(
    wav = wav, window_ms = meta$window_ms, overlap = meta$overlap
  )
  sonogram@SpecGram$f <- sonogram@SpecGram$f / 1000
  sonogram@SpecGram$t <- sonogram@SpecGram$t * 1000
  data$sonogram <- raster(
    sonogram@SpecGram$S[rev(seq_along(sonogram@SpecGram$f)), ],
    xmn = min(sonogram@SpecGram$t),
    xmx = max(sonogram@SpecGram$t),
    ymn = min(sonogram@SpecGram$f),
    ymx = max(sonogram@SpecGram$f)
  )
  data$maximum <- floor(tail(sonogram@SpecGram$t, 1))

  updateSliderInput(
    session,
    "starttime",
    value = find_start(data, input),
    min = floor(min(sonogram@SpecGram$t)),
    max = data$maximum - input$timeinterval
  )
  return(data)
}

read_pulse <- function(input, data, connection) {
  if (input$node_model == "") {
    sql <- sprintf(
      "SELECT
        pulse.id, start_time, end_time, start_frequency, end_frequency,
        peak_time, peak_frequency,
        CASE WHEN class IS NULL THEN 0 ELSE class END AS class,
        CASE WHEN colour IS NULL THEN 'black' ELSE colour END AS colour,
        CASE
          WHEN linetype IS NULL THEN 'solid' ELSE linetype END AS linetype,
        CASE WHEN angle IS NULL THEN 45 ELSE angle END AS angle
      FROM pulse
      LEFT JOIN class ON pulse.class = class.id
      WHERE spectrogram = %s
      ORDER BY start_time",
      dbQuoteLiteral(connection, data$current_spectrogram)
    )
    return(dbGetQuery(connection, sql))
  }
  sql <- sprintf(
    "SELECT
      p.id, start_time, end_time, start_frequency, end_frequency,
      peak_time, peak_frequency, pr.node,
      CASE WHEN class IS NULL THEN 0 ELSE class END AS class,
      CASE WHEN colour IS NULL THEN 'black' ELSE colour END AS colour,
      CASE
        WHEN linetype IS NULL THEN 'solid' ELSE linetype END AS linetype,
      CASE WHEN angle IS NULL THEN 45 ELSE angle END AS angle
    FROM pulse AS p
    LEFT JOIN class ON p.class = class.id
    LEFT JOIN prediction AS pr ON pr.pulse = p.id
    INNER JOIN node AS n ON pr.node = n.id
    WHERE spectrogram = %s AND n.model = %s
    ORDER BY start_time",
    dbQuoteLiteral(connection, data$current_spectrogram),
    dbQuoteLiteral(connection, as.integer(input$node_model))
  )
  pulse <- dbGetQuery(connection, sql)
  nc <- get_node_classification(connection, as.integer(input$node_model))
  colnames(nc) <- c("node", "dominant", "classification")
  pulse <- merge(pulse, nc, by = "node")
  pulse[order(pulse$start_time), ]
}
