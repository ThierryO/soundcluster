library(RSQLite)
library(pool)
library(raster)

connect_pulse_db <- function(path) {
  db <- file.path(path, "soundcluster.sqlite")
  pool <- dbPool(drv = SQLite(), dbname = db)
  connection <- poolCheckout(pool)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS recording (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      fingerprint TEXT NOT NULL UNIQUE,
      timestamp INTEGER NOT NULL,
      duration REAL NOT NULL,
      total_duration REAL NOT NULL,
      sample_rate REAL NOT NULL,
      te_factor REAL NOT NULL,
      left_channel INTEGER NOT NULL,
      rds TEXT,
      filename TEXT
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS spectrogram (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      fingerprint TEXT NOT NULL UNIQUE,
      recording INTEGER NOT NULL REFERENCES recording (id),
      window_ms REAL NOT NULL,
      overlap REAL NOT NULL,
      min_frequency REAL NOT NULL,
      max_frequency REAL NOT NULL
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS species (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      parent INTEGER REFERENCES species (id),
      gbif INTEGER UNIQUE,
      name TEXT NOT NULL UNIQUE
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS behaviour (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL UNIQUE,
      color TEXT DEFAULT 'black',
      linetype TEXT DEFAULT 'solid',
      angle INTEGER DEFAULT 45
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS class (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      abbreviation TEXT NOT NULL UNIQUE,
      description TEXT,
      species INTEGER REFERENCES species (id),
      behaviour INTEGER REFERENCES behaviour (id),
      colour TEXT DEFAULT 'black',
      linetype TEXT DEFAULT 'solid',
      angle INTEGER DEFAULT 45
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS pulse (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      fingerprint TEXT NOT NULL UNIQUE,
      spectrogram INTEGER NOT NULL REFERENCES spectrogram (id),
      class INTEGER REFERENCES class (id),
      peak_time REAL NOT NULL,
      peak_frequency REAL NOT NULL,
      peak_amplitude REAL NOT NULL,
      start_time REAL NOT NULL,
      start_frequency REAL NOT NULL,
      start_amplitude REAL NOT NULL,
      end_time REAL NOT NULL,
      end_frequency REAL NOT NULL,
      select_amplitude REAL NOT NULL
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS prediction (
      pulse INTEGER NOT NULL REFERENCES pulse (id),
      class INTEGER NOT NULL REFERENCES class (id),
      prediction REAL NOT NULL
    )"
  )
  dbClearResult(res)

  poolReturn(connection)
  return(pool)
}

import_rds <- function(path, pool) {
  if (file_test("-d", path)) {
    list.files(
      path, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE
    ) %>%
      sapply(import_rds,pool = pool)
    return(NULL)
  }
  connection <- poolCheckout(pool)
  dbBegin(connection)
  on.exit(dbRollback(connection))

  sound_pulse <- readRDS(path)
  sound_pulse@Recording %>%
    mutate(rds = path, timestamp = as.integer(timestamp)) %>%
    dbWriteTable(conn = connection, name = "staging_recording", overwrite = TRUE)
  sound_pulse@Spectrogram %>%
    dbWriteTable(conn = connection, name = "staging_spectrogram")
  sound_pulse@Pulse %>%
    select(-shape) %>%
    dbWriteTable(conn = connection, name = "staging_pulse")

  dbSendQuery(conn = connection, "
    INSERT INTO recording
    SELECT r.id, s.fingerprint, s.timestamp, s.duration, s.total_duration,
           s.sample_rate, s.te_factor, s.left_channel, s.rds, s.filename
    FROM staging_recording AS s
    LEFT JOIN recording AS r ON s.fingerprint = r.fingerprint
    WHERE r.id IS NULL") %>%
    dbClearResult()

  dbSendQuery(conn = connection, "
    INSERT INTO spectrogram
    SELECT NULL AS id, ss.fingerprint, pr.id AS recording, ss.window_ms,
           ss.overlap, ss.min_frequency, ss.max_frequency
    FROM staging_spectrogram AS ss
    INNER JOIN recording AS pr ON ss.recording = pr.fingerprint
    LEFT JOIN spectrogram AS ps ON ss.fingerprint = ps.fingerprint
    WHERE ps.id IS NULL") %>%
    dbClearResult()

  dbSendQuery(conn = connection, "
    INSERT INTO pulse
    SELECT NULL AS id, sp.fingerprint, ps.id AS spectrogram, NULL AS class,
           sp.peak_time, sp.peak_frequency, sp.peak_amplitude, sp.start_time,
           sp.start_frequency, sp.start_amplitude, sp.end_time, sp.end_frequency,
           sp.select_amplitude
    FROM staging_pulse AS sp
    INNER JOIN spectrogram AS ps ON ps.fingerprint = sp.spectrogram
    LEFT JOIN pulse AS pp ON sp.fingerprint = pp.fingerprint
    WHERE pp.id IS NULL") %>%
    dbClearResult()

  dbRemoveTable(connection, "staging_recording")
  dbRemoveTable(connection, "staging_spectrogram")
  dbRemoveTable(connection, "staging_pulse")

  dbCommit(connection)
  dbSendQuery(connection, "VACUUM") %>%
    dbClearResult()
  on.exit()
  poolReturn(connection)
  return(NULL)
}

sample_spectrogram <- function(pool) {
  conn <- poolCheckout(pool)
  dbGetQuery(conn = conn, "
  WITH cte AS (
    SELECT spectrogram, CAST(COUNT(id) AS 'REAL') AS n
    FROM pulse
    WHERE class IS NULL
    GROUP BY spectrogram
  )

  SELECT spectrogram
  FROM cte
  ORDER BY LOG10(n) * ABS(RANDOM()) DESC
  LIMIT 1") %>%
    pull(spectrogram) -> spectrogram
  poolReturn(conn)
  return(spectrogram)
}

read_spectrogram <- function(pool, spectrogram) {
  conn <- poolCheckout(pool)
  sprintf("
    SELECT rds
    FROM recording AS r
    INNER JOIN spectrogram AS s ON r.id = s.recording
    WHERE s.id = %i", spectrogram) %>%
    dbGetQuery(conn = conn) %>%
    pull("rds") %>%
    readRDS() %>%
    slot("Pulse") %>%
    inner_join(
      sprintf("
        SELECT
          p.id,
          p.fingerprint,
          r.filename,
          COALESCE(p.class, 0) AS class,
          COALESCE(c.colour, 'black') AS colour,
          COALESCE(c.linetype, 'solid') AS linetype,
          COALESCE(c.angle, 45) AS angle
        FROM pulse AS p
        INNER JOIN spectrogram AS s ON s.id = p.spectrogram
        INNER JOIN recording AS r ON r.id = s.recording
        LEFT JOIN class AS c ON p.class = c.id
        WHERE s.id = %i", spectrogram
      ) %>%
      dbGetQuery(conn = conn),
      by = "fingerprint"
  ) %>%
    arrange(start_time + end_time, peak_frequency, select_amplitude) ->
    sound_pulse
  poolReturn(conn)
  return(sound_pulse)
}

shape2raster <- function(x) {
  vapply(
    seq_along(x$shape),
    function(i) {
      list(
        raster(
          x$shape[[i]] * (x$peak_amplitude[i] - x$start_amplitude[i]) +
            x$start_amplitude[i],
          xmn = x$start_time[i], xmx = x$end_time[i],
          ymn = x$start_frequency[i], ymx = x$end_frequency[i]
        )
      )
    },
    list(NA)
  )
}

pool <- connect_pulse_db(
  path = file.path("~", "github", "thierryo", "vleermuizen", "soundpulse")
)
onStop(function() {
  poolClose(pool)
})

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

find_start <- function(data, input) {
  start <- mean(
    c(
      data$raster$start_time[data$current_pulse],
      data$raster$end_time[data$current_pulse]
    )
  ) - 0.5 * input$timeinterval
  start <- pmin(
    pmax(0, start),
    data$maximum - input$timeinterval
  )
  floor(start)
}

get_class_counts <- function(pool) {
  conn <- poolCheckout(pool)
  dbGetQuery(conn, "
    WITH cte AS (
      SELECT class, COUNT(id) AS n
      FROM pulse
      GROUP BY class
    )

    SELECT abbreviation, description, n
    FROM cte
    INNER JOIN class ON cte.class = class.id") -> counts
  poolReturn(conn)
  return(counts)
}

get_pca <- function(pool) {
  conn <- poolCheckout(pool)
  dbGetQuery(conn, "
    SELECT
      id,
      peak_time - start_time AS peak_time,
      end_time - start_time AS time_range,
      peak_frequency,
      end_frequency - start_frequency AS frequency_range,
      (peak_frequency - start_frequency) /
        end_frequency - start_frequency AS rel_peak_frequency,
      peak_amplitude - select_amplitude AS peak_amplitude
    FROM pulse") -> pca
  poolReturn(conn)
  rn <- as.character(pca$id)
  pca %>%
    select(-id) %>%
    as.matrix() -> pca
  rownames(pca) <- rn
  prcomp(pca, center = TRUE, scale = TRUE)
}

fit_model <- function(this_class, this_df, dataset) {
  dataset %>%
    mutate(output = class == this_class) -> this_dataset
  formula <- "output ~ PC1 + I(PC1 ^ 2)"
  if (this_df >= 5) {
    formula <- paste(formula, "+ PC2 + I(PC2 ^ 2) + PC1:PC2")
  }
  glm(as.formula(formula), data = this_dataset, family = binomial)
}

get_models <- function(pool) {
  conn <- poolCheckout(pool)
  dbGetQuery(conn, "
    SELECT CAST(id AS CHARACTER) AS id, class
    FROM pulse
    WHERE class IS NOT NULL
  ") -> available
  poolReturn(conn)
  pca$x[available$id, ] %>%
    as.data.frame() %>%
    bind_cols(available) -> dataset
  available %>%
    count(class) %>%
    filter(n >= 40) %>%
    mutate(
      complement = nrow(available) - n,
      df = floor(pmin(n, complement) / 20),
      model = map2(class, df, ~fit_model(.x, .y, data = dataset))
    )
}
