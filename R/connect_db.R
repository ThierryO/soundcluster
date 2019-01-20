#' Connect to the SQLite database
#'
#' The database is an SQLite database named `soundcluster.sqlite`. This file will be created if it doesn't exist at the `path`.
#' @param path the path to the database
#' @export
#' @importFrom assertthat assert_that is.string
#' @importFrom utils file_test
#' @importFrom pool dbPool poolCheckout poolReturn
#' @importFrom RSQLite SQLite dbSendQuery dbClearResult
connect_db <- function(path = ".") {
  assert_that(
    is.string(path),
    file_test("-d", path)
  )

  db <- file.path(path, "soundcluster.sqlite")
  pool <- dbPool(drv = SQLite(), dbname = db)
  connection <- poolCheckout(pool)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS device (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      make TEXT NOT NULL,
      model TEXT NOT NULL,
      serial TEXT,
      sample_rate REAL NOT NULL,
      te_factor REAL NOT NULL,
      left_channel INTEGER NOT NULL
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS recording (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      fingerprint TEXT NOT NULL UNIQUE,
      timestamp INTEGER NOT NULL,
      duration REAL NOT NULL,
      total_duration REAL NOT NULL,
      device INTEGER NOT NULL REFERENCES device (id),
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
    "CREATE TABLE IF NOT EXISTS pyramid (
      pulse INTEGER NOT NULL REFERENCES pulse (id),
      quadrant INTEGER NOT NULL,
      value REAL NOT NULL
    )"
  )
  dbClearResult(res)
  res <- dbSendQuery(
    connection,
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_pyramid ON
    pyramid (pulse, quadrant)"
  )
  dbClearResult(res)
  res <- dbSendQuery(
    connection,
    "CREATE INDEX IF NOT EXISTS idx_pyramid_quadrant ON
    pyramid (quadrant)"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS model (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      grid_x INTEGER NOT NULL,
      grid_y INTEGER NOT NULL
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS model_pulse (
      model INTEGER NOT NULL REFERENCES model (id),
      pulse INTEGER NOT NULL REFERENCES pulse (id)
    )"
  )
  dbClearResult(res)
  res <- dbSendQuery(
    connection,
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_model_pulse ON
    model_pulse (model, pulse)"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS layer (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      model INTEGER NOT NULL REFERENCES model (id),
      weight REAL NOT NULL
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS model_variable (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL UNIQUE
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS node (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      model INTEGER NOT NULL REFERENCES model (id),
      x INTEGER NOT NULL,
      y INTEGER NOT NULL
    )"
  )
  dbClearResult(res)
  res <- dbSendQuery(
    connection,
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_node ON
    node (model, x, y)"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS node_value (
      node INTEGER NOT NULL REFERENCES node (id),
      layer INTEGER NOT NULL REFERENCES layer (id),
      variable INTEGER NOT NULL REFERENCES model_variable (id),
      value REAL NOT NULL
    )"
  )
  dbClearResult(res)
  res <- dbSendQuery(
    connection,
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_node_value ON
    node_value (layer, node, variable)"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS scaling (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      model INTEGER NOT NULL REFERENCES model (id),
      variable INTEGER NOT NULL REFERENCES model_variable (id),
      center REAL NOT NULL,
      sd REAL NOT NULL
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS prediction (
      pulse INTEGER NOT NULL REFERENCES pulse (id),
      node INTEGER NOT NULL REFERENCES node (id),
      distance REAL NOT NULL
    )"
  )
  dbClearResult(res)
  res <- dbSendQuery(
    connection,
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_prediction ON
    prediction (pulse, node)"
  )
  dbClearResult(res)
  res <- dbSendQuery(
    connection,
    "CREATE INDEX IF NOT EXISTS idx_prediction_node ON
    prediction (node)"
  )
  dbClearResult(res)

  poolReturn(connection)
  new("soundDatabase", Connection = pool)
}
