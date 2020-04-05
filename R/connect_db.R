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
    "CREATE TABLE IF NOT EXISTS location (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      location TEXT NOT NULL
    )"
  )
  dbClearResult(res)

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
      location INTEGER NOT NULL REFERENCES location (id),
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
    "CREATE TABLE IF NOT EXISTS chunk_set (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      spectrogram INTEGER NOT NULL REFERENCES spectrogram (id),
      time_resolution REAL NOT NULL,
      frequency_min REAL NOT NULL,
      frequency_max REAL NOT NULL,
      frequency_resolution REAL NOT NULL,
      amplitude_min REAL NOT NULL,
      amplitude_max REAL NOT NULL,
      amplitude_threshold REAL NOT NULL
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS chunk_series (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      chunk_set INTEGER NOT NULL REFERENCES chunk_set (id),
      start INTEGER NOT NULL,
      width INTEGER NOT NULL
    )"
  )
  dbClearResult(res)

  sql <- paste(
    "CREATE TABLE IF NOT EXISTS chunk (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      chunk_series INTEGER NOT NULL REFERENCES chunk_set (id),
      start INTEGER NOT NULL,
      width INTEGER NOT NULL
    )"
  )
  res <- dbSendQuery(connection, sql)
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
      start_time REAL NOT NULL,
      start_frequency REAL NOT NULL,
      end_time REAL NOT NULL,
      end_frequency REAL NOT NULL
    )"
  )
  dbClearResult(res)

  poolReturn(connection)
  new("soundDatabase", Connection = pool)
}
