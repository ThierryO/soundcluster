#' Connect to the SQLite database
#'
#' The database is an SQLite database named `soundcluster.sqlite`. This file will be created if it doesn't exist at the `path`.
#' @param path the path to the database
#' @export
#' @importFrom assertthat assert_that is.string
#' @importFrom utils file_test
#' @importFrom RSQLite dbConnect SQLite dbSendQuery dbClearResult
connect_db <- function(path = ".") {
  assert_that(
    is.string(path),
    file_test("-d", path)
  )

  db <- file.path(path, "soundcluster.sqlite")
  connection <- dbConnect(SQLite(), dbname = db)

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
      window_n REAL NOT NULL,
      overlap REAL NOT NULL
    )"
  )
  dbClearResult(res)

  res <- dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS pulse (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      fingerprint TEXT NOT NULL UNIQUE,
      spectrogram INTEGER NOT NULL REFERENCES spectrogram (id),
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
      quadrant integer NOT NULL,
      value REAL NOT NULL
    )"
  )
  dbClearResult(res)

  new("soundDatabase", Connection = connection)
}
