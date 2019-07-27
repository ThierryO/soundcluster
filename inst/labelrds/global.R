library(kohonen)
library(RSQLite)
library(pool)
library(raster)
library(purrr)
library(tidyr)
library(stringr)
library(dplyr)
library(keras)

connect_pulse_db <- function(path, size = 2 ^ 5) {
  db <- file.path(path, "soundcluster.sqlite")
  pool <- dbPool(drv = SQLite(), dbname = db)
  connection <- poolCheckout(pool)

  dbSendQuery(
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
  ) %>%
    dbClearResult()

  dbSendQuery(
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
  ) %>%
    dbClearResult()

  dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS species (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      parent INTEGER REFERENCES species (id),
      gbif INTEGER UNIQUE,
      name TEXT NOT NULL UNIQUE
    )"
  ) %>%
    dbClearResult()

  dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS behaviour (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL UNIQUE,
      color TEXT DEFAULT 'black',
      linetype TEXT DEFAULT 'solid',
      angle INTEGER DEFAULT 45
    )"
  ) %>%
    dbClearResult()

  dbSendQuery(
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
  ) %>%
    dbClearResult()

  dbSendQuery(
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
  ) %>%
    dbClearResult()

  sprintf("col_%02i INTEGER", seq_len(size)) %>%
    paste(collapse = ",\n        ") %>%
    sprintf(fmt = "
      CREATE TABLE IF NOT EXISTS shape (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        pulse INTEGER NOT NULL REFERENCES pulse (id),
        row INTEGER NOT NULL,
        %s
      )") %>%
    dbSendQuery(conn = connection) %>%
    dbClearResult()

  dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS model (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      x_dim INTEGER NOT NULL,
      y_dim INTEGER NOT NULL
    )"
  ) %>%
    dbClearResult()

  dbSendQuery(
    connection,
    "CREATE TABLE IF NOT EXISTS node (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      model INTEGER NOT NULL,
      x_pos INTEGER NOT NULL,
      y_pos INTEGER NOT NULL
    )"
  ) %>%
    dbClearResult()

  poolReturn(connection)
  return(pool)
}

import_rds <- function(path, pool, size = 2 ^ 5) {
  if (file_test("-d", path)) {
    list.files(
      path, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE
    ) %>%
      sapply(import_rds,pool = pool)
    return(NULL)
  }
  message(path)
  connection <- poolCheckout(pool)
  dbBegin(connection)
  on.exit(dbRollback(connection))

  sound_pulse <- readRDS(path)
  sound_pulse@Recording %>%
    mutate(rds = path, timestamp = as.integer(timestamp)) %>%
    dbWriteTable(conn = connection, name = "staging_recording",
                 overwrite = TRUE, temporary = TRUE)
  sound_pulse@Spectrogram %>%
    dbWriteTable(conn = connection, name = "staging_spectrogram",
                 temporary = TRUE)
  if (nrow(sound_pulse@Pulse)) {
    sound_pulse@Pulse %>%
      select(-shape) %>%
      dbWriteTable(conn = connection, name = "staging_pulse", temporary = TRUE)
    sound_pulse@Pulse %>%
      transmute(
        fingerprint,
        shape = purrr::map(
          shape,
          ~floor(.x * 256) %>%
            pmax(-255) %>%
            as.data.frame() %>%
            mutate(row = row_number())
        )
      ) %>%
      unnest() %>%
      rename_at(
        vars(starts_with("V")),
        ~gsub("V([0-9])", "\\1", .x) %>%
         as.integer() %>%
         sprintf(fmt = "col_%02i")
      ) %>%
      mutate_at(vars(starts_with("col_")), as.integer) %>%
      dbWriteTable(conn = connection, name = "staging_shape", temporary = TRUE)
  }

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

  if (nrow(sound_pulse@Pulse)) {
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

    sprintf("ss.col_%02i", seq_len(size)) %>%
      paste(collapse = ",  ") %>%
      sprintf(fmt = "
        INSERT INTO shape
        SELECT NULL AS id, pp.id AS pulse, ss.row, %s
        FROM pulse AS pp
        INNER JOIN staging_shape AS ss ON pp.fingerprint = ss.fingerprint
        LEFT JOIN shape AS ps ON ps.pulse = pp.id AND ps.row = ss.row
        WHERE ps.id IS NULL") %>%
      dbSendQuery(conn = connection) %>%
      dbClearResult()
  }

  dbRemoveTable(connection, "staging_recording")
  dbRemoveTable(connection, "staging_spectrogram")
  if (nrow(sound_pulse@Pulse)) {
    dbRemoveTable(connection, "staging_pulse")
    dbRemoveTable(connection, "staging_shape")
  }

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
    SELECT
      node,
      SUM(class IS NOT NULL) AS done,
      SUM(class IS NULL) AS to_do
    FROM prediction AS pr
    INNER JOIN pulse AS p ON pr.pulse = p.id
    GROUP BY node") %>%
    mutate(
      weight = (done + 0.1) ^ -1 * log(to_do)
    ) %>%
    sample_n(size = 1, weight = weight) %>%
    pull(node) %>%
    sprintf(fmt = "
      WITH cte_spectrogram AS (
        SELECT p.spectrogram
        FROM prediction AS pr
        INNER JOIN pulse AS p ON pr.pulse = p.id
        WHERE node = %1$i
        GROUP BY p.spectrogram
      ),
      cte_node AS (
        SELECT
          p.spectrogram,
          pr.node,
          SUM(p.class IS NOT NULL) AS done,
          SUM(p.class IS NULL) AS to_do
        FROM cte_spectrogram AS cs
        INNER JOIN pulse AS p ON cs.spectrogram = p.spectrogram
        INNER JOIN prediction AS pr ON pr.pulse = p.id
        GROUP BY p.spectrogram, pr.node
        HAVING SUM(p.class IS NULL) > 0
      )

      SELECT
        spectrogram,
        SUM(
          CASE
            WHEN node = %1$i
            THEN 10 * to_do / (done + 0.1)
            ELSE to_do / (done + 0.1)
            END
        ) AS weight
      FROM cte_node
      GROUP BY spectrogram") %>%
    dbGetQuery(conn = conn) %>%
    sample_n(size = 1, weight = weight) %>%
    pull(spectrogram) -> spectrogram
  poolReturn(conn)
  return(spectrogram)
}

read_spectrogram <- function(pool, spectrogram, size = 2 ^ 5) {
  conn <- poolCheckout(pool)
  sprintf("col_%02i", seq_len(size)) %>%
    paste(collapse = ", ") %>%
    sprintf(fmt = "
      SELECT pulse AS id, %s
      FROM pulse
      INNER JOIN shape ON pulse.id = shape.pulse
      WHERE spectrogram = %i
      ORDER BY pulse, row",
      spectrogram
    ) %>%
    dbGetQuery(conn = conn) %>%
    group_by(id) %>%
    nest(.key = "shape") %>%
    mutate(shape = purrr::map(shape, as.matrix) %>%
             purrr::map(`/`, 256)) %>%
    inner_join(
      sprintf("
        SELECT
          p.id,
          p.start_time,
          p.end_time,
          p.peak_time,
          p.start_frequency,
          p.end_frequency,
          p.peak_frequency,
          p.start_amplitude,
          p.peak_amplitude,
          p.select_amplitude,
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
      by = "id"
    ) %>%
    left_join(
      sprintf("
        WITH cte_class AS (
          SELECT pr.node, p.class, SUM(1 / pr.distance) AS weight
          FROM pulse AS p
          INNER JOIN prediction AS pr ON p.id = pr.pulse
          WHERE p.class IS NOT NULL
          GROUP BY pr.node, p.class
        ),
        cte_total AS (
          SELECT node, SUM(weight) AS tot
          FROM cte_class
          GROUP BY node
        ),
        cte_weight AS (
          SELECT
            c.node, c.class, cl.abbreviation,
            100 * c.weight / t.tot AS prediction
          FROM cte_class AS c
          INNER JOIN class AS cl ON c.class = cl.id
          INNER JOIN cte_total AS t ON c.node = t.node
        )

        SELECT p.id, w.class, w.abbreviation, w.prediction
        FROM cte_weight AS w
        INNER JOIN prediction AS pr ON pr.node = w.node
        INNER JOIN pulse AS p ON pr.pulse = p.id
        WHERE p.spectrogram = %i
        ORDER BY p.id, w.prediction DESC", spectrogram
      ) %>%
        dbGetQuery(conn = conn) %>%
        mutate(
          prediction = sprintf("%s (%.0f%%)", abbreviation, prediction)
        ) %>%
        group_by(id) %>%
        summarise(
          dominant = class[1],
          prediction = paste(prediction, collapse = ", ")
        ),
      by = "id"
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

get_shapes <- function(pool, size = 2 ^ 5, max_time_range = 100) {
  conn <- poolCheckout(pool)
  sprintf("s.col_%02i", seq_len(size)) %>%
    paste(collapse = ", ") %>%
    sprintf(fmt = "
      SELECT pulse, %s
      FROM shape AS s
      INNER JOIN pulse AS p ON s.pulse = p.id
      WHERE p.end_time - p.start_time < %f
      ORDER BY s.pulse, s.row",
      max_time_range
    ) %>%
    dbGetQuery(conn = conn) %>%
    group_by(pulse) %>%
    nest(.key = "shape") %>%
    mutate(shape = purrr::map(shape, as.matrix)) -> shapes
  sprintf("
    SELECT
      id,
      peak_frequency,
      peak_amplitude,
      end_time - start_time AS time_range,
      end_frequency - start_frequency AS frequency_range,
      select_amplitude,
      (peak_time - start_time) / (end_time - start_time) AS rel_time,
      (peak_frequency - start_frequency) / (end_frequency - start_frequency) AS
        rel_frequency
    FROM pulse
    WHERE end_time - start_time < %f",
    max_time_range
  ) %>%
    dbGetQuery(conn = conn) -> pulses
  poolReturn(conn)

  rn <- shapes$pulse
  sapply(shapes$shape, identity, simplify = "array") %>%
    pmax(0) %>%
    pmin(255) %>%
    `/`(255) -> shapes
  dimnames(shapes)[[1]] <- sprintf("row_%02i", seq_len(size))
  dimnames(shapes)[[3]] <- rn
  pulses %>%
    dplyr::select(-id) %>%
    as.matrix() %>%
    `dimnames<-`(list(pulses$id, colnames(pulses)[-1])) -> pulses
  prcomp(pulses, center = TRUE, scale. = TRUE)$x %>%
    round(2) %>%
    as.data.frame() %>%
    do.call(what = order) %>%
    `[`(seq(1, nrow(pulses), by = 10)) %>%
    sort() -> validation
  return(list(shapes = shapes, validation = validation))
}

reshape_pulse <- function(shapes, validation, inverse = FALSE) {
  if (!missing(validation)) {
    if (inverse) {
      shapes <- shapes[, , -validation, drop = FALSE]
    } else {
      shapes <- shapes[, , validation, drop = FALSE]
    }
  }
  output_dims <- c(prod(dim(shapes)[1:2]), dim(shapes)[3])
  shapes %>%
    array_reshape(dim = output_dims, order = "F") %>%
    t()
}


fit_autoencoder <- function(
  shapes, batch_size = 100L, epochs = 50L, epsilon_std = 1.0
) {
  original_dim <- prod(dim(shapes$shapes)[1:2])
  intermediate_dim <- 4 ^ (log(sqrt(original_dim), 2) - 2)
  latent_dim <- floor(log(dim(shapes$shapes)[3] - length(shapes$validation)))

  test <- reshape_pulse(shapes = shapes$shapes, validation = shapes$validation)
  train <- reshape_pulse(shapes = shapes$shapes,
                         validation = shapes$validation, inverse = TRUE)

  x <- layer_input(shape = original_dim)
  h <- layer_dense(x, intermediate_dim, activation = "relu")
  z_mean <- layer_dense(h, latent_dim)
  z_log_var <- layer_dense(h, latent_dim)

  sampling <- function(arg){
    z_mean <- arg[, seq_len(latent_dim)]
    z_log_var <- arg[, latent_dim + seq_len(latent_dim)]

    epsilon <- k_random_normal(
      shape = c(k_shape(z_mean)[[1]]),
      mean = 0,
      stddev = epsilon_std
    )

    z_mean + k_exp(z_log_var / 2) * epsilon
  }

  z <- layer_concatenate(list(z_mean, z_log_var)) %>%
    layer_lambda(sampling)

  decoder_h <- layer_dense(units = intermediate_dim, activation = "relu")
  decoder_mean <- layer_dense(units = original_dim, activation = "sigmoid")
  h_decoded <- decoder_h(z)
  x_decoded_mean <- decoder_mean(h_decoded)

  vae <- keras_model(x, x_decoded_mean)

  encoder <- keras_model(x, z_mean)

  decoder_input <- layer_input(shape = latent_dim)
  h_decoded_2 <- decoder_h(decoder_input)
  x_decoded_mean_2 <- decoder_mean(h_decoded_2)
  generator <- keras_model(decoder_input, x_decoded_mean_2)

  vae_loss <- function(x, x_decoded_mean) {
    xent_loss <- original_dim * loss_binary_crossentropy(x, x_decoded_mean)
    kl_loss <- k_mean(1 + z_log_var - k_square(z_mean) - k_exp(z_log_var),
                      axis = -1L) / -2
    xent_loss + kl_loss
  }

  vae %>%
    compile(optimizer = "rmsprop", loss = vae_loss) %>%
    fit(train, train, shuffle = TRUE,  epochs = epochs, batch_size = batch_size,
        validation_data = list(test, test)
    )
  return(list(encoder = encoder, decoder = generator))
}

update_autoencoder <- function(
  pool, size = 2 ^ 15, max_time_range = 100, batch_size = 100L, epochs = 50L,
  epsilon_std = 1.0
) {
  shapes <- get_shapes(
    pool = pool, size = size, max_time_range = max_time_range
  )
  autoencoder <- fit_autoencoder(
    shapes = shapes, batch_size = batch_size, epochs = epochs,
    epsilon_std = epsilon_std
  )

  conn <- poolCheckout(pool)

  tibble(
    type = c(1, -1),
    model = list(autoencoder$encoder, autoencoder$decoder)
  ) %>%
    mutate(model = purrr::map(model, serialize_model)) %>%
    dbWriteTable(conn = conn, name = "encoder", overwrite = TRUE)

  n_ae <- floor(log(dim(shapes$shapes)[3] - length(shapes$validation)))
  reshape_pulse(shapes$shapes) %>%
    predict(object = autoencoder$encoder) %>%
    `colnames<-`(sprintf("AE_%02i", seq_len(n_ae))) %>%
    data.frame() %>%
    mutate(pulse = as.integer(dimnames(shapes$shapes)[[3]])) %>%
    dbWriteTable(conn = conn, name = "autoencoder", overwrite = TRUE)

  poolReturn(conn)
}

fit_som <- function(pool, xdim, ydim) {
  conn <- poolCheckout(pool)
  dbListFields(conn, "autoencoder") %>%
    str_subset("^AE_") %>%
    paste(collapse = ", ") %>%
    sprintf(fmt = "
      SELECT
        id,
        peak_frequency,
        peak_amplitude - select_amplitude AS signal_amplitude,
        end_time - start_time AS time_range,
        end_frequency - start_frequency AS frequency_range,
        (peak_time - start_time) / (end_time - start_time) AS rel_time,
        (peak_frequency - start_frequency) / (end_frequency - start_frequency) AS
          rel_frequency,
        %s
      FROM pulse AS p
      INNER JOIN autoencoder AS a ON p.id = a.pulse") %>%
    dbGetQuery(conn = conn) -> dataset

  grid_dim <- floor(log(nrow(dataset)))
  if (missing(xdim)) {
    xdim <- grid_dim
  }
  if (missing(ydim)) {
    ydim <- grid_dim
  }
  dataset %>%
    dplyr::select(-id) %>%
    as.matrix() %>%
    `rownames<-`(dataset$id) %>%
    scale() %>%
    som(grid = somgrid(xdim = xdim, ydim = ydim)) -> sommap

  sprintf(
    "SELECT id FROM model WHERE x_dim = %i AND y_dim = %i", xdim, ydim
  ) %>%
    dbGetQuery(conn = conn) %>%
    pull(id) -> model_id
  if (is.null(model_id)) {
    tibble(id = NA, x_dim = xdim, y_dim = ydim) %>%
      dbWriteTable(conn = conn, name = "model", append = TRUE)
    sprintf(
      "SELECT id FROM model WHERE x_dim = %i AND y_dim = %i", xdim, ydim
    ) %>%
      dbGetQuery(conn = conn) %>%
      pull(id) -> model_id
  }
  if (dbExistsTable(conn = conn, "node")) {
    sommap$codes[[1]] %>%
      as.data.frame() %>%
      mutate(
        id = NA_integer_,
        model = model_id,
        x_pos = 1 + (row_number() - 1) %/% xdim,
        y_pos = 1 + (row_number() - 1) %% xdim
      ) %>%
      dbWriteTable(conn = conn, name = "node", append = TRUE)
  } else {
    sommap$codes[[1]] %>%
      as.data.frame() %>%
      mutate(
        id = row_number(),
        model = model_id,
        x_pos = 1 + (id - 1) %/% xdim,
        y_pos = 1 + (id - 1) %% xdim
      ) %>%
      dbWriteTable(
        conn = conn, name = "node",
        field.types = c(
          id = "INTEGER PRIMARY KEY AUTOINCREMENT", model = "INTEGER NOT NULL",
          x_pos = "INTEGER NOT NULL", y_pos = "INTEGER NOT NULL")
      )
  }
  sprintf("SELECT min(id) - 1 AS node FROM node WHERE model = %i", model_id) %>%
    dbGetQuery(conn = conn) %>%
    pull(node) -> start_node

  tibble(
    pulse = dataset$id,
    node = sommap$unit.classif + start_node,
    distance = sommap$distances
  ) %>%
    dbWriteTable(conn = conn, name = "prediction", append = TRUE)

  dbSendQuery(conn = conn, "VACUUM") %>%
    dbClearResult()

  poolReturn(conn)
}
