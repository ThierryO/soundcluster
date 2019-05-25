library(shiny)
library(soundcluster)
library(pool)
library(RSQLite)
library(raster)

shinyServer(function(input, output, session) {
  data <- reactiveValues(
    clamped = NULL,
    current_pulse = NULL,
    current_spectrogram = NULL,
    maximum = NULL,
    nodes = NULL,
    pulse = NULL,
    sonogram = NULL,
    spectrograms = NULL,
    title = NULL
  )

  observeEvent(input$dt_refresh, {
    set_dropdown(session = session, pool = pool)
    dbGetQuery(
      pool,
      "WITH cte_pulse AS (
        SELECT
          spectrogram, COUNT(id) AS pulses, SUM(class IS NOT NULL) AS labeled
        FROM pulse GROUP BY spectrogram
      )
      SELECT
        s.id AS spectrogram, s.fingerprint, COALESCE(cp.pulses, 0) AS pulses,
        COALESCE(cp.labeled, 0) AS labeled, ROUND(r.duration, 2) AS duration,
        ROUND(r.duration / r.total_duration, 2) AS fraction,
        d.make || ' ' || d.model || COALESCE(' ' || d.serial, '') AS device,
        r.filename
      FROM spectrogram AS s
      INNER JOIN recording AS r ON s.recording = r.id
      INNER JOIN device AS d ON r.device = d.id
      LEFT JOIN cte_pulse AS cp ON s.id = cp.spectrogram
      ORDER BY labeled, pulses - labeled DESC, filename"
    ) -> data$spectrograms
  })

  observeEvent(input$refresh_model, {
    set_dropdown(session = session, pool = pool)
    dbGetQuery(
      pool,
      "WITH cte_model AS (
        SELECT
          m.id, m.grid_x, m.grid_y,
          COUNT(pr.pulse) AS predictions
        FROM model AS m
        INNER JOIN node AS n ON m.id = n.model
        LEFT JOIN prediction AS pr ON n.id = pr.node
        GROUP BY m.id, m.grid_x, m.grid_y
      )

      SELECT
        id,
        id || ': ' || grid_x || 'x' || grid_y || ' (' || predictions || ')'
          AS label
      FROM cte_model
      ORDER BY id"
    ) -> models

    updateSelectInput(
      session,
      "node_model",
      choices = setNames(models$id, models$label),
      selected = head(models$id, 1)
    )
  })

  observeEvent(input$node_model, {
    if (input$node_model == "") {
      return(NULL)
    }
    connection <- poolCheckout(pool)
    sql <- sprintf(
      "SELECT
        n.id AS node, n.x, n.y,
        COUNT(pr.pulse) AS pulses,
        SUM(p.class IS NOT NULL) AS labeled,
        SUM(p.class IS NULL) AS unlabeled,
        ROUND(MIN(distance), 2) AS min_dist,
        ROUND(MAX(distance), 2) AS max_dist
      FROM prediction AS pr
      INNER JOIN node AS n ON pr.node = n.id
      INNER JOIN pulse AS p on pr.pulse = p.id
      WHERE n.model = %s
      GROUP BY n.id, n.x, n.y
      ORDER BY labeled, unlabeled DESC",
      dbQuoteLiteral(
        connection,
        input$node_model
      )
    )
    dbGetQuery(connection, sql) -> data$nodes
    poolReturn(connection)

    output$dt_node_quality <- DT::renderDataTable(
      DT::datatable(
        data$nodes,
        rownames = FALSE,
        selection = "single",
        options = list(lengthMenu = 5, pageLength = 5)
      )
    )
  })

  observeEvent(data$spectrograms, {
    if (is.null(data$spectrograms)) {
      return(NULL)
    }
    output$dt_spectrogram <- DT::renderDataTable(
      DT::datatable(
        data$spectrograms[, -1:-2],
        rownames = FALSE,
        selection = "single",
        options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 15)
      )
    )
  })

  observeEvent(input$dt_node_quality_rows_selected, {
    if (is.null(input$dt_node_quality_rows_selected)) {
      return(NULL)
    }

    connection <- poolCheckout(pool)
    sql <- sprintf(
      "WITH cte_spectrogram AS (
        SELECT
          p.spectrogram,
          COUNT(pr.pulse) AS predictions,
          SUM(p.class IS NULL) AS unlabeled,
          MIN(pr.distance) AS min_dist,
          MAX(pr.distance) AS max_dist
        FROM prediction AS pr
        INNER JOIN pulse AS p ON pr.pulse = p.id
        WHERE pr.node = %s
        GROUP BY p.spectrogram
      )

      SELECT
        s.id AS spectrogram, s.fingerprint, cs.predictions, cs.unlabeled,
        ROUND(cs.min_dist, 1) AS 'min. distance',
        ROUND(cs.max_dist, 1) AS 'max. distance',
        ROUND(r.duration, 2) AS duration,
        ROUND(r.duration / r.total_duration, 2) AS 'fraction used',
        d.make || ' ' || d.model || COALESCE(' ' || d.serial, '') AS device,
        r.filename
      FROM cte_spectrogram AS cs
      INNER JOIN spectrogram AS s ON cs.spectrogram = s.id
      INNER JOIN recording AS r ON s.recording = r.id
      INNER JOIN device AS d ON r.device = d.id
      ORDER BY unlabeled DESC, max_dist DESC
      ",
      dbQuoteLiteral(
        connection,
        data$nodes$node[input$dt_node_quality_rows_selected]
      )
    )
    dbGetQuery(connection, sql) -> data$spectrograms
    poolReturn(connection)

    output$dt_node_quality_spectrogram <- DT::renderDataTable(
      DT::datatable(
        data$spectrograms[, -1:-2],
        rownames = FALSE,
        selection = "single",
        options = list(lengthMenu = 5, pageLength = 5)
      )
    )
  })

  observeEvent(input$dt_node_quality_spectrogram_rows_selected, {
    if (is.null(input$dt_node_quality_spectrogram_rows_selected)) {
      return(NULL)
    }
    data$current_spectrogram <- data$spectrograms$spectrogram[
      input$dt_node_quality_spectrogram_rows_selected
    ]
    data <- set_pulses(
      session = session, pool = pool, data = data, input = input
    )
  })

  observeEvent(input$dt_spectrogram_rows_selected, {
    if (is.null(input$dt_spectrogram_rows_selected)) {
      return(NULL)
    }
    data$current_spectrogram <- data$spectrograms$spectrogram[
      input$dt_spectrogram_rows_selected
    ]
    data <- set_pulses(
      session = session, pool = pool, data = data, input = input
    )
  })

  observeEvent(
    data$sonogram,
    {
      if (is.null(data$sonogram)) {
        return(NULL)
      }
      amplitude_range <- pretty(cellStats(data$sonogram, "range"), 10)
      data$clamped <- NULL
      updateSliderInput(
        session,
        "amplitude",
        value = c(0, max(amplitude_range)),
        min = min(amplitude_range),
        max = max(amplitude_range)
      )
    }
  )

  observeEvent(
    input$amplitude,
    {
      if (is.null(data$sonogram)) {
        data$clamped <- NULL
      } else {
        data$clamped <- clamp(
          data$sonogram,
          lower = input$amplitude[1],
          upper = input$amplitude[2]
        )
      }
    }
  )

  output$sonogram <- renderPlot({
    if (is.null(data$clamped) || input$starttime < 0) {
      return(NULL)
    }
    isolate(
      breaks <- pretty(input$amplitude[1]:input$amplitude[2], 20)
    )
    plot(
      data$clamped,
      breaks = breaks,
      col = topo.colors(length(breaks)),
      xlim = input$starttime + c(0, input$timeinterval),
      ylim = input$frequency,
      xlab = "time (ms)",
      ylab = "frequency (kHz)",
      asp = input$aspect,
      main = data$title
    )
    rect(
      xleft = data$pulse$start_time,
      xright = data$pulse$end_time,
      ybottom = data$pulse$start_frequency,
      ytop = data$pulse$end_frequency,
      col = data$pulse$colour,
      border = data$pulse$colour,
      density = 2,
      angle = data$pulse$angle,
      lty = data$pulse$linetype,
      lwd = 2
    )
    rect(
      xleft = data$pulse$start_time[data$current_pulse],
      xright = data$pulse$end_time[data$current_pulse],
      ybottom = data$pulse$start_frequency[data$current_pulse],
      ytop = data$pulse$end_frequency[data$current_pulse],
      col = data$pulse$colour[data$current_pulse],
      border = data$pulse$colour[data$current_pulse],
      density = 2,
      angle = data$pulse$angle[data$current_pulse],
      lty = data$pulse$linetype[data$current_pulse],
      lwd = 4
    )
    abline(
      h = c(20, 30, 40, 50, 60, 80, 90, 110),
      lty = 2,
      col = "white",
      lwd = 2
    )
    abline(h = c(18, 21, 27, 35), lty = 3, col = "white", lwd = 2)
    points(
      data$pulse$peak_time,
      data$pulse$peak_frequency,
      col = data$pulse$colour,
      pch = 13,
      cex = 2
    )
  })

  observeEvent(
    input$step_backward,
    {
      if (is.null(data$current_pulse)) {
        return(NULL)
      }
      if (data$current_pulse == 1) {
        return(NULL)
      }
      data$current_pulse  <- data$current_pulse - 1
      if (input$skip_labeled) {
        while (
          data$current_pulse > 1 &&
          data$pulse$class[data$current_pulse] > 0
        ) {
          data$current_pulse  <- data$current_pulse - 1
        }
      }
    }
  )

  observeEvent(
    input$step_forward,
    {
      if (is.null(data$current_pulse)) {
        return(NULL)
      }
      if (data$current_pulse == nrow(data$pulse)) {
        return(NULL)
      }
      data$current_pulse  <- data$current_pulse + 1

      if (input$skip_labeled) {
        while (
          data$current_pulse < nrow(data$pulse) &&
          data$pulse$class[data$current_pulse] > 0
        ) {
          data$current_pulse  <- data$current_pulse + 1
        }
      }
    }
  )

  observeEvent(
    data$current_pulse,
    {
      if (is.null(data$current_pulse)) {
        return(NULL)
      }
      updateSliderInput(
        session,
        "starttime",
        value = find_start(data, input)
      )
      updateSelectInput(
        session,
        "class_id",
        selected = as.character(data$pulse$class[data$current_pulse])
      )
    }
  )

  output$prediction <- renderText({
    if (input$node_model == "") {
      return(NULL)
    }
    data$pulse$classification[data$current_pulse]
  })

  observeEvent(
    input$timeinterval,
    {
      if (is.null(data$current_pulse)) {
        return(NULL)
      }
      updateSliderInput(
        session,
        "starttime",
        value = find_start(data, input),
        step = input$timeinterval,
        max = data$maximum - input$timeinterval
      )
    }
  )

  observeEvent(
    input$new_behaviour,
    {
      if (input$behaviour_name != "") {
        connection <- poolCheckout(pool)
        current <- dbGetQuery(
          connection,
          sprintf(
            "SELECT count(id) AS n
            FROM behaviour
            WHERE name = %s",
            dbQuoteString(connection, input$behaviour_name)
          )
        )$n
        if (current == 0) {
          res <- dbSendQuery(
            connection,
            sprintf(
              "INSERT INTO behaviour (name) VALUES (%s)",
              dbQuoteString(connection, input$behaviour_name)
            )
          )
          dbClearResult(res)
          behaviour <- dbGetQuery(
            connection,
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
        }
        poolReturn(connection)
      }
      updateTextInput(session, "behaviour_name", value = "")
    }
  )

  observeEvent(
    input$new_species,
    {
      if (input$species_name != "") {
        connection <- poolCheckout(pool)
        current <- dbGetQuery(
          connection,
          sprintf(
            "SELECT count(id) AS n
            FROM species
            WHERE name = %s",
            dbQuoteString(connection, input$species_name)
          )
        )$n
        if (current == 0) {
          if (input$species_parent == "0") {
            this_parent <- "NULL"
          } else {
            this_parent <- input$species_parent
          }
          if (is.na(input$species_gbif)) {
            this_gbif <- "NULL"
          } else {
            this_gbif <- dbQuoteLiteral(connection, input$species_gbif)
          }
          res <- dbSendQuery(
            connection,
            sprintf(
              "INSERT INTO species (name, parent, gbif) VALUES (%s, %s, %s)",
              dbQuoteString(connection, input$species_name),
              this_parent,
              this_gbif
            )
          )
          dbClearResult(res)
          species <- dbGetQuery(
            connection,
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
            choices = c(setNames(species$id, species$name), "[no parent]" = "0")
          )
        }
        poolReturn(connection)
      }
      updateTextInput(session, "species_name", value = "")
      updateNumericInput(session, "species_gbif", value = NA)
      updateSelectInput(session, "species_parent", selected = 0)
    }
  )

  observeEvent(
    input$new_class,
    {
      if (
        input$class_abbrev != "" &&
        input$species != "-1" &&
        input$behaviour != "-1"
      ) {
        connection <- poolCheckout(pool)
        current <- dbGetQuery(
          connection,
          sprintf(
            "SELECT count(id) AS n
            FROM class
            WHERE abbreviation = %s",
            dbQuoteString(connection, input$class_abbrev)
          )
        )$n
        if (current == 0) {
          if (input$species == "0") {
            this_species <- "NULL"
          } else {
            this_species <- input$species
          }
          if (input$behaviour == "0") {
            this_behaviour <- "NULL"
          } else {
            this_behaviour <- input$behaviour
          }
          res <- dbSendQuery(
            connection,
            sprintf(
              "INSERT INTO class
                (abbreviation, description, species, behaviour, colour,
                linetype, angle)
              VALUES (%s, %s, %s, %s, %s, %s, %s)",
              dbQuoteString(connection, input$class_abbrev),
              dbQuoteString(connection, input$class_description),
              this_species,
              this_behaviour,
              dbQuoteString(connection, input$class_color),
              dbQuoteString(connection, input$class_linetype),
              dbQuoteLiteral(connection, input$class_angle)
            )
          )
          dbClearResult(res)
        }
        class <- dbGetQuery(
          connection,
          "SELECT id, abbreviation
          FROM class ORDER BY abbreviation"
        )
        poolReturn(connection)
        updateSelectInput(
          session,
          "class_id",
          choices = c(
            setNames(class$id, class$abbreviation),
            "[no class]" = "0", "[new class]" = "-1"
          ),
          selected = "0"
        )
      }
      updateTextInput(session, "class_abbrev", value = "")
      updateTextInput(session, "class_description", value = "")
    }
  )

  observeEvent(
    input$use_dominant,
    {
      if (!"dominant" %in% colnames(data$pulse)) {
        return(NULL)
      }
      if (is.na(data$pulse$dominant[data$current_pulse])) {
        return(NULL)
      }
      connection <- poolCheckout(pool)
      res <- dbSendQuery(
        connection,
        sprintf(
          "UPDATE pulse SET class = %s WHERE id = %s",
          dbQuoteLiteral(connection, data$pulse$dominant[data$current_pulse]),
          dbQuoteLiteral(connection, data$pulse$id[data$current_pulse])
        )
      )
      dbClearResult(res)
      data$pulse <- read_pulse(
        input = input, data = data, connection = connection
      )
      poolReturn(connection)
      if (data$current_pulse == nrow(data$pulse)) {
        return(NULL)
      }
      data$current_pulse  <- data$current_pulse + 1
      if (input$skip_labeled) {
        while (
          data$current_pulse < nrow(data$pulse) &&
          data$pulse$class[data$current_pulse] > 0
        ) {
          data$current_pulse  <- data$current_pulse + 1
        }
      }
    }
  )

  observeEvent(
    input$update_class,
    {
      if (as.integer(input$class_id) < 1) {
        return(NULL)
      }
      connection <- poolCheckout(pool)
      res <- dbSendQuery(
        connection,
        sprintf(
          "UPDATE pulse SET class = %s WHERE id = %s",
          dbQuoteLiteral(connection, as.integer(input$class_id)),
          dbQuoteLiteral(connection, data$pulse$id[data$current_pulse])
        )
      )
      dbClearResult(res)
      data$pulse <- read_pulse(
        input = input, data = data, connection = connection
      )
      poolReturn(connection)
      if (data$current_pulse == nrow(data$pulse)) {
        return(NULL)
      }
      data$current_pulse  <- data$current_pulse + 1
      if (input$skip_labeled) {
        while (
          data$current_pulse < nrow(data$pulse) &&
          data$pulse$class[data$current_pulse] > 0
        ) {
          data$current_pulse  <- data$current_pulse + 1
        }
      }
    }
  )
})
