library(shiny)
library(soundcluster)
library(RSQLite)
library(raster)

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

shinyServer(function(input, output, session) {
  data <- reactiveValues(
    clamped = NULL,
    current_pulse = NULL,
    current_spectrogram = NULL,
    db = NULL,
    maximum = NULL,
    pulse = NULL,
    sonogram = NULL,
    spectrograms = NULL,
    title = NULL
  )

  observeEvent(
    input$path,
    data$db <- connect_db(input$path)
  )

  observeEvent(
    data$db,
    {
      if (is.null(data$db)) {
        return(NULL)
      }
      behaviour <- dbGetQuery(
        data$db@Connection,
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
        data$db@Connection,
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
        data$db@Connection,
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

      data$current_spectrogram <- dbGetQuery(
        data$db@Connection,
        "WITH cte_node AS (
          SELECT pr.node, SUM(p.class IS NOT NULL) AS done
          FROM pulse AS p INNER JOIN prediction AS pr ON p.id = pr.pulse
          GROUP BY pr.node
        ),
        cte_spectrogram AS (
          SELECT
            p.spectrogram, pr.node, CAST(SUM(p.class IS NULL) AS REAL) AS to_do
          FROM pulse AS p INNER JOIN prediction AS pr ON p.id = pr.pulse
          GROUP BY p.spectrogram, pr.node
        )

        SELECT
          cs.spectrogram,
          SUM(cs.to_do / (1 + cn.done)) AS weight,
          (CAST(random() AS REAL) + 9223372036854775808) /
            (9223372036854775808 + 9223372036854775807) AS random
        FROM cte_spectrogram AS cs
        INNER JOIN cte_node AS cn ON cs.node = cn.node
        GROUP BY cs.spectrogram
        ORDER BY weight * random DESC
        LIMIT 1"
      )$spectrogram
    }
  )

  observeEvent(
    data$spectrograms,
    {
      if (is.null(data$spectrograms)) {
        return(NULL)
      }
      data$current_spectrogram <- sample(
        data$spectrograms$id, 1, prob = data$spectrograms$n_pulse
      )
    }
  )

  observeEvent(
    data$current_spectrogram,
    {
      if (is.null(data$current_spectrogram)) {
        return(NULL)
      }
      meta <- dbGetQuery(
        data$db@Connection,
        sprintf(
          "SELECT
            s.fingerprint, s.window_ms, s.overlap,
            r.duration, r.filename, d.te_factor,
            CASE WHEN d.left_channel = 1 THEN 'left' ELSE 'right' END AS channel
          FROM spectrogram AS s
          INNER JOIN recording AS r ON s.recording = r.id
          INNER JOIN device AS d ON r.device = d.id
          WHERE s.id = %s",
          dbQuoteLiteral(data$db@Connection, data$current_spectrogram)
        )
      )
      data$pulse <- dbGetQuery(
        data$db@Connection,
        sprintf(
          "SELECT
            pulse.id, start_time, end_time, start_frequency, end_frequency,
            peak_time, peak_frequency,
            CASE WHEN class IS NULL THEN 0 ELSE class END AS class,
            CASE WHEN color IS NULL THEN 'black' ELSE color END AS color,
            CASE
              WHEN linetype IS NULL THEN 'solid' ELSE linetype END AS linetype,
            CASE WHEN angle IS NULL THEN 45 ELSE angle END AS angle
          FROM pulse
          LEFT JOIN class ON pulse.class = class.id
          WHERE spectrogram = %s
          ORDER BY start_time",
          dbQuoteLiteral(data$db@Connection, data$current_spectrogram)
        )
      )

      max_freq <- ceiling(max(c(data$pulse$end_frequency, 150)))
      updateSliderInput(
        session, "frequency", value = c(0, max_freq), max = max_freq
      )
      updateSliderInput(
        session, "timeinterval",
        value = max_freq * as.numeric(input$aspect) * 1.35
      )
      data$title <- meta$filename
      data$current_pulse <- 1
      wav <- sound_wav(
        filename = meta$filename, channel = meta$channel,
        te_factor = meta$te_factor, max_length = meta$duration
      )
      sonogram <- wav2spectrogram(
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
    }
  )

  observeEvent(
    data$sonogram,
    {
      if (is.null(data$sonogram)) {
        return(NULL)
      }
      amplitude_range <- pretty(cellStats(data$sonogram, "range"), 10)
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
      col = data$pulse$color,
      border = data$pulse$color,
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
      col = data$pulse$color[data$current_pulse],
      border = data$pulse$color[data$current_pulse],
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
    }
  )

  observeEvent(
    data$current_pulse,
    {
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

  observeEvent(
    input$timeinterval,
    {
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
        current <- dbGetQuery(
          data$db@Connection,
          sprintf(
            "SELECT count(id) AS n
            FROM behaviour
            WHERE name = %s",
            dbQuoteString(data$db@Connection, input$behaviour_name)
          )
        )$n
        if (current == 0) {
          res <- dbSendQuery(
            data$db@Connection,
            sprintf(
              "INSERT INTO behaviour (name) VALUES (%s)",
              dbQuoteString(data$db@Connection, input$behaviour_name)
            )
          )
          dbClearResult(res)
          behaviour <- dbGetQuery(
            data$db@Connection,
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
      }
      updateTextInput(session, "behaviour_name", value = "")
    }
  )

  observeEvent(
    input$new_species,
    {
      if (input$species_name != "") {
        current <- dbGetQuery(
          data$db@Connection,
          sprintf(
            "SELECT count(id) AS n
            FROM species
            WHERE name = %s",
            dbQuoteString(data$db@Connection, input$species_name)
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
            this_gbif <- dbQuoteLiteral(data$db@Connection, input$species_gbif)
          }
          res <- dbSendQuery(
            data$db@Connection,
            sprintf(
              "INSERT INTO species (name, parent, gbif) VALUES (%s, %s, %s)",
              dbQuoteString(data$db@Connection, input$species_name),
              this_parent,
              this_gbif
            )
          )
          dbClearResult(res)
          species <- dbGetQuery(
            data$db@Connection,
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
        current <- dbGetQuery(
          data$db@Connection,
          sprintf(
            "SELECT count(id) AS n
            FROM class
            WHERE abbreviation = %s",
            dbQuoteString(data$db@Connection, input$class_abbrev)
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
            data$db@Connection,
            sprintf(
              "INSERT INTO class
                (abbreviation, description, species, behaviour, color, linetype,
                angle)
              VALUES (%s, %s, %s, %s, %s, %s, %s)",
              dbQuoteString(data$db@Connection, input$class_abbrev),
              dbQuoteString(data$db@Connection, input$class_description),
              this_species,
              this_behaviour,
              dbQuoteString(data$db@Connection, input$class_color),
              dbQuoteString(data$db@Connection, input$class_linetype),
              dbQuoteLiteral(data$db@Connection, input$class_angle)
            )
          )
          dbClearResult(res)
        }
        class <- dbGetQuery(
          data$db@Connection,
          "SELECT id, abbreviation
          FROM class ORDER BY abbreviation"
        )
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
    input$update_class,
    {
      if (as.integer(input$class_id) < 1) {
        return(NULL)
      }
      res <- dbSendQuery(
        data$db@Connection,
        sprintf(
          "UPDATE pulse SET class = %s WHERE id = %s",
          dbQuoteLiteral(data$db@Connection, as.integer(input$class_id)),
          dbQuoteLiteral(
             data$db@Connection, data$pulse$id[data$current_pulse]
          )
        )
      )
      dbClearResult(res)
      data$pulse <- dbGetQuery(
        data$db@Connection,
        sprintf(
          "SELECT
            pulse.id, start_time, end_time, start_frequency, end_frequency,
            peak_time, peak_frequency,
            CASE WHEN class IS NULL THEN 0 ELSE class END AS class,
            CASE WHEN color IS NULL THEN 'black' ELSE color END AS color,
            CASE
              WHEN linetype IS NULL THEN 'solid' ELSE linetype END AS linetype,
            CASE WHEN angle IS NULL THEN 45 ELSE angle END AS angle
          FROM pulse
          LEFT JOIN class ON pulse.class = class.id
          WHERE spectrogram = %s
          ORDER BY start_time",
          dbQuoteLiteral(data$db@Connection, data$current_spectrogram)
        )
      )
      if (data$current_pulse == nrow(data$pulse)) {
        return(NULL)
      }
      data$current_pulse  <- data$current_pulse + 1
    }
  )
})
