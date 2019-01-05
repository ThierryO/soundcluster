library(shiny)
library(soundcluster)
library(RSQLite)
library(raster)

find_start <- function(data, input) {
  start <- mean(
    data$pulse$start_time[data$current_pulse],
    data$pulse$end_time[data$current_pulse]
  ) - input$timeinterval / 2
  start <- pmin(
    pmax(0, start),
    data$maximum
  )
  floor(start)
}

shinyServer(function(input, output, session) {
  data <- reactiveValues(
    db = NULL,
    spectrograms = NULL,
    current_spectrogram = NULL,
    pulse = NULL,
    current_pulse = NULL,
    sonogram = NULL,
    clamped = NULL,
    title = NULL,
    maximum = NULL
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
        "SELECT name FROM behaviour ORDER BY name"
      )
      updateSelectInput(
        session,
        "behaviour",
        choices = c(behaviour$name, "[no behaviour]", "[new behaviour]")
      )
      species <- dbGetQuery(
        data$db@Connection,
        "SELECT name FROM species ORDER BY name"
      )
      updateSelectInput(
        session,
        "species",
        choices = c(species$name, "[no species]", "[new species]")
      )
      updateSelectInput(
        session,
        "species_parent",
        choices = c(species$name, "[no parent]"),
        selected = "[no parent]"
      )
      class <- dbGetQuery(
        data$db@Connection,
        "SELECT abbreviation FROM class ORDER BY abbreviation"
      )
      updateSelectInput(
        session,
        "class",
        choices = c(class$abbreviation, "[new class]")
      )
      data$spectrograms <- dbGetQuery(
        data$db@Connection,
        "SELECT s.id, count(p.id) AS n_pulse
        FROM spectrogram AS s LEFT JOIN pulse AS p on s.id = p.spectrogram
        GROUP BY s.id
        ORDER BY count(p.id) DESC"
      )
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
            id, start_time, end_time, start_frequency, end_frequency, peak_time,
            peak_frequency
          FROM pulse
          WHERE spectrogram = %s
          ORDER BY start_time",
          dbQuoteLiteral(data$db@Connection, data$current_spectrogram)
        )
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
      data$maximum <- floor(max(sonogram@SpecGram$t) - input$timeinterval)

      updateSliderInput(
        session,
        "starttime",
        value = find_start(data, input),
        min = floor(min(sonogram@SpecGram$t)),
        max = data$maximum
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
    breaks <- pretty(input$amplitude[1]:input$amplitude[2], 20)
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
    abline(
      h = c(20, 30, 40, 50, 60, 80, 90, 110),
      lty = 2,
      col = "white",
      lwd = 2
    )
    abline(h = c(18, 21, 27, 35), lty = 3, col = "white", lwd = 2)
    segments(
      x0 = c(data$pulse$start_time, data$pulse$start_time,
             data$pulse$start_time, data$pulse$end_time),
      x1 = c(data$pulse$start_time, data$pulse$end_time,
             data$pulse$end_time, data$pulse$end_time),
      y0 = c(data$pulse$start_frequency, data$pulse$start_frequency,
             data$pulse$end_frequency, data$pulse$start_frequency),
      y1 = c(data$pulse$end_frequency, data$pulse$start_frequency,
             data$pulse$end_frequency, data$pulse$end_frequency),
      col = "black",
      lwd = 1
    )
    isolate(
      segments(
        x0 = c(
          data$pulse$start_time[data$current_pulse],
          data$pulse$start_time[data$current_pulse],
          data$pulse$start_time[data$current_pulse],
          data$pulse$end_time[data$current_pulse]
        ),
        x1 = c(
          data$pulse$start_time[data$current_pulse],
          data$pulse$end_time[data$current_pulse],
          data$pulse$end_time[data$current_pulse],
          data$pulse$end_time[data$current_pulse]
        ),
        y0 = c(
          data$pulse$start_frequency[data$current_pulse],
          data$pulse$start_frequency[data$current_pulse],
          data$pulse$end_frequency[data$current_pulse],
          data$pulse$start_frequency[data$current_pulse]
        ),
        y1 = c(
          data$pulse$end_frequency[data$current_pulse],
          data$pulse$start_frequency[data$current_pulse],
          data$pulse$end_frequency[data$current_pulse],
          data$pulse$end_frequency[data$current_pulse]
        ),
        col = "red",
        lwd = 2
      )
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
      updateSliderInput(
        session,
        "starttime",
        value = find_start(data, input)
      )
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
      updateSliderInput(
        session,
        "starttime",
        value = find_start(data, input)
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
        step = input$timeinterval
      )
    }
  )

  observeEvent(
    input$new_behaviour,
    {
      if (!input$behaviour_name %in% c("", "[no behaviour]")) {
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
            "SELECT name FROM behaviour ORDER BY name"
          )
          updateSelectInput(
            session,
            "behaviour",
            choices = c(behaviour$name, "[no behaviour]", "[new behaviour]"),
            selected = "[no behaviour]"
          )
        }
      }
      updateTextInput(session, "behaviour_name", value = "")
    }
  )

  observeEvent(
    input$new_species,
    {
      if (!input$species_name %in% c("", "[no species]")) {
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
          if (input$species_parent == "[no parent]") {
            parent <- "NULL"
          } else {
            parent <- as.character(
              dbGetQuery(
                data$db@Connection,
                sprintf(
                  "SELECT id FROM species WHERE name = %s",
                  dbQuoteString(data$db@Connection, input$species_parent)
                )
              )$id
            )
          }
          if (is.na(input$species_gbif)) {
            gbif <- "NULL"
          } else {
            gbif <- dbQuoteLiteral(data$db@Connection, input$species_gbif)
          }
          res <- dbSendQuery(
            data$db@Connection,
            sprintf(
              "INSERT INTO species (name, parent, gbif) VALUES (%s, %s, %s)",
              dbQuoteString(data$db@Connection, input$species_name),
              parent,
              gbif
            )
          )
          dbClearResult(res)
          species <- dbGetQuery(
            data$db@Connection,
            "SELECT name FROM species ORDER BY name"
          )
          updateSelectInput(
            session,
            "species",
            choices = c(species$name, "[no species]", "[new species]"),
            selected = "[no species]"
          )
          updateSelectInput(
            session,
            "species_parent",
            choices = c(species$name, "[no parent]"),
            selected = "[no parent]"
          )
        }
      }
      updateTextInput(session, "species_name", value = "")
      updateNumericInput(session, "species_gbif", value = NA)
      updateSelectInput(session, "species_parent", selected = "[no parent]")
    }
  )

  observeEvent(
    input$new_class,
    {
      if (
        input$class_abbrev != "" &&
        input$species != "[new species]" &&
        input$behaviour != "[new behaviour]"
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
          if (input$species == "[no species]") {
            species == "NULL"
          } else {
            species <- as.character(
              dbGetQuery(
                data$db@Connection,
                sprintf(
                  "SELECT id FROM species WHERE name = %s",
                  dbQuoteString(data$db@Connection, input$species)
                )
              )$id
            )
          }
          if (input$behaviour == "[no behaviour]") {
            behaviour == "NULL"
          } else {
            behaviour <- as.character(
              dbGetQuery(
                data$db@Connection,
                sprintf(
                  "SELECT id FROM behaviour WHERE name = %s",
                  dbQuoteString(data$db@Connection, input$behaviour)
                )
              )$id
            )
          }
          res <- dbSendQuery(
            data$db@Connection,
            sprintf(
              "INSERT INTO class (abbreviation, description, species, behaviour)
              VALUES (%s, %s, %s, %s)",
              dbQuoteString(data$db@Connection, input$class_abbrev),
              dbQuoteString(data$db@Connection, input$class_description),
              species,
              behaviour
            )
          )
          dbClearResult(res)
        }
        class <- dbGetQuery(
          data$db@Connection,
          "SELECT abbreviation FROM class ORDER BY abbreviation"
        )
        updateSelectInput(
          session,
          "class",
          choices = c(class$abbreviation, "[new class]")
        )
      }
      updateTextInput(session, "class_abbrev", value = "")
      updateTextInput(session, "class_description", value = "")
    }
  )
})
