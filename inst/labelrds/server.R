library(shiny)
library(soundcluster)
library(dplyr)
library(raster)

shinyServer(function(input, output, session) {
  data <- reactiveValues(
    current_pulse = NULL,
    maximum = NULL,
    raster = NULL,
    spectrogram = NULL
  )

  observeEvent(input$new_spectrogram, {
    set_dropdown(session = session, pool = pool)
    data$spectrogram <- sample_spectrogram(pool)
  })

  observeEvent(input$remodel, {
    add_prediction(pool)
  })

  observeEvent(data$spectrogram, {
    read_spectrogram(pool, data$spectrogram) %>%
      mutate(
        raster = shape2raster(.)
      ) -> data$raster
    data$current_pulse <- 1
    if (input$skip_labeled) {
      while (
        data$current_pulse < nrow(data$raster) &&
        data$raster$class[data$current_pulse] > 0
      ) {
        data$current_pulse  <- data$current_pulse + 1
      }
    }
    data$maximum <- max(data$raster$end_time)
    min_freq <- floor(min(data$raster$start_frequency))
    max_freq <- ceiling(max(data$raster$end_frequency))
    updateSliderInput(
      session, "frequency", value = c(min_freq, max_freq), max = max_freq
    )
    updateSliderInput(
      session, "timeinterval",
      value = max_freq * as.numeric(input$aspect) * 1.35
    )
    updateSliderInput(
        session,
        "amplitude",
        value = c(
          min(data$raster$select_amplitude),
          max(data$raster$peak_amplitude)
        )
      )
  })

  observeEvent(
    input$step_forward,
    {
      if (is.null(data$current_pulse)) {
        return(NULL)
      }
      if (data$current_pulse == nrow(data$raster)) {
        return(NULL)
      }
      data$current_pulse  <- data$current_pulse + 1
      if (input$skip_labeled) {
        while (
          data$current_pulse < nrow(data$raster) &&
          data$raster$class[data$current_pulse] > 0
        ) {
          data$current_pulse  <- data$current_pulse + 1
        }
      }
    }
  )

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
          data$raster$class[data$current_pulse] > 0
        ) {
          data$current_pulse  <- data$current_pulse - 1
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
        selected = as.character(data$raster$class[data$current_pulse])
      )
    }
  )

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
          dbQuoteLiteral(connection, data$raster$id[data$current_pulse])
        )
      )
      dbClearResult(res)
      data$raster$class[data$current_pulse] <- as.integer(input$class_id)
      dbGetQuery(
        connection,
        sprintf(
          "SELECT colour, linetype, angle FROM class WHERE id = %s",
          dbQuoteLiteral(connection, as.integer(input$class_id))
        )
      ) -> data$raster[data$current_pulse, c("colour", "linetype", "angle")]
      poolReturn(connection)
      if (data$current_pulse == nrow(data$raster)) {
        return(NULL)
      }
      data$current_pulse  <- data$current_pulse + 1
      if (input$skip_labeled) {
        while (
          data$current_pulse < nrow(data$raster) &&
          data$raster$class[data$current_pulse] > 0
        ) {
          data$current_pulse  <- data$current_pulse + 1
        }
      }
    }
  )

  observeEvent(
    input$use_dominant,
    {
      if (!"dominant" %in% colnames(data$raster)) {
        return(NULL)
      }
      if (is.na(data$raster$dominant[data$current_pulse])) {
        return(NULL)
      }
      connection <- poolCheckout(pool)
      res <- dbSendQuery(
        connection,
        sprintf(
          "UPDATE pulse SET class = %s WHERE id = %s",
          dbQuoteLiteral(connection, data$raster$dominant[data$current_pulse]),
          dbQuoteLiteral(connection, data$raster$id[data$current_pulse])
        )
      )
      dbClearResult(res)
      data$raster$class[data$current_pulse] <-
        data$raster$dominant[data$current_pulse]
      dbGetQuery(
        connection,
        sprintf(
          "SELECT colour, linetype, angle FROM class WHERE id = %s",
          dbQuoteLiteral(connection, data$raster$dominant[data$current_pulse])
        )
      ) -> data$raster[data$current_pulse, c("colour", "linetype", "angle")]
      poolReturn(connection)
      if (data$current_pulse == nrow(data$raster)) {
        return(NULL)
      }
      data$current_pulse  <- data$current_pulse + 1
      if (input$skip_labeled) {
        while (
          data$current_pulse < nrow(data$raster) &&
          data$raster$class[data$current_pulse] > 0
        ) {
          data$current_pulse  <- data$current_pulse + 1
        }
      }
    }
  )

  output$sonogram <- renderPlot({
    if (is.null(data$raster)) {
      return(NULL)
    }
    data$raster %>%
      filter(
        input$starttime < end_time,
        start_time < input$starttime + input$timeinterval
      ) %>%
      arrange(select_amplitude, desc(end_time - start_time),
              desc(end_frequency - start_frequency)) -> selection
    breaks <- pretty(input$amplitude[1]:input$amplitude[2], 20)
    extent(input$starttime, xmax = input$starttime + input$timeinterval,
           ymin = input$frequency[1], ymax = input$frequency[2]
    ) %>%
      plot(
        col = NA,
        xlim = input$starttime + c(0, input$timeinterval),
        ylim = input$frequency,
        xlab = "time (ms)",
        ylab = "frequency (kHz)",
        asp = input$aspect,
        main = selection$filename[1]
      )
    lapply(selection$raster, plot, add = TRUE, legend = FALSE, breaks = breaks,
             col = topo.colors(length(breaks)))
    plot(data$raster$raster[[data$current_pulse]], add = TRUE, breaks = breaks,
         col = terrain.colors(length(breaks)))
    rect(
      xleft = selection$start_time,
      xright = selection$end_time,
      ybottom = selection$start_frequency,
      ytop = selection$end_frequency,
      col = selection$colour,
      border = selection$colour,
      density = 2,
      angle = selection$angle,
      lty = selection$linetype,
      lwd = 2
    )
    rect(
      xleft = data$raster$start_time[data$current_pulse],
      xright = data$raster$end_time[data$current_pulse],
      ybottom = data$raster$start_frequency[data$current_pulse],
      ytop = data$raster$end_frequency[data$current_pulse],
      col = data$raster$colour[data$current_pulse],
      border = data$raster$colour[data$current_pulse],
      density = 2,
      angle = data$raster$angle[data$current_pulse],
      lty = data$raster$linetype[data$current_pulse],
      lwd = 4
    )
    abline(
      h = c(20, 30, 40, 50, 60, 80, 90, 110),
      lty = 2,
      col = "red",
      lwd = 2
    )
    abline(h = c(18, 21, 27, 35), lty = 3, col = "red", lwd = 2)
    points(
      selection$peak_time,
      selection$peak_frequency,
      col = selection$colour,
      pch = 13,
      cex = 2
    )
  })

  output$prediction <- renderText({
    if (is.null(data$raster)) {
      return(NULL)
    }
    data$raster$prediction[data$current_pulse]
  })
})
