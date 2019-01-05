library(shiny)
library(soundcluster)
library(RSQLite)
library(raster)

find_start <- function(data) {
  start <- mean(
    data$pulse$start_time[data$current_pulse],
    data$pulse$end_time[data$current_pulse]
  ) - input$timeinterval / 2
  start <- pmin(
    pmax(0, start),
    floor(max(sonogram@SpecGram$t) - input$timeinterval)
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

      updateSliderInput(
        session,
        "starttime",
        value = find_start(data),
        min = floor(min(sonogram@SpecGram$t)),
        max = floor(max(sonogram@SpecGram$t) - input$timeinterval)
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
    if (is.null(data$clamped)) {
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
        value = find_start(data)
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
        value = find_start(data)
      )
    }
  )

  observeEvent(
    input$timeinterval,
    {
      updateSliderInput(
        session,
        "starttime",
        value = find_start(data),
        step = input$timeinterval
      )
    }
  )
})
