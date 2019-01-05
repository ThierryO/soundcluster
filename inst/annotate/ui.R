library(shiny)

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 2,
      textInput("path", label = "project path", value = "~"),
      actionButton("step_backward", label = "<<<"),
      actionButton("step_forward", label = ">>>"),
      sliderInput(
        "starttime",
        label = "start time (ms)",
        value = 0,
        min = 0,
        max = 10000,
        step = 200,
        animate = animationOptions(interval = 1000)
      ),
      sliderInput(
        "frequency",
        label = "frequency (kHz)",
        value = c(0, 150),
        min = 0,
        max = 150
      ),
      sliderInput(
        "timeinterval",
        label = "interval (ms)",
        value = 200,
        step = 50,
        min = 50,
        max = 1000
      ),
      sliderInput(
        "amplitude",
        label = "amplitude (dB)",
        value = c(0, 50),
        min = -50,
        max = 100
      ),
      selectInput(
        "aspect",
        label = "aspect ratio",
        choices = c("1/4" = 0.25, "1/2" = 0.5, "1" = 1, "2" = 2),
        selected = 1
      )
    ),
    mainPanel(
      plotOutput("sonogram", height = "850px")
    )
  )
))
