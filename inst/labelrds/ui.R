library(shiny)

shinyUI(
  navbarPage(
    title = "Soundcluster",
    tabPanel(
      "Validate model",
      textInput("path", label = "db path", value = "~"),
      actionButton("connect", label = "open connection"),
      actionButton("refresh_model", "Display all models"),
      selectizeInput(
        "node_model", label = "Model", choices = character(0), width = "400px"
      ),
      DT::dataTableOutput("dt_node_quality"),
      DT::dataTableOutput("dt_node_quality_spectrogram")
    ),
    tabPanel(
      "Check pulses",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          actionButton("step_backward", label = "<<<"),
          actionButton("step_forward", label = ">>>"),
          checkboxInput("skip_labeled", label = "skip labeled", value = TRUE),
          checkboxInput("raw_spectrogram", label = "display raw spectrogram",
                        value = TRUE),
          actionButton("use_dominant", label = "use dominant class"),
          textOutput("prediction"),
          selectizeInput(
            "class_id", label = "class",
            choices = c("[no class]" = "0", "[new class]" = "-1"),
            selected = "0"
          ),
          actionButton("update_class", label = "update class"),
          conditionalPanel(
            condition = "input.class_id == '-1'",
            textInput("class_abbrev", label = "abbreviation", value = ""),
            textInput("class_description", label = "description", value = ""),
            selectInput(
              "species", label = "species",
              choices = c("[no species]" = "0", "[new species]" = "-1"),
              selected = "0"
            ),
            conditionalPanel(
              condition = "input.species == '-1'",
              textInput("species_name", label = "name", value = ""),
              selectInput(
                "species_parent", label = "parent", choices = "[no parent]",
                selected = "[no parent]"
              ),
              numericInput("species_gbif", label = "gbif", value = NA),
              actionButton("new_species", label = "add species")
            ),
            selectInput(
              "behaviour", label = "behaviour",
              choices = c("[no behaviour]" = "0", "[new behaviour]" = "-1"),
              selected = "0"
            ),
            conditionalPanel(
              condition = "input.behaviour == -1",
              textInput("behaviour_name", label = "name", value = ""),
              actionButton("new_behaviour", label = "add behaviour")
            ),
            textInput("class_color", label = "color", value = "black"),
            selectInput(
              "class_linetype", label = "linetype",
              choices =
                c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
              selected = "solid"
            ),
            sliderInput(
              "class_angle", label = "angle",
              value = 45, min = -90, max = 90, step = 5
            ),
            actionButton("new_class", label = "add class")
          ),
          sliderInput(
            "starttime",
            label = "start time (ms)",
            value = -1,
            min = -1,
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
    )
  )
)
