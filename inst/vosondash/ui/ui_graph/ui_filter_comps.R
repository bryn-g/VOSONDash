fluidRow(column(
  width = 12,
  fluidRow(column(
    width = 12,
    verbatimTextOutput("comp_input_ui")
  )),
  fluidRow(
    column(width = 4,
           shinyjs::disabled(
             selectInput(
               "comp_mode_picker",
               div("Type", style = "font-weight: normal;"),
               choices = c("weak", "strong"),
               selected = "weak",
               multiple = FALSE
             )
           )),
    column(width = 8,
           fluidRow(
           disabled(
             sliderInput(
               "comp_slider",
               div("Size", style = "font-weight: normal;"),
               min = 1,
               max = 500,
               value = c(1, 500),
               ticks = FALSE
             )
           )# ,
           # disabled(actionButton("comp_set", "Set"))
    )
  )),
  fluidRow(column(
    width = 12,
    verbatimTextOutput("comp_summary_ui")
  )),
  fluidRow(column(
    width = 12,
    verbatimTextOutput("comp_count_ui")
  ))
))
