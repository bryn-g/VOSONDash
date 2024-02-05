fluidRow(column(
  width = 12,
  fluidRow(
    column(width = 4,
           shinyjs::disabled(
             selectInput(
               "graph_comp_type_sel",
               div("Type", style = "font-weight: normal;"),
               choices = c("weak", "strong"),
               selected = "weak",
               multiple = FALSE
             )
           )),
    column(width = 8,
           disabled(
             sliderInput(
               "graph_comp_slider",
               div("Size", style = "font-weight: normal;"),
               min = 1,
               max = 500,
               value = c(1, 500),
               ticks = FALSE
             )
           ))
  ),
  fluidRow(column(
    width = 12,
    verbatimTextOutput("comp_summary_ui")
  )),
  fluidRow(column(
    width = 12,
    verbatimTextOutput("comp_count_ui")
  )),  
  fluidRow(column(
    width = 12,
    actionButton(
      "graph_comps_recalc",
      "Recalculate",
      icon = icon("calculator")
    )
  )))
)