fluidRow(column(
  width = 12,
  
  fluidRow(column(
    width = 12,
    prettyToggle(
      inputId = 'expand_component_filter_check',
      label_on = div("Component Filter", style = "font-weight: bold;"),
      label_off = div("Component Filter", style = "font-weight: bold;"),
      status_on = "success",
      status_off = "info",
      outline = TRUE,
      plain = TRUE,
      icon_on = icon("angle-double-down"),
      icon_off = icon("angle-double-right")
    )
  )),
  conditionalPanel(
    condition = 'input.expand_component_filter_check',
    disabled(checkboxInput(
      'reset_on_change_check',
      div("Recalculate on category change", style = "font-weight: normal;"),
      TRUE
    )),
    fluidRow(
      column(width = 4,
             shinyjs::disabled(
               selectInput(
                 "graph_component_type_select",
                 div("Type", style = "font-weight: normal;"),
                 choices = c("Weak", "Strong"),
                 selected = "Weak",
                 multiple = FALSE
               )
             )),
      column(width = 8,
             disabled(
               sliderInput(
                 "graph_component_slider",
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
      verbatimTextOutput("component_summary_ui")
    )) # ,
    # fluidRow(
    #   column(width = 12,
    #      pickerInput(
    #        "component_membership_select",
    #        div("Component", style = "font-weight: normal;"),
    #        choices = c("None"),
    #        selected = "None"
    #      )
    #   ))
  )
  
))
