fluidRow(column(
  width = 12,
  fluidRow(column(
    width = 12,
    prettyToggle(
      inputId = 'expand_elabel_visuals_check',
      label_on = tags$b("Labels"),
      label_off = tags$b("Labels"),
      outline = TRUE,
      plain = TRUE,
      icon_on = icon("angle-double-down"),
      icon_off = icon("angle-double-right"),
      status_on = "success",
      status_off = "info",
      inline = TRUE
    )
  )),
  conditionalPanel(
    condition = 'input.expand_elabel_visuals_check',
    fluidRow(column(
      width = 12,
      div(disabled(
        checkboxInput("edge_labels_check", "Attribute", FALSE)
      ), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
      conditionalPanel(condition = 'input.edge_labels_check',
                       fluidRow(column(
                         width = 6,
                         pickerInput(
                           "edge_label_select",
                           label = NULL,
                           choices = c("None"),
                           selected = "None",
                           multiple = FALSE
                         )
                       )))
    )),
    fluidRow(column(
      width = 6,
      disabled(
        colourpicker::colourInput(
          "edge_label_color",
          "Colour",
          "#000000FF",
          allowTransparent = TRUE,
          closeOnClick = FALSE
        )
      )
    ),
    column(
      width = 6,
      sliderInput(
        "edge_label_size",
        "Size",
        min = 0.1,
        max = 5,
        value = 1,
        step = 0.1,
        ticks = FALSE
      )
    ))
  )
))
