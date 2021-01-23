tabPanel(
  "Edges",
  fluidRow(column(
    width = 12,
    div("Edge Filters", style = "font-weight: bold;"),
    div(disabled(
      checkboxInput("graph_multi_edge_check", "Multiple Edges", TRUE)
    ), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
    div(disabled(
      checkboxInput("graph_loops_edge_check", "Loops", TRUE)
    ), class = "div_inline", style = "margin-right:8px; margin-top:0px;")
  )),
  fluidRow(
    column(
      width = 6,
      colourpicker::colourInput(
        "edge_color",
        "Colour",
        "#CCCCCCFF",
        allowTransparent = TRUE,
        closeOnClick = FALSE
      )
    ),
    column(
      width = 6,
      sliderInput(
        "edge_width",
        "Width",
        min = 0,
        max = 10,
        value = 0.9,
        step = 0.1,
        ticks = FALSE
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      sliderInput(
        "edge_curved",
        "Curved",
        min = 0,
        max = 1,
        value = 0,
        step = 0.1,
        ticks = FALSE
      )
    )),
    fluidRow(column(
      width = 12,
      prettyToggle(
        inputId = 'expand_edge_arrows_check',
        label_on = tags$b("Arrows"),
        label_off = tags$b("Arrows"),
        outline = TRUE,
        plain = TRUE,
        icon_on = icon("angle-double-down"),
        icon_off = icon("angle-double-right"),
        status_on = "success",
        status_off = "info",
        inline = TRUE
      )
    )),
    fluidRow(column(
      width = 12,
      conditionalPanel(condition = 'input.expand_edge_arrows_check',
                       fluidRow(
                         column(
                           width = 6,
                           sliderInput(
                             "edge_arrow_size",
                             "Arrow Size",
                             min = 0,
                             max = 5,
                             value = 0.4,
                             step = 0.1,
                             ticks = FALSE
                           )
                         ), column(
                           width = 6,
                           sliderInput(
                             "edge_arrow_width",
                             "Arrow Width",
                             min = 0,
                             max = 5,
                             value = 1,
                             step = 0.1,
                             ticks = FALSE
                           )
                         )
                       ))
    )),
    source("ui/ui_edge_labels.R")$value
  )
  