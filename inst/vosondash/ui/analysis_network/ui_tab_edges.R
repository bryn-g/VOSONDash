tabPanel(
  "Edges",
  h4("Edge Attributes"),
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
  fluidRow(column(
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
  
  fluidRow(column(width = 12,
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
                  ))),
  hr(style = "border-top: 1px solid #cccccc; margin-top: 0px; margin-bottom: 5px;"),
  h4("Edge Labels"),
  source("ui/analysis_network/ui_edge_labels.R")$value
)
