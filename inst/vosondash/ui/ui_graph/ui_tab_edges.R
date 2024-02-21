tabPanel(
  "Edges",
  h4("Edge Attributes"),
  fluidRow(
    column(
      width = 6,
      disabled(
      colourpicker::colourInput(
        "edge_color",
        "Colour",
        "#CCCCCCFF",
        allowTransparent = TRUE,
        closeOnClick = FALSE
      ))
    ),
    column(
      width = 6,
      disabled(
      sliderInput(
        "edge_width",
        "Width",
        min = 0,
        max = 10,
        value = 0.9,
        step = 0.1,
        ticks = FALSE
      )
    ))
  ),
  fluidRow(column(
    width = 6,
    disabled(
    sliderInput(
      "edge_curved",
      "Curved",
      min = 0,
      max = 1,
      value = 0,
      step = 0.1,
      ticks = FALSE
    )
  ))),
  
  fluidRow(column(width = 12,
                  fluidRow(
                    column(
                      width = 6,
                      disabled(
                      sliderInput(
                        "edge_arrow_size",
                        "Arrow Size",
                        min = 0,
                        max = 5,
                        value = 0.4,
                        step = 0.1,
                        ticks = FALSE
                      ))
                    ), column(
                      width = 6,
                      disabled(
                      sliderInput(
                        "edge_arrow_width",
                        "Arrow Width",
                        min = 0,
                        max = 5,
                        value = 1,
                        step = 0.1,
                        ticks = FALSE
                      )
                    ))
                  ))),
  conditionalPanel(condition = "input.canvas_tab == 'visNetwork'",
                   fluidRow(column(width = 12,
                                   h5("VisNetwork Edges"),
                  
                 disabled(
                   checkboxInput("visnet_edge_arrows_chk", "Arrows", TRUE)
                 ),
                 conditionalPanel(condition = "input.visnet_edge_arrows_chk",
                  disabled(
                    checkboxGroupInput(
                      inputId = "visnet_edge_arrows",
                      label = NULL, 
                      choices = c("to", "from", "middle"),
                      selected = "to"
                    )
                  )),
                   disabled(
                     checkboxInput("visnet_edge_smooth_chk", "Edge Smoothing", FALSE)
                   ),
                   conditionalPanel(condition = "input.visnet_edge_smooth_chk",
                    
                    disabled(
                      selectInput(
                        "visnet_edge_smooth_type",
                        label = NULL,
                        choices = c(
                          "dynamic",
                          "continuous",
                          "discrete",
                          "diagonalCross",
                          "straightCross",
                          "horizontal",
                          "vertical",
                          "curvedCW",
                          "curvedCCW",
                          "cubicBezier"
                        ),
                        selected = "curvedCW",
                        multiple = FALSE,
                        selectize = TRUE
                      )
                    )
                     
                   )
                   ))),
  fluidRow(column(
    width = 6,
    disabled(actionButton("reset_edges_btn", label = "reset"))
    )),
  hr(style = "border-top: 1px solid #cccccc; margin-top: 0px; margin-bottom: 5px;"),
  h4("Edge Labels"),
  source("ui/ui_graph/ui_edge_labels.R")$value
)
