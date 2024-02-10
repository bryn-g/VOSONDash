tabPanel(
  "Nodes",
  
  h4("Node Attributes"),
  # node size
  fluidRow(
    column(
      width = 4,
      div("Node Size", style = "font-weight: bold;", class = "custom_node_size_div"),
      pickerInput(
        "node_size_sel",
        label = NULL,
        choices = c("None"),
        multiple = FALSE
      )
    ),
    column(width = 6,
           disabled(
             sliderInput(
               "node_size_slider",
               label = "Multiplier",
               min = 0.1,
               max = 15,
               step = 0.1,
               value = c(1),
               ticks = FALSE,
               animate = FALSE
             )
           ))
  ),
  
  fluidRow(column(width = 6,
                  disabled(
                    sliderInput(
                      "igraph_node_base_size_slider",
                      label = "igraph base size",
                      min = 0.1,
                      max = 8,
                      step = 0.1,
                      value = c(4),
                      ticks = FALSE,
                      animate = FALSE
                    )
                  )),
           column(width = 6,
                  disabled(
                    sliderInput(
                      "visnet_node_base_size_slider",
                      label = "visnet base size",
                      min = 1,
                      max = 40,
                      step = 1,
                      value = c(20),
                      ticks = FALSE,
                      animate = FALSE
                    )
                  ))),
  
  # node colors from graphml
  fluidRow(column(
    width = 12,
    checkboxInput(
      "node_use_g_cols_chk",
      div("Node colors from graphml", style = "margin-bottom:5px;")
      ,
      TRUE
    )
  )),
  fluidRow(column(
    width = 12,
    actionButton("node_attr_reset_btn", "Reset")
  )),
  hr(style = "border-top: 1px solid #cccccc; margin-top: 0px; margin-bottom: 5px;"),
  h4("Node Labels"),
  # node labels
  source("ui/ui_graph_node_labels.R")$value,
  fluidRow(column(
    width = 12,
    actionButton("node_labels_reset_btn", "Reset")
  ))
  
) # end top tabpanel
