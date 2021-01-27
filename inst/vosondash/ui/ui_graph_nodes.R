#fluidRow(column(width = 12,
    
#         fluidRow(column(width = 12,
                         tabPanel("Nodes", 
  
  h4("Node Attributes"),
  # node size
  fluidRow(column(
    width = 4,
    div("Node Size", style = "font-weight: bold;", class = "custom_node_size_div"),
    # disabled(
    pickerInput(
      "graph_node_size_select",
      label = NULL,
      choices = c("None"),
      multiple = FALSE
    )
  ),
  column(width = 6,
         disabled(
           sliderInput(
             "graph_node_size_slider",
             label = "Multiplier",
             min = 0.1,
             max = 15,
             step = 0.1,
             value = c(1),
             ticks = FALSE,
             animate = FALSE
           )
         ))),
  
  # node colors from graphml
  fluidRow(column(width = 12,
                  checkboxInput(
                    'use_vertex_colors_check',
                    div("Node colors from graphml", style = "margin-bottom:5px;")
                    ,
                    TRUE
                  ))), 
  hr(style = "border-top: 1px solid #cccccc; margin-top: 0px; margin-bottom: 5px;"),
  h4("Node Labels"),
  # node labels
  source("ui/ui_node_labels.R")$value,
  
  # node isolates filter
  fluidRow(column(
    width = 12,
    hr(style = "border-top: 1px solid #cccccc; margin-top: 0px; margin-bottom: 5px;"),
    h4("Node Filters"),
    div(disabled(
      checkboxInput("graph_isolates_check", "Isolates", TRUE)
    ), class = "div_inline")
  ))
  
) # end top tabpanel
