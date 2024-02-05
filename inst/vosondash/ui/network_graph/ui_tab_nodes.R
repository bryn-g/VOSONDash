tabPanel(
  "Nodes",
  icon = icon("circle-dot"),
  fluidRow(column(
    width = 12,
    div("Labels", style = "font-weight: bold;"),
    disabled(checkboxInput("node_index_check", "Node index", FALSE)),
    disabled(checkboxInput("node_labels_check", "Label attribute", FALSE)),
    conditionalPanel(condition = 'input.node_labels_check',
                     fluidRow(column(
                       width = 6, #shinyjs::disabled(
                         selectInput(
                           "node_label_sel",
                           label = NULL,
                           choices = c("None"),
                           selected = "None",
                           multiple = FALSE
                         )
                       #)
                     )),
                     checkboxInput("node_sel_labels_check", "Only selected nodes", FALSE)),
    disabled(checkboxInput("visnet_id_select_check", "visnet id select control", FALSE)),
    conditionalPanel(condition = gbl_is_macos,
                     disabled(
                       checkboxInput("macos_font_check", "Arial Unicode MS", TRUE)
                     ))

 )),
  tags$hr(),

  fluidRow(column(
    width = 4,
    div("Node Size", style = "font-weight: bold;", class = "custom_node_size_div"),
    disabled(
      selectInput(
        "graph_node_size_select",
        label = NULL,
        choices = c(
          "None",
          "Degree",
          "Indegree",
          "Outdegree",
          "Betweenness",
          "Closeness"
        ),
        multiple = FALSE,
        selectize = TRUE
      )
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
                      "visgraph_node_base_size_slider",
                      label = "visnet base size",
                      min = 1,
                      max = 40,
                      step = 1,
                      value = c(20),
                      ticks = FALSE,
                      animate = FALSE
                    )
                  ))),
  checkboxInput(
    "use_node_colors_check",
    div("Node colors from graphml", style = "margin-bottom:5px;")
    ,
    TRUE
  ),
  conditionalPanel(condition = 'input.node_labels_check',
                   checkboxInput("node_label_prop_size_check", "Proportionate Label Size", TRUE)
  ),
 div("Mastodon"),
 checkboxInput("mast_images", "Avatar images", FALSE),
 checkboxInput("mast_square_images", "Square", FALSE),
 checkboxInput("mast_border_images", "Border", FALSE)
)