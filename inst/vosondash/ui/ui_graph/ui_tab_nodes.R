tabPanel(
  "Nodes",
  icon = icon("circle-dot"),
  source("ui/ui_graph/ui_node_labels.R")$value,
  tags$hr(),
  
  fluidRow(column(
    width = 4,
    div("Node Size", style = "font-weight: bold;", class = "custom_node_size_div"),
    disabled(
      selectInput(
        "node_size_sel",
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
             "node_size_slider",
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
  disabled(checkboxInput(
    "node_use_g_cols_chk",
    div("Node colors from graphml", style = "margin-bottom:5px;")
    ,
    TRUE
  )),
  conditionalPanel(condition = 'input.node_labels_chk',
                   disabled(
                     checkboxInput("node_label_prop_chk", "Proportionate Label Size", TRUE)
                   )),
  div("Mastodon"),
  disabled(checkboxInput("node_mtdn_img_chk", "Avatar images", FALSE)),
  disabled(checkboxInput("node_mtdn_img_sq_chk", "Square", FALSE)),
  disabled(checkboxInput("node_mtdn_img_bord_chk", "Border", FALSE))
)
