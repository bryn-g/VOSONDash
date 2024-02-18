tabPanel(
  "Layout",
  icon = icon("circle-nodes"),
  
  # seed
  fluidRow(column(
    width = 12,
    tags$b("Seed"),
    div(
      div(
        disabled(
          numericInput(
            "graph_seed_input",
            label = NULL,
            value = 100,
            min = 1,
            max = 1000000,
            width = "80px"
          )
        ),
        class = "div_inline",
        style = "padding-top:6px"
      ),
      div(disabled(actionButton("graph_seed_set_btn", label = "set")), class = "div_inline"),
      div(disabled(actionButton("graph_reseed_btn", label = icon("arrows-rotate"))),
          po_info(i_graph_reseed), class = "div_inline")
    )
  )),
  
  fluidRow(column(
    width = 8,
    div(tags$b("Graph Layout"),
        po_info(i_graph_layout),
        style = "margin-bottom:5px;"),
    
    disabled(
      selectInput(
        "graph_layout_select",
        label = NULL,
        choices = c(
          "Auto" = "Auto",
          "Fruchterman-Reingold" = "FR",
          "Kamada-Kawai" = "KK",
          "Davidson-Harel" = "DH",
          "Large Graph Layout" = "LGL",
          "Graphopt",
          "Distributed Recursive Layout" = "DrL",
          "GEM Force-Directed Layout" = "GEM",
          "Multidimensional Scaling Layout" = "MDS",
          "Grid",
          "Sphere",
          "Circle",
          "Star",
          "Tree",
          "Random"
        ),
        selectize = TRUE,
        selected = "Auto"
      )
    )
  )), # end fluidRow
  
  conditionalPanel(condition = "input.graph_layout_select == 'FR' | input.graph_layout_select == 'Graphopt'",
   fluidRow(column(
     width = 8,
     numericInput(
       inputId = "graph_niter",
       "Iterations (niter)",
       value = 500,
       min = 1,
       max = 1000000
     )
   ))
  ), # end conditionalPanel
  
  conditionalPanel(condition = "input.graph_layout_select == 'Graphopt'",
    fluidRow(
      column(
        width = 8,
        numericInput(
          inputId = "graph_charge",
          "Charge",
          value = 0.001,
          min = 0.001,
          max = 1.0,
          step = 0.001
        )
      ),
      column(
        width = 8,
        numericInput(
          inputId = "graph_mass",
          "Mass",
          value = 30,
          min = 1,
          max = 1000
        )
      )
    ),
  
    fluidRow(
      column(
        width = 8,
        numericInput(
          inputId = "graph_spr_len",
          "Spring Length",
          value = 0,
          min = 0,
          max = 1000
        )
      ),
      column(
        width = 8,
        numericInput(
          inputId = "graph_spr_const",
          "Constant",
          value = 1,
          min = 1,
          max = 1000
        )
      )
    )),
  
  # spread
  fluidRow(column(width = 6,
  disabled(actionButton("graph_layout_set_btn", label = "apply layout")))),
  hr(style = "border-top: 1px solid #cccccc; margin-top: 0px; margin-bottom: 5px;"),
  fluidRow(column(width = 8,
                  h4("Adjust Node Coordinates"),
                  disabled(
                    sliderInput(
                      "igraph_spread_slider",
                      "coordinate multiplier",
                      min = 0.25,
                      max = 2.5,
                      step = 0.1,
                      value = c(1),
                      ticks = FALSE
                    )
                  ),
                  sliderInput(
                    "igraph_x_slider",
                    "x-axis range",
                    min = -4,
                    max = 4,
                    step = 0.1,
                    value = c(-1,1),
                    ticks = TRUE
                  ),
                  sliderInput(
                    "igraph_y_slider",
                    "y-axis range",
                    min = -4,
                    max = 4,
                    step = 0.1,
                    value = c(-1,1),
                    ticks = TRUE
                  )
      )
    ),
  fluidRow(column(width = 6,
                  actionButton("graph_reset_coord_btn", label = "reset")))

)