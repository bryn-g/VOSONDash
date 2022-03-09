tabPanel("Layout",
         
         fluidRow(
           column(
             width = 12,
             
             h4("Graph layout"),
             fluidRow(
               column(
                 width = 6,
                 pickerInput(
                   inputId = "graph_layout_select",
                   label = NULL,
                   choices = c(
                     "Auto",
                     "Fruchterman-Reingold" = "FR",
                     "Kamada-Kawai" = "KK",
                     "Davidson-Harel (slow)" = "DH",
                     "Large Graph Layout" = "LGL",
                     "Graphopt",
                     "Distributed Recursive Layout" = "DrL",
                     "GEM Force-Directed Layout" = "GEM",
                     "Multidimensional Scaling Layout" = "MDS",
                     "Grid",
                     "Sphere",
                     "Circle",
                     "Star",
                     "Random"
                   )
                 )
               ),
               column(
                 width = 5,
                 style = "padding-left: 0px;",
                 
                 div(
                   disabled(actionButton("graph_reseed_button", label = icon("sync"))),
                   vpopover(po_reseed_graph()$title, po_reseed_graph()$content),
                   class = "div_inline"
                 ),
                 div(
                   id = "seed",
                   "",
                   style = "margin-left: 10px;",
                   class = "div_inline"
                 )
               )
             ),
             
             conditionalPanel(condition = 'input.graph_layout_select == "FR" | input.graph_layout_select == "Graphopt"',
                              fluidRow(column(
                                width = 6,
                                numericInput(
                                  inputId = "graph_niter",
                                  "Iterations (niter)",
                                  value = 500,
                                  min = 1,
                                  max = 1000000
                                )
                              ))),
             conditionalPanel(
               condition = 'input.graph_layout_select == "Graphopt"',
               fluidRow(
                 column(
                   width = 6,
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
                   width = 6,
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
                   width = 6,
                   numericInput(
                     inputId = "graph_spr_len",
                     "Spring Length",
                     value = 0,
                     min = 0,
                     max = 1000
                   )
                 ),
                 column(
                   width = 6,
                   numericInput(
                     inputId = "graph_spr_const",
                     "Constant",
                     value = 1,
                     min = 1,
                     max = 1000
                   )
                 )
               )
             ),
             fluidRow(column(width = 12,
                             disabled(
                               sliderInput(
                                 "graph_spread_slider",
                                 "Plot Spread",
                                 min = 0.25,
                                 max = 2.5,
                                 step = 0.1,
                                 value = c(1),
                                 ticks = FALSE
                               )
                             )))
             
           )
         ))
