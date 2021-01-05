tabPanel("Edges",
         fluidRow(column(
           width = 12,
           div("Edge Filters", style = "font-weight: bold;"),
           div(disabled(
             checkboxInput("graph_multi_edge_check", "Multiple Edges", TRUE)
           ), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
           div(disabled(
             checkboxInput("graph_loops_edge_check", "Loops", TRUE)
           ), class = "div_inline", style = "margin-right:8px; margin-top:0px;")
         )))
