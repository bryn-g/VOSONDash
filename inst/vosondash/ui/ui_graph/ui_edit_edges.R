tabPanel("Edges",
         icon = icon("minus"),
         fluidRow(
           div(
             checkboxInput("graph_dt_e_truncate_text_chk", "Truncate text", TRUE),
             style = "margin-left:12px; margin-right:5px;",
             class = "div_inline"
           )
         ),
         DT::dataTableOutput("dt_edges"))
