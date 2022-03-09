fluidRow(
  # graph data table
  tabBox(
    width = 12,
    title = "Graph Data",
    selected = "Vertices",
    id = "selected_dt_tab",
    tabPanel(
      "Vertices",
      fluidRow(
        div(
          checkboxInput("graph_dt_v_truncate_text_check", "Truncate text", TRUE),
          style = "margin-left:12px; margin-right:5px;",
          class = "div_inline"
        )
      ),
      DT::dataTableOutput("dt_vertices")
    ),
    tabPanel("Edges",
             fluidRow(
               div(
                 checkboxInput("graph_dt_e_truncate_text_check", "Truncate text", TRUE),
                 style = "margin-left:12px; margin-right:5px;",
                 class = "div_inline"
               )
             ),
             DT::dataTableOutput("dt_edges"))
  )
)
