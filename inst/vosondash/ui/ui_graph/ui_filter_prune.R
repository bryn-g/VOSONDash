fluidRow(
  column(
    width = 12,
    fluidRow(
      column(
        width = 12,
        DT::dataTableOutput("dt_nodes_rm")
      )
    ),
    fluidRow(
      column(
        width = 3,
        actionButton("prune_reset_btn", "reset")
      ),
      column(
        width = 3,
        actionButton("rm_sel_rows_btn", "prune selected")
      )
    )
  )
)
