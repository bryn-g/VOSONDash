tabPanel(
  "Nodes",
  icon = icon("circle-dot"),
  fluidRow(
    div(
      checkboxInput("graph_dt_v_truncate_text_chk", "Truncate text", TRUE),
      style = "margin-left:12px; margin-right:5px;",
      class = "div_inline"
    )
  ),
  DT::dataTableOutput("dt_nodes"),
  fluidRow(
    column(
      width = 4,
      selectInput(
        "prune_nodes_sel",
        "Pruned Nodes",
        choices = c(),
        multiple = TRUE,
        selectize = FALSE
      ),
      div(
        actionButton("prune_nodes_ret_btn", "Un-prune Selected"),
        style = "margin-right:10px;",
        class = "div_inline"
      ),
      div(actionButton("prune_reset_btn", "Reset"), class = "div_inline")
    ),
    column(
      width = 1,
      actionButton("prune_desel_rows_btn", "Deselect All"),
      actionButton("rm_sel_rows_btn", "Prune Selected"),
      actionButton("rm_unsel_rows_btn", "Prune Unselected")
    )
  )
)
