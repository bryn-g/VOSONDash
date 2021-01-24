fluidRow(column(
  width = 12,
  
  fluidRow(column(
    width = 6,
    selectInput(
      "nbh_order_select",
      div("Neighbourhood Order", style = "font-weight: normal;"),
      choices = c(1:10),
      selected = 1,
      multiple = FALSE
    )
  )),
  
  fluidRow(column(
    width = 12,
    actionButton("nbh_select_button", label = "Select Nodes"),
    actionButton("nbh_prune_unselected", label = div(icon("filter"), "Filter"))
  )),
  
  fluidRow(column(
    width = 12,
    disabled(actionButton("nbh_undo_button", label = "Undo")),
    actionButton("nbh_deselct_button", "Deselect All"),
    actionButton("nbh_reset_button", "Reset")
  ))
  
))
