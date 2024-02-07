tabPanel(
  "Select",
  icon = icon("hand-pointer"),
  tags$h5("Neighbourhood Select"),
  tags$p("Select starting nodes from graph data or visNetwork first."),
  fluidRow(
    column(
      width = 4,
      selectInput(
        "nbh_order_sel",
        div("Order", style = "font-weight: normal;"),
        choices = c(1:10),
        selected = 1,
        multiple = FALSE
      )
    ),
    column(width = 8,
           fluidRow(
             actionButton("nbh_sel_btn", label = "Select Connected")
           ),
           fluidRow(
             actionButton(
               "nbh_prune_unsel",
               label = "Unselected",
               icon = icon("scissors")
             )
           ),)
  ),
  fluidRow(column(
    width = 12,
    disabled(actionButton("nbh_undo_btn", label = "Undo")),
    actionButton("nbh_desel_btn", "Deselect All"),
    actionButton("nbh_reset_btn", "Reset")
  ))
)
