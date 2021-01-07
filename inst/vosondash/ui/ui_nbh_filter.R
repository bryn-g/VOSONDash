fluidRow(column(
  width = 12,
  
  fluidRow(column(
    width = 12,
    prettyToggle(
      inputId = 'expand_nbh_check',
      label_on = div("Neighbourhood Select", style = "font-weight: bold;"),
      label_off = div("Neighbourhood Select", style = "font-weight: bold;"),
      status_on = "success",
      status_off = "info",
      outline = TRUE,
      plain = TRUE,
      icon_on = icon("angle-double-down"),
      icon_off = icon("angle-double-right")
    )
  )),
  conditionalPanel(
    condition = 'input.expand_nbh_check',
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
  )
  
))
