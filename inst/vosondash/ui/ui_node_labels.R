fluidRow(column(
  width = 12,
  div("Labels", style = "font-weight: bold;"),
  div(disabled(
    checkboxInput("node_labels_check", "Attribute", FALSE)
  ), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
  div(disabled(
    checkboxInput("node_index_check", "Index", FALSE)
  ), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
  div(disabled(
    checkboxInput("node_sel_labels_check", "Selected", TRUE)
  ), class = "div_inline"),
  conditionalPanel(condition = 'input.node_labels_check',
                   fluidRow(column(
                     width = 6,
                     pickerInput(
                       "node_label_select",
                       label = NULL,
                       choices = c("None"),
                       selected = "None",
                       multiple = FALSE
                     )
                   ))),
  fluidRow(column(
    width = 4,
    disabled(
      colourInput(
        "node_label_color",
        NULL,
        "#000000FF",
        allowTransparent = TRUE,
        closeOnClick = FALSE
      )
  )))
))
