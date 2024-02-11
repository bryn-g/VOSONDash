fluidRow(
  column(
    width = 12,
    div("Labels", style = "font-weight: bold;"),
    disabled(checkboxInput("node_index_chk", "Node index", FALSE)),
    disabled(checkboxInput(
      "node_labels_chk", "Label attribute", FALSE
    )),
    conditionalPanel(
      condition = 'input.node_labels_chk',
      fluidRow(column(
        width = 6,
        #shinyjs::disabled(
        selectInput(
          "node_label_sel",
          label = NULL,
          choices = c("None"),
          selected = "None",
          multiple = FALSE
        )
        #)
      )),
      checkboxInput("node_sel_labels_chk", "Only selected nodes", FALSE),
    ),
    disabled(
      checkboxInput("visnet_id_sel_chk", "visnet id select control", FALSE)
    ),
    conditionalPanel(condition = gbl_is_macos,
                     disabled(
                       checkboxInput("macos_font_check", "Arial Unicode MS", TRUE)
                     ))
    
  )
)