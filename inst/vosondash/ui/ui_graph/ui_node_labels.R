fluidRow(
  column(
    width = 12,
    h4("Node Labels"),
    fluidRow(column(
      width = 6,
    disabled(
      selectInput(
        "node_labels_picker",
        label = NULL,
        choices = c(
          "None",
          "index",
          "attribute"
        ),
        selectize = TRUE,
        selected = "None"
      )
    ))),
    conditionalPanel(
      condition = "input.node_labels_picker == 'attribute'",
      fluidRow(column(
        width = 6,
        selectInput(
          "node_label_sel",
          label = NULL,
          choices = c("None"),
          selected = "None",
          multiple = FALSE
        )
      )),
      checkboxInput("node_sel_labels_chk", "Only selected nodes", FALSE),
    ),
    disabled(
      checkboxInput("visnet_id_sel_chk", "visnet id select control", FALSE)
    ),
    conditionalPanel(condition = gbl_is_macos,
                     disabled(
                       checkboxInput("macos_font_chk", "Arial Unicode MS", TRUE)
                     )),
    fluidRow(column(
      width = 6,
      disabled(
        colourpicker::colourInput(
          "node_label_color",
          "Colour",
          "#000000FF",
          allowTransparent = TRUE,
          closeOnClick = FALSE
        )
      )
    ),
    column(
      width = 6,
      disabled(sliderInput(
        "node_label_size",
        "Size",
        min = 0.1,
        max = 5,
        value = 1,
        step = 0.1,
        ticks = FALSE
      ))
    )),
    fluidRow(column(
      width = 6,
      disabled(sliderInput(
        "node_label_rot",
        "Position",
        value = as.numeric(format(round(-pi / 2, 2), nsmall = 2)),
        min = as.numeric(format(round(-pi, 2), nsmall = 2)),
        max = as.numeric(format(round(pi, 2), nsmall = 2)),
        step = 0.01,
        ticks = FALSE
      ))
    ),
    column(
      width = 6,
      disabled(sliderInput(
        "node_label_dist",
        "Distance",
        value = 0.6,
        min = 0,
        max = 5,
        step = 0.1,
        ticks = FALSE
      )
    ))),
    fluidRow(column(
      width = 6,
      disabled(actionButton("reset_node_labels_btn", label = "reset"))
    )),
    
  )
)
