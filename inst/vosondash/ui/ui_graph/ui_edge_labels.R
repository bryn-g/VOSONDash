fluidRow(column(width = 12,
                fluidRow(
                  column(
                    width = 12,
                    div(disabled(
                      checkboxInput("edge_labels_chk", "Attribute", FALSE)
                    ), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
                    conditionalPanel(condition = 'input.edge_labels_chk',
                                     fluidRow(column(
                                       width = 6,
                                       pickerInput(
                                         "edge_label_sel",
                                         label = NULL,
                                         choices = c("None"),
                                         selected = "None",
                                         multiple = FALSE
                                       )
                                     )))
                  )
                ),
                fluidRow(
                  column(width = 6,
                         disabled(
                           colourpicker::colourInput(
                             "edge_label_color",
                             "Colour",
                             "#000000FF",
                             allowTransparent = TRUE,
                             closeOnClick = FALSE
                           )
                         )),
                  column(
                    width = 6,
                    sliderInput(
                      "edge_label_size",
                      "Size",
                      min = 0.1,
                      max = 5,
                      value = 1,
                      step = 0.1,
                      ticks = FALSE
                    )
                  )
                )))
