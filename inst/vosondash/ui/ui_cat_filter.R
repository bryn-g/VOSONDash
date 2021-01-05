fluidRow(column(
  width = 12,
  fluidRow(column(
    width = 12,
    prettyToggle(
      inputId = 'expand_categorical_filter_check',
      label_on = div(
        tags$b("Categorical Filter"),
        vpopover(po_cat_filter()$title, po_cat_filter()$content)
      ),
      label_off = div(
        tags$b("Categorical Filter"),
        vpopover(po_cat_filter()$title, po_cat_filter()$content)
      ),
      outline = TRUE,
      plain = TRUE,
      icon_on = icon("angle-double-down"),
      icon_off = icon("angle-double-right"),
      status_on = "success",
      status_off = "info",
      inline = TRUE
    )
  )),
  conditionalPanel(condition = 'input.expand_categorical_filter_check',
                   fluidRow(
                     column(
                       width = 6,
                       disabled(
                         selectInput(
                           "graph_cat_select",
                           div("Category", style = "font-weight: normal;"),
                           choices = c("All"),
                           multiple = FALSE
                         )
                       ),
                       checkboxInput("graph_legend_check", "Legend", TRUE)
                     ),
                     column(width = 6,
                            disabled(
                              selectInput(
                                "graph_sub_cats_select",
                                div("View", style = "font-weight: normal;"),
                                choices = c("All"),
                                multiple = TRUE,
                                selected = "All",
                                selectize = FALSE,
                                size = 3
                              )
                            ))
                   ))
))
