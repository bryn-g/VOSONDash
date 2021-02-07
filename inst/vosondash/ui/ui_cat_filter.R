fluidRow(column(width = 12,
                
                # div(
                #   tags$b("Categorical Filter"),
                #   vpopover(po_cat_filter()$title, po_cat_filter()$content)
                # )
                
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
                    # checkboxInput("graph_legend_check", "Legend", FALSE)
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
                )))
