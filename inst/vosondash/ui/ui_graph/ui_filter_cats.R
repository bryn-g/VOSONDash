fluidRow(column(width = 12,
                fluidRow(
                  disabled(checkboxInput(
                    "cat_use_g_cols_chk", div("Node colors from graphml", style = "margin-bottom:5px;"), FALSE
                  ))
                ),
                fluidRow(
                  column(width = 6,
                         disabled(
                           selectInput(
                             "cat_sel",
                             div("Category", style = "font-weight: normal;"),
                             choices = c("All"),
                             multiple = FALSE
                           )
                         ),),
                  column(width = 6,
                         disabled(
                           selectInput(
                             "cat_sub_sel",
                             div("View", style = "font-weight: normal;"),
                             choices = c("All"),
                             multiple = TRUE,
                             selected = "All",
                             selectize = FALSE,
                             size = 3
                           )
                         ))
                )))
