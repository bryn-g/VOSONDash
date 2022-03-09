fluidRow(column(width = 12,
                fluidRow(
                  column(width = 6,
                         disabled(
                           selectInput(
                             "graph_cat_select",
                             div("Category", style = "font-weight: normal;"),
                             choices = c("All"),
                             multiple = FALSE
                           )
                         ), ),
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
