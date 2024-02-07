fluidRow(column(width = 12,
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
