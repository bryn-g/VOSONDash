tabPanel("Filters",
         
         fluidRow(column(
           width = 12,
           
           rank_list(
             text = "Apply filters in order:",
             labels = list(
               "rm_pruned" = list(
                 div("  ", class = "div_inline"),
                 div("Pruned", class = "div_inline")
               ),
               "rm_isolates" = list(
                 div("", class = "div_inline"),
                 div(
                   checkboxInput("graph_isolates_check", "Remove Isolates", FALSE),
                   class = "div_inline"
                 )
               ),
               "rm_loops" = list(
                 div("", class = "div_inline"),
                 div(
                   checkboxInput("graph_loops_edge_check", "Remove Loops", FALSE),
                   class = "div_inline"
                 )
               ),
               "rm_multiedges" = list(
                 div("", class = "div_inline"),
                 div(
                   checkboxInput("graph_multi_edge_check", "Merge Multiple Edges", FALSE),
                   class = "div_inline"
                 )
               ),
               "rm_components" = list(
                 div("", class = "div_inline"),
                 div(
                   checkboxInput("graph_components_check", "Components", FALSE),
                   class = "div_inline"
                 )
               ),
               "rm_categories" = list(
                 div("  ", class = "div_inline"),
                 div("Categorical", class = "div_inline")
               )
             ),
             input_id = "filter_order",
             options = sortable_options(swap = TRUE)
           )
         )),
         fluidRow(column(width = 12, hr())),
         fluidRow(
           tabBox(
             title = NULL,
             id = "node_filters_tabset",
             width = 12,
             tabPanel("Components",
                      source("ui/ui_comp_filter.R")$value),
             tabPanel("Categorical",
                      source("ui/ui_cat_filter.R")$value)
           )
         ))