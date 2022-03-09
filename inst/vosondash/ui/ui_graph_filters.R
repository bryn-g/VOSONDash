tabPanel("Filters",
         
         fluidRow(column(
           width = 12,
           h4("Graph Filters"),
           uiOutput("filter_rank_list")
         )),
         fluidRow(column(width = 12,
                         actionButton(
                           "graph_filter_sort_reset",
                           "Reset",
                           icon = icon("elementor")
                         ),
                         hr()
        )),
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