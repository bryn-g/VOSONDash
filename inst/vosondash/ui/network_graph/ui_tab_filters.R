tabPanel("Filters",
         icon = icon("filter"),
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
                      source("ui/network_graph/ui_filter_comps.R")$value),
             tabPanel("Categorical",
                      source("ui/network_graph/ui_filter_cats.R")$value)
           )
         ))