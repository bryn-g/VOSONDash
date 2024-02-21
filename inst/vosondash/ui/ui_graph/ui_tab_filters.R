tabPanel(
  "Filters",
  icon = icon("filter"),
  fluidRow(column(
    width = 12,
    h4("Graph Filters"),
    uiOutput("filter_rank_list")
  )),
  fluidRow(column(
    width = 12,
    actionButton("graph_filter_sort_reset", "reset"),
    hr()
  )),
  fluidRow(
    tabBox(
      title = NULL,
      id = "node_filters_tabset",
      height = 300,
      width = 12,
      tabPanel("Components", source("ui/ui_graph/ui_filter_comps.R")$value),
      tabPanel("Categorical", source("ui/ui_graph/ui_filter_cats.R")$value),
      tabPanel("Prune", source("ui/ui_graph/ui_filter_prune.R")$value)
    )
  )
)
