tabPanel(
  "Filters",

  fluidRow(column(width = 12, h4("Order Filters"))),
  fluidRow(column(width = 12,
rank_list(
  text = NULL,
  labels = list(
    "rm_pruned" = list(
      div(checkboxInput("graph_pruned_check", "{nodes} Remove Pruned", FALSE), class = "div_inline")
    ),    
    "rm_isolates" = list(
      div(checkboxInput("graph_isolates_check", "{nodes} Remove Isolates", FALSE), class = "div_inline")
    ),
    "rm_loops" = list(
      div(checkboxInput("graph_loops_edge_check", "{edges} Remove Loops", FALSE), class = "div_inline")
    ),
    "rm_multiedges" = list(
      div(checkboxInput("graph_multi_edge_check", "{edges} Merge Multiple Edges", FALSE), class = "div_inline")
    ),
    "rm_components" = list(
      div(checkboxInput("graph_components_check", "{nodes} Components", FALSE), class = "div_inline")
    ),
    "rm_categories" = list(
      div(checkboxInput("graph_categories_check", "{nodes} Categories", FALSE), class = "div_inline")
    )
  ),
  input_id = "filter_order",
  options = sortable_options(
    swap = TRUE
  )
))),

fluidRow(
  tabBox(
    title = NULL,
    id = "node_filters_tabset",
    width = 12,
    tabPanel(
      "Components",
      source("ui/ui_comp_filter.R")$value),
    tabPanel(
      "Categorical",
      source("ui/ui_cat_filter.R")$value)#,
    # tabPanel(
    #   "Neighbourhood",
    #   source("ui/ui_nbh_filter.R")$value)
  ))

)