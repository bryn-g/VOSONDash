tabPanel(
  "Filters",

  fluidRow(column(width = 12,
rank_list(
  text = NULL,
  labels = list(
    "rm_pruned" = list(
      div("{node}", class = "div_inline"),
      div("Pruned", class = "div_inline")
    ),    
    "rm_isolates" = list(
      div("{node}", class = "div_inline"),
      # div("Remove Isolates", class = "div_inline"),   
      div(checkboxInput("graph_isolates_check", "Remove Isolates", FALSE), class = "div_inline")
    ),
    "rm_loops" = list(
      div("{edge}", class = "div_inline"),
      # div("Remove Loops", class = "div_inline"),       
      div(checkboxInput("graph_loops_edge_check", "Remove Loops", FALSE), class = "div_inline")
    ),
    "rm_multiedges" = list(
      div("{edge}", class = "div_inline"),
      # div("Merge Multiple Edges", class = "div_inline"),       
      div(checkboxInput("graph_multi_edge_check", "Merge Multiple Edges", FALSE), class = "div_inline")
    ),
    "rm_components" = list(
      div("{node}", class = "div_inline"),
      div("Components", class = "div_inline")
    ),
    "rm_categories" = list(
      div("{node}", class = "div_inline"),
      div("Categorical", class = "div_inline")
    )
  ),
  input_id = "filter_order",
  options = sortable_options(
    swap = TRUE
  )
))),
fluidRow(column(width = 12, hr())),
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