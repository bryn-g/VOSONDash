tabPanel("Visual",
  icon = icon("swatchbook"),
  fluidRow(
    tabBox(
      width = 12,
      selected = "Layout",
      id = "selected_visuals_tab",
      source("ui/ui_graph/ui_tab_canvas.R")$value,
      source("ui/ui_graph/ui_tab_layout.R")$value,
      source("ui/ui_graph/ui_tab_nodes.R")$value,
      source("ui/ui_graph/ui_tab_edges.R")$value
    ))
  )
