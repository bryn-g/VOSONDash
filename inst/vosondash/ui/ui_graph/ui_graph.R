tabPanel("Network Graph",
         fluidRow(
           column(width = 3, offset = 0,
                  fluidRow(
                    tabBox(
                      width = 12,
                      selected = "Files",
                      id = "selected_graph_controls_tab",
                      source("ui/ui_graph/ui_tab_files.R")$value,
                      source("ui/ui_graph/ui_tab_visuals.R")$value,
                      source("ui/ui_graph/ui_tab_filters.R")$value,
                      source("ui/ui_graph/ui_tab_select.R")$value
                    )
                  )) |> tagAppendAttributes(id = "ctrl_column"),
           column(
             width = 9,
             offset = 0,
             fluidRow(
               # graph type tabs
               uiOutput("vis_plot_ui"),
               uiOutput("graph_summary_ui"),
               uiOutput("graph_dl_btn_ui"),
               uiOutput("graph_legend_ui")
             )
           ) |> tagAppendAttributes(id = "plot_column")
         ),
         # uiOutput("ctrl_plot_ui"),
         fluidRow(
           # graph metrics and data tables
           tabBox(
             width = 12,
             title = "Graph Data",
             selected = "Metrics",
             id = "selected_dt_tab",
             source("ui/ui_graph/ui_metrics.R")$value,
             source("ui/ui_graph/ui_assort.R")$value,
             source("ui/ui_graph/ui_edit_nodes.R")$value,
             source("ui/ui_graph/ui_edit_edges.R")$value
           )
         ))
