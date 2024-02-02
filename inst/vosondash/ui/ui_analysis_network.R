#### network graphs tab  ---------------------------------------------------------------------------------------------- #
tabItem(tabName = "network_graphs_tab",
        fluidRow(
          column(width = 3, offset = 0,
                 fluidRow(
                   tabBox(width = 12, selected = "Files",
                          id = "selected_graph_controls_tab",
                          source("ui/analysis_network/ui_tab_load_data.R")$value,
                          source("ui/analysis_network/ui_tab_visual.R")$value,
                          source("ui/analysis_network/ui_tab_filters.R")$value,
                          source("ui/analysis_network/ui_tab_select.R")$value
                   )
                 )
          ),
          column(width = 9, offset = 0,
                 fluidRow(
                   # graph type tabs
                   uiOutput("vis_plot_ui"),
                   uiOutput("plot_height_ui"),
                   uiOutput("graph_summary_ui"),
                   uiOutput("graph_dl_button_ui"),
                   uiOutput("graph_legend_ui")
                 )
          )
        ),
        
        fluidRow(
          # graph data table
          tabBox(width = 12, title = "Graph Data", selected = "Metrics",
                 id = "selected_dt_tab",
                 source("ui/ui_network_metrics.R")$value,
                 source("ui/ui_network_assortativity.R")$value,
                 tabPanel("Nodes",
                          icon = icon("circle-dot"),
                          fluidRow(
                            div(checkboxInput("graph_dt_v_truncate_text_check", "Truncate text", TRUE), style = "margin-left:12px; margin-right:5px;", class = "div_inline")
                          ),
                          DT::dataTableOutput("dt_nodes"),
                          fluidRow(
                            column(width = 4, selectInput("pruned_nodes_select", "Pruned Nodes", choices = c(), multiple = TRUE, selectize = FALSE),
                                   div(actionButton("prune_return_button", "Un-prune Selected"), style = "margin-right:10px;", class = "div_inline"), 
                                   div(actionButton("prune_reset_button", "Reset"), class = "div_inline")),
                            column(width = 1, actionButton("prune_deselect_rows_button", "Deselect All"),
                                   actionButton("prune_selected_rows_button", "Prune Selected"),
                                   actionButton("prune_unselected_rows_button", "Prune Unselected"))
                          )),
                 tabPanel("Edges",
                          icon = icon("minus"),
                          fluidRow(
                            div(checkboxInput("graph_dt_e_truncate_text_check", "Truncate text", TRUE), style = "margin-left:12px; margin-right:5px;", class = "div_inline")
                          ),
                          DT::dataTableOutput("dt_edges"))
          )
        )
)
#### end network_graphs_tab