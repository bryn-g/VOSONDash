#### network graphs tab  --------------------------------------------------------------------------------------------- #
tabItem(tabName = "network_graphs_tab",
        fluidRow(
                column(
                        width = 3,
                        offset = 0,
                        fluidRow(
                                source("ui/ui_load_file.R")$value,
                                
                                tabBox(
                                        title = NULL,
                                        id = "graph_controls_tabset",
                                        width = 12,
                                        source("ui/ui_graph_nodes.R")$value,
                                        source("ui/ui_graph_edges.R")$value,
                                        source("ui/ui_graph_layout.R")$value
                                ),
                                
                                conditionalPanel(
                                        condition = js_is_mac,
                                        sidebarPanel(width = 12, class = "custom_well_for_controls",
                                                     fluidRow(column(
                                                             width = 12,
                                                             
                                                             disabled(checkboxInput(
                                                                     "macos_font_check", "Arial Unicode MS", TRUE
                                                             ))
                                                     )))
                                        
                                )
                        )
                ),
                
                column(
                        width = 9,
                        offset = 0,
                        fluidRow(
                                # graph type tabs
                                uiOutput("vis_plot_ui"),
                                uiOutput("plot_height_ui"),
                                uiOutput("graph_summary_ui"),
                                uiOutput("graph_legend_ui"),
                                
                                # graph info and download buttons
                                sidebarPanel(
                                        id = "graph_info_well",
                                        width = 12,
                                        class = "custom_well_for_buttons",
                                        fluidRow(
                                                div(shinyjs::disabled(
                                                        checkboxInput('expand_data_desc_check', label = NULL, FALSE)
                                                ), class = "div_inline"),
                                                div(textOutput("graph_name"), class = "div_inline",
                                                    style = "margin-bottom:10px;"),
                                                div(
                                                        disabled(
                                                                downloadButton(
                                                                        "analysis_graphml_download_button",
                                                                        label = "Graphml",
                                                                        title = "Download Plot Graphml File"
                                                                )
                                                        ),
                                                        style = "float:right; margin-right:10px;",
                                                        class = "div_inline"
                                                ),
                                                div(
                                                        disabled(
                                                                downloadButton("graph_download_button",
                                                                               label = "Plot HTML",
                                                                               title = "Download Plot as HTML File")
                                                        ),
                                                        style = "float:right; margin-right:10px;",
                                                        class = "div_inline"
                                                )
                                        ),
                                        
                                        fluidRow(
                                                conditionalPanel(condition = 'input.expand_data_desc_check',
                                                                 div(htmlOutput("graph_desc")))
                                        )
                                        
                                )
                        )
                )
        ),
        
        fluidRow(
                # graph data table
                tabBox(
                        width = 12,
                        title = "Graph Data",
                        selected = "Vertices",
                        id = "selected_dt_tab",
                        tabPanel(
                                "Vertices",
                                fluidRow(
                                        div(
                                                checkboxInput("graph_dt_v_truncate_text_check", "Truncate text", TRUE),
                                                style = "margin-left:12px; margin-right:5px;",
                                                class = "div_inline"
                                        )
                                ),
                                DT::dataTableOutput("dt_vertices"),
                                fluidRow(
                                        column(
                                                width = 4,
                                                selectInput(
                                                        "pruned_vertices_select",
                                                        "Pruned Nodes",
                                                        choices = c(),
                                                        multiple = TRUE,
                                                        selectize = FALSE
                                                ),
                                                div(
                                                        actionButton("prune_return_button", "Un-prune Selected"),
                                                        style = "margin-right:10px;",
                                                        class = "div_inline"
                                                ),
                                                div(actionButton("prune_reset_button", "Reset"), class = "div_inline")
                                        ),
                                        column(
                                                width = 1,
                                                actionButton("prune_deselect_rows_button", "Deselect All"),
                                                actionButton("prune_selected_rows_button", "Prune Selected"),
                                                actionButton("prune_unselected_rows_button", "Prune Unselected")
                                        )
                                )
                        ),
                        tabPanel("Edges",
                                 fluidRow(
                                         div(
                                                 checkboxInput("graph_dt_e_truncate_text_check", "Truncate text", TRUE),
                                                 style = "margin-left:12px; margin-right:5px;",
                                                 class = "div_inline"
                                         )
                                 ),
                                 DT::dataTableOutput("dt_edges"))
                )
        ))
#### end network_graphs_tab
