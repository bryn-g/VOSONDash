#### network graphs tab  --------------------------------------------------------------------------------------------- #
tabItem(tabName = "network_graphs_tab",
        fluidRow(
                column(
                        width = 3,
                        offset = 0,
                        
                        fluidRow(
                                #column(width = 12,
                                tabBox(
                                        title = NULL,
                                        id = "graph_controls_tabset",
                                        width = 12,
                                        source("ui/ui_graph_nodes.R")$value,
                                        source("ui/ui_graph_edges.R")$value,
                                        source("ui/ui_graph_layout.R")$value
                                )#)
                        ),
                                
                        fluidRow(
                                #column(width = 12,
                                       tabBox(
                                               title = NULL,
                                               id = "node_filters_tabset",
                                               width = 12,
                                               tabPanel(
                                                       "Components",
                                                       source("ui/ui_comp_filter.R")$value),
                                               tabPanel(
                                                       "Neighbourhood",
                                                       source("ui/ui_nbh_filter.R")$value),
                                               tabPanel(
                                                       "Categorical",
                                                       source("ui/ui_cat_filter.R")$value)   
                                       )),#),
                                
                        
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
                ), # end col width 3
                #),
        #fluidRow(
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
                                                div(#shinyjs::disabled(
                                                        prettyToggle(
                                                                inputId = 'expand_data_desc_check',
                                                                label_on = "",
                                                                label_off = "",
                                                                status_off = "info",
                                                                outline = TRUE,
                                                                plain = TRUE,
                                                                icon_on = icon("angle-double-down"),
                                                                icon_off = icon("angle-double-right")
                                                        #),
                                                        
                                                        # checkboxInput('expand_data_desc_check', label = NULL, FALSE)
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
        
        
        
        ))#)
#### end network_graphs_tab
