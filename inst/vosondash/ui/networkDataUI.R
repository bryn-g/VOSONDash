tabItem(tabName = "network_data_tab",
        fluidRow(
                column(
                        width = 3,
                        offset = 0,
                        source("ui/ui_load_file.R")$value,
                        fluidRow(box(width = 12, solidHeader = TRUE,
                        
                       fluidRow(
                         column(
                           width = 12,
                           div("Prune Nodes", class = "vdash-div-box-heading"),
                           actionButton("prune_selected_rows_button", label = div(icon("scissors"), "Selected")),
                           actionButton("prune_unselected_rows_button", label = div(icon("scissors"), "Un-selected")),
                           actionButton("prune_deselect_rows_button", "Deselect All"))
                       ),
                                     
                        fluidRow(
                          column(
                            width = 12,
                            selectInput(
                              "pruned_vertices_select",
                              NULL,
                              choices = c(),
                              multiple = TRUE,
                              size = 10,
                              selectize = FALSE
                        ))),
                        fluidRow(
                          column(
                            width = 12,
                              actionButton("prune_return_button", label = div(icon("arrow-right"), "Return List Selected")),
                              actionButton("prune_reset_button", "Return All"))
                          )
                        ))   
                ),
                
                column(
                        width = 9,
                        offset = 0,
                        source("ui/ui_graph_data.R")$value,
                )
        )
)
