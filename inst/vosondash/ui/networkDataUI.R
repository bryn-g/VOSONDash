tabItem(tabName = "network_data_tab",
        fluidRow(
                column(
                        width = 3,
                        offset = 0,
                        source("ui/ui_load_file.R")$value,
                                
                ),
                
                column(
                        width = 9,
                        offset = 0,
                        source("ui/ui_graph_data.R")$value,
                )
        )
)
