tabItem(tabName = "keys_tab",
        fluidRow(column(width = 3,
                        source(
                          "ui/auth/ui_load_auth_files.R"
                        )$value),
                 column(width = 9,
                        fluidRow(
                          tabBox(
                            width = 12,
                            title = div(span(icon("key"), "API Keys")),
                            id = "selected_keys_tab",
                            tabPanel("Youtube",
                                     fluidRow(column(
                                       width = 12,
                                       source("ui/auth/ui_ytbe_api_key.R")$value
                                     )),
                                     value = "youtube_keys_tab"),
                            
                            tabPanel("Logs",
                                     fluidRow(
                                       column(
                                         width = 12,
                                         h4("Keys & Token Log"),
                                         verbatimTextOutput('api_keys_log_output', placeholder = TRUE)
                                       )
                                     ),
                                     value = "log_keys_tab")
                          )
                        ))))