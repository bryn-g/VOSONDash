tabItem(tabName = "dash_logger_tab",
        fluidRow(column(
          width = 12,
          h4("Application Log"),
          verbatimTextOutput("app_log_txt", placeholder = TRUE)
        )))
