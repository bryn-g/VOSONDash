tabItem(tabName = "dash_logger_tab",
        fluidRow(column(
          width = 10,
          h4("Application Log"),
          verbatimTextOutput("app_log_txt", placeholder = TRUE)
        )))
