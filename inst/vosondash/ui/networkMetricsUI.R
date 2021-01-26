#### network metric tab  ---------------------------------------------------------------------------------------------- #
tabItem(tabName = "network_metrics_tab",
        fluidRow(
          box(
            width = 6,
            title = "Network Metrics",
            verbatimTextOutput("network_metrics_details_output", placeholder = TRUE)
          ),
          box(
            width = 6,
            title = "Distribution",
            selectInput("metrics_distrib_sel",
                        label = NULL,
                        choices = c("component", "indegree", "outdegree", "degree"),
                        selected = "component"),
            plotOutput("metrics_distrib_plot")
          )
        )
)
#### end network_metrics_tab