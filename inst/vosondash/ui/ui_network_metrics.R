tabPanel("Metrics",
         icon = icon("ruler-combined"),
         id = "metrics_tab_panel",
         fluidRow(
           box(
             width = 5,
             title = "Network Metrics",
             verbatimTextOutput("net_metrics_txt", placeholder = TRUE)
           ),
           box(
             width = 7,
             title = "Distribution",
             selectInput(
               "metrics_distrib_sel",
               label = NULL,
               choices = c("component", "indegree", "outdegree", "degree"),
               selected = "component"
             ),
             plotOutput("metrics_distrib_plot")
           )
         ))
