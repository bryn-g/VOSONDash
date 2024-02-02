tabPanel("Assortativity",
         icon = icon("braille"),
         fluidRow(
           box(
             width = 5,
             title = "Assortativity",
             verbatimTextOutput("assortativity_details_output", placeholder = FALSE),
             h4("Homophily Index"),
             verbatimTextOutput("assortativity_homophily_output", placeholder = FALSE)
           ),
           box(
             width = 7,
             title = "Mixing Matrix",
             verbatimTextOutput("mixing_matrix_details_output", placeholder = FALSE),
             DT::dataTableOutput("mixing_matrix")
           )
         ))