tabPanel("Assortativity",
         icon = icon("braille"),
         id = "assort_tab_panel",
         fluidRow(
           box(
             width = 5,
             title = "Assortativity",
             verbatimTextOutput("assort_details", placeholder = FALSE),
             h4("Homophily Index"),
             verbatimTextOutput("homophily_details", placeholder = FALSE)
           ),
           box(
             width = 7,
             title = "Mixing Matrix",
             verbatimTextOutput("mixmat_details", placeholder = FALSE),
             DT::dataTableOutput("mixmat")
           )
         ))
