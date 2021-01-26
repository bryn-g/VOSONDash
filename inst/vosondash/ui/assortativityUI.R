#### assortativity tab  ----------------------------------------------------------------------------------------------- #
tabItem(tabName = "assortativity_tab",
        fluidRow(
          box(
            width = 6,
            title = "Assortativity",
            verbatimTextOutput("assortativity_details_output"),
            h4("Homophily Index"),
            verbatimTextOutput("assortativity_homophily_output", placeholder = TRUE)),
          box(
            width = 6,
              title = "Mixing Matrix",
              uiOutput("mm_message_output"),
              # verbatimTextOutput("mixing_matrix_details_output", placeholder = TRUE),
              DT::dataTableOutput("mixing_matrix")
          ))
)
#### end assortativity tab