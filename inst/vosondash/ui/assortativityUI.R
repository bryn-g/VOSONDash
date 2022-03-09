#### assortativity tab  ----------------------------------------------------------------------------------------------- #
tabItem(tabName = "assortativity_tab",
        fluidRow(
          box(
            width = 5,
            title = "Assortativity",
            verbatimTextOutput("assortativity_details_output"),
            h4("Homophily Index"),
            verbatimTextOutput("assortativity_homophily_output", placeholder = TRUE)),
          box(
            width = 7,
              title = "Mixing Matrix",
              uiOutput("mm_message_output"),
              DT::dataTableOutput("mixing_matrix")
          ))
)
#### end assortativity tab