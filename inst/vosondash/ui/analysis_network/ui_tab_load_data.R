tabPanel(
  "Files",
  icon = icon("file-arrow-up"),
  fileInput(
    'graphml_data_file',
    'Choose graphml file',
    accept = c('.graphml')
  ),
  fluidRow(column(
    width = 12,
    div(shinyjs::disabled(
      selectInput(
        "demo_data_select",
        label = "Package Datasets",
        choices = c("No Demo Dataset Files Found"),
        selected = NULL,
        multiple = FALSE
      )
    )),
    div(shinyjs::disabled(
      actionButton("demo_data_select_button", label = "Load graphml")
    ))
  ))
)