tabPanel(
  "Files",
  icon = icon("file-arrow-up"),
  fileInput(
    "graphml_data_file",
    label = "Choose graphml file",
    multiple = FALSE,
    accept = c(".graphml")
  ),
  fluidRow(column(
    width = 12,
    div(shinyjs::disabled(
      selectInput(
        "pkg_data_sel",
        label = "Package Datasets",
        choices = c("No Dataset Files Found"),
        selected = NULL,
        multiple = FALSE
      )
    )),
    div(shinyjs::disabled(
      actionButton("pkg_data_sel_btn", label = "Load graphml")
    ))
  ))
)
