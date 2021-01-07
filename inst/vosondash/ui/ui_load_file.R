sidebarPanel(width = 12,
             class = "custom_well_for_controls",
             
             fluidRow(column(
               width = 12,
               
               prettyToggle(
                 inputId = 'expand_load_file_check',
                 label_on = div(strong("Open GraphML")),
                 label_off = div(strong("Open GraphML")),
                 status_on = "default",
                 status_off = "default",
                 outline = TRUE,
                 plain = TRUE,
                 icon_on = icon("file-upload"),
                 icon_off = icon("file-upload")
               ),
               conditionalPanel(
                 condition = 'input.expand_load_file_check',
                 
                 fileInput('graphml_data_file',
                           'Choose file',
                           accept = c('.graphml')),
                 
                 prettyToggle(
                   inputId = 'expand_demo_data_check',
                   label_on = div("Package Datasets"),
                   label_off = div("Package Datasets"),
                   status_off = "info",
                   outline = TRUE,
                   plain = TRUE,
                   icon_on = icon("angle-double-down"),
                   icon_off = icon("angle-double-right")
                 ),
                 
                 conditionalPanel(condition = 'input.expand_demo_data_check',
                                  fluidRow(column(
                                    width = 12,
                                    pickerInput(
                                      inputId = "demo_data_select",
                                      label = NULL,
                                      choices = c("No Demo Dataset Files Found")
                                    ),
                                    div(shinyjs::disabled(
                                      actionButton("demo_data_select_button", label = "Load graphml")
                                    ))
                                  )))
               )
             )))
