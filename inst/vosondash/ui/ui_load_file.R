fluidRow(box(width = 12, solidHeader = TRUE,
             fluidRow(
               column(
                 width = 12,
                 div(
                   icon("file-upload",
                        class = "vdash-div-box-heading-icon"),
                   "Open GraphML",
                   class = "vdash-div-box-heading"
                 ),
                 
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
                   icon_off = icon("angle-double-right"),
                   value = FALSE
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
                                  ))),
                 
                 fluidRow(column(
                   width = 12,
                   br(),
                   pickerInput(
                     inputId = "igd_data_select",
                     label = div("igraphdata Datasets"),
                     choices = c("No Datasets Found")
                   )
                 ))
               )
             )))