tabItem(tabName = "web_collection_tab",
        fluidRow(
          column(width = 4, offset = 0,
                 fluidRow(
                   tabBox(
                     title = NULL,
                     id = "web_ctrl_tabset",
                     width = 12,
                     tabPanel(
                       "Collect Data",
                       
                       div(tags$b("Seed Page URL")),
                       textInput("web_url_input", label = NULL, value = ""),
                       div(
                         div(tags$b("Follow hrefs"), class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                         div(
                           selectInput(
                             "web_crawl_type_sel",
                             label = NULL,
                             choices = c(
                               "external" = "ext",
                               "internal" = "int",
                               "all" = "all"
                             ),
                             multiple = FALSE,
                             selected = "external",
                             width = "100px"
                           ),
                           class = "div_inline"
                         )
                       ),
                       div(
                         div(tags$b("Max depth"), class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                         div(
                           textInput(
                             "web_max_depth_input",
                             label = NULL,
                             value = "",
                             width = "50px"
                           ),
                           class = "div_inline"
                         )
                       ),
                       div(
                         div(tags$b("Request delay"), class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                         div(
                           checkboxInput(
                             "web_robots_delay_chk",
                             label = "Use robots.txt",
                             value = TRUE,
                             width = NULL
                           ),
                           class = "div_inline"
                         )
                       ),
                       conditionalPanel(condition = "!input.web_robots_delay_chk",
                                        div(
                                          div(
                                            tags$b("Custom delay (secs)"),
                                            class = "div_inline",
                                            style = "padding-bottom:10px;padding-right:10px;"
                                          ),
                                          div(
                                            textInput(
                                              "web_delay_input",
                                              label = NULL,
                                              value = 2,
                                              width = "50px"
                                            ),
                                            class = "div_inline"
                                          )
                                        )),
                       
                       actionButton("web_add_url_btn", label = "Add"),
                       conditionalPanel(
                         condition = "output.seed_table_toggle",
                         DTOutput("web_seed_urls_table"),
                         div(
                           actionButton("web_remove_url_btn", label = "Remove"),
                           style = "padding-top:10px;"
                         )
                       ),
                       p(""),
                       disabled(
                         actionButton(
                           "web_collect_btn",
                           label = "Collect Hyperlinks",
                           icon = icon("cloud-arrow-down")
                         )
                       )
                       
                     ),
                     # end tabPanel
                     tabPanel(
                       "Create Network",
                       div(tags$b("Network")),
                       selectInput(
                         "web_network_type_select",
                         label = NULL,
                         choices = c("activity", "actor"),
                         multiple = FALSE
                       ),
                       p(""),
                       disabled(
                         actionButton(
                           "web_create_btn",
                           label = "Create Network",
                           icon = icon("share-nodes")
                         )
                       )
                       
                     ) # end tabPanel
                   ) # end tabBox
                 ) # end fluidRow
          ), # end column
          
          column(width = 8,
                 offset = 0,
                 fluidRow(
                   tabBox(
                     width = 12,
                     title = div(
                       span(
                         actionButton(
                           "clear_web_console",
                           label = icon("erase", lib = "glyphicon"),
                           style = "padding: 2px 8px;",
                           title = "Clear Console"
                         ),
                         style = "padding-right: 10px;"
                       ),
                       span(
                         icon("globe", class = "web_green"),
                         "Hyperlink Network Collection"
                       )
                     ),
                     tabPanel(
                       "Console",
                       width = 12,
                       verbatimTextOutput("web_arguments_output"),
                       tags$head(
                         tags$style(
                           "#web_arguments_output{overflow-y:scroll; max-height: 80px;}"
                         )
                       ),
                       
                       # hyperlink collect console
                       pre(id = "web_console", style = "height: 300px; overflow-y: scroll")
                     )
                   ),
                   
                   ui_collect_data_btns("hyperlink")
                 ))
        ),
        
        fluidRow(
          # hyperlink collection data table
          tabBox(
            width = 12,
            title = "Hyperlink Data",
            tabPanel(
              "Results",
              fluidRow(
                div(
                  checkboxInput("dt_web_truncate_text_chk", "Truncate text", TRUE),
                  class = "div_inline"
                )
              ),
              DT::dataTableOutput("dt_web_data")
            )
          )
        )
)
