tabItem(tabName = "reddit_collection_tab",
        fluidRow(
          column(width = 4, offset = 0,
                 fluidRow(
                   tabBox(title = NULL,
                          id = "reddit_control_tabset",
                          width = 12,
                          tabPanel(
                            "Collect Data",
                            div(tags$b("Add Reddit URL"),
                                po_info(i_rddt_thread_url),
                                style = "margin-bottom:5px;"),
                            
                            div(
                              textInput("rd_url_input", label = NULL, value = ""),
                              div(tags$b("Sort comments"), class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                              div(
                                selectInput(
                                  "rd_sort_select",
                                  label = NULL,
                                  choices = c(
                                    "not set" = NA_character_,
                                    "best" = "best",
                                    "top" = "top",
                                    "new" = "new",
                                    "controversial" = "controversial",
                                    "old" = "old",
                                    "qa" = "qa"
                                  ),
                                  multiple = FALSE,
                                  selected = "not set",
                                  width = "100px"
                                ),
                                class = "div_inline"
                              )
                            ),
                            actionButton("rd_add_url_button", label = "Add"),
                            conditionalPanel(
                              div(tags$b("Thread URLs"), class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                              condition = "output.rd_urls_table_toggle",
                              DTOutput("rd_urls_table"),
                              div(actionButton("rd_remove_url_button", label = "Remove"), style = "padding-top:10px;")
                            ),
                            p(""),
                            disabled(
                              actionButton(
                                "reddit_collect_button",
                                label = "Collect Threads",
                                icon = icon("cloud-arrow-down")
                              )
                            )
                            
                          ), # end tabPanel
                          tabPanel(
                            "Create Network",
                            div(tags$b("Network")),
                            selectInput(
                              "reddit_network_type_select",
                              label = NULL,
                              choices = c("activity", "actor"),
                              multiple = FALSE
                            ),
                            conditionalPanel(
                              condition = "input.reddit_network_type_select == 'activity' ||
                                                        input.reddit_network_type_select == 'actor'",
                              checkboxInput("reddit_network_text", "Add Text", FALSE)
                            ),
                            p(""),
                            disabled(
                              actionButton(
                                "reddit_create_button",
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
                           "clear_reddit_console",
                           label = icon("erase", lib = "glyphicon"),
                           style = "padding: 2px 8px;",
                           title = "Clear Console"
                         ),
                         style = "padding-right: 10px;"
                       ),
                       span(
                         icon("reddit", class = "reddit_red"),
                         "Reddit Network Collection"
                       )
                     ),
                     tabPanel(
                       "Console",
                       width = 12,
                       verbatimTextOutput("reddit_arguments_output"),
                       
                       # reddit collect console
                       pre(id = "reddit_console", style = "height: 300px; overflow-y: scroll")
                     )
                   ),
                   
                   ui_collect_data_btns("reddit")
                 ))
        ),
        # reddit collection data table
        fluidRow(
          tabBox(
            width = 12,
            title = "Reddit Data",
            tabPanel(
              "Results",
              fluidRow(
                div(
                  checkboxInput("expand_show_reddit_cols", "Column filters", FALSE),
                  style = "margin-left:12px; margin-right:5px;",
                  class = "div_inline"
                ),
                div(
                  checkboxInput("dt_reddit_truncate_text_check", "Truncate text", TRUE),
                  class = "div_inline"
                )
              ),
              uiOutput("reddit_data_cols_ui"),
              DT::dataTableOutput("dt_reddit_data")
            )
          ))
)