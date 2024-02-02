tabItem(tabName = "mtdn_collection_tab",
  fluidRow(
    column(width = 4, offset = 0,

      fluidRow(
        tabBox(title = NULL, id = "mtdn_control_tabset", width = 12,
        
          tabPanel("Collect Data",
            fluidRow(
              tabBox(title = NULL, id = "mtdn_collect_tabset", width = 12,
                tabPanel("Search",
                  textInput("mtdn_search_hashtag_text", label = "Hashtag", value = "", width = "75%"),
                  textInput("mtdn_search_instance_text", label = "Instance", value = "mastodon.social", width = "50%"),
                  selectInput("mtdn_search_timeline_sel", "Timeline", list("global", "local"), width = "100px"),
                  numericInput("mtdn_search_count_num", label = "Count", value = 100, min = 1, width = "90px"),
                  p(""),
                  disabled(actionButton("mtdn_search_collect_btn", label = "Collect Posts", icon = icon("cloud-arrow-down")))     
                ),
                
                tabPanel("Threads",
                  div(tags$b("Add Mastodon URL"), style = "margin-bottom:5px;"),
                  div(textInput("mtdn_thread_url_text", label = NULL, value = "")),
                  actionButton("mtdn_thread_add_url_btn", label = "Add"),
                  conditionalPanel(
                    div(tags$b("Thread URLs"), class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                    condition = "output.mtdn_thread_url_table_toggle",
                    DTOutput("mtdn_thread_url_table"),
                    div(actionButton("mtdn_thread_rm_url_btn", label = "Remove"), style = "padding-top:10px;")
                  ),
                  p(""),
                  disabled(actionButton("mtdn_thread_collect_btn", label = "Collect Threads", icon = icon("cloud-arrow-down")))    
                )
              )
            ) # end fluidRow
          ), # end tabPanel
      
          tabPanel("Create Network",
            div(tags$b("Network")),
            selectInput("mtdn_network_type_sel", label = NULL, choices = c("activity", "actor"), multiple = FALSE),
            conditionalPanel(
              condition = "input.mtdn_network_type_sel == 'activity'",
              checkboxInput("mtdn_network_tag_chk", "Tag Network", FALSE),
            ),
            conditionalPanel(
              condition = "input.mtdn_network_type_sel == 'actor'",
              checkboxInput("mtdn_network_server_chk", "Server Network", FALSE),
            ),
            conditionalPanel(
              condition = "input.mtdn_network_tag_chk == FALSE & input.mtdn_network_server_chk == FALSE",
              checkboxInput("mtdn_network_add_text_chk", "Add Text", FALSE)
            ),
            p(""),
            disabled(actionButton("mtdn_network_create_btn", label = "Create Network", icon = icon("share-nodes")))
          ) # end tabPanel
        
        ) # end tabBox
      ) # end fluidRow
    
    ), # end column
          
    column(width = 8, offset = 0,
      fluidRow(
        tabBox(width = 12,
          title = div(
            span(actionButton("mtdn_console_clear_btn", label = icon("erase", lib = "glyphicon"), 
              style = "padding: 2px 8px;", title = "Clear Console"), style = "padding-right: 10px;"),
            span(icon("mastodon", class = "mastodon_red"), "Mastodon Network Collection")
          ),                          
          tabPanel("Console", width = 12,
            verbatimTextOutput("mtdn_params_output"),
                                   
            # mastodon collect console
            pre(id = "mtdn_console", style = "height: 300px; overflow-y: scroll")
          )
        ), # end tabBox
                   
        sidebarPanel(width = 12, class = "custom_well_for_buttons",
          fluidRow(
            collectDataButtonsUI("mastodon"),
            collectNetworkButtonsUI("mastodon"),
            collectGraphButtonsUI("mastodon"),
            collectViewGraphButtonsUI("mastodon")
          )
        )
      )
    ) # end column
  ),
        
  fluidRow(
    # mastodon collection data table
    tabBox(width = 12, title = "Mastodon Data",
      tabPanel("Posts", 
        fluidRow(
          div(
            checkboxInput('mtdn_data_posts_cols_sel_chk', 'Column select', FALSE),
            style = "margin-left:12px; margin-right:5px;", class = "div_inline"
          ),                            
          div(
            checkboxInput("mtdn_data_posts_trunc_text_chk", "Truncate text", TRUE), 
            class = "div_inline"
          )
        ),
        uiOutput("mtdn_data_posts_cols_ui"),
        DT::dataTableOutput("mtdn_data_posts_dt")
      ),
                 
      tabPanel("Users",
        fluidRow(
          div(
            checkboxInput('mtdn_data_users_cols_sel_chk', 'Column select', FALSE),
            style = "margin-left:12px; margin-right:5px;", class = "div_inline"
          ),                            
          div(
            checkboxInput("mtdn_data_users_trunc_text_chk", "Truncate text", TRUE), 
            class = "div_inline"
          )
        ),
        uiOutput("mtdn_data_users_cols_ui"),  
        DT::dataTableOutput("mtdn_data_users_dt")
      )
    )
  ) # end fluidRow

) # end tabItem
