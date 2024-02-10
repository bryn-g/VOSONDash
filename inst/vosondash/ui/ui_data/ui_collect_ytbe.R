tabItem(tabName = "ytbe_collection_tab",
  fluidRow(
    column(width = 3,
           offset = 0,
           
           # youtube api key
           fluidRow(
             sidebarPanel(
               width = 12,
               class = "custom_well_for_controls",
               checkboxInput('expand_youtube_keys_panel_check', 'Show API Key', FALSE),
               conditionalPanel(
                 condition = 'input.expand_youtube_keys_panel_check',
                 textInput("youtube_api_key_input", label = "Data API Key", value = "")
               )
             ),
             
             # collect tab set
             tabBox(
               title = NULL,
               id = "youtube_control_tabset",
               width = 12,
               
               # collect tab panel
               tabPanel(
                 "Collect Data",
                 
                 # video url input
                 div(tags$b("Add Youtube URL"),
                     po_info(i_ytbe_video_url),
                     style = "margin-bottom:5px;"),
                 textAreaInput(
                   "youtube_video_id_input",
                   label = NULL,
                   value = "",
                   width = NULL,
                   height = NULL,
                   cols = NULL,
                   rows = 2,
                   placeholder = NULL,
                   resize = "vertical"
                 ),
                 actionButton("youtube_add_video_id_button", label = "Add"),
                 selectInput(
                   "youtube_video_id_list_output",
                   "",
                   c(),
                   multiple = TRUE,
                   selectize = FALSE,
                   size = 3
                 ),
                 actionButton("youtube_remove_video_id_button", label = "Remove"),
                 
                 # number of comments
                 ui_inline_input(
                   "Max Comments",
                   numericInput(
                     "youtube_max_comments_input",
                     label = NULL,
                     value = 200,
                     min = 1,
                     width = "90px"
                   )
                 ),
                 
                 p(""),
                 disabled(
                   actionButton(
                     "youtube_collect_button",
                     label = "Collect Comments",
                     icon = icon("cloud-arrow-down")
                   )
                 )
               ),
               # end tabPanel
               
               # create network tab
               tabPanel(
                 "Create Network",
                 div(tags$b("Network")),
                 selectInput(
                   "youtube_network_type_select",
                   label = NULL,
                   choices = c("activity", "actor"),
                   multiple = FALSE
                 ),
                 conditionalPanel(
                   condition = "input.youtube_network_type_select == 'activity' ||
                input.youtube_network_type_select == 'actor'",
                checkboxInput("youtube_network_text", "Add Text", FALSE)
                 ),
                conditionalPanel(
                  condition = "input.youtube_network_type_select == 'actor' &&
                input.youtube_network_text == 1",
                checkboxInput(
                  "youtube_network_replies_from_text",
                  "Find Replies in Text",
                  FALSE
                )
                ),
                conditionalPanel(
                  condition = "input.youtube_network_type_select == 'actor'",
                  checkboxInput("youtube_network_video_data", "Add Video Details", FALSE)
                ),
                conditionalPanel(
                  condition = "input.youtube_network_type_select == 'actor' &&
                input.youtube_network_video_data == 1",
                checkboxInput("youtube_network_video_subs", "Only replace Video ID's", FALSE)
                ),
                p(""),
                disabled(
                  actionButton(
                    "youtube_create_button",
                    label = "Create Network",
                    icon = icon("share-nodes")
                  )
                )
                
               ) # end tabPanel
             ) # end tabBox
           ) # end fluidRow
    ), # end column
    
    # console
    column(width = 9, offset = 0,
      fluidRow(
        ui_console_tabbox(
          title = ui_console_title("Youtube Network Collection", "youtube", "youtube_red", "ytbe_console_clear_btn"),
          id_params_out = "ytbe_collect_params_output",
          id_console = "ytbe_console"
        ),
        ui_collect_data_btns("youtube")
      )
    )
  ), # end fluidRow
        
  fluidRow(
    # collect data table
    tabBox(width = 12, title = "Youtube Data",
      tabPanel("Results", 
        fluidRow(
          div(
            checkboxInput('expand_show_youtube_cols', 'Column filters', FALSE),
              style = "margin-left:12px; margin-right:5px;",
            class = "div_inline"
          ),                            
          div(checkboxInput("dt_youtube_truncate_text_check", "Truncate text", TRUE), class = "div_inline")
        ),
        uiOutput("youtube_data_cols_ui"),                          
        DT::dataTableOutput("dt_youtube_data"))
      )
    ) # end fluidRow
) # end tabItem
