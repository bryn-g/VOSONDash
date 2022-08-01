#### api keys tab ----------------------------------------------------------------------------------------------------- #
tabItem(tabName = "keys_tab",
        fluidRow(
          column(width = 3,
                 
                 fluidRow(
                   box(
                     width = 12,
                     solidHeader = TRUE,
                     h4("Saved Keys"),
                     textOutput("user_keys_path"),
                     checkboxInput(
                       'load_and_use_keys_check',
                       'Load and Use API Keys on app start',
                       FALSE
                     ),
                     actionButton("keys_load_button", label = "Load Keys"),
                     disabled(actionButton("keys_save_button", label = "Save Keys"))
                   )
                 ),
                 fluidRow(
                   box(
                     width = 12,
                     solidHeader = TRUE,
                     h4("Saved Tokens"),
                     textOutput("user_tokens_path"),
                     br(),
                     actionButton("tokens_load_button", label = "Load Tokens"),
                     disabled(actionButton("tokens_save_button", label = "Save Tokens"))
                   )
                 )),
          
          column(width = 9, offset = 0,
                 fluidRow(
                   tabBox(
                     width = 12,
                     title = span(icon("key"), "API Keys"),
                     id = "selected_keys_tab",
                     tabPanel("Twitter",
                              fluidRow(
                                column(
                                  width = 6,
                                  offset = 0,
                                  fluidRow(
                                    column(
                                      width = 12,
                                      offset = 0,
                                      div(
                                        h4("Twitter API User Access Tokens (v1.1)"),
                                        p(
                                          "User Tokens require an App name, Consumer Key, Consumer Secret, a twitter user account and
                                                                               a web browser that allows new tabs to be opened. The user will be asked to log into
                                                                               twitter and authorize the app before the token can be created."
                                        ),
                                        p(
                                          "Dev Tokens require an App Name and all four API Keys as found in their twitter
                                                                               developer app settings."
                                        )
                                      ),
                                      style = "margin-left:10px"
                                    ),
                                    #),
                                    
                                    # ---
                                    sidebarPanel(
                                      width = 12,
                                      class = "custom_well_for_keys",
                                      h4(
                                        icon("coins", class = "twitter_blue"),
                                        "Bearer Token"
                                      ),
                                      textInput(
                                        "keys_bearer_token_name_input",
                                        label = "Name",
                                        value = ""
                                      ),
                                      textInput(
                                        "keys_bearer_token_input",
                                        label = "Bearer Token",
                                        value = ""
                                      ),
                        
                                      fluidRow(
                                        div(
                                          actionButton(
                                            "create_bearer_token",
                                            "Create Token",
                                            icon("compass-drafting")
                                          )
                                        , style = "display:inline-block;float:right;margin-right:15px;margin-left:5px;"),
                                        style = "padding-bottom:0px; margin-bottom:0px"
                                      ),

                                      style = "margin-left:10px"
                                    ),
                                    
                                    # ---
                                    sidebarPanel(
                                      width = 12,
                                      class = "custom_well_for_keys",
                                      h4(
                                        icon("coins", class = "twitter_blue"),
                                        "Create Token (OAuth 1.0a)"
                                      ),
                                      textInput(
                                        "keys_twitter_app_name_input",
                                        label = "App Name",
                                        value = ""
                                      ),
                                      textInput(
                                        "keys_twitter_api_key_input",
                                        label = "Consumer Key (API Key)",
                                        value = ""
                                      ),
                                      textInput(
                                        "keys_twitter_api_secret_input",
                                        label = "Consumer Secret (API Secret)",
                                        value = ""
                                      ),
                                      
                                      fluidRow(
                                        div(
                                          checkboxInput(
                                            "web_auth_check",
                                            div(
                                              "Note: Incomplete process will end session",
                                              vpopover(po_web_auth()$title, po_web_auth()$content),
                                              class = "div_inline"
                                            ),
                                            value = FALSE,
                                            width = NULL
                                          ),
                                          style = "display:inline-block; margin-left:15px;"
                                        ),
                                        div(disabled(
                                          actionButton(
                                            "create_web_auth_token",
                                            "Create User Token",
                                            icon("compass-drafting")
                                          )
                                        ), style = "display:inline-block;float:right;margin-right:15px;margin-left:5px;"),
                                        style = "padding-bottom:0px; margin-bottom:0px"
                                      ),
                                      
                                      textInput(
                                        "keys_twitter_access_token_input",
                                        label = "Access Token",
                                        value = ""
                                      ),
                                      textInput(
                                        "keys_twitter_access_token_secret_input",
                                        label = "Access Token Secret",
                                        value = ""
                                      ),
                                      fluidRow(div(
                                        actionButton(
                                          "create_app_token",
                                          "Create Dev Token",
                                          icon("compass-drafting"),
                                          style = "float:right;margin-right:15px;padding-bottom:2px"
                                        )
                                      ))
                                      ,
                                      style = "margin-left:10px"
                                    ),
                                    sidebarPanel(
                                      width = 12,
                                      class = "custom_well_for_keys",
                                      h4("Created Token"),
                                      verbatimTextOutput("save_token_output"),
                                      disabled(actionButton("save_token", "Add Token to Select List"))
                                      ,
                                      style = "margin-left:10px"
                                    )
                                  )
                                ),
                                
                                column(
                                  width = 6,
                                  offset = 0,
                                  fluidRow(
                                    column(
                                      width = 12,
                                      offset = 0,
                                      div(p(
                                        "Select the token to use for data collection."
                                      )
                                      , style = "margin-left:10px")
                                    ),
                                    sidebarPanel(
                                      width = 12,
                                      class = "custom_well_for_controls",
                                      style = "padding-bottom: 5px",
                                      h4(icon("twitter", class = "twitter_blue"), "Select Token"),
                                      selectInput(
                                        "twitter_token_select",
                                        "Select twitter token",
                                        c("None"),
                                        selected = NULL,
                                        width = 340,
                                        size = NULL
                                      ),
                                      textOutput("twitter_set_token", container = span, inline = FALSE),
                                      fluidRow(
                                        actionButton("use_selected_token", "Use Token", icon("copy"), style = "margin-left:15px;"),
                                        actionButton(
                                          "delete_selected_token",
                                          "Delete",
                                          icon("circle-minus"),
                                          style = "float:right;margin-right:15px;"
                                        )
                                      )
                                      ,
                                      style = "margin-left:15px; margin-right:10px"
                                    )
                                  )
                                )
                                
                              ), value = "twitter_keys_tab"),
                     
                     tabPanel("Youtube",
                              fluidRow(column(
                                width = 12, offset = 0,
                                fluidRow(
                                  sidebarPanel(
                                    width = 6,
                                    class = "custom_well_for_keys",
                                    h4(icon("youtube", class = "youtube_red"), "Youtube Auth"),
                                    textInput(
                                      "keys_youtube_api_key_input",
                                      label = "Data API Key",
                                      value = ""
                                    ),
                                    actionButton("keys_youtube_populate_button", "Use Key", icon("copy"))
                                    ,
                                    style = "margin-left:10px;"
                                  )
                                )
                              )), value = "youtube_keys_tab"),
                     
                     tabPanel("Log",
                              fluidRow(
                                column(
                                  width = 12,
                                  offset = 0,
                                  
                                  h4("Log Messages"),
                                  verbatimTextOutput('api_keys_log_output', placeholder = TRUE)
                                  
                                )
                              ), value = "keys_log_tab")
                   )
                 ))
        ))
#### end keys_tab