# voson dashboard shiny app ui

ui <-
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      useShinyjs(),
      extendShinyjs(text = disable_tab_jscode, functions = c("disableTab")),
      inlineCSS(disable_tab_css),
      
      extendShinyjs(text = gbl_scroll_console, functions = c("scroll_console")),
      # custom ui stylesheet
      tags$head(
        tags$script(src = "popper.js"),
        tags$script(
          HTML(
            "$(function () { $('[data-toggle = \"popover\"]').popover() })
             $('.popover-dismiss').popover({ trigger: 'focus' })"
          )
        ),
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      
      navbarPage(
        id = "navbarpage_selection",
        selected = "data_tab",
        title = div(
          tags$img(
            src = "vosonlab_48px.png",
            height = "40px",
            style = "margin-right:5px;"
          ),
          "VOSON Dashboard"
        ),
        windowTitle = "VOSON Dash",
        inverse = TRUE,
        tabPanel(
          "Network Data",
          value = "data_tab",
          icon = icon("database"),
          source("ui/networkDataUI.R")$value
        ),
        tabPanel(
          "Network Graphs",
          value = "network_graphs_tab",
          icon = icon("share-nodes"),
          source("ui/networkGraphsUI.R")$value
        ),
        navbarMenu(
          "Analysis",
          icon = icon("microscope"),
          "Network",
          tabPanel(
            "Metrics",
            value = "network_metrics_tab",
            icon = icon("ruler-combined"),
            source("ui/networkMetricsUI.R")$value
          ),
          tabPanel(
            "Assortativity",
            value = "assortativity_tab",
            icon = icon("braille"),
            source("ui/assortativityUI.R")$value
          ),
          "---",
          "Text",
          tabPanel(
            "Word Frequency",
            value = "word_freq_tab",
            icon = icon("arrow-up-wide-short"),
            source("ui/textAnalysisUI.R")$value
          )
        ),
        navbarMenu(
          "Collection",
          icon = icon("cloud-arrow-down"),
          "API Access",
          tabPanel("Keys",
                   icon = icon("key"),
                   source("ui/apiKeysUI.R")$value),
          "---",
          "Social Media",
          tabPanel(
            "Twitter",
            value = "twitter_ui",
            icon = icon("twitter"),
            source("ui/twitterUI.R")$value
          ),
          tabPanel(
            "Youtube",
            value = "youtube_ui",
            icon = icon("youtube"),
            source("ui/youtubeUI.R")$value
          ),
          tabPanel(
            "Reddit",
            value = "reddit_ui",
            icon = icon("reddit"),
            source("ui/redditUI.R")$value
          )
        )
        
      )
      
    )
  )
