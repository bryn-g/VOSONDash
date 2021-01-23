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
        selected = "network_graphs_tab",
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
        
        tabPanel("Network Data",
          value = "data_tab",
          icon = icon("database"),
          source("ui/networkDataUI.R")$value
        ),
        navbarMenu(
          "Analysis",
          icon = icon("microscope"),
          "Network",
          tabPanel(
            "Graphs",
            value = "network_graphs_tab",
            icon = icon("share-alt"),
            source("ui/networkGraphsUI.R")$value
          ),
          tabPanel("Metrics",
                   icon = icon("ruler-combined"),
                   source("ui/networkMetricsUI.R")$value),
          tabPanel("Assortativity",
                   icon = icon("braille"),
                   source("ui/assortativityUI.R")$value),
          "---",
          "Text",
          tabPanel(
            "Text Analysis",
            value = "word_freq_tab",
            icon = icon("sort-amount-up"),
            source("ui/textAnalysisUI.R")$value
          )
        ),
        navbarMenu(
          "Collection",
          icon = icon("cloud-download-alt"),
          "API Access",
          tabPanel("Keys",
                   icon = icon("key"),
                   source("ui/apiKeysUI.R")$value),
          "---",
          "Social Media",
          tabPanel("Twitter",
                   icon = icon("twitter"),
                   source("ui/twitterUI.R")$value),
          tabPanel("Youtube",
                   icon = icon("youtube"),
                   source("ui/youtubeUI.R")$value),
          tabPanel("Reddit",
                   icon = icon("reddit"),
                   source("ui/redditUI.R")$value)
        )
        
      )
      
    )
  )
