source("ui/ui_utils.R")
source("ui/ui_tips.R")

ui <-
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      useShinyjs(),
      extendShinyjs(text = gbl_disable_tab_js, functions = c("disableTab")),
      inlineCSS(gbl_disable_tab_css),
      
      extendShinyjs(text = gbl_scroll_console, functions = c("scroll_console")),
      # custom ui stylesheet
      tags$head(
        tags$script(src = popover_js$file),
        tags$script(popover_js$script),
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      
      navbarPage(
        id = "nav_sel_tab_id",
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
        navbarMenu(
          "Network Analysis",
          icon = icon("globe"),
          tabPanel(
            "Network Graph",
            value = "network_graphs_tab",
            icon = icon("circle-nodes"),
            source("ui/ui_network_graph.R")$value
          )
        ),
        navbarMenu(
          "Text Analysis",
          icon = icon("quote-right"),
          tabPanel(
            "Word Frequency",
            value = "word_freq_tab",
            icon = icon("arrow-up-wide-short"),
            source("ui/ui_network_text.R")$value
          )
        ),
        navbarMenu(
          "Collection",
          icon = icon("cloud-arrow-down"),
          "Social Media",
          tabPanel(
            "Mastodon",
            value = "mastodon_ui",
            icon = icon("mastodon"),
            source("ui/ui_collect_mtdn.R")$value
          ),
          tabPanel(
            "Youtube",
            value = "youtube_ui",
            icon = icon("youtube"),
            source("ui/ui_collect_ytbe.R")$value
          ),
          tabPanel(
            "Reddit",
            value = "reddit_ui",
            icon = icon("reddit"),
            source("ui/ui_collect_rddt.R")$value
          ),
          tabPanel(
            "Web",
            value = "web_ui",
            icon = icon("globe"),
            source("ui/ui_collect_web.R")$value
          )
        ),
        navbarMenu(
          title = NULL,
          icon = icon("pen-to-square"),
          tabPanel(
            "Logging",
            value = "dash_logger_tab",
            icon = icon("pen"),
            source("ui/ui_console_log.R")$value
          )
        )
      )
      
    )
  )
