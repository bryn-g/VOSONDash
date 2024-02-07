tabPanel("Canvas", icon = icon("chess-board"),
         # uiOutput("resize_plot_slider_ui"),
         fluidRow(
           column(
             width = 12,
             sliderInput(
               "plot_height",
               "Canvas height (px)",
               min = 250,
               max = 1400,
               value = 525,
               step = 25
             ),
             tags$h5("Overlay"),
             disabled(checkboxInput("overlay_summary_chk", "summary", TRUE)),
             disabled(
               checkboxInput("overlay_dl_btns_chk", "download buttons", TRUE)
             ),
             disabled(checkboxInput("overlay_legend_chk", "legend", TRUE))
           )
         ))