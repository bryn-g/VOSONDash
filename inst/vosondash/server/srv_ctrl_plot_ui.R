output$ctrl_plot_ui <- renderUI({
  fluidRow(
    column(width = (12-g_plot_rv$width), offset = 0,
           fluidRow(
             tabBox(
               width = 12,
               selected = "Files",
               id = "selected_graph_controls_tab",
              source("ui/ui_graph/ui_tab_files.R")$value,
              # source("ui/ui_graph/ui_tab_visuals.R", local = TRUE),
              source("ui/ui_graph/ui_tab_visuals.R")$value,
              source("ui/ui_graph/ui_tab_filters.R")$value,
              source("ui/ui_graph/ui_tab_select.R")$value
             )
           )),
    column(
      width = g_plot_rv$width,
      offset = 0,
      fluidRow(
        # graph type tabs
        uiOutput("vis_plot_ui"),
        uiOutput("graph_summary_ui"),
        uiOutput("graph_dl_button_ui"),
        uiOutput("graph_legend_ui")
      )
    )
  )
})

output$resize_plot_slider_ui <- renderUI({
  req(g_plot_rv$width)
  fluidRow(
    column(
      width = 12,
      disabled(sliderInput(
        "plot_col_size",
        "Canvas width",
        min = 1,
        max = 11,
        value = g_plot_rv$width,
        step = 1
      )), 
      disabled(actionButton("plot_col_size_btn", label = "reset interface"))))
})
