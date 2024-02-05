# div style for inline input
ui_inline_input <- function(label, input) {
  div(
    div(label, class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
    div(input, class = "div_inline")
  )
}

# div style for console title
ui_console_title <- function(title, ico, ico_color_cls, id_clear_btn) {
  div(
    span(
      actionButton(id_clear_btn, label = icon("erase", lib = "glyphicon"), 
        style = "padding: 2px 8px;", title = "Clear Console"),
      style = "padding-right: 10px;"
    ),
    span(icon(ico, class = ico_color_cls), title)
  )
}

# collect console
ui_console_tabbox <- function(title, id_params_out, id_console, height = "300") {
  tabBox(
    width = 12,
    title = title,
    tabPanel("Console", width = 12,
      verbatimTextOutput(id_params_out),
      pre(id = id_console, style = paste0("height:", height, "px;overflow-y:scroll"))
    )
  )
}

# collect data action buttons
ui_collect_data_btns <- function(id) {
  sidebarPanel(
    width = 12,
    class = "custom_well_for_buttons",
    fluidRow(
      collect_data_btns_ui(id),
      collect_network_btns_ui(id),
      collect_graph_btns_ui(id),
      collect_view_graph_btns_ui(id)
    )
  )  
}